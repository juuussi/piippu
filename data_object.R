# A class that keeps data as a numeric matrix and stores separately factors and strings coded as levels.
# This is useful because
# - C++-interface stays simple (not deal with dataframes) and 
# - cache locality is improved (data.frames don't have a contiguous memory representation)
#   + can store matrices by-row (as they are mostly referenced by-row)

library("methods")

setClass("DataObject", 
         list(data_matrix = "matrix", lvls="list"))

DataObject <- function(data_matrix, lvls=list()) {
  if(is.null(colnames(data_matrix))) stop("Cannot create a DataObject without column names.")
  temp <- new("DataObject", data_matrix = data_matrix, 
          lvls=lvls)
  return (temp)
}

asDataObject <- function(dataframe) {
  
  if(!is.data.frame(dataframe)) stop("Input must be a data.frame.")
  
  dataframe <- data.frame(as.list(dataframe), stringsAsFactors = TRUE)
  lvls <- lapply(dataframe[,vapply(FUN.VALUE=logical(1), dataframe, is.factor), drop=FALSE], FUN=levels)
  dataframe[, vapply(FUN.VALUE=logical(1), dataframe, is.factor)] <- lapply(dataframe[, vapply(FUN.VALUE=logical(1), dataframe, is.factor),drop=FALSE], FUN=unclass)
  data_matrix <- data.matrix(dataframe)
  colnames(data_matrix) <- colnames(dataframe)
  
  return (DataObject(data_matrix = data_matrix, lvls = lvls))
}

add_column <- function(object, name, values) {
  if(is.factor(values)) {
    object@lvls <- c(object@lvls, levels(values))
    object@data_matrix <- cbind(object@data_matrix, unclass(values))
  }
  else 
    object@data_matrix <- cbind(object@data_matrix, values)
    
  colnames(object@data_matrix)[ncol(object@data_matrix)] <- name
  
  return(object)
}
# TODO: implement proper method dispatch
rename_column <- function(object, orig_col, new_col) {
  
  valid_arg(object, expected_class="DataObject", expected_length=1, stop_on_false=TRUE)
  
  i <- which(colnames(object@data_matrix) == orig_col)
  colnames(object@data_matrix)[i] <- new_col
  i <- which(names(object@lvls) == orig_col)
  names(object@lvls)[i] <- new_col
  
  return(object)
}

#TODO: fix to camelcase when proper dispatch exists
reorder_columns <- function(object, new_order) {
  if(!base::setequal(new_order, colnames(object@data_matrix))) 
    stop("Column names in new_order do not match internal column names.")
  
  object@data_matrix <- object@data_matrix[,new_order]
  object@lvls <- object@lvls[new_order]
  
  return(object)
  
}

combine_factors <- function(x,y) {
  valid_arg(x, expected_class="factor", stop_on_false=TRUE)
  valid_arg(y, expected_class="factor", stop_on_false=TRUE)
  
  lvls <- unique(c(levels(x),levels(y)))
  y <- match(levels(y)[y], lvls)
  
  return( factor(lvls[c(x,y)]) )
}
combine_factor_codes <- function(int_x,int_y, lvls_x, lvls_y) {
  valid_arg(int_x, expected_class="integer", stop_on_false=TRUE)
  valid_arg(int_y, expected_class="integer", stop_on_false=TRUE)
  
  lvls <- unique(c(lvls_x, lvls_y))
  int_y <- match(lvls_y[int_y], lvls)
  
  return( list(codes=c(int_x,int_y), levels=lvls) )
}

filter_out_data <- function(object, column, vals) {
  
  valid_arg(object, expected_class="DataObject", expected_length=1, stop_on_false=TRUE)
  valid_arg(column, expected_class="character", expected_length=1, stop_on_false=TRUE)
  valid_arg(vals, expected_class="integer", stop_on_false=TRUE)
  
  # Remove rows with values in `vals`
  object@data_matrix <- object@data_matrix[!(object@data_matrix[,column, drop=TRUE] %in% vals),]
  # TODO: Consider if revising levels is necessary
  #object$lvls < object$lvls[]
  return (object)
  
}

getColumn <- function(x, name) {
  if(!(name %in% colnames(x@data_matrix))) return(NULL)
  if(!is.null(x@lvls[[name]])) return (factor(x@lvls[[name]][x@data_matrix[,name]], x@lvls[[name]]))
  return (x@data_matrix[,name, drop=TRUE])
}

setColumn <- function(object, i,j, value) {
  if(length(j) > 1) stop("j is of >1 length")
  if(is.null(object@lvls[[j]]) == !is.factor(value)) stop("Type mismatch")
  if(is.factor(value)) {
    object@lvls <- c(object@lvls[[j]], setdiff(levels(value), object@lvls[[j]]))
    value <- factor(value, object@lvls)
    value <- unclass(value)
  }
  object@data_matrix[i,column] <- value
}

getLevels <- function(x) {
  valid_arg(x, expected_class="DataObject", expected_length=1, stop_on_false=TRUE)
  return(x@lvls)
}
rowbind <- function(x,y, deparse.level=0) {
  if(ncol(x@data_matrix) != ncol(y@data_matrix)) stop("number of columns of matrices must match")
  if(deparse.level != 0) stop("deparse.level!=0 is not implemented for DataObject")
  tmp <- matrix(0, nrow = nrow(x@data_matrix) + nrow(y@data_matrix), ncol=ncol(x@data_matrix))
  tmplevels <- list()
  for(i in 1:ncol(x@data_matrix)) {
    if(!is.null(x@lvls[[i]])) {
      tmpfactor <- combine_factor_codes(x@data_matrix[,i], y@data_matrix[,i], x@lvls[[i]], y@lvls[[i]])
      tmp[,i] <- tmpfactor$codes
      tmplevels <- c(tmplevels, list(tmpfactor$levels))
    } 
    else tmp[,i] <- c(x@data_matrix[,i], y@data_matrix[,i])
  }
  colnames(tmp) <- colnames(x@data_matrix)
  return (DataObject(tmp, tmplevels))
}

setMethod(`$`, signature=c("DataObject"), definition=getColumn)
setMethod(`[`, signature=c("DataObject"), definition=function(x, i, j, drop) {
  if(missing(j)) j <- colnames(x@data_matrix)
  if(missing(i)) i <- 1:nrow(x@data_matrix)
  
  if(length(j) != 1) {
    return (DataObject(data_matrix = x@data_matrix[i,j, drop=drop], lvls=x@lvls[j]))
  } else {
    if(!is.character(j)) j <- colnames(x@data_matrix)[j]
    return (getColumn(x,j)[i])
  }
})

setMethod(`[<-`, signature=c("DataObject"), definition=function(x, i, j, value) {
  if(missing(j)) j <- colnames(x@data_matrix)
  if(missing(i)) i <- 1:nrow(x@data_matrix)
  
  if (!is.character(j)) j <- colnames(x@data_matrix)[j]
  fctrs <- vapply(FUN.VALUE=logical(1), 1:ncol(value), function(z) is.factor(value[,z]))
  if(any(fctrs)) {
    for(k in seq_along(j)) setColumn(x, i,j[k], v[k])
  } else x@data_matrix[i,j] <- value
  
  return(x)
})
setMethod("nrow", signature="DataObject", definition=function(x) {
  return (NROW(x@data_matrix))
})
setMethod("ncol", signature="DataObject", definition=function(x) {
  return (NCOL(x@data_matrix))
})
setMethod("colnames", signature="DataObject", definition=function(x) {
  return (colnames(x@data_matrix))
})
