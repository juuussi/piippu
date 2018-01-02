# A class that keeps data as a numeric matrix and stores separately factors and strings coded as levels.
# This is useful because
# - C++-interface stays simple (does not deal with dataframes) and 
# - cache locality is improved (data.frames don't have a contiguous memory representation)
#   + can store matrices by-row (as they are mostly referenced by-row)

library("methods")

setClass("DataObject", 
         list(data_matrix = "matrix", lvls="list"))

DataObject <- function(data_matrix, lvls=list()) {
  
  if(!is.matrix(data_matrix) && is.vector(data_matrix)) { # Actually one-row matrix but represented as vector
    nms <- names(data_matrix)
    data_matrix <- matrix(data_matrix, nrow=1)
    colnames(data_matrix) <- nms
  }
  if(is.null(colnames(data_matrix))) stop("Cannot create a DataObject without column names.")
  temp <- new("DataObject", data_matrix = data_matrix, 
          lvls=lvls)
  return (temp)
}

asDataObject <- function(df) {
  
  if(!is.data.frame(df)) stop("Input must be a data.frame.")
  
  df <- data.frame(as.list(df), stringsAsFactors = TRUE)
  factorcols <- vapply(FUN.VALUE=logical(1), df, is.factor)
  
  lvls <- lapply(df[, factorcols, drop=FALSE], FUN=levels)
  df[, factorcols] <- lapply(df[, factorcols, drop=FALSE], FUN=unclass)
  data_matrix <- data.matrix(df)
  colnames(data_matrix) <- colnames(df)
  
  return (DataObject(data_matrix = data_matrix, lvls = lvls))
}

add_column <- function(object, name, values) {
  
  if(is.character(values)) {
    values <- factor(values)
  } 
  if(is.factor(values)) {
    object@lvls <- c(object@lvls, list(levels(values)))
    names(object@lvls)[length(object@lvls)] <- name
    object@data_matrix <- cbind(object@data_matrix, unclass(values))
  }
  else {
    if(!is.integer(values)) 
      stop("Values must be of type `integer`.")
    object@data_matrix <- cbind(object@data_matrix, values)
  }
    
  colnames(object@data_matrix)[ncol(object@data_matrix)] <- name
  
  return(object)
}

rename_column <- function(object, orig_col, new_col) {
  
  valid_arg(object, expected_class="DataObject", expected_length=1, stop_on_false=TRUE)
  
  i <- which(colnames(object@data_matrix) == orig_col)
  colnames(object@data_matrix)[i] <- new_col
  i <- which(names(object@lvls) == orig_col)
  names(object@lvls)[i] <- new_col
  
  return(object)
}

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

# filter_out_data <- function(object, column, vals) {
#   
#   valid_arg(object, expected_class="DataObject", expected_length=1, stop_on_false=TRUE)
#   valid_arg(column, expected_class="character", expected_length=1, stop_on_false=TRUE)
#   valid_arg(vals, expected_class="integer", stop_on_false=TRUE)
#   
#   # Remove rows with values in `vals`
#   object@data_matrix <- object@data_matrix[!(object@data_matrix[,column, drop=TRUE] %in% vals),]
#   # TODO: Consider if revising levels is necessary
#   #object$lvls < object$lvls[]
#   return (object)
#   
# }

getColumn <- function(x, name) {
  if(!(name %in% colnames(x@data_matrix))) return(NULL)
  if(!is.null(x@lvls[[name]])) return (factor(x@lvls[[name]][x@data_matrix[,name]], x@lvls[[name]]))
  return (x@data_matrix[,name, drop=TRUE])
}

setColumn <- function(object, i,j, value) {
  if(length(j) > 1) stop("j is of >1 length")
  if(!is.character(j)) 
    j <- colnames(object)[j]
  
  val_char <- is.factor(value) || is.character(value)
  if(!val_char && !is.integer(value)) {
    stop("Numeric values must be of type `integer`.")
  } 
  
  if(is.null(object@lvls[[ j ]]) == val_char) stop("Type mismatch")
  if(val_char) {
    # append additional levels so we don't have to change any existing values
    if(is.factor(value)) {
      val_lvls <- levels(value)
    } else {
      val_lvls <- unique(value)
    }
    object@lvls[[j]] <- c(object@lvls[[ j ]], base::setdiff(val_lvls, object@lvls[[ j ]]))
    value <- factor(value, object@lvls[[j]])
    value <- unclass(value)
    # we may have unused factor levels now, but this can be fixed when truly necessary 
  }
  object@data_matrix[i,j] <- value
  return (object)
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
setMethod(`$<-`, signature=c("DataObject"), definition=function(x, name, value) {
  if(!name %in% colnames(x)) 
  {
    object <- add_column(x, name, value)
    return (object)
  }
  else {
    x[,name] <- value
    return (x)
  }
})
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
  if(missing(i)) i <- 1:NROW(x@data_matrix)
  
  
  if(!is.data.frame(value) && !is.character(value)) {
    if(!is.integer(value)) {
      stop("Numeric values must be of type `integer`.")
    } else {
      value <- as.matrix(value)
    }
  }
  else {
    # There has to be a better way to do this
    if(is.data.frame(value))
      value <- data.frame(as.list(value), stringsAsFactors=TRUE)
    else # is character vector
      value <- data.frame(value, stringsAsFactors=TRUE)
  }
  
  if (!is.character(j)) {
    j <- colnames(x)[j]
  }
  
  fctrs <- vapply(FUN.VALUE=logical(1), 1:NCOL(value), function(z) is.factor(value[,z,drop=TRUE]))
  
  if(any(fctrs)) {
    for(k in seq_along(j)) {
      x <- setColumn(x, i,j[k], value[, k, drop=TRUE])
    }
  } else x@data_matrix[i,j] <- as.matrix(value)
  
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

setMethod("as.data.frame", signature="DataObject", definition=function(x) {
  df <- data.frame(lapply(1:NCOL(x@data_matrix), 
                    function(column) {
                      collvls <- x@lvls[[colnames(x)[column]]]
                      if(!is.null(collvls)) 
                        return (collvls[x@data_matrix[,column,drop=FALSE]])
                      else 
                        return (x@data_matrix[,column, drop=FALSE])
                    }), stringsAsFactors=FALSE)
  colnames(df) <- colnames(x)
  return(df)
})
setMethod("head", signature="DataObject", definition=function(x,n=6L) {
  return (DataObject(head(x@data_matrix, n), lvls=getLevels(x)))
})