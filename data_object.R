# A class that keeps data as a numeric matrix and stores separately factors and strings coded as levels.

setClass("DataObject", 
         list(data_matrix = "matrix", lvls="list"))

DataObject <- function(data_matrix) {
  temp <- new("DataObject", data_matrix = data_matrix, 
          lvls=list())
  return (temp)
}

asDataObject <- function(dataframe) {
  
  if(!is.data.frame(dataframe)) stop("Input must be a data.frame.")
  
  dataframe <- data.frame(as.list(dataframe), stringsAsFactors = TRUE)
  lvls <- lapply(dataframe[,vapply(FUN.VALUE=logical(1), dataframe, is.factor), drop=FALSE], FUN=levels)
  dataframe[, vapply(FUN.VALUE=logical(1), dataframe, is.factor)] <- lapply(dataframe[, vapply(FUN.VALUE=logical(1), dataframe, is.factor),drop=FALSE], FUN=unclass)
  data_matrix <- data.matrix(dataframe)
  
  return (new("DataObject", data_matrix = data_matrix, lvls = lvls))
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

filter_out_data <- function(object, column, vals) {
  
  valid_arg(object, expected_class="DataObject", expected_length=1, stop_on_false=TRUE)
  valid_arg(column, expected_class="character", expected_length=1, stop_on_false=TRUE)
  valid_arg(vals, expected_class="integer", stop_on_false=TRUE)
  
  # Remove rows with ids in `ids`
  object@data_matrix <- object@data_matrix[!(object@data_matrix[,column, drop=TRUE] %in% vals),]
  # TODO: Consider if revising levels is necessary
  #object$lvls < object$lvls[]
  return (object)
  
}