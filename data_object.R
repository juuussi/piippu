# A class that keeps data as a numeric matrix and stores separately factors and strings coded as levels.

data_object <- function(data_matrix) {
  structure(list(data_matrix = data_matrix, lvls=data.frame()), class="data_object")
}

as.data_object <- function(dataframe) {
  
  if(!is.data.frame(dataframe)) stop("Input must be a data.frame.")
  
  dataframe <- data.frame(as.list(dataframe), stringsAsFactors = TRUE)
  lvls <- lapply(dataframe[,vapply(FUN.VALUE=logical(1), dataframe, is.factor), drop=FALSE], FUN=levels)
  dataframe[, vapply(FUN.VALUE=logical(1), dataframe, is.factor)] <- lapply(dataframe[, vapply(FUN.VALUE=logical(1), dataframe, is.factor),drop=FALSE], FUN=unclass)
  data_matrix <- data.matrix(dataframe)
  
  return (structure(list(data_matrix = data_matrix, lvls = lvls ), class="data_object"))
}

# TODO: implement proper method dispatch
rename_column <- function(object, orig_col, new_col) {
  if(class(object) != "data_object") stop("object is not of type data_object")
  
  i <- which(colnames(object$data_matrix) == orig_col)
  colnames(object$data_matrix)[i] <- new_col
  i <- which(names(object$lvls) == orig_col)
  names(object$lvls)[i] <- new_col
  
  return(object)
}
