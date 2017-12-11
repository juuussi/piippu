
base_dir <- "~/projects/piippu/"
source(base_dir %>% paste0("data_object.R"))

testdata <- as.data.frame(matrix(1:9, 3,3))
colnames(testdata) <- c("a", "b", "c")
testdata <- cbind(testdata, char=c("a","b","c"))
testdata <- asDataObject(testdata)

expect_true(is.integer(testdata@data_matrix))
expect_true(all(testdata@data_matrix == matrix(c(1:9, 1:3),3,4)))
expect_true(all(colnames(testdata) == c("a","b","c","char")))

testdata[1,3] <- 0L
testdata[,1:3] <- matrix(9:17, 3,3)
expect_true(is.integer(testdata@data_matrix))

testframe <- data.frame(asd=c("r","t","y"),stringsAsFactors=FALSE)
testdata[,c("char")] <- testframe
expect_true(is.integer(testdata@data_matrix))
expect_true(all(as.data.frame(testdata) == data.frame(matrix(c(9:17), 3,3), c("r","t","y"), stringsAsFactors=FALSE)))
expect_true(all(as.data.frame(testdata) == data.frame(matrix(c(9:17), 3,3), c("r","t","y"), stringsAsFactors=TRUE)))
expect_true(all(colnames(as.data.frame(testdata)) == c("a","b","c","char")))
expect_true(is.integer(testdata@data_matrix))

testdata[,4] <- c("c","a","o")
expect_true(all(as.data.frame(testdata) == data.frame(matrix(c(9:17), 3,3), c("c","a","o"), stringsAsFactors=FALSE)))
expect_true(all(as.data.frame(testdata) == data.frame(matrix(c(9:17), 3,3), c("c","a","o"), stringsAsFactors=TRUE)))
expect_true(is.integer(testdata@data_matrix))

testdata <- add_column(testdata, "char2", c("a","c","b"))
expect_true(is.integer(testdata@data_matrix))
