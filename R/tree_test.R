# library(data.tree)
#
# filesys <- Node$new("/", t = "dir")
# filesys$AddChild("a", t = "dir")
# filesys$AddChild("b", t = "file", size = 100)
# filesys$`a`$AddChild("c", t = "file", size = 200)
#
# # filesys$size <- function(self) sum(sapply(self$children, function(x) x$size),
# #                                    na.rm = TRUE)
#
# Aggregate(node = filesys, attribute = "size", aggFun = sum)
#
# filesys$Do(function(node) node$totalSize <- Aggregate(node, attribute = "size", aggFun = sum), traversal = "post-order")
# print(filesys, "size", "totalSize")

