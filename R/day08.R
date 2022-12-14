#' Day 08: Treetop Tree House
#'
#' [Treetop Tree House](https://adventofcode.com/2022/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' The expedition comes across a peculiar patch of tall trees all planted
#' carefully in a grid. The Elves explain that a previous expedition
#' planted these trees as a reforestation effort. Now, they\'re curious if
#' this would be a good location for a [tree
#' house](https://en.wikipedia.org/wiki/Tree_house){target="_blank"}.
#'
#' First, determine whether there is enough tree cover here to keep a tree
#' house *hidden*. To do this, you need to count the number of trees that
#' are *visible from outside the grid* when looking directly along a row or
#' column.
#'
#' The Elves have already launched a
#' [quadcopter](https://en.wikipedia.org/wiki/Quadcopter){target="_blank"}
#' to generate a map with the height of each tree ([your puzzle
#' input]{title="The Elves have already launched a quadcopter (your puzzle input)."}).
#' For example:
#'
#'     30373
#'     25512
#'     65332
#'     33549
#'     35390
#'
#' Each tree is represented as a single digit whose value is its height,
#' where `0` is the shortest and `9` is the tallest.
#'
#' A tree is *visible* if all of the other trees between it and an edge of
#' the grid are *shorter* than it. Only consider trees in the same row or
#' column; that is, only look up, down, left, or right from any given tree.
#'
#' All of the trees around the edge of the grid are *visible* - since they
#' are already on the edge, there are no trees to block the view. In this
#' example, that only leaves the *interior nine trees* to consider:
#'
#' -   The top-left `5` is *visible* from the left and top. (It isn\'t
#'     visible from the right or bottom since other trees of height `5` are
#'     in the way.)
#' -   The top-middle `5` is *visible* from the top and right.
#' -   The top-right `1` is not visible from any direction; for it to be
#'     visible, there would need to only be trees of height *0* between it
#'     and an edge.
#' -   The left-middle `5` is *visible*, but only from the right.
#' -   The center `3` is not visible from any direction; for it to be
#'     visible, there would need to be only trees of at most height `2`
#'     between it and an edge.
#' -   The right-middle `3` is *visible* from the right.
#' -   In the bottom row, the middle `5` is *visible*, but the `3` and `4`
#'     are not.
#'
#' With 16 trees visible on the edge and another 5 visible in the interior,
#' a total of *`21`* trees are visible in this arrangement.
#'
#' Consider your map; *how many trees are visible from outside the grid?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b(example_data_08())
f08a <- function(x) {
  tree_matrix <- f08_get_matrix(x)
  visible_matrix <- f08_get_visible_matrix(tree_matrix)
  return(sum(visible_matrix))
}


#' @rdname day08
#' @export
f08b <- function(x) {
  tree_matrix <- f08_get_matrix(x)
  senic_matrix <- f08_get_senic_matrix(tree_matrix)
  return(max(senic_matrix))

}

#' @example
#' f08_get_matrix(example_data_08())
f08_get_matrix <- function(x) {
  n <- nchar(x[[1]])
  x <- strsplit(x, "")
  x <- as.integer(unlist(x))
  return(matrix(x, ncol = n, byrow= TRUE))
}
#' @example
#' ex <- f08_get_matrix(example_data_08())
#' f08_tree_hidden(ex[2,])
#' f08_tree_hidden(ex[3,])
#' f08_tree_hidden(ex[4,])
#' f08_tree_hidden(ex[,3])
f08_tree_hidden <- function(l){
  hidden_l <- sapply(seq(length(l)), function(i){
    if(i == 1 | i == length(l)) return(FALSE)
    spot <- l[[i]]
    look_left <- head(l, i-1)
    look_right <- tail(l, length(l)-i)
    hidden <- (spot < max(look_left)) & (spot < max(look_right))
    # message(look_left, "-", spot, "-", look_right, " = ", hidden)
    # if(hidden) message(look_left, "-", spot, "-", look_right)
    return(hidden)
  })
  return(hidden_l)
}


f08_tree_visible <- function(l){
  hidden_l <- sapply(seq(length(l)), function(i){
    if(i == 1 | i == length(l)) return(TRUE)
    spot <- l[[i]]
    look_left <- head(l, i-1)
    look_right <- tail(l, length(l)-i)
    hidden <- (spot > max(look_left)) | (spot > max(look_right))
    # message(look_left, "-", spot, "-", look_right, " = ", hidden)
    # if(hidden) message(look_left, "-", spot, "-", look_right)
    return(hidden)
  })
  return(hidden_l)
}

#' @example
#' ex <- f08_get_matrix(example_data_08())
#' f08_get_hidden_matrix(ex)
#' sum(f08_get_hidden_matrix(ex))
f08_get_hidden_matrix <- function(m){
  # message("~~ check rows ~~")
  row_hidden <- t(apply(m, 1, f08_tree_hidden))

  # message("~~ check cols ~~")
  col_hidden <- apply(m, 2, f08_tree_hidden)
  # return(col_hidden)
  return(row_hidden & col_hidden)
}

#' @example
#' ex <- f08_get_matrix(example_data_08())
#' f08_get_visible_matrix(ex)
f08_get_visible_matrix <- function(m){
  # message("~~ check rows ~~")
  row_visible <- t(apply(m, 1, f08_tree_visible))

  # message("~~ check cols ~~")
  col_visible <- apply(m, 2, f08_tree_visible)
  # return(col_hidden)
  return(row_visible | col_visible)
}

#' @example
#' ex <- f08_get_matrix(example_data_08())
#' f08_senic_score2d(ex[4,])
#' f08_senic_score2d(ex[,3])
f08_senic_score2d <- function(l){
  senic_l <- sapply(seq(length(l)), function(i){
    spot <- l[[i]]
    look_left <- head(l, i-1)
    look_right <- tail(l, length(l)-i)

    left_score <- f08_senic_1d(spot,rev(look_left))
    right_score <- f08_senic_1d(spot, look_right)
    senic_score <- left_score * right_score

    # message(look_left, "-", spot, "-", look_right, " : ", left_score,"*",right_score,"=", senic_score)
    # if(hidden) message(look_left, "-", spot, "-", look_right)
    return(senic_score)
  })
  return(senic_l)
}

#' @example
#' f08_senic_1d(5, c(4,9))
#' f08_senic_1d(5, c(3,3))
#' f08_senic_1d(5, c())
f08_senic_1d <- function(tree, other_trees){
  if(all(tree > other_trees)) return(length(other_trees))
  min(which(tree <= other_trees))
}


#' @example
#' ex <- f08_get_matrix(example_data_08())
#' f08_get_senic_matrix(ex)
f08_get_senic_matrix <- function(m){
  # message("~~ check rows ~~")
  row_senic <- t(apply(m, 1, f08_senic_score2d))

  # message("~~ check cols ~~")
  col_senic <- apply(m, 2, f08_senic_score2d)
  # return(col_hidden)
  return(row_senic * col_senic)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = c("30373",
          "25512",
          "65332",
          "33549",
          "35390"
    )
  )
  l[[example]]
}
