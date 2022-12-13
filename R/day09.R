#' Day 09: Rope Bridge
#'
#' [Rope Bridge](https://adventofcode.com/2022/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' This rope bridge creaks as you walk along it. You aren\'t sure how old
#' it is, or whether it can even support your weight.
#'
#' It seems to support the Elves just fine, though. The bridge spans a
#' gorge which was carved out by the massive river far below you.
#'
#' You step carefully; as you do, the ropes stretch and twist. You decide
#' to distract yourself by modeling rope physics; maybe you can even figure
#' out where *not* to step.
#'
#' Consider a rope with a knot at each end; these knots mark the *head* and
#' the *tail* of the rope. If the head moves far enough away from the tail,
#' the tail is pulled toward the head.
#'
#' Due to nebulous reasoning involving [Planck
#' lengths](https://en.wikipedia.org/wiki/Planck_units#Planck_length){target="_blank"},
#' you should be able to model the positions of the knots on a
#' two-dimensional grid. Then, by following a hypothetical *series of
#' motions* (your puzzle input) for the head, you can determine how the
#' tail will move.
#'
#' [Due to the aforementioned Planck
#' lengths]{title="I'm an engineer, not a physicist!"}, the rope must be
#' quite short; in fact, the head (`H`) and tail (`T`) must *always be
#' touching* (diagonally adjacent and even overlapping both count as
#' touching):
#'
#'     ....
#'     .TH.
#'     ....
#'
#'     ....
#'     .H..
#'     ..T.
#'     ....
#'
#'     ...
#'     .H. (H covers T)
#'     ...
#'
#' If the head is ever two steps directly up, down, left, or right from the
#' tail, the tail must also move one step in that direction so it remains
#' close enough:
#'
#'     .....    .....    .....
#'     .TH.. -> .T.H. -> ..TH.
#'     .....    .....    .....
#'
#'     ...    ...    ...
#'     .T.    .T.    ...
#'     .H. -> ... -> .T.
#'     ...    .H.    .H.
#'     ...    ...    ...
#'
#' Otherwise, if the head and tail aren\'t touching and aren\'t in the same
#' row or column, the tail always moves one step diagonally to keep up:
#'
#'     .....    .....    .....
#'     .....    ..H..    ..H..
#'     ..H.. -> ..... -> ..T..
#'     .T...    .T...    .....
#'     .....    .....    .....
#'
#'     .....    .....    .....
#'     .....    .....    .....
#'     ..H.. -> ...H. -> ..TH.
#'     .T...    .T...    .....
#'     .....    .....    .....
#'
#' You just need to work out where the tail goes as the head follows a
#' series of motions. Assume the head and the tail both start at the same
#' position, overlapping.
#'
#' For example:
#'
#'     R 4
#'     U 4
#'     L 3
#'     D 1
#'     R 4
#'     D 1
#'     L 5
#'     R 2
#'
#' This series of motions moves the head *right* four steps, then *up* four
#' steps, then *left* three steps, then *down* one step, and so on. After
#' each step, you\'ll need to update the position of the tail if the step
#' means the head is no longer adjacent to the tail. Visually, these
#' motions occur as follows (`s` marks the starting position as a reference
#' point):
#'
#'     == Initial State ==
#'
#'     ......
#'     ......
#'     ......
#'     ......
#'     H.....  (H covers T, s)
#'
#'     == R 4 ==
#'
#'     ......
#'     ......
#'     ......
#'     ......
#'     TH....  (T covers s)
#'
#'     ......
#'     ......
#'     ......
#'     ......
#'     sTH...
#'
#'     ......
#'     ......
#'     ......
#'     ......
#'     s.TH..
#'
#'     ......
#'     ......
#'     ......
#'     ......
#'     s..TH.
#'
#'     == U 4 ==
#'
#'     ......
#'     ......
#'     ......
#'     ....H.
#'     s..T..
#'
#'     ......
#'     ......
#'     ....H.
#'     ....T.
#'     s.....
#'
#'     ......
#'     ....H.
#'     ....T.
#'     ......
#'     s.....
#'
#'     ....H.
#'     ....T.
#'     ......
#'     ......
#'     s.....
#'
#'     == L 3 ==
#'
#'     ...H..
#'     ....T.
#'     ......
#'     ......
#'     s.....
#'
#'     ..HT..
#'     ......
#'     ......
#'     ......
#'     s.....
#'
#'     .HT...
#'     ......
#'     ......
#'     ......
#'     s.....
#'
#'     == D 1 ==
#'
#'     ..T...
#'     .H....
#'     ......
#'     ......
#'     s.....
#'
#'     == R 4 ==
#'
#'     ..T...
#'     ..H...
#'     ......
#'     ......
#'     s.....
#'
#'     ..T...
#'     ...H..
#'     ......
#'     ......
#'     s.....
#'
#'     ......
#'     ...TH.
#'     ......
#'     ......
#'     s.....
#'
#'     ......
#'     ....TH
#'     ......
#'     ......
#'     s.....
#'
#'     == D 1 ==
#'
#'     ......
#'     ....T.
#'     .....H
#'     ......
#'     s.....
#'
#'     == L 5 ==
#'
#'     ......
#'     ....T.
#'     ....H.
#'     ......
#'     s.....
#'
#'     ......
#'     ....T.
#'     ...H..
#'     ......
#'     s.....
#'
#'     ......
#'     ......
#'     ..HT..
#'     ......
#'     s.....
#'
#'     ......
#'     ......
#'     .HT...
#'     ......
#'     s.....
#'
#'     ......
#'     ......
#'     HT....
#'     ......
#'     s.....
#'
#'     == R 2 ==
#'
#'     ......
#'     ......
#'     .H....  (H covers T)
#'     ......
#'     s.....
#'
#'     ......
#'     ......
#'     .TH...
#'     ......
#'     s.....
#'
#' After simulating the rope, you can count up all of the positions the
#' *tail visited at least once*. In this diagram, `s` again marks the
#' starting position (which the tail also visited) and `#` marks other
#' positions the tail visited:
#'
#'     ..##..
#'     ...##.
#'     .####.
#'     ....#.
#'     s###..
#'
#' So, there are *`13`* positions the tail visited at least once.
#'
#' Simulate your complete hypothetical series of motions. *How many
#' positions does the tail of the rope visit at least once?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
f09a <- function(x) {
  head_path <- f09_move_head(x)
  # message("Head moves ", length(head_path), " times")
  tail_path <- f09_move_tail(head_path)
  # message("Tail moves ", length(tail_path), " times")

  return(length(unique(tail_path)))
}


#' @rdname day09
#' @export
#' @examples
#' f09b(example_data_09(), 10)
f09b <- function(x, rope_len) {

  head_path <- f09_move_head(x)
  # message("Head moves ", length(head_path), " times")
  tail_path <- head_path
  for(i in seq(rope_len)){
    tail_path <- f09_move_tail(tail_path)
  }
  print(f09_vis(tail_path))
  return(length(unique(tail_path)))
}

#' @example
#' test <- f09_move_head(c("R 4", "U 4", "L 1"))
#'f09_vis(test)
#'f09_vis(f09_move_tail(test))
f09_vis <- function(path){

  path_i <- unlist(lapply(path,"[[",1))
  path_i <- path_i +abs(min(path_i)) +1

  path_j <- unlist(lapply(path,"[[",2))
  path_j <- path_j +abs(min(path_j)) +1

  Matrix::sparseMatrix(i = path_i,
               j = path_j,
               x= seq(length(path)))
}


#' @example
#' mh <- f09_move_head(example_data_09())
#' mt <- f09_move_tail(mh)
#' Reduce("+", mt)
f09_move_tail <- function(head_path){

  tail_path <- list(head_path[[1]]) ## Start
  tail_new <- tail_path[[1]]
  i <- 1
  while(i < length(head_path)){
    # message(i)
    # print(tail_new)
    while(!f09_test_tail(tail_new, head_path[[i]])){
      # message("Skip", i)
      i <- i + 1
      if(i > length(head_path)) break
    }
    # message("ADD", i)
    tail_new <- head_path[[i-1]]
    tail_path <- c(tail_path, list(tail_new))
  }
  return(tail_path)
}

## Check the total distance between H and T positions (m & m-1)
#' @example
#' mh <- f09_move_head(example_data_09())
#'f09_test_tail(mh[[1]],  mh[[2]])
#'f09_test_tail(mh[[1]],  mh[[3]])
#'f09_test_tail(mh[[4]],  mh[[6]]) #FALSE the turn happens
f09_test_tail <- function(g1, g2){
  x_diff <- g2[[1]] - g1[[1]]
  y_diff <- g2[[2]] - g1[[2]]
  # message("x:", x_diff, " y:", y_diff)
  return(abs(x_diff) >= 2 | abs(y_diff) >=2)
}


#' @example
#' f09_move_head(c("R 5", "U 2"))
#' mh <- f09_move_head(example_data_09())
#' Reduce("+", mh)
#' mh[4:6]
#' mh[1:3]
#'
f09_move_head <- function(move_list, g1 = f09_grid()){

  head_pos <- list(c(0,0))

  for(move in move_list){
    # new_grids <- f09_move(head_pos[[length(head_pos)]], move)
    move <- strsplit(move, " ")[[1]]
    move_d <- move[[1]]
    move_i <- as.integer(move[[2]])

    for(i in seq(move_i)){
      new_pos <- f09_move2(head_pos[[length(head_pos)]], move_d)
      head_pos <- c(head_pos, list(new_pos))
    }

  }
  return(head_pos)
}

#' @example
#' f09_move2(c(0,0), "U")
#' f09_move2(c(0,0), "D")
#' f09_move2(c(0,0), "L")
#' f09_move2(c(0,0), "R")
#' (g <- f09_move2(c(0,0), "R"))
#' (g <- f09_move2(g, "U"))
f09_move2 <- function(Hs, move_d){
  He <-list(`x` = Hs[[1]], `y` = Hs[[2]])

  if(move_d == "R"){
    He$y <- He$y + 1
  }else if(move_d == "L"){
    He$y <- He$y - 1

  }else if(move_d == "U"){
    He$x <- He$x + 1
  }else if(move_d == "D"){
    He$x <- He$x - 1
  } else stop("UNKOWN COMMAND")

  return(c(He$x, He$y))
}

#' #' @example
#' #' g <- f09_grid()
#' #' g2 <- f09_move(g, "R 4")
#' #' trail_g2 <- Reduce("+", g2)
#' #' g3 <- f09_move(g2[[length(g2)]], "U 4")
#' #' Reduce("+", c(g2, g3))
#' f09_move <- function(grid, move){
#'   # print(move)
#'   move <- strsplit(move, " ")[[1]]
#'   move_d <- move[[1]]
#'   move_i <- as.integer(move[[2]])
#'
#'   grid_list <- list(grid)
#'   for(i in seq(move_i)){
#'     new_grid <- f09_move2(grid_list[[length(grid_list)]], move_d)
#'     grid_list <- c(grid_list, list(new_grid))
#'   }
#'   return(tail(grid_list, -1))
#' }


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(
      "R 4",
      "U 4",
      "L 3",
      "D 1",
      "R 4",
      "D 1",
      "L 5",
      "R 2"
    ),
    b = c("R 5",
          "U 8",
          "L 8",
          "D 3",
          "R 17",
          "D 10",
          "L 25",
          "U 20")
  )
  l[[example]]
}
