#' Day 05: Supply Stacks
#'
#' [Supply Stacks](https://adventofcode.com/2022/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' The expedition can depart as soon as the final supplies have been
#' unloaded from the ships. Supplies are stored in stacks of marked
#' *crates*, but because the needed supplies are buried under many other
#' crates, the crates need to be moved.
#'
#' The ship has a *giant cargo crane* capable of moving crates between
#' stacks. To ensure none of the crates get crushed or fall over, the crane
#' operator will move them in a series of carefully-planned steps.
#' After the crates are moved, the desired crates will be at the top
#' of each stack.
#'
#' The Elves don\'t want to interrupt the crane operator during this
#' delicate procedure, but they forgot to ask her *which* crate will end up
#' where, and they want to be ready to unload them as soon as possible so
#' they can embark.
#'
#' They do, however, have a drawing of the starting stacks of crates *and*
#' the movement procedure (your puzzle input). For example:
#'
#'         [D]
#'     [N] [C]
#'     [Z] [M] [P]
#'      1   2   3
#'
#'     move 1 from 2 to 1
#'     move 3 from 1 to 3
#'     move 2 from 2 to 1
#'     move 1 from 1 to 2
#'
#' In this example, there are three stacks of crates. Stack 1 contains two
#' crates: crate `Z` is on the bottom, and crate `N` is on top. Stack 2
#' contains three crates; from bottom to top, they are crates `M`, `C`, and
#' `D`. Finally, stack 3 contains a single crate, `P`.
#'
#' Then, the movement procedure is given. In each step of the
#' procedure, a quantity of crates is moved from one stack to a different
#' stack. In the first step of the above movement procedure, one crate
#' is moved from stack 2 to stack 1, resulting in this configuration:
#'
#'     [D]
#'     [N] [C]
#'     [Z] [M] [P]
#'      1   2   3
#'
#' In the second step, three crates are moved from stack 1 to stack 3.
#' Crates are moved *one at a time*, so the first crate to be moved (`D`)
#' ends up below the second and third crates:
#'
#'             [Z]
#'             [N]
#'         [C] [D]
#'         [M] [P]
#'      1   2   3
#'
#' Then, both crates are moved from stack 2 to stack 1. Again, because
#' crates are moved *one at a time*, crate `C` ends up below crate `M`:
#'
#'             [Z]
#'             [N]
#'     [M]     [D]
#'     [C]     [P]
#'      1   2   3
#'
#' Finally, one crate is moved from stack 1 to stack 2:
#'
#'             [Z]
#'             [N]
#'             [D]
#'     [C] [M] [P]
#'      1   2   3
#'
#' The Elves just need to know *which crate will end up on top of each
#' stack*; in this example, the top crates are `C` in stack 1, `M` in stack
#' 2, and `Z` in stack 3, so you should combine these together and give the
#' Elves the message *`CMZ`*.
#'
#' *After the movement procedure completes, what crate ends up on top
#' of each stack?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f05a(x)` returns .... For Part Two,
#'   `f05b(x)` returns ....
#' @export
#' @examples
#' f05a(example_data_05())
#' f05b(example_data_05())
f05a <- function(x) {

  input <- f05_parse_crate_input(x)
  crates <- input$crates
  # message(length(crates), " Stacks")
  # message(length(input$moves), " Moves")

  for(move in input$moves){
    crates <- f05_move_crates(crates, move)
  }

  top_crates <- paste0(rapply(crates, function(x) tail(x, 1)),collapse = "")
  crate_code <- gsub("\\[|\\]","",paste0(top_crates,collapse = ""))
  return(crate_code)
}


#' @rdname day05
#' @export
f05b <- function(x) {
  input <- f05_parse_crate_input(x)
  crates <- input$crates
  # message(length(crates), " Stacks")
  # message(length(input$moves), " Moves")

  for(move in input$moves){
    crates <- f05_move_crates_9001(crates, move)
  }

  top_crates <- paste0(rapply(crates, function(x) tail(x, 1)),collapse = "")
  crate_code <- gsub("\\[|\\]","",paste0(top_crates,collapse = ""))
  return(crate_code)
}

#' @examples
#' example_input <- f05_parse_crate_input(example_data_05())
#' test_input <- f05_parse_crate_input(x)
#' test_input$crates
f05_parse_crate_input <- function(x) {
  ## find split
  split_i <- match("", x)
  # message("split at :", split_i)

  crate_lines <- x[1:(split_i - 2)]
  crate_setup <- gsub("] " ,"],",crate_lines)
  crate_setup <- gsub("    " ,"[_],",crate_setup)

  n_crates <- nchar(crate_setup[[length(crate_setup)]])

  crate_list <- purrr::map(crate_setup, function(c){
    diff = (n_crates - nchar(c))%/%4
    # message(diff)
    c <- paste0(c, rep(",[_]",diff))
    unlist(strsplit(c,","))
  })

  crate_list2 <- purrr::transpose(crate_list)

  crate_list2 <- purrr::map(crate_list2, function(c){
    c <- rev(c)
    c <-c[c!="[_]"]
    return(unlist(c))
  })

  move <- x[(split_i + 1): length(x)]
  return(list(crates = crate_list2, moves = move))
}

#' @examples
#' (cs1 <- f05_move_crates(example_input$crates, example_input$moves[[1]]))
#' (cs2 <- f05_move_crates(cs1, example_input$move[[2]]))
#' (cs3 <- f05_move_crates(cs2, example_input$move[[3]]))
#' (cs4 <- f05_move_crates(cs3, example_input$move[[4]]))
f05_move_crates <- function(crates, move){
  ## parse move
  move_split <- strsplit(move," ")[[1]]
  n_crates <- as.integer(move_split[[2]])
  stack_start <- as.integer(move_split[[4]])
  stack_end <- as.integer(move_split[[6]])
  # message("n: ", n_crates," move: ",stack_start," -> ", stack_end)

  ## move crates
  for(i in seq(n_crates)){
    crates <- f05_move_crate(crates, stack_start, stack_end)
  }

  return(crates)
}

#' @examples
#' c2 <- f05_move_crate(example_input$crates, 2, 1)
#' c3 <- f05_move_crate(c2, 1, 3)
#' c4 <- f05_move_crate(c3, 1, 3)
#' c5 <- f05_move_crate(c4, 1, 3)
f05_move_crate <- function(crates, start, end){

  stopifnot(length(crates[[start]]) >= 1)

  cm <- crates[[start]][[length(crates[[start]])]]
  crates[[start]] <- crates[[start]][-length(crates[[start]])]
  crates[[end]] <- c(crates[[end]], cm)
  return(crates)
}

#' @examples
#' (c9_1 <- f05_move_crates_9001(example_input$crates, example_input$moves[[1]]))
#' (c9_2 <- f05_move_crates_9001(c9_1, example_input$move[[2]]))
#' (c9_3 <- f05_move_crates_9001(c9_2, example_input$move[[3]]))
#' (c9_4 <- f05_move_crates_9001(c9_3, example_input$move[[4]]))
f05_move_crates_9001 <- function(crates, move){
  ## parse move
  move_split <- strsplit(move," ")[[1]]
  n_crates <- as.integer(move_split[[2]])
  stack_start <- as.integer(move_split[[4]])
  stack_end <- as.integer(move_split[[6]])
  # message("n: ", n_crates," move: ",stack_start," -> ", stack_end)

  ## move crates
  cm <- tail(crates[[stack_start]], n_crates)
  crates[[stack_start]] <- head(crates[[stack_start]], length(crates[[stack_start]]) - n_crates)
  crates[[stack_end]] <- c(crates[[stack_end]], cm)
  return(crates)

  return(crates)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    a = c(
      "    [D]",
      "[N] [C]",
      "[Z] [M] [P]",
      "1   2   3 ",
      "",
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    )
  )
  l[[example]]
}
