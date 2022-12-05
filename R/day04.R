#' Day 04: Camp Cleanup
#'
#' [Camp Cleanup](https://adventofcode.com/2022/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' Space needs to be cleared before the last supplies can be unloaded from
#' the ships, and so several Elves have been assigned the job of cleaning
#' up sections of the camp. Every section has a unique *ID number*, and
#' each Elf is assigned a range of section IDs.
#'
#' However, as some of the Elves compare their section assignments with
#' each other, they\'ve noticed that many of the assignments *overlap*. To
#' try to quickly find overlaps and reduce duplicated effort, the Elves
#' pair up and make a *big list of the section assignments for each pair*
#' (your puzzle input).
#'
#' For example, consider the following list of section assignment pairs:
#'
#'     2-4,6-8
#'     2-3,4-5
#'     5-7,7-9
#'     2-8,3-7
#'     6-6,4-6
#'     2-6,4-8
#'
#' For the first few pairs, this list means:
#'
#' -   Within the first pair of Elves, the first Elf was assigned sections
#'     `2-4` (sections `2`, `3`, and `4`), while the second Elf was
#'     assigned sections `6-8` (sections `6`, `7`, `8`).
#' -   The Elves in the second pair were each assigned two sections.
#' -   The Elves in the third pair were each assigned three sections: one
#'     got sections `5`, `6`, and `7`, while the other also got `7`, plus
#'     `8` and `9`.
#'
#' This example list uses single-digit section IDs to make it easier to
#' draw; your actual list might contain larger numbers. Visually, these
#' pairs of section assignments look like this:
#'
#'     .234.....  2-4
#'     .....678.  6-8
#'
#'     .23......  2-3
#'     ...45....  4-5
#'
#'     ....567..  5-7
#'     ......789  7-9
#'
#'     .2345678.  2-8
#'     ..34567..  3-7
#'
#'     .....6...  6-6
#'     ...456...  4-6
#'
#'     .23456...  2-6
#'     ...45678.  4-8
#'
#' Some of the pairs have noticed that one of their assignments *fully
#' contains* the other. For example, `2-8` fully contains `3-7`, and `6-6`
#' is fully contained by `4-6`. In pairs where one assignment fully
#' contains the other, one Elf in the pair would be exclusively cleaning
#' sections their partner will already be cleaning, so these seem like the
#' most in need of reconsideration. In this example, there are *`2`* such
#' pairs.
#'
#' *In how many assignment pairs does one range fully contain the other?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b(example_data_04())
f04a <- function(x) {
  sections <- purrr::map(x, f04_get_sections)
  full_overlaps <- purrr::map_lgl(sections, f04_full_overlap)
  return(sum(full_overlaps))
}


#' @rdname day04
#' @export
f04b <- function(x) {
  sections <- purrr::map(x, f04_get_sections)
  any_overlaps <- purrr::map_lgl(sections, f04_any_overlap)
  return(sum(any_overlaps))
}

#' @examples
#' f04_get_sections("2-4,6-8")
f04_get_sections <- function(x) {
  x_num <- as.numeric(stringr::str_extract_all(x, "\\d+")[[1]])
  sections <- list(seq(from = x_num[[1]], to = x_num[[2]]),
                seq(from = x_num[[3]], to = x_num[[4]]))
  return(sections)
}

#' @examples
#' f04_full_overlap(list(1:3, 2:3))
f04_full_overlap <- function(section_pair){
  overlap <- intersect(section_pair[[1]], section_pair[[2]])
  any(purrr::map_lgl(section_pair,~setequal(.x,overlap)))
}

#' @examples
#' f04_any_overlap(list(1:3, 2:3))
#' f04_any_overlap(list(1:2, 3:4))
f04_any_overlap <- function(section_pair){
  overlap <- intersect(section_pair[[1]], section_pair[[2]])
  !identical(overlap, integer(0))
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = c(
      "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    )
  )
  l[[example]]
}
