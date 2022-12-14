#' Day 07: No Space Left On Device
#'
#' [No Space Left On Device](https://adventofcode.com/2022/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' You can hear birds chirping and raindrops hitting leaves as the
#' expedition proceeds. Occasionally, you can even hear much louder sounds
#' in the distance; how big do the animals get out here, anyway?
#'
#' The device the Elves gave you has problems with more than just its
#' communication system. You try to run a system update:
#'
#'     $ system-update --please --pretty-please-with-sugar-on-top
#'     Error: No space left on device
#'
#' Perhaps you can delete some files to make space for the update?
#'
#' You browse around the filesystem to assess the situation and save the
#' resulting terminal output (your puzzle input). For example:
#'
#'     $ cd /
#'     $ ls
#'     dir a
#'     14848514 b.txt
#'     8504156 c.dat
#'     dir d
#'     $ cd a
#'     $ ls
#'     dir e
#'     29116 f
#'     2557 g
#'     62596 h.lst
#'     $ cd e
#'     $ ls
#'     584 i
#'     $ cd ..
#'     $ cd ..
#'     $ cd d
#'     $ ls
#'     4060174 j
#'     8033020 d.log
#'     5626152 d.ext
#'     7214296 k
#'
#' The filesystem consists of a tree of files (plain data) and directories
#' (which can contain other directories or files). The outermost directory
#' is called `/`. You can navigate around the filesystem, moving into or
#' out of directories and listing the contents of the directory you\'re
#' currently in.
#'
#' Within the terminal output, lines that begin with `$` are *commands you
#' executed*, very much like some modern computers:
#'
#' -   `cd` means *change directory*. This changes which directory is the
#'     current directory, but the specific result depends on the argument:
#'     -   `cd x` moves *in* one level: it looks in the current directory
#'         for the directory named `x` and makes it the current directory.
#'     -   `cd ..` moves *out* one level: it finds the directory that
#'         contains the current directory, then makes that directory the
#'         current directory.
#'     -   `cd /` switches the current directory to the outermost
#'         directory, `/`.
#' -   `ls` means *list*. It prints out all of the files and directories
#'     immediately contained by the current directory:
#'     -   `123 abc` means that the current directory contains a file named
#'         `abc` with size `123`.
#'     -   `dir xyz` means that the current directory contains a directory
#'         named `xyz`.
#'
#' Given the commands and output in the example above, you can determine
#' that the filesystem looks visually like this:
#'
#'     - / (dir)
#'       - a (dir)
#'         - e (dir)
#'           - i (file, size=584)
#'         - f (file, size=29116)
#'         - g (file, size=2557)
#'         - h.lst (file, size=62596)
#'       - b.txt (file, size=14848514)
#'       - c.dat (file, size=8504156)
#'       - d (dir)
#'         - j (file, size=4060174)
#'         - d.log (file, size=8033020)
#'         - d.ext (file, size=5626152)
#'         - k (file, size=7214296)
#'
#' Here, there are four directories: `/` (the outermost directory), `a` and
#' `d` (which are in `/`), and `e` (which is in `a`). These directories
#' also contain files of various sizes.
#'
#' Since the disk is full, your first step should probably be to find
#' directories that are good candidates for deletion. To do this, you need
#' to determine the *total size* of each directory. The total size of a
#' directory is the sum of the sizes of the files it contains, directly or
#' indirectly. (Directories themselves do not count as having any intrinsic
#' size.)
#'
#' The total sizes of the directories above can be found as follows:
#'
#' -   The total size of directory `e` is *584* because it contains a
#'     single file `i` of size 584 and no other directories.
#' -   The directory `a` has total size *94853* because it contains files
#'     `f` (size 29116), `g` (size 2557), and `h.lst` (size 62596), plus
#'     file `i` indirectly (`a` contains `e` which contains `i`).
#' -   Directory `d` has total size *24933642*.
#' -   As the outermost directory, `/` contains every file. Its total size
#'     is *48381165*, the sum of the size of every file.
#'
#' To begin, find all of the directories with a total size of *at most
#' 100000*, then calculate the sum of their total sizes. In the example
#' above, these directories are `a` and `e`; the sum of their total sizes
#' is *`95437`* (94853 + 584). (As in this example, this process can count
#' files more than once!)
#'
#' Find all of the directories with a total size of at most 100000. *What
#' is the sum of the total sizes of those directories?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f07a(x)` returns .... For Part Two,
#'   `f07b(x)` returns ....
#' @export
#' @examples
#' f07a(example_data_07())
f07a <- function(x) {
  elf_file_sys <- f07_build_file_sys(x)
  elf_file_sys$Do(function(node) node$totalSize <- data.tree::Aggregate(node, attribute = "size", aggFun = sum), traversal = "post-order")
  # print(elf_file_sys, "size","totalSize")
  totalSize_list <- elf_file_sys$Get("totalSize")
  is_dir <- is.na(elf_file_sys$Get("size"))
  small_dirs <- totalSize_list[is_dir & totalSize_list < 100000]
  return(sum(small_dirs))
}


#' @rdname day07
#' @export
#' @example
#' efs <- f07_build_file_sys(example_data_07())
#' print(efs, "size")
#' f07b(example_data_07())
f07b <- function(x) {
  ## build file sys, get dir size
  elf_file_sys <- f07_build_file_sys(x)
  elf_file_sys$Do(function(node) node$totalSize <- data.tree::Aggregate(node, attribute = "size", aggFun = sum), traversal = "post-order")
  # print(elf_file_sys, "size","totalSize")
  totalSize_list <- elf_file_sys$Get("totalSize")

  ## calc total space needed
  space_used <- sum(elf_file_sys$Get("size"), na.rm = TRUE)
  total_disk <- 70000000
  space_needed <- 30000000

  space_unused <- total_disk - space_used
  update_space <- space_needed - space_unused

  # message("Space Used: ", space_used, ", Space Free: ", space_unused)
  # message("Need to delete: ", update_space)

  ## Find smallest directory > update_space
  is_dir <- is.na(elf_file_sys$Get("size"))
  big_dirs <- totalSize_list[is_dir & totalSize_list > update_space]
  return(big_dirs[which.min(big_dirs)])
}

#' @example
#' cn <- f07_build_file_sys(c("$ cd /"))
#' cn <- f07_build_file_sys(c("$ cd /", "dir a"))
#' cn <- f07_build_file_sys(example_data_07()[1:15])
#' cn <- f07_build_file_sys(example_data_07())
#' print(cn, "size")
f07_build_file_sys <- function(command_list) {
  file_sys <- data.tree::Node$new("/")
  # file_sys$AddChild("a")
  current_node <- file_sys$root
  for(c in command_list){
    # print(current_node)
    current_node <- f07_parse_command(c, current_node)
  }

  return(file_sys)
}

#' @example
#' test_sys <- Node$new("/")
#' test_sys$AddChild("a")
#' print(test_sys)
#' f07_parse_command("$ cd /", test_sys)
#' n2 <- f07_parse_command("$ cd a", test_sys)
f07_parse_command <- function(x, current_node){
  x <- unlist(strsplit(x," "))

  if(x[[1]] == "$"){
    # message("command")
    if(x[[2]] == "cd"){
      new_node = x[[3]]
      if(new_node == "/") return(current_node$root) ## go to root
      else if(new_node == "..") return(current_node$parent)
      else{
        # message("change dir to: ", new_node)
        # current_node <- current_node$Get(new_node)
        current_node <- current_node[[new_node]]
      }
          } else if(x[[2]] == "ls"){
      # message("list dir")
      return(current_node)
    } else stop("UNKNOWN COMMAND")
  } else {

    node_info = x[[1]]
    node_name = x[[2]]

    if(node_info == "dir"){
      # message("Add dir: ", node_name)
      current_node$AddChild(node_name)

    } else if(grepl("\\d+",node_info)) {
      # message("Add file: ", node_name)
      current_node$AddChild(node_name, size = as.numeric(node_info))
    } else stop("UNKNOWN COMMAND")
  }
  return(current_node)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day07
#' @export
example_data_07 <- function(example = 1) {
  l <- list(
    a = c(
      "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k"
    )
  )
  l[[example]]
}
