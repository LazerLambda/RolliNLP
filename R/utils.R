
foldl <- function(f, b, t) {
  head <- t[1]
  if (length(t) == 1) {
    return(f(b, head))
  }
  tail <- t[2:length(t)]
  foldl(f, f(b, head), tail)
}

#' Concatenate two Strings
#'
#' This function is a wraper for the 'paste0' function
#' and does exactly the same. Put the first string to the
#' left and the second string to the right.
#' @export
`%cat%` <- function(x, y) {
  paste0(x, y)
}

#' Format a string
#'
#' A string with marks in the form of "{}" is formated at
#' these points with the values passed on the right side.
#' On the right side is to be passed a vector with the
#' same amount of substituents as the string has "{}" marks.
#' @examples
#' "Hello {}. This is supposed to be a {}!" %fmt% c("World", "Test")
#' @export
`%fmt%` <- function(input_str, vec) {
  # TODO:
  # check if x is atomic string
  # check if vec fulfils all criteria

  foldl(function(x, y) {sub("\\{\\}", y, x)}, input_str, vec)
}
