
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
#'
#' @usage x \%cat\% y
#'
#' @param x left hand side character string
#' @param y right hand side character string
#'
#' @example "Hello" %cat% "World"
#'
#' @export
`%cat%` <- function(x, y) {

  checkmate::assertCharacter(x)
  checkmate::assertAtomic(x)

  checkmate::assertCharacter(y)
  checkmate::assertAtomic(y)

  paste0(x, y)
}



#' Format a string
#'
#' A string with marks in the form of "{}" is formated at
#' these points with the values passed on the right side.
#' On the right side is to be passed a vector with the
#' same amount of substituents as the string has "{}" marks.
#'
#' @usage input_str \%fmt\% sub_vec
#'
#' @param input_str a character string with some placeholders of the form "{}" or "{index}"
#' @param sub_vec a character string vector of the strings to fill the placeholders
#'
#' @examples
#' "Hello {}. This is supposed to be a {}!" %fmt% c("World", "Test")
#'
#' "This is supposed to be a {2}. Hello {1}." %fmt% c("World", "Test")
#'
#' @export
`%fmt%` <- function(input_str, sub_vec) {

  # check for correct input
  checkmate::assertCharacter(input_str)
  checkmate::assertAtomic(input_str)

  checkmate::assertCharacter(sub_vec)

  # Return initial string if there are no formatings
  if (gregexpr("\\{[[:digit:]]|\\{\\}", input_str)[[1]][1] < 0) {
    return(input_str)
  }

  # If {} is used for formating
  if (grepl("\\{\\}", input_str)) {

    fmt_length <- nrow(stringr::str_match_all(input_str, "\\{\\}")[[1]])
    # Throw error if list length does  not match the exact number of formatings
    if (length(sub_vec) != fmt_length) {
      stop("ERROR: Length of substitute list must be equal to the number of formatings.")
    }

    return(
      foldl(function(str, subst) {sub("\\{\\}", subst, str)}, input_str, sub_vec)
    )
  }

  # If {index} is used for formating
  if (grepl("\\{[[:digit:]]+\\}", input_str)) {

    fmt_length <- max(as.numeric(stringr::str_match_all(input_str, "\\{([[:digit:]]+)\\}")[[1]][, 2]))
    # Throw error if list length does  not match the exact number of formatings
    if (length(sub_vec) != fmt_length) {
      stop("ERROR: Length of substitute list must be equal to the number of formatings.")
    }

    no_list <-  seq_len(fmt_length)
    return(
      foldl(
        function(str, no) {
          sub("\\{" %cat% as.character(no) %cat% "\\}", sub_vec[no], str)
        },
        input_str,
        no_list
      )
    )
  }

  stop("ERROR: Formating is not correct. Please use ?RolliNLP::`%fmt%` for further information about usage.")
}
