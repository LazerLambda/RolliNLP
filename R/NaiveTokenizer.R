partitionText <- function(string) {
  punctuation <- "[$&+,:;=?@#|'<>\\.^*()%!-]"

  atomic_string <- strsplit(string, "")
  partitioned_vec <- foldl(
    function(acc, elem){
      # Special case: Begining
      if (length(acc) == 0) {
        acc <- append(acc, elem)
        return(acc)
      }
      # drop spaces
      if (nchar(elem) == 0 || elem == " ") {
        acc <- append(acc, "")
        return(acc)
      }
      # append add punctuation and add new empty string
      if (grepl(punctuation, elem)) {
        acc <- append(acc, elem)
      } else {
        curr_word <- acc[[length(acc)]]
        acc[[length(acc)]] <- paste0(curr_word, elem)
      }
      return(acc)
    },
    list(),
    atomic_string[[1]]
  )
  # remove last element if empty
  if (partitioned_vec[length(partitioned_vec)] == "") {
    return(partitioned_vec[seq_len(length(partitioned_vec) - 1)])
  } else {
    return(partitioned_vec)
  }

}

#' N-gram Tokenizer
#'
#' Get n-grams from a vector of tokens, supplied as parameter.
#'
#' @usage getNGrams(token_vec, n = 2)
#'
#' @param token_vec vector of atomic character strings
#' @param n cardinality of the pairs
#'
#' @examples
#' getNGrams(c("Hello", "World", "!"), n = 2)
#'
#' @export
getNGrams <- function(token_vec, n = 2) {
  checkmate::assertCharacter(token_vec)
  checkmate::assertTRUE(n >= 2)
  foldl(function(acc, i) {
      append(acc, list(token_vec[i:(i + (n - 1))]))
    },
    list(),
    seq_len(length(token_vec) - (n - 1))
  )
}


#' Naive Tokenizer
#'
#' Simple Tokenizer to split words among punctuation and whitespaces.
#' If possible, prefer a DL Tokenizer.
#' WARNING: This tokenizer is build for the english language and can be applied
#' to other latin-based or cyrillic-based languages. This tokenizer does not work on other alphabets
#' like chinese, devanagari, thai, japanese, hebrew or arabic.
#'
#' @param string character string to be tokenized
#'
#' @export
naiveTokenizer <- function(string) {
  checkmate::assertCharacter(string)
  partitionText(string)

}
