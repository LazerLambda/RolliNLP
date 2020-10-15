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


getNGrams <- function(token_vec, n = 2) {
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
#' WARNING: This tokenizer is build for the english language and can be applied
#' to other latin-based or cyrillic-based languages. This tokenizer does not work on other alphabets
#' like chinese, devanagari, thai, japanese, hebrew or arabic.
#'
#' @usage naiveTokenizer(string)
#' @param string character string to be tokenized
#'
#' @export
naiveTokenizer <- function(string) {

  # Split among punctuation
  partitioned_text <- partitionText(string)

  # Find bigrams
  partitioned_bigram <- getNGrams(partitioned_text, n = 2)
  # Find trigrams
  partitioned_trigram <- getNGrams(partitioned_text, n = 3)

  # check for co-occurences in words and punctuation
  # e.g. New York, John's

  # count unigrams
  unigrams <- CountMap$new()
  purrr::walk(partitioned_text, function(x) unigrams$add(paste0(x, collapse = "")))
  unigrams$sort()

  # count bigrams
  bigrams <- CountMap$new()
  purrr::walk(partitioned_bigram, function(x) bigrams$add(paste0(x, collapse = "")))
  bigrams$sort()

  # count trigrams
  trigrams <- CountMap$new()
  purrr::walk(partitioned_trigram, function(x) trigrams$add(paste0(x, collapse = "")))
  trigrams$sort()

  tokens <- list()

  # check if most occuring bigram appears more often than least occuring unigram
  head_uni <- unigrams$head(length(partitioned_text))
  head_bi <- bigrams$head(1)

  if (head_uni[[length(head_uni)]] > head_bi[[1]]) {
    tokens <- unigrams
    return(tokens)
  } else {
    # merge occurence of bigram

    return(tokens)
  }
}
