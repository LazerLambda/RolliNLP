

#' Python-like dictionary
#'
#' With this dictionary one can attach data to
#' a key. The limitation here is that the key
#' must be an atomic character string. Functions get()
#' and put() are provided to edit and get informations
#' about the class.
#'
#' @examples
#' cl <- DictMap$new()
#' cl$put ("key", "value")
#' cl$get("key") == "value"
#'
#' @param key atomic character string
#' @param value any value
#'
#' @export
DictMap <- R6::R6Class(
  "DictMap",
  private = list(
    main_list = list()
  ),
  public = list(
    get = function(key) {
      checkmate::assertCharacter(key)
      checkmate::assertAtomic(key)
      unname(private$main_list[key])[[1]]
    },
    put = function(key, value) {
      checkmate::assertCharacter(key)
      checkmate::assertAtomic(key)
      checkmate::assertTRUE(length(key) == 1 && is.atomic(key))
      if (length(value) > 1) {
        value <- list(value)
      }
      private$main_list[key] <- value
    },
    items = function() {
      data.frame(keys = names(private$main_list), values = unlist(unname(private$main_list)))
    }
  ))


#' Python-like Counter dictionary
#'
#' Counter dictionary to count the occurences of a
#' character string. The class provides additionaly
#' a function head(top_n) to get the top n occurences.
#' Also included is a sort function to sort the internal list.
#' WARNING: This operation cannot be undone.
#'
#' @examples
#' cl <- CountMap$new()
#' cl$add("Hello")
#' cl$add("Hello")
#' cl$get("Hello") == 2
#' cl$head(2)
#'
#' @export
CountMap <- R6::R6Class(
  "CountMap",
  inherit = DictMap,
  public = list(
    add = function(key) {
      checkmate::assertCharacter(key)
      checkmate::assertAtomic(key)
      count <- unname(private$main_list[key])[[1]]

      if (is.null(count)) {
        private$main_list[key] <- 1
      } else {
        private$main_list[key] <- count + 1
      }
    },
    put = function(key, value) {
      checkmate::assertCharacter(key)
      checkmate::assertAtomic(key)
      checkmate::assertInt(value)
      checkmate::assertAtomic(value)

      private$main_list[key] <- value
    },
    order = function() {
      private$main_list <-
        private$main_list[order(unlist(private$main_list),decreasing=TRUE)]
    },
    head = function(top_n) {
      checkmate::assertInt(top_n)
      checkmate::assertAtomic(top_n)

      top_n <- ifelse(top_n > length(private$main_list),
        yes = length(private$main_list),
        no = top_n)

      private$main_list[order(unlist(private$main_list),decreasing=TRUE)][seq_len(top_n)]
    }
  )
)

#' Word dictionary
#'
#' A datastructure to construct a dictionary for words as keys
#' to numbers as values. This class also provides the method word2vec()
#' to transform an already tokenized character vector into
#' a word embeding. The default value for a missing value is 0 here.
#' Specific editing can be done using the put() function.
#'
#' @examples
#' wd <- WordDict$new(lower = TRUE)
#' wd$add("Hello")
#' wd$add("World")
#' wd$get("world") == 2
#' wd <- WordDict$new(lower = FALSE)
#' wd$add("Hello")
#' wd$add("World")
#' wd$get("world") == 0 # Missing word
#' wd$get("World") == 2
#' wd$word2vec(c("Hello", "World")) == c(1,2)
#' wd$word2vec(c("Hello", "world")) == c(1,0)
#'
#' @export
WordDict <- R6::R6Class("WordDict",
  inherit = DictMap,
  private = list(
    index = 1
  ),
  public = list(
    lower = F,
    initialize = function(lower = F){
      self$lower = lower
    },
    get = function(key) {
      checkmate::assertCharacter(key)
      checkmate::assertAtomic(key)

      ret_val <- unname(private$main_list[key])[[1]]
      if (is.null(ret_val)) {
        return(0)
      } else {
        return(ret_val)
      }
    },
    add = function(word) {
      checkmate::assertCharacter(word)
      checkmate::assertAtomic(word)

      if (self$lower) {
        word <- tolower(word)
      }

      if (is.null(private$main_list[word][[1]])) {
        private$main_list[word] <- private$index
        private$index <- private$index + 1
      }
    },
    word2vec = function(words) {
      checkmate::assertCharacter(words)

      if (self$lower) {
        words <- lapply(words, tolower)
      } else {
        words <- as.list(words)
      }
      sapply(words, function(x) {self$get(x)}
      )
    }
  )
)
