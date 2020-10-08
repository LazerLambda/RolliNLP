

#' Python-like dictionary
#'
#' With this dictionary one can attach data to
#' a key. The limitation here is that the key
#' must be an atomic character string. Functions get()
#' and put() are provided to edit and get informations
#' about the class.
#'
#' @usage cl <- DictMap$new()
#' cl$put("a", 2)
#' cl$get("a")
#'
#' @param key atomic character string
#' @param value any
#'
#' @export
DictMap <- R6::R6Class(
  "DictMap",
  private = list(
    main_list = list()
  ),
  public = list(
    get = function(key) {
      assertCharacter(key)
      assertAtomic(key)
      unname(private$main_list[key])[[1]]
    },
    put = function(key, value) {
      assertCharacter(key)
      assertAtomic(key)
      assertTRUE(length(key) == 1 && is.atomic(key))
      if (length(value) > 1) {
        value <- list(value)
      }
      private$main_list[key] <- value
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
#' @usage cl <- DictMap$new()
#' cl$add("a")
#' cl$add("a")
#' cl$get("a") == 2
#' cl$head(1)
#'
#' @param key atomic character string
#' @param value any
#' @export
CountMap <- R6::R6Class(
  "CountMap",
  inherit = DictMap,
  public = list(
    add = function(key) {
      assertCharacter(key)
      assertAtomic(key)
      count <- unname(private$main_list[key])[[1]]

      if (is.null(count)) {
        private$main_list[key] <- 1
      } else {
        private$main_list[key] <- count + 1
      }
    },
    put = function(key, value) {
      assertCharacter(key)
      assertAtomic(key)
      assertInt(value)
      assertAtomic(value)

      private$main_list[key] <- value
    },
    order = function() {
      private$main_list <-
        private$main_list[order(unlist(private$main_list),decreasing=TRUE)]
    },
    head = function(top_n) {
      assertInt(top_n)
      assertAtomic(top_n)

      top_n <- ifelse(top_n > length(private$main_list),
        yes = length(private$main_list),
        no = top_n)

      private$main_list[order(unlist(private$main_list),decreasing=TRUE)][seq_len(top_n)]
    }
  )
)
