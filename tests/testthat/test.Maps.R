
context("'DictMap': Correct behaviour")

test_that("DictMap adding different types", {
  test_map <- RolliNLP::DictMap$new()

  a_v <- 2
  b_v <- "c"
  c_v <- list(1,2,3)
  d_v <- matrix(c(1,1,1,1), nrow = 2, ncol = 2)
  test_map$put("a", a_v)
  test_map$put("b", b_v)
  test_map$put("c", c_v)
  test_map$put("d", d_v)

  expect_equal(test_map$get("a"), a_v)
  expect_equal(test_map$get("b"), b_v)
  expect_equal(test_map$get("c"), c_v)
  expect_equal(test_map$get("d"), d_v)
  expect_null(test_map$get("e"))
})

test_that("DictMap behaviour with wrong input", {
  test_map <- RolliNLP::DictMap$new()

  a_v <- 2
  b_v <- "c"
  c_v <- list(1,2,3)
  d_v <- matrix(c(1,1,1,1), nrow = 2, ncol = 2)
  test_map$put("a", a_v)
  test_map$put("b", b_v)
  test_map$put("c", c_v)
  test_map$put("d", d_v)

  expect_error(test_map$put(2, a_v))
  expect_error(test_map$put(NULL, a_v))
  expect_error(test_map$put(c(1,2), a_v))
  expect_error(test_map$put(c("a", "b"), a_v))
})


context("'CountMap': Correct behaviour")

test_that("CountMap adding correct input", {
  test_cmap <- CountMap$new()

  init_vec <- rep(0, length(LETTERS))
  test_df <- data.frame(LETTERS, init_vec)

  for (e in seq_len(10000)) {
    current_letter <- sample(LETTERS, 1)
    index <- which(LETTERS == current_letter)
    test_df[index, 2] <-
      test_df[index, 2] + 1
    test_cmap$add(current_letter)
  }

  r <- lapply(seq_along(LETTERS), function(x) {
    expect_equal(test_df[x, 2], test_cmap$get(test_df[x, 1]))
    })
})

# TODO: Implement test for items() function

test_that("CountMap: Expecting correct behaviour, head() function", {
  test_cmap <- CountMap$new()

  n <- 5
  test_vec <- n:1
  test_let <- sample(LETTERS, n)

  random_occ <- test_vec[sample(test_vec, length(test_vec))]

  for (e in test_vec) {
    for (i in seq_len(test_vec[[e]])) {
      test_cmap$add(test_let[[e]])
    }
  }
  expect_equal(unlist(unname(test_cmap$head(n))), n:1)
  expect_equal(unlist(unname(test_cmap$head(2 * n))), n:1)
  expect_equal(unlist(unname(test_cmap$head(n - as.integer(n / 2)))), n:(n / 2))
})

# TODO: Implement test for error handling

test_that("CountMap Error handling", {})


context("'WordDict': Correct behaviour")

test_that("WordDict: Expecting correct behaviour", {
  t <- WordDict$new(lower = F)
  test_vec <- c("Hello", "World", "This", "is", "supposed", "to", "be", "a", "test")
  purrr::walk(test_vec, t$add)
  test_vec_no <- seq_len(length(test_vec))
  purrr::walk(test_vec, function(x) {
    val <- t$get(x)
    expect_equal(val, test_vec_no[[val]])
  })
})

test_that("WordDict: Expecting correct behaviour with unkown words", {
  t <- WordDict$new(lower = F)
  test_vec <- c("Hello", "World", "This", "is", "supposed", "to", "be", "a", "great")
  purrr::walk(test_vec, t$add)
  test_vec_no <- seq_len(length(test_vec))
  # Construct vector to check results
  check_vec <- append(test_vec, "test")
  # Add 0 for missing word
  check_vec_no <- append(test_vec_no, 0)
  purrr::walk(seq_len(length(test_vec)), function(x) {
    word <- test_vec[[x]]
    val <- t$get(word)
    expect_equal(val, check_vec_no[[val]])
  })
})

test_that("WordDict: Expecting correct behaviour with repeating words", {
  t <- WordDict$new(lower = F)
  test_vec <- c("Hello", "World", "This", "World", "is", "beautiful")
  purrr::walk(test_vec, t$add)
  # Construct check vector
  check_vec_no <- c(1,2,3,2,4,5)
  purrr::walk(seq_len(length(test_vec)), function(x) {
    val <- t$get(test_vec[[x]])
    expect_equal(val, check_vec_no[[x]])
  })
})

test_that("WordDict: Expecting correct behaviour with lower cases", {
  t <- WordDict$new(lower = T)
  test_vec <- c("Hello", "World", "This", "world", "is", "beautiful")
  purrr::walk(test_vec, t$add)
  # Construct check vector
  check_vec_no <- c(0,0,0,2,4,5)
  purrr::walk(seq_len(length(test_vec)), function(x) {
    val <- t$get(test_vec[[x]])
    expect_equal(val, check_vec_no[[x]])
  })
})

test_that("WordDict: word2vec() Expecting correct behaviour I", {
  t <- WordDict$new(lower = T)
  test_vec <- c("Hello", "World", "This", "world", "is", "beautiful")
  purrr::walk(test_vec, t$add)
  # Construct check vector
  check_vec_no <- c(1,2,3,2,4,5)
  expect_equal(t$word2vec(test_vec), check_vec_no)
})

test_that("WordDict: word2vec() Expecting correct behaviour II", {
  t <- WordDict$new(lower = F)
  test_vec <- c("Hello", "World", "This", "world", "is", "beautiful")
  purrr::walk(test_vec, t$add)
  # Construct check vector
  check_vec_no <- c(1,2,3,4,5,6)
  expect_equal(t$word2vec(test_vec), check_vec_no)
})

test_that("WordDict: word2vec() Expecting correct behaviour III", {
  t <- WordDict$new(lower = F)
  test_vec <- c("Hello", "World", "This", "world", "is")
  purrr::walk(test_vec, t$add)
  test_vec <- c("Hello", "World", "This", "world", "is", "beautiful")
  # Construct check vector
  check_vec_no <- c(1,2,3,4,5,0)
  expect_equal(t$word2vec(test_vec), check_vec_no)
})
