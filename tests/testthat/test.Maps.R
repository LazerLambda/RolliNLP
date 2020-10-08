
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

test_that("CountMap Error handling", {})
