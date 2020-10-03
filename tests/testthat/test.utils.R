
## %cat% tests

context("`%cat%`: Correct input")

test_that("Strings are concatenated correctly I", {
  expect_equal("Hello" %cat% "World", "HelloWorld")
})

test_that("Strings are concatenated correctly II", {
  expect_equal("" %cat% "World", "World")
})

test_that("Strings are concatenated correctly III", {
  expect_equal("Hello" %cat% "", "Hello")
})

test_that("Strings are concatenated correctly III", {
  expect_equal(character(0) %cat% character(0), character(0))
})





context("`%cat%`: Incorrect input")

test_that("Strings are concatenated correctly I", {
  expect_error(NULL %cat% "")
})

test_that("Strings are concatenated correctly II", {
  expect_error("" %cat% NULL)
})

test_that("Strings are concatenated correctly III", {
  expect_error(NULL %cat% NULL)
})

test_that("Strings are concatenated correctly III", {
  expect_error(1 %cat% list())
})


### %fmt% tests

context("`%fmt%`: Correct input")

test_that("Strings are formated correctly I", {
  expect_equal(
    "Hello {}. This is supposed to be a {}." %fmt% c("World", "Test"),
    "Hello World. This is supposed to be a Test."
  )
})

test_that("Strings are formated correctly II", {
  expect_equal(
    "Hello {1}. This is supposed to be a {2}." %fmt% c("World", "Test"),
    "Hello World. This is supposed to be a Test."
  )
})

test_that("Strings are formated correctly III", {
  expect_equal(
    "This is supposed to be a {2}. Hello {1}." %fmt% c("World", "Test"),
    "This is supposed to be a Test. Hello World."
  )
})

test_that("Strings are formated correctly IV", {
  expect_equal(
    "This is supposed to be a Test. Hello World." %fmt% c("World", "Test"),
    "This is supposed to be a Test. Hello World."
  )
})

test_that("Strings are formated correctly V", {
  expect_equal(
    "This is supposed to be a {o}. Hello World." %fmt% c("World", "Test"),
    "This is supposed to be a {o}. Hello World."
  )
})

test_that("Strings are formated correctly V", {
  expect_equal(
    "Hello {}." %fmt% "World",
    "Hello World."
  )
})

test_that("Strings are formated correctly VI", {
  expect_equal(
    "Hello {1}." %fmt% "World",
    "Hello World."
  )
})



context("`%fmt%`: Incorrect input")

test_that("Incorrect data type input I", {
  expect_error(NULL %fmt% c("World", "Test"),)
  expect_error("NULL {}" %fmt% NULL,)
  expect_error(NULL %fmt% NULL,)
})

test_that("Incorrect Input treatment II", {
  expect_error("This is supposed to be a {2}. Hello {1}." %fmt% c(1, 2))
})

test_that("Incorrect Input treatment II", {
  expect_error(list() %fmt% c("World", "Test"))
})

test_that("Incorrect Input treatment III", {
  expect_error("This is supposed to be a {2}. Hello {1}." %fmt% c("World"))
})

test_that("Incorrect Input treatment IV", {
  expect_error("This is supposed to be a {2}. Hello {1}." %fmt% c("World", "T", "est"))
})

test_that("Incorrect Input treatment V", {
  expect_error("This is supposed to be a {2}. Hello {1}." %fmt% list("World", "Test"))
})

test_that("Incorrect Input treatment V", {
  expect_error("Hello {1}." %fmt% character())
})
