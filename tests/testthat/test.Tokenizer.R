
context("partitionText: Expecting correct behavior")

test_that("partitionText: Simple test I", {
  expect_equal(
    partitionText("Hello World."),
    list("Hello", "World", "."))
})

test_that("partitionText: Simple test I", {
  expect_error(partitionText(2))
})

# TODO: tests for getNGram
