test_that("combine.lists overwrites values as expected", {
  list1 <- list(
    x = 10,
    y = list(
      x = 20,
      y = 30
    )
  )
  list2 <- list(y = list(y = 40))
  expect_identical(
    combine.lists(list1, list2),
    list(
      x = 10,
      y = list(
        x = 20,
        y = 40
      )
    )
  )
})

test_that("combine.lists handles unnamed structures correctly", {
  list1 <- list(
    x = 10,
    20
  )
  list2 <- list(x = 20, y = 30, list(x = 30))
  expect_identical(
    combine.lists(list1, list2),
    list(x = 20, 20, y = 30, list(x = 30))
  )
})

test_that("combine.lists understands vectors", {
  vec1 <- c(x = 1, y = 2, z = 3)
  vec2 <- c(y = 4, 12)
  expect_identical(
    combine.lists(vec1, vec2),
    c(x = 1, y = 4, z = 3, 12)
  )
})

test_that("combine.lists deals with empty input correctly", {
  list1 <- list()
  list2 <- list()
  expect_identical(
    combine.lists(list1, list2),
    list()
  )
  list1 <- list()
  list2 <- list(x = 20)
  expect_identical(
    combine.lists(list1, list2),
    list2
  )
  list1 <- list(x = 20)
  list2 <- list()
  expect_identical(
    combine.lists(list1, list2),
    list2
  )
})
