test_that("get.top.ten returns the top ten named and values from a named vector ", {
  input.vec <- c(
    "a" = 1, "b" = 2, "c" = 20, "d" = 3, "e" = 4, "f" = 5,
    "g" = 6, "h" = 7, "i" = 8, "j" = 9, "k" = 10
  )
  output.df <- data.frame(
    Subjects = c("a", "b", "d", "e", "f", "g", "h", "i", "j", "k"),
    Counts = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  expect_identical(get.top.ten(decreasing = FALSE, input.vec, "Subjects"), output.df)
})
