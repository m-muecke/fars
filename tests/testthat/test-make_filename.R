test_that("works with sample inputs", {
  filename <- make_filename("2014")
  expect_equal(filename, "accident_2014.csv.bz2")

  filename <- make_filename("2013")
  expect_equal(filename, "accident_2013.csv.bz2")

  filename <- make_filename(c("2015", "2014"))
  expect_equal(filename, c("accident_2015.csv.bz2", "accident_2014.csv.bz2"))
})
