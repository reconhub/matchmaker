test_that("all files are shown with no input", {
  expect_identical(matchmaker_example(), list.files(system.file("extdata", package = "matchmaker")))
})

test_that("specific files can be called up", {
  expect_true(file.exists(matchmaker_example("coded-data.csv")))
  expect_true(file.exists(matchmaker_example("spelling-dictionary.csv")))
  expect_error(file.exists(matchmaker_example("bleepbloop.txt")), "No file found")
})
