context("Tests")
# These tests should either return error (s it's intended or be silent
test_that("User Data", {
  expect_error(test_data_user())
  expect_error(test_data_user(matrix()))
  expect_error(test_data_user(data.frame()))
  foo <- data.frame(1)
  names(foo) <- NULL
  expect_error(test_data_user(foo))
  expect_warning(test_data_user(data.frame(a = 1, a = 2, check.names = FALSE)))
  expect_silent(mtcars)
})

test_that("DWC Dictionary Data", {
  expect_error(test_data_dwc())
  expect_error(test_data_dwc(matrix()))
  expect_error(test_data_dwc(NA))
  expect_error(test_data_dwc(data.frame()))
  expect_silent(test_data_dwc(
    data.frame(foo = 1, bar = 1), "foo", "bar"
  ))
  expect_error(test_data_dwc(
    data.frame(foo = 1, bar = 1), "foo2", "bar2"
  ))
  expect_error(test_data_dwc(
    data.frame(foo = 1, foo = 1, bar = 1, check.names = FALSE), "foo", "bar"
  ))
  expect_error(test_data_dwc(
    data.frame(foo = 1, bar = 1, bar = 1, check.names = FALSE), "foo", "bar"
  ))
})

test_that("Renaming Data", {
  expect_error(test_data_renamed())
  expect_error(test_data_renamed(NA))
  expect_error(test_data_renamed(matrix()))
  expect_error(test_data_renamed(data.frame()))
  expect_error(test_data_renamed(data.frame(1)))
  expect_error(test_data_renamed(data.frame(1, 2)))
  expect_error(test_data_renamed(data.frame(name_old = 1, name_old = 2)))
  expect_error(test_data_renamed(
    data.frame(name_new = 1, name_new = 2, name_old = 3, check.names = FALSE)
  ))
  expect_error(test_data_renamed(
    data.frame(name_old = 1, name_old = 2, name_new = 3, check.names = FALSE)
  ))
  expect_silent(test_data_renamed(data.frame(name_new = 1, name_old = 2)))
})

test_that("Cloud Path", {
  expect_error(test_cloud())
  expect_error(test_cloud(NA))
  expect_error(test_cloud(c("a", "b")))
  expect_silent(test_cloud("valid path to cloud"))
})

test_that("Cloud Path", {
  expect_error(test_columns_cloud())
  expect_error(test_columns_cloud(NA))
  expect_error(test_columns_cloud(c(1, 1)))
  expect_error(test_columns_cloud(c("a", "a")))
  expect_silent(test_columns_cloud(c("a", "b")))
})