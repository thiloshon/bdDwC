context("Dictionary functions")

test_that("Download Cloud Data", {
  # Test if works
  expect_silent(download_cloud_data())
  foo <- download_cloud_data()
  expect_equal(ncol(foo), 2)
  expect_gt(nrow(foo), 100)
  expect_equal(colnames(foo), c("fieldname", "standard"))
  expect_true(any(grepl("coordinateUncertaintyInMeters", foo$standard)))
  expect_equal(unique(apply(foo, 2, class)), "character")
  # Test for wrong path to Cloud Data
  expect_error(download_cloud_data(1, 2, 3, 4))
  expect_warning(download_cloud_data("https://"))
  # Test for wrong columns
  expect_silent(download_cloud_data(
    column_field = "fieldname", column_stand = "standard"
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = "fieldname"
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = c("fieldname", "fieldname")
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = 1
  ))
  expect_error(download_cloud_data(
    column_field = "fieldname", column_stand = "A"
  ))
})

test_that("Clean Cloud Data", {
  # Test if works
  expect_silent(clean_dwc(data_darwin_cloud$data))
  foo <- clean_dwc(data_darwin_cloud$data)
  expect_equal(ncol(foo), 2)
  expect_equal(colnames(foo), c("fieldname", "standard"))
  expect_lte(nrow(foo), nrow(data_darwin_cloud$data))
  # Test for wrong columns
  expect_error(clean_dwc(mtcars))
  expect_error(clean_dwc(data_darwin_cloud$data, "a"))
  # Test for wrong data
  expect_error(
    clean_dwc(data.frame(fieldname = c(NA, NA), standard = c(NA, NA)))
  )
})

test_that("Download Darwin Core Info", {
  # Test if works
  expect_silent(get_darwin_core_info())
  foo <- get_darwin_core_info()
  expect_equal(ncol(foo), 2)
  expect_true(any(grepl("previousIdentifications", foo$name)))
  expect_equal(colnames(foo), c("name", "definition"))
  # Test for wrong path to Darwin Core
  expect_warning(get_darwin_core_info("http://"))
  expect_equal(nrow(get_darwin_core_info("http://")), 1)
  # Test for wrong regex
  foo <- get_darwin_core_info(regex_term = "foo bar buz this wont occur")
  expect_equal(nrow(foo), 1)
  expect_equal(nrow(foo), 1)
  expect_equal(foo$name, NA)
  # Test for impossible offset to names definition
  foo <- get_darwin_core_info(name_to_def = 1e6)
  expect_equal(class(foo$name), "character")
  expect_gt(length(foo$name), 10)
  expect_equal(length(unique(foo$definition)), 1)
  expect_true(
    all(bdDwC:::data_darwin_core_info == bdDwC:::get_darwin_core_info())
  )
})