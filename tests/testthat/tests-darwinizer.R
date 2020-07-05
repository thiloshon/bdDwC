context("Darwinizer functions")

test_that("Darwinizer", {
  # With given Darwin Cloud version
  # Sample data should return specific result
  if (data_darwin_cloud$date == "2018-08-20") {
    # Test provided data
    result <- darwinize_names(
      data_user = data_reptiles,
      data_dwc = data_darwin_cloud$data
    )
    # Darwinized type
    expect_equal(unique(result$match_type), "Darwinized")
    # Number of records darwinized
    expect_equal(nrow(result), 3)
    # Number of columns
    expect_equal(ncol(result), 3)

    # Test identical match
    result <- darwinize_names(
      data_user = data.frame(year = 1, DIA = 1),
      data_dwc = data_darwin_cloud$data
    )
    expect_equal(length(unique(result$match_type)), 2)
    # Number of records darwinized
    expect_equal(nrow(result), 2)
    # Number of columns
    expect_equal(ncol(result), 3)
  }

  # Test what happens when all records have identical match
  result <- darwinize_names(
    data_user = data.frame(year = 1, day = 1),
    data_dwc = data.frame(fieldname = letters[1:2], standard = c("year", "day"))
  )
  # Darwinized type
  expect_equal(unique(result$match_type), "Identical")
  # Number of records darwinized
  expect_equal(nrow(result), 2)
  # Number of columns
  expect_equal(ncol(result), 3)

  # Test no lower case matches
  result <- darwinize_names(
    data_user = data.frame(year = 1, day = 1),
    data_dwc = data.frame(fieldname = letters[1:2], standard = LETTERS[1:2])
  )
  expect_equal(nrow(result), 0)
})

test_that("Rename User Data", {
  input <- darwinize_names(
    data_user = data_reptiles,
    data_dwc = data_darwin_cloud$data
  )
  result <- rename_user_data(data_reptiles, input)
  # Renamed data should have same dimensions
  expect_equal(ncol(result), ncol(data_reptiles))
  expect_equal(nrow(result), nrow(data_reptiles))
  # No renaming could be made
  result <- rename_user_data(mtcars, input)
  expect_true(is.null(result))
})

test_that("Linking Names", {
  # Expect wrong input
  expect_error(link_old_new(mtcars))
  expect_error(link_old_new(data.frame()))
  expect_error(link_old_new(matrix(1:10)))
  expect_error(link_old_new(matrix(1:10)))
  # Expect correct input/output
  input <- darwinize_names(
    data_user = data_reptiles,
    data_dwc = data_darwin_cloud$data
  )
  expect_equal(length(link_old_new(input)), nrow(input))
  # Test wrong linker
  expect_error(link_old_new(input, 0))
  expect_error(link_old_new(input, NA))
})