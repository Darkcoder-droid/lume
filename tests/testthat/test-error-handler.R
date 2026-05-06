test_that("friendly messages are stable", {
  expect_match(friendly_error_message("upload_format"), "Unsupported")
  expect_match(friendly_error_message("insufficient_data"), "Not enough")
  expect_match(friendly_error_message("generic"), "Something")
})

test_that("safe_execute returns NULL on failure", {
  out <- safe_execute(stop("boom"), context = "test", session = NULL, error_type = "generic")
  expect_null(out)
})
