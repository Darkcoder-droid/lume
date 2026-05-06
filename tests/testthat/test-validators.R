test_that("filename sanitization works", {
  expect_equal(sanitize_filename("../bad name?.csv"), "bad_name_.csv")
})

test_that("text sanitization trims control chars", {
  expect_equal(sanitize_text("  hi\nthere\t"), "hi there")
})

test_that("file validation catches extension and size", {
  ok <- validate_file_input(list(name = "x.csv", size = 10))
  bad_ext <- validate_file_input(list(name = "x.exe", size = 10))
  bad_size <- validate_file_input(list(name = "x.csv", size = 60 * 1024^2))

  expect_true(ok$ok)
  expect_false(bad_ext$ok)
  expect_equal(bad_ext$code, "upload_format")
  expect_false(bad_size$ok)
  expect_equal(bad_size$code, "upload_size")
})

test_that("data structure validation works", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_true(validate_data_structure(df)$ok)
  expect_false(validate_data_structure(df[0, , drop = FALSE])$ok)
  expect_false(validate_data_structure(df[, 0, drop = FALSE])$ok)
})

test_that("large data sampling works", {
  df <- data.frame(x = seq_len(150000), y = runif(150000))
  smp <- sample_large_data(df, max_rows = 100000)
  expect_equal(nrow(smp), 100000)
})
