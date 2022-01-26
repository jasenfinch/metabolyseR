test_that('roc correctly ignores irrelevant objects in lists',{
  expect_identical(roc(list(0,0)),tibble())
})