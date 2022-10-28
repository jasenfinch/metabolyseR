
test_that('keepMethods errors if incorrect method specified',{
  expect_error(keepMethods('incorrect'))
})