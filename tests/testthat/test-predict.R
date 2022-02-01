test_that("predict works", {
  x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
    occupancyMaximum(cls = 'day') %>%
    transformTICnorm()
  
  training_data <- x %>%
    keepClasses(cls = 'day',
                classes = c('H','1'))
  
  test_data <- x %>%
    keepClasses(cls = 'day',
                classes = c('2','3'))
  
  rf <- randomForest(training_data,
                     cls = 'day',
                     returnModels = TRUE)
  
  predictions <- predict(rf,
                         test_data)
  
  expect_s3_class(predictions,'tbl_df')
  expect_error(predict(rf,
                       test_data,
                       idx = 'day'))
})

test_that("predit throws an error if unsupervised random forest used",{
  x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
    occupancyMaximum(cls = 'day') %>%
    transformTICnorm()
  
  training_data <- x %>%
    keepClasses(cls = 'day',
                classes = c('H','1'))
  
  test_data <- x %>%
    keepClasses(cls = 'day',
                classes = c('2','3'))
  
  rf <- randomForest(training_data,
                     cls = NULL,
                     returnModels = TRUE)
  
  expect_error(predict(rf,
                       test_data))
})

test_that("predict throws an error if RandomForest object does not contain models",{
  x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
    occupancyMaximum(cls = 'day') %>%
    transformTICnorm()
  
  training_data <- x %>%
    keepClasses(cls = 'day',
                classes = c('H','1'))
  
  test_data <- x %>%
    keepClasses(cls = 'day',
                classes = c('2','3'))
  
  rf <- randomForest(training_data,
                     cls = 'day',
                     returnModels = FALSE)
  
  expect_error(predict(rf,
                       test_data))
})
