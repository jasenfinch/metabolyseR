library(metaboData)

context('split')

test_that('split works',{
  d <-  analysisData(abr1$neg,abr1$fact) %>%
    split(cls = 'day')
  
  expect_identical(class(d),'list')
  expect_length(d,6)
})
