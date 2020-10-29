context('plotLDA')

test_that('plotLDA returns a plot',{
  
  p <- analysisParameters(c('pre-treatment'))
  
  parameters(p,'pre-treatment') <- list(
    keep = list(features = list(features = colnames(abr1$neg)[190:200])),
    keep = list(classes = list(classes = c(1,6))),
    occupancyFilter = list(maximum = list()),
    transform = list(TICnorm = list())
  )
  
  analysis <- metabolyse(abr1$neg,abr1$fact,p,verbose = FALSE)
  pl <- plotLDA(analysis)
  
  expect_true(identical(class(pl),c("gg","ggplot")))
})
