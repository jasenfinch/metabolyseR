library(metaboData)

context('plotFeature')

test_that('plotFeature returns a plot',{
  d <- analysisData(abr1$neg,abr1$fact)
  pl <- d %>%
    plotFeature(feature = 'N133',cls = 'day')
  
  expect_false(F %in% (class(pl) == c('gg','ggplot')))
})
