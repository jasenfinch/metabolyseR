# library(FIEmspro)
# 
# context('metabolyse')
# 
# test_that('metabolyse-works', {
#   
#   data(abr1)
#   p <- analysisParameters()
#   p@preTreat <- list(
#     occupancyFilter = list(maximum = list()),
#     transform = list(TICnorm = list())
#   )
#   p@featureSelection$pars$fs.rf$nreps <- 1
#   analysis <- metabolyse(abr1$neg[,150:200],abr1$fact,p) 
#   
#   expect_true(isS4(analysis))
#   expect_true(class(analysis) == 'Analysis')
# })
