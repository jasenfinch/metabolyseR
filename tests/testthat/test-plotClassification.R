suppressPackageStartupMessages(library(FIEmspro))

context('plotClassification')

test_that('plotClassification returns a plot',{
  data(abr1)
  p <- analysisParameters(c('preTreat','classification'))
  p@preTreat <- list(
    occupancyFilter = list(maximum = list()),
    transform = list(TICnorm = list())
  )
  p@classification$nCores <- 1
  cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:200][1:10,]
  cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:200][1:10,]
  dat <- rbind(cls1,cls2)
  inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  analysis <- metabolyseR::metabolyse(dat,info,p)
  pl <- metabolyseR::plotClassification(analysis)
  
  expect_false(F %in% (class(pl) == c('gg','ggplot')))
})
