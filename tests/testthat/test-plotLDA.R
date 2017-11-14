suppressPackageStartupMessages(library(FIEmspro))

context('plotLDA')

test_that('plotLDA returns a plot',{
  data(abr1)
  p <- analysisParameters(c('preTreat'))
  p@preTreat <- list(
    occupancyFilter = list(maximum = list()),
    transform = list(TICnorm = list())
  )
  cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:200][1:10,]
  cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:200][1:10,]
  dat <- rbind(cls1,cls2)
  inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  analysis <- metabolyseR::metabolyse(dat,info,p)
  pl <- metabolyseR::plotLDA(analysis)
  
  expect_false(F %in% (class(pl) == c('gg','ggplot')))
})
