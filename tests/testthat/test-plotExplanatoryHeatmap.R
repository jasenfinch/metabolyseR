library(metaboData)

context('plotExplanatoryHeatmap')

test_that('plotExplanatoryHeatmap returns a plot',{
  p <- analysisParameters(c('preTreat','featureSelection'))
  p@preTreat <- list(
    occupancyFilter = list(maximum = list()),
    transform = list(TICnorm = list())
  )
  p@featureSelection$nCores <- 1
  p@featureSelection$pars$fs.rf$nreps <- 1
  cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:220][1:10,]
  cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:220][1:10,]
  dat <- rbind(cls1,cls2)
  inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  analysis <- metabolyseR::metabolyse(dat,info,p,verbose = F)
  pl <- metabolyseR::plotExplanatoryHeatmap(analysis,threshold = 0.5)
  
  expect_false(F %in% (class(pl) == c('gg','ggplot')))
})
