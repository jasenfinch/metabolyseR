library(metaboData)

context('plotExplanatoryHeatmap')

test_that('plotExplanatoryHeatmap returns a plot',{
  p <- analysisParameters(c('preTreat','modelling'))
  p@preTreat <- list(
    occupancyFilter = list(maximum = list()),
    transform = list(TICnorm = list())
  )
  p@modelling$randomForest$nCores <- 1
  p@modelling$randomForest$cls <- 'day'

  cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:220][1:10,]
  cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:220][1:10,]
  dat <- rbind(cls1,cls2)
  inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  analysis <- metabolyse(dat,info,p,verbose = F)
  pl <- plotExplanatoryHeatmap(analysis,threshold = 0.5)
  
  expect_true(class(pl) == 'list')
})

test_that('plotExplanatoryHeatmap returns a plot',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = 'injorder',perm = 3,nCores = 2)
  
  pl <- plotExplanatoryHeatmap(rf$injorder,measure = 'IncNodePurity')
  
  expect_identical(class(pl),c("patchwork","plot_filler","gg","ggplot"))
})