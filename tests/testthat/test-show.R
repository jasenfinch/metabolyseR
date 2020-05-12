# library(metaboData)
# 
# context('show')
# 
# test_that('AnalysisParameters show method works',{
#   p <- analysisParameters()
#   out <- capture.output(show(p))
# 
#   expect_length(out,53)
# })
# 
# test_that('AnalysisData show method works',{
#   p <- analysisData(abr1$neg,abr1$fact)
#   out <- capture.output(show(p))
#   
#   expect_length(out,7)
# })
# 
# test_that('Analysis show method works',{
#   data(abr1)
#   
#   p <- analysisParameters()
#   parameters(p,'pre-treatment') <- preTreatmentParameters(
#     list(
#       occupancyFilter = 'maximum',
#       transform = 'TICnorm')
#   )
#   
#   changeParameter(p,'nCores') <- 1
#   
#   cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:200][1:10,]
#   cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:200][1:10,]
#   dat <- rbind(cls1,cls2)
#   inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
#   inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
#   info <- rbind(inf1,inf2)
#   
#   analysis <- metabolyse(dat,info,p,verbose = F)
#   out <- capture.output(show(analysis))
# 
#   expect_length(out,20)
# })
