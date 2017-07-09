suppressPackageStartupMessages(library(FIEmspro))

context('correlations')

test_that('correlations-works', {
  # skip_on_travis()
  data(abr1)
  cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:200][1:10,]
  cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:200][1:10,]
  dat <- rbind(cls1,cls2)
  inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  correlations <- metabolyse(dat,info,analysisParameters('correlations'))
  expect_true(class(correlations@correlations)[1] == 'tbl_df')
})

