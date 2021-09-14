library(metaboData)

context('plotRSD')

test_that('plotRSD works for Analysis class',{
  d <- analysisData(abr1$neg,abr1$fact)
  
  pl <- plotRSD(d,cls = 'day')
  
  expect_s3_class(pl,'patchwork')
})

test_that('plotRSD errors correctly when numeric sample info column specified',{
  d <- analysisData(abr1$neg,abr1$fact)
  
  pl <- plotRSD(d,cls = 'day')
  
  expect_error(plotRSD(d,cls = 'class'))
})

test_that('plotRSD works for Analysis class',{
  d <- new('Analysis')
  
  raw(d) <- analysisData(abr1$neg[,300:400],abr1$fact)
  
  pl <- plotRSD(d,cls = 'day')
  
  expect_s3_class(pl,'patchwork')
})
