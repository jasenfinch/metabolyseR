test_that("multiplication works", {
  d <- analysisData(
    metaboData::abr1$neg,
    metaboData::abr1$fact
  )
  
  flat <- flatten(d)
  
  expect_s3_class(flat,'tbl_df')
})
