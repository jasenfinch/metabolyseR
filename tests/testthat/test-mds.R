d <- analysisData(
  metaboData::abr1$neg,
  metaboData::abr1$fact) %>%
  keepFeatures(features = features(.)[200:250]) %>% 
  clsAdd('sample_names',paste0(seq_len(nSamples(.)),'_a'))

rf <- randomForest(d)

test_that("mds works", {
  mds_results <- mds(rf)
  
  expect_s3_class(mds_results,'tbl_df')
})

test_that('mds correctly ignores irrelevant objects in lists',{
  expect_identical(mds(list(0,0)),tibble())
})
