library(metaboData)

context('correctionMethods')

test_that('correctionMethods returns methods correctly',{
  m <- sapply(correctionMethods(),is.function)
  expect_false(F %in% m)
})

test_that('correctionMethods returns descriptions correctly',{
  m <- sapply(correctionMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(correctionMethods(description = T))
  m <- names(correctionMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(correctionMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('methods work',{
  m <- names(correctionMethods())
  d <- analysisData(abr1$neg[,200:250],abr1$fact) %>%
    keepClasses(classes = c(1,6))
  d <- clsAdd(d,'block',1)
  m <- map(m,~{
    method <- correctionMethods(.)
    res <- method(d)
    return(res)
  })
  
  expect_false(F %in% sapply(m,function(x){identical(slotNames(x),c('data','info'))}))
  expect_false(F %in% sapply(m,function(x){identical(class(x %>% dat()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(F %in% sapply(m,function(x){identical(class(x %>% sinfo()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(F %in% sapply(m,function(x,col){ncol(dat(x)) == col},col = ncol(dat(d))))
  expect_false(F %in% sapply(m,function(x,row){nrow(dat(x)) == row},row = nrow(dat(d))))
  expect_false(F %in% sapply(m,function(x,col){ncol(sinfo(x)) == col},col = ncol(sinfo(d))))
  expect_false(F %in% sapply(m,function(x,row){nrow(sinfo(x)) == row},row = nrow(dat(d))))
})