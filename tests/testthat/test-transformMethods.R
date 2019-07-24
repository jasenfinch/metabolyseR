suppressPackageStartupMessages(library(metaboData))

context('transformMethods')

test_that('transformMethods returns methods correctly',{
  m <- sapply(metabolyseR:::transformMethods(),is.function)
  expect_false(F %in% m)
})

test_that('transformMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::transformMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::transformMethods(description = T))
  m <- names(metabolyseR:::transformMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::transformMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('methods work',{
  m <- names(metabolyseR:::transformMethods())
  data("abr1")
  d <- analysisData(abr1$neg[abr1$fact$class %in% c('1','6'),500:600],cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),])))
  m <- map(m,~{
    method <- metabolyseR:::transformMethods(.)
    res <- method(d)
    return(res)
  })
  
  expect_false(F %in% sapply(m,function(x){identical(slotNames(x),c('data','info'))}))
  expect_false(F %in% sapply(m,function(x){identical(class(x %>% dat()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(F %in% sapply(m,function(x){identical(class(x %>% info()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(F %in% sapply(m,function(x,col){ncol(dat(x)) == col},col = ncol(dat(d))))
  expect_false(F %in% sapply(m,function(x,row){nrow(dat(x)) == row},row = nrow(dat(d))))
  expect_false(F %in% sapply(m,function(x,col){ncol(info(x)) == col},col = ncol(info(d))))
  expect_false(F %in% sapply(m,function(x,row){nrow(info(x)) == row},row = nrow(dat(d))))
})