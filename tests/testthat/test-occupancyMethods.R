library(metaboData)

context('occupancyMethods')

test_that('occupancyMethods returns methods correctly',{
  m <- sapply(occupancyMethods(),is.function)
  expect_false(F %in% m)
})

test_that('occupancyMethods returns descriptions correctly',{
  m <- sapply(occupancyMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(occupancyMethods(description = T))
  m <- names(occupancyMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(occupancyMethods(description = T),names)
  expect_false(F %in% unlist(lapply(
    n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(occupancyMethods(description = T),
              function(x){length(x$arguments)})
  m <- sapply(occupancyMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(occupancyMethods())
  dat <- analysisData(data = abr1$neg, info = abr1$fact)
  m <- lapply(m,function(x,dat){
    method <- occupancyMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(
    m,
    function(x){slotNames(x) == c('data','info')}))
  expect_false(F %in% sapply(
    m,
    function(x){class(x) == 'AnalysisData'}))
  expect_false(F %in% sapply(
    m,
    function(x,row){nrow(x %>% dat()) == row},row = nrow(dat %>% dat())))
  expect_false(F %in% sapply(
    m,
    function(x,col){ncol(x %>% sinfo()) == col},col = ncol(dat %>% sinfo())))
  expect_false(F %in% sapply(
    m,
    function(x,row){nrow(x %>% sinfo()) == row},row = nrow(dat %>% sinfo())))
})
