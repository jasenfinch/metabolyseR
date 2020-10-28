library(metaboData)

context('occupancyMethods')

test_that('occupancyMethods returns methods correctly',{
  m <- map_lgl(occupancyMethods(),is.function)
  expect_false(F %in% m)
})

test_that('occupancyMethods returns descriptions correctly',{
  m <- map_lgl(occupancyMethods(description = TRUE),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(occupancyMethods(description = TRUE))
  m <- names(occupancyMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(occupancyMethods(description = TRUE),names)
  expect_false(FALSE %in% unlist(lapply(
    n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- map_dbl(occupancyMethods(description = TRUE),
              ~{length(.x$arguments)})
  m <- map_dbl(occupancyMethods(),~{length(formals(.x)[-1])})
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
  
  expect_false(FALSE %in% map_lgl(
    m,
    ~{slotNames(.x) == c('data','info')}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{class(.x) == 'AnalysisData'}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{nrow(x %>% dat()) == nrow(dat %>% dat())}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{ncol(.x %>% sinfo()) == ncol(dat %>% sinfo())}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{nrow(.x %>% sinfo()) == nrow(dat %>% sinfo())}))
})
