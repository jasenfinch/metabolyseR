library(metaboData)

context('occupancyMethods')

test_that('occupancyMethods returns methods correctly',{
  m <- map_lgl(occupancyMethods(),is.function)
  expect_false(F %in% m)
})

test_that('occupancyMethods errors if incorrect method specified',{
  expect_error(occupancyMethods('incorrect'))
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
    ~{identical(slotNames(.x),c('data','info'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{class(.x) == 'AnalysisData'}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{nrow(.x %>% dat()) == nrow(dat %>% dat())}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{ncol(.x %>% sinfo()) == ncol(dat %>% sinfo())}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{nrow(.x %>% sinfo()) == nrow(dat %>% sinfo())}))
})

test_that('occupancy methods error argument `occupancy` is non-numeric',{
  dat <- analysisData(data = metaboData::abr1$neg, info = metaboData::abr1$fact)
  
  expect_error(occupancyMaximum(dat,occupancy = 'wrong'))
  expect_error(occupancyMinimum(dat,occupancy = 'wrong'))
})
