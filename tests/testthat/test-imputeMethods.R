library(metaboData)

context('imputeMethods')

plan(future::multisession,workers = 2)

test_that('imputeMethods returns methods correctly',{
  m <- map_lgl(imputeMethods(),is.function)
  expect_false(F %in% m)
})

test_that('imputeMethods returns descriptions correctly',{
  m <- map_lgl(imputeMethods(description = TRUE),is.list)
  expect_false(FALSE %in% m)
})

test_that('description names match method names',{
  d <- names(imputeMethods(description = TRUE))
  m <- names(imputeMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(imputeMethods(description = TRUE),names)
  expect_false(FALSE %in% unlist(lapply(
    n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- map_dbl(imputeMethods(description = TRUE),
               ~{length(.x$arguments)})
  m <- map_dbl(imputeMethods(),~{length(formals(.x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(imputeMethods())
  d <- abr1 %>%
    {
      analysisData(.$neg,.$fact)
    } %>%
    keepClasses(classes = c(1,6)) %>%
    {
      keepFeatures(.,features = features(.)[500:600])
    }
  
  m <- m %>%
    map(~{
      method <- imputeMethods(.x)
      res <- method(d)
      return(res)
    })
  
  expect_false(FALSE %in% ('AnalysisData' == map_chr(m,class)))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(dat(.x)), c('tbl_df','tbl','data.frame'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(sinfo(.x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{ncol(dat(.x)) == ncol(dat(d))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{nrow(dat(.x)) == nrow(dat(d))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{ncol(sinfo(.x)) == ncol(sinfo(d))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{nrow(sinfo(.x)) == nrow(sinfo(d))}))
})
