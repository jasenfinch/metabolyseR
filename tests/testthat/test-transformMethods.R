library(metaboData)

context('transformMethods')

test_that('transformMethods returns methods correctly',{
  m <- map_lgl(transformMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('methods work',{
  m <- names(transformMethods())
  
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[500:600]) %>%
    keepClasses(classes = c('1','6'))
  
  m <- map(m,~{
    method <- transformMethods(.)
    res <- method(d)
    return(res)
  })
  
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(slotNames(.x),c('data','info'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(.x %>% dat()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(.x %>% sinfo()),
                          c('tbl_df',"tbl","data.frame"))}))
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
    ~{nrow(sinfo(.x)) == nrow(dat(d))}))
})
