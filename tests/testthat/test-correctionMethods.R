library(metaboData)

context('correctionMethods')

test_that('correctionMethods returns methods correctly',{
  m <- map_lgl(correctionMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('correctionMethods errors if incorrect method specified',{
  expect_error(correctionMethods('incorrect'))
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
  
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(slotNames(.x),c('data','info'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(.x %>% 
                        dat()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(.x %>% 
                        sinfo()),
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
