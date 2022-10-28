
test_that('imputeMethods returns methods correctly',{
  m <- map_lgl(imputeMethods(),is.function)
  expect_true(all(m))
})

test_that('methods work',{
  d <- analysisData(metaboData::abr1$neg[,500:550],
                    metaboData::abr1$fact) %>%
    keepClasses(classes = c(1,6))
  
  m <- names(imputeMethods()) %>%
    map(~{
      method <- imputeMethods(.x)
      method(d)
    })
  
  expect_true(all(map_chr(m,class) == 'AnalysisData'))
              
  expect_true(all(map_lgl(m,~{'tbl_df' %in% class(dat(.x))})))
  expect_true(all(map_lgl(m,~{'tbl_df' %in% class(sinfo(.x))})))
  
  expect_true(all(map_lgl(m,~{ncol(dat(.x)) == ncol(dat(d))})))
  expect_true(all(map_lgl(m,~{ncol(sinfo(.x)) == ncol(sinfo(d))})))
  expect_true(all(map_lgl(m,~{nrow(sinfo(.x)) == nrow(sinfo(d))})))
})
