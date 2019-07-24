
pairwises <- function(cl){
  cl %>%
    as.character() %>%
    unique() %>%
    sort() %>%
    combn(2) %>%
    apply(2,str_c,collapse = '~')
}