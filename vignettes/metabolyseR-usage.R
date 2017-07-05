## ----echo=FALSE,fig.width=6----------------------------------------------
  DiagrammeR::grViz(readr::read_file('figures/metabolyseR.gv'))

## ----echo=FALSE,fig.width=6----------------------------------------------
  DiagrammeR::grViz(readr::read_file('figures/preTreat.gv'))

## ----echo=FALSE,results='asis'-------------------------------------------
desc <- metabolyseR:::fsMethods(description = T)
meth <- names(metabolyseR:::fsMethods())
args <- lapply(metabolyseR:::fsMethods(),formals)
args <- lapply(args,function(x){x$dat <- NULL;return(x)})
names
args <- lapply(args,function(x){paste(names(x),x,collapse = ' = ')})

tab <- tibble::tibble(Method = meth, Details = desc, `Arguments/Defaults` = )

knitr::kable(tab)

