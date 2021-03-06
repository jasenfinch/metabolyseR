
clsLen <- function(x,cls){
  x %>%
    clsExtract(cls) %>%
    unique() %>%
    length()
}

shapeValues <- function(classLength){
  sym <- 0:25
  if (classLength / max(sym) == 1) {
    val <- sym
  }
  if (classLength / max(sym) < 1) {
    val <- sym[1:classLength]
  }
  if (classLength / max(sym) > 1) {
    if (classLength %% max(sym) == 0) {
      val <- rep(sym,classLength / max(sym))
    } else {
      val <- c(rep(sym,
                   floor(classLength / max(sym))),
               sym[1:(classLength %% max(sym))])
    }
  }
  return(val)
}

plotTheme <- function(pl,legendPosition,title,xLabel,yLabel){
  pl +
    labs(title = title,
         x = xLabel,
         y = yLabel) +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',
                                    hjust = 0.5),
          axis.title = element_text(face = 'bold'),
          legend.title = element_text(face = 'bold'),
          legend.position = legendPosition,
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          strip.background = element_blank(),
          strip.text = element_text(face = 'bold'))
  
}

plotBase <- function(d,xAxis,yAxis){
  ggplot(d,aes(x = !!sym(xAxis),y  = !!sym(yAxis))) +
    geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
    geom_vline(xintercept = 0,linetype = 2,colour = 'grey') +
    coord_fixed()
}

plotEllipses <- function(pl,cls,ellipses,classLength){
  if (isTRUE(ellipses)) {
    if (!is.null(cls)){
      if (classLength <= 12) {
        pl <- pl +
          stat_ellipse(aes(colour = !!sym(cls)),
                       geom = 'polygon',
                       type = 'norm',
                       linetype = 5,
                       fill = NA) 
      } else {
        message('Number of classes > 12, ellipses removed.')
      }  
    } else {
     pl <- pl +
       stat_ellipse(colour = ptol_pal()(1),
                    geom = 'polygon',
                    type = 'norm',
                    linetype = 5,
                    fill = NA)
    }
  }
  return(pl)
}

plotLabel <- function(pl,label,labelSize,classLength) {
  if (!is.null(label)) {
    pl <- pl +
      geom_text_repel(aes(label = !!sym(label)),size = labelSize)
  }
  return(pl)
}

plotShape <- function(pl,cls,shape,classLength,pointSize = 3){
  
  if (is.null(cls)){
    pl <- pl +
      geom_point(size = pointSize,shape = 21,fill = ptol_pal()(1))
  } else {
    if (isFALSE(shape) & classLength <= 12) {
      pl <- pl +
        geom_point(aes(fill = !!sym(cls)),shape = 21,size = pointSize) 
    } else {
      if (classLength > 12 & isFALSE(shape)) {
        message('Number of classes > 12, using shape aesthetic.')
      }
      
      pl <- pl + 
        geom_point(aes(colour = !!sym(cls),shape = !!sym(cls)),size = pointSize) 
      
      if (classLength > 6) {
        val <- shapeValues(classLength)
        
        pl <- pl + 
          scale_shape_manual(values = val)
      }
    }  
  }
  
  return(pl)
}

plotColour <- function(pl,classLength){
  if (classLength <= 12) {
    pl <- pl + 
      scale_colour_ptol() +
      scale_fill_ptol()
  } else {
    if (classLength %% 12 == 0) {
      pal <- rep(ptol_pal()(12),classLength / 12)
    } else {
      pal <- c(rep(ptol_pal()(12),
                   floor(classLength / 12)),
               ptol_pal()(12)[1:(classLength %% 12)])
    }
    pl <- pl + 
      scale_colour_manual(values = pal) +
      scale_fill_manual(values = pal)
  }
  return(pl)
}

scatterPlot <- function(d,
                        cls,
                        xAxis,
                        yAxis,
                        ellipses,
                        shape,
                        label,
                        labelSize,
                        legendPosition,
                        classLength,
                        title,
                        xLabel,
                        yLabel){
  pl <- d %>%
    plotBase(xAxis,yAxis) %>% 
    plotEllipses(cls,ellipses,classLength) %>%
    plotLabel(label,labelSize,classLength) %>%
    plotShape(cls,shape,classLength) 
  
  if (!is.null(cls)){
    pl <- pl %>%
      plotColour(classLength)  
  }
  pl %>%
    plotTheme(legendPosition,
              title = title,
              xLabel = xLabel,
              yLabel = yLabel)
}
