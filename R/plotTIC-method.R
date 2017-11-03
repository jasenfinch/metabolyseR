#' @export

setMethod('plotTIC',signature = 'Analysis',
          function(analysis, by = 'injOrder') {
            dat <- rawData(analysis)
            info <- dat$Info
            dat <- dat$Data
            
            index <- info[,by] 
            colnames(index)[1] <- 'Index'
            
            dat <- dat %>%
              bind_cols(index) %>%
              mutate(Colour = factor(Index)) %>%
              rowid_to_column(var = 'ID') %>%
              gather('Feature','Intensity',-ID,-Index,-Colour) %>%
              group_by(ID,Index,Colour) %>%
              summarise(TIC = sum(Intensity))
           
            classCheck <- dat %>%
              group_by(Index) %>%
              summarise(Frequency = n())
             
            pl <- dat %>%
              ggplot(aes(x = Index,y = TIC,colour = Colour)) +
              theme_bw() +
              guides(colour = F) +
              xlab(by)
            
            if (T %in% (classCheck$Frequency > 1)) {
              pl <- pl + 
                geom_boxplot(aes(group = Index),outlier.shape = NA,colour = 'black') +
                geom_point(position = 'jitter')
            } else {
              pl <- pl + geom_point()
            }
            
            if (nrow(classCheck) <= 12) {
              pl <- pl + scale_colour_ptol()
            } else {
              if (nrow(classCheck) %% 12 == 0) {
                pal <- rep(ptol_pal()(12),nrow(classCheck) / 12)
              } else {
                pal <- c(rep(ptol_pal()(12),floor(nrow(classCheck) / 12)),ptol_pal()(12)[1:(nrow(classCheck) %% 12)])
              }
              pl <- pl + scale_colour_manual(values = pal)
            }
            
            pl
          }
)