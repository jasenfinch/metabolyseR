#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel miniTabstripPanel miniTabPanel
#' @importFrom shiny dialogViewer runGadget icon
#' @export

analysisParameterSelect <- function(){
  ui <- miniPage(
    gadgetTitleBar("Analysis Parameter Selection"),
    miniTabstripPanel(
      miniTabPanel('Pre-treatment',icon = icon('filter'),
                   miniContentPanel()
      ),
      miniTabPanel('Classification',
                   miniContentPanel()
      ),
      miniTabPanel('Feature Selection',
                   miniContentPanel()
      ),
      miniTabPanel('Correlations',
                   miniContentPanel()
      )
    )
  )
  
  server <- function(input,output,session){
    
  }
  viewer <- dialogViewer('Analysis Parameter Selection')
  runGadget(ui, server, viewer = viewer)
  
}