#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    
        # div(id = ns("input_values"),
        #     splitLayout(
        #       cellWidths = c("30%", "30%", "30%"),
        #       numericInput(ns("n_clust"), "Number of clusters", value = 3),
        #       numericInput(ns("iter.max"), "Random sets of cluster centers", value = 1),
        #       numericInput(ns("n_start"), "Random sets of cluster centers", value = 10)
        #       
        #     ),
        #     p("Fill in the mean occurrence values")
        # )
        fluidRow(
          column(2, numericInput(ns("n_clust"), "Number of clusters", value = 3)),
          column(2, numericInput(ns("iter.max"), "Maximum number of iterations allowed", value = 1)),
          column(2, numericInput(ns("n_start"), "Random sets of cluster centers", value = 10))
        )    
    
      
  )
}
    
#' test Server Functions
#'
#' @noRd 
mod_test_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_test_ui("test_ui_1")
    
## To be copied in the server
# mod_test_server("test_ui_1")
