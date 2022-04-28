#' upload_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_upload_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("file"), 
      label = HTML("Currently accepted data formats:<br>
                   .csv, .tsv, .sav(SPSS), or .xlsx, .xls (Excel) files"), 
      accept = c(".csv", ".tsv", ".sav", ".xlsx", ".xls")),
  )
}

#' upload_file Server Functions
#'
#' @noRd
mod_upload_file_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dta <- reactive({
      
      req(input$file)
      
      tryCatch(
        
        {
          dta <- read_file(input$file$datapath)
          
          dta <- process_file(dta)
          
          shinyFeedback::showToast(
            type = "success", message = "Data imported succesfully",
            .options = list(positionClass = "toast-top-center")
          )
          
          return(dta)
          
        },
        
        error = function(e){
          
          #' NOTE: The `Invalid file` error message in found in the `read_file()` function
          if(e$message == "Invalid file"){
            
            shinyFeedback::showToast(
              type = "error", title = "Invalid file!", 
              message = HTML("Accepted file types are:<br>.csv, .tsv, .xlsx, .xls, .sav"),
              keepVisible = TRUE,
              .options = list(positionClass = "toast-top-center",
                              closeButton = TRUE)
            )
            
          } else {
            
            shinyFeedback::showToast(
              type = "error", title = "Oopss!", 
              message = "Something went wrong when uploading your file",
              .options = list(positionClass = "toast-top-center",
                              showDuration = 1000)
            )
            
          }
          
          
          print(paste("Error when reading file at :", Sys.time(), e))
          
          return(e)
          
        }
      )
      
      
      
    })
    
    return(dta)
    
  })
}

## To be copied in the UI
# mod_upload_file_ui("upload_file_ui_1")

## To be copied in the server
# mod_upload_file_server("upload_file_ui_1")
