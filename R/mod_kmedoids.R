#' Partitioning Around Medoids (kmeds) UI Function
#'
#' @description A shiny Module for running a k-medoids clustering using the \code{cluster::pam}.       
#' The module comes with a UI where the user can adjust the parameters 
#' of the clustering, such as Number of Clusters, the seed for the random number generator
#' and others..
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @return The \code{cluster::pam} object along with the a. clustering vector
#' and b. the silhouette table
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmedoids_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
        h3("K-medoids clustering")
      ),
      fluidRow(
        pickVarsInput(ns("vars_cluster")),
      ),
      fluidRow(
        col_4(
          numericInput(ns("n_clust"), "Number of clusters", value = 3)
          ),#,class = "small-font"),# style = "margin-top: 15px"),
        col_6(
          selectInput(ns("metric"), 
                      "Metric for dissimilarity matrix", 
                      choices = c("Euclidean" = "euclidean",
                                  "Manhattan" = "manhattan",
                                  "Gower's distance" = "gower")
          )), #,  class = "small-font")
      )   
  )
}

#' kmedoids Server Functions
#'
#' @param dta The dataset. Need a df with at least 2 rows
#' @param seed Numeric length 1. The seed number for reproducibility
#' 
#' @noRd 
mod_kmedoids_server <- function(id, dta, seed = reactive(123)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(dta(), {
      
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "vars_cluster",
                                      selected = character(0),
                                      choices = names(dta())
                                      
      )
    }, priority = 10)
    
    
    dta_cleaned <- reactive({
      
      req(dta())
      
      if(!any(input$vars_cluster %in% names(dta()))) {
        validate("Please select at least 1 variable for clustering")
      }
      
      # 1. scale
      dta() %>% 
        select(all_of(input$vars_cluster)) %>% 
        mutate(across(where(is.numeric), scale2, na.rm = TRUE)) %>% 
        # as.data.frame keeps the id index of the clustering group
        # It keeps the ommited (in case of NA's) indexes
        # The dissimilarity produces by the cluster::daisy, recognises the
        # ommited cases
        as.data.frame()
      
    })
    
    
    observeEvent(dta_cleaned(), {
      
      all_vars_numeric <- all(purrr::map_lgl(dta_cleaned(), ~inherits(., "numeric")))
      
      shinyFeedback::feedback("metric", show = !all_vars_numeric,
                              text = "At least one of your variables is non numeric. 
                                      Gower's distance is used", color = "#068bf8",
                              icon = icon("info-sign", lib = "glyphicon")
      )
      
      if(isFALSE(all_vars_numeric) & input$metric != "gower"){
        
        updateSelectInput(inputId = "metric", selected = "gower")
        
      } 
      
      
    })
    
    
    diss_matrix <- reactive({
      
      req(dta_cleaned())
      
      calc_diss_matrix(
        dta = dta_cleaned() %>% na.omit(),
        metric = input$metric
      ) 
      
    })
    
    k_meds <- reactive({
      
      # checks before running
      # 1. k<n_obs, 
      req(diss_matrix())
      
      tryCatch(
        expr = {
          set.seed(seed())
          
          res <- cluster::pam(
            diss_matrix(), 
            k = input$n_clust,
            keep.diss = FALSE,
            keep.data = FALSE
          )
          
          res$silhouette <- get_sil_widths(res, diss_matrix())
          res$diss_matrix <- diss_matrix()
          res$vars_cluster <- isolate(names(dta_cleaned()))
          res
        },
        
        error = function(e){
          
          print(e)
          validate("Something has gone wrong with the k-medoids clustering")
          
        }
      )
      
    })
    
    output$k_meds <- renderPrint({k_meds()})
    
    
    return( k_meds )
    
  })
}

## To be copied in the UI
# mod_kmedoids_ui("kmedoids_ui_1")

## To be copied in the server
# mod_kmedoids_server("kmedoids_ui_1")
