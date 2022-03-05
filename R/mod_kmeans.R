#' kmeans UI Function
#'
#' @description A shiny Module for running a k-means clustering.   
#' The module comes with a UI where the user can adjust the parameters 
#' of the clustering, such as Number of Clusters, the seed for the random number generator
#' and others..
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmeans_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(title = "Parameters",
                            fluidRow(
                              numericInput(ns("n_clust"), "Number of clusters", value = 3),
                              numericInput(ns("n_start"), "Random sets of cluster centers", value = 10)
                            )
                            ),
    verbatimTextOutput(ns("clust_index"))
  
    
  )
}
    
#' kmeans Server Functions
#'
#' @param dta The dataset. Need a df with at least 2 rows
#' @param vars_cluster String. A vector  of the variables to use for the clustering
#' @param seed Numeric length 1. The seed number for reproducibility
#' @noRd 
mod_kmeans_server <- function(id, dta, vars_cluster, seed = 123){
  
  stopifnot(length(seed) == 1)
  stopifnot(is.numeric(seed))

  # stopifnot(inherits(dta(), "data.frame"))
  # stopifnot(nrow(dta()) > 1)
  # 
  # stopifnot(all(vars_cluster %in% names(dta())))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      dta_org = NULL
  
    )
  
    
    # observe({
    #   req(dta())
    #   rv$dta_org <- dta() 
    # })
    
    
    
    dta_cleaned <- reactive({
      
      req(dta())
      #req(vars_cluster())
      
      if(!isTruthy(vars_cluster())) {
        validate("Please select at least 1 numeric variable for clustering")
      }
      
      if(!any(vars_cluster() %in% find_vars_of_type(dta(), "numeric"))) {
        validate("Please select at least 1 numeric variable for clustering")
      }
      
      # 1. scale
      # 2. remove NAs
      # 3. only numeric for Kmeans
      dta() %>% 
        select(all_of(vars_cluster())) %>% 
        purrr::keep(is.numeric) %>% 
        mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>% 
        as.data.frame()
        
    })
    
    
    k_means <- reactive({
      
      set.seed(seed)
      
      kmeans(
        dta_cleaned() %>% na.omit(),
        centers = input$n_clust,
        nstart = input$n_start
        )
      
    })
    
    # return the clustering vector and 
    # control the NA's also
    clustering_vector <- reactive({
      
      get_cluster_indx(dta_cleaned(), k_means()$cluster)
      
    })
    
    output$k_means <- renderPrint({
      
      k_means()
      
      })
    
    output$clust_index <- renderPrint(clustering_vector())
    
    
    return(
      list(
        res = k_means,
        clust_index = clustering_vector
      )
        
      )
      
    
  })
}
    
## To be copied in the UI
# mod_kmeans_ui("kmeans_ui_1")
    
## To be copied in the server
# mod_kmeans_server("kmeans_ui_1")
