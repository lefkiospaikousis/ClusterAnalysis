#' Partitioning Around Medoids (kmeds) UI Function
#' 
#'
#' @description A shiny Module for running a k-medoids clustering.   
#' The module comes with a UI where the user can adjust the parameters 
#' of the clustering, such as Number of Clusters, the seed for the random number generator
#' and others..
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @return A List of the a. The \code{cluster::pam} result, and b. the clustering vector
#' which is the corrected if the dataset to be used has missing values
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmedoids_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_4(
      h3("K-medoids parameters"),
      fluidRow(
        col_4(
          numericInput(ns("n_clust"), "Number of clusters", value = 3), 
          class = "small-font", style = "margin-top: 15px"),
        col_4(
          selectInput(ns("metric"), 
                      "Metric for dissimilarity matrix", 
                      choices = c("Euclidean" = "euclidean",
                                  "Manhattan" = "manhattan",
                                  "Gower's distance" = "gower")
          ),  class = "small-font")
      )   
    )
    #,verbatimTextOutput(ns("clust_index"))
  )
}

#' kmedoids Server Functions
#'
#' @param dta The dataset. Need a df with at least 2 rows
#' @param vars_cluster String. A vector  of the variables to use for the clustering
#' @param seed Numeric length 1. The seed number for reproducibility
#' 
#' @noRd 
mod_kmedoids_server <- function(id, dta, vars_cluster, seed = reactive(123)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dta_cleaned <- reactive({
      
      req(dta())
      
      if(!isTruthy(vars_cluster())) {
        validate("Please select at least 1 variable for clustering")
      }
      
      # if(!any(vars_cluster() %in% find_vars_of_type(dta(), "numeric"))) {
      #   validate("Please select at least 1 variable for clustering")
      # }
      
      # 1. scale
      dta() %>% 
        select(all_of(vars_cluster())) %>% 
        mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>% 
        # always do it a dataframe. This keeps the id index of the clustering group
        # as the data.frame keeps the ommited (in case of NA's) indexes
        # The dissimilarity produces by the cluster::daisy, recongnises thw
        # ommited cases
        as.data.frame()
      
    })
    
    
    diss_matrix <- reactive({
      
      req(dta_cleaned())
      
      all_vars_numeric <- all(purrr::map_lgl(dta_cleaned(), ~inherits(., "numeric")))
      
      
      shinyFeedback::feedback("metric", show = !all_vars_numeric,
                              text = "At least one of your variables is 
                                     non numeric. Gower's distance is used",
                              color = "#068bf8",
                              icon = icon("info-sign", lib = "glyphicon") 
      )
      
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
          
          cluster::pam(
            diss_matrix(), 
            k = input$n_clust,
            keep.diss = FALSE,
            keep.data = FALSE
          )
          
        },
        
        error = function(e){
          
          print(e)
          validate("Something has gone wrong with the k-medoids clustering")
          
        }
      )
     
    })
    
    # return the clustering vector and 
    # The cluster::daisy handles the missing cases
    clustering_vector <- reactive({
      
      #k_meds()$clustering
      get_pam_cluster_indx(dta_cleaned(), k_meds()$clustering)
      
    })
    
    output$k_meds <- renderPrint({k_meds()})
    
    output$clust_index <- renderPrint(clustering_vector())
    
    
    return(
      list(
        res = k_meds
        #clust_index = clustering_vector
      )
      
    )
    
  })
}

## To be copied in the UI
# mod_kmedoids_ui("kmedoids_ui_1")

## To be copied in the server
# mod_kmedoids_server("kmedoids_ui_1")
