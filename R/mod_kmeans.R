#' K-Means Clustering UI Function
#'
#' @description A shiny Module for running a k-means clustering.   
#' The module comes with a UI where the user can adjust the parameters 
#' of the clustering, such as Number of Clusters, the seed for the random number generator
#' and others..
#' 
#' @return The \code{kmeans} object along with the a. clustering vector
#' and b. the silhouette table
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmeans_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    col_4(
      
      div(
        fluidRow(
          h3("K-means parameters")
        ),
        fluidRow(
          col_4(
            numericInput(ns("n_clust"), "Number of clusters", value = 3),  class = "small-font", style = "margin-top: 15px"),
          col_4(
            numericInput(ns("iter.max"), "Maximum number of iterations", value = 1),  class = "small-font"),
          col_4(
            numericInput(ns("n_start"), "Random sets of cluster centers", value = 10),  class = "small-font")
        ) 
        
        #, style = "border-style: solid; border-color: coral"
        )
      
    )
    
  )
}

#' kmeans Server Functions
#'
#' @param dta The dataset. Need a df with at least 2 rows
#' @param vars_cluster String. A vector  of the variables to use for the clustering
#' @param seed Numeric length 1. The seed number for reproducibility
#' @noRd 
mod_kmeans_server <- function(id, dta, vars_cluster, seed = reactive(123)){
  
  # stopifnot(length(seed) == 1)
  # stopifnot(is.numeric(seed))
  
  # stopifnot(inherits(dta(), "data.frame"))
  # stopifnot(nrow(dta()) > 1)
  # 
  # stopifnot(all(vars_cluster %in% names(dta())))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    dta_cleaned <- reactive({
      
      req(dta())
      
      if(!isTruthy(vars_cluster())) {
        validate("Please select at least 1 numeric variable for clustering")
      }
      
      if(!any(vars_cluster() %in% find_vars_of_type(dta(), "numeric"))) {
        validate("Please select at least 1 numeric variable for clustering")
      }
      
      # 1. scale
      # 2. only numeric for Kmeans
      dta() %>% 
        select(all_of(vars_cluster())) %>% 
        purrr::keep(is.numeric) %>% 
        mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>% 
        as.data.frame()
      
    })
    
    # I do it only for the silhouetee 
    diss_matrix <- reactive({
      
      req(dta_cleaned())
      
      calc_diss_matrix(
        dta = dta_cleaned() %>% na.omit(), metric = "euclidean"
      ) 
      
    })
    
    
    k_means <- reactive({
      
      req(dta_cleaned())
      
      tryCatch(
        expr = {
          set.seed(seed())
          
          res <- kmeans(
            dta_cleaned() %>% na.omit(),
            centers = input$n_clust,
            nstart  = input$n_start
          )
          
          res$silhouette <- get_sil_widths(res, diss_matrix())
          res$diss_matrix <- diss_matrix()
          res
        },
        
        error = function(e){
          
          print(e)
          validate("Something has gone wrong with the k-means clustering")
          
        }
      )
      
    })
    
    output$k_means <- renderPrint({
      
      k_means()
      
    })
    
    output$clust_index <- renderPrint(clustering_vector())
    
    
    return(
      k_means
      # list(
      #   res = k_means,
      #   silhouette = tbl_silhouette
      # )
      
    )
    
    
  })
}

## To be copied in the UI
# mod_kmeans_ui("kmeans_ui_1")

## To be copied in the server
# mod_kmeans_server("kmeans_ui_1")
