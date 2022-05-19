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
        fluidRow(
          h3("K-means clustering")
        ),
        fluidRow(
          pickVarsInput(ns("vars_cluster"))
        ),
        fluidRow(
          col_4(
            numericInput(ns("n_clust"), "Number of clusters", value = 3),  style = "margin-top: 17px"),
          col_4(
            numericInput(ns("iter.max"), "Maximum number of iterations", value = 1)),
          col_4(
            numericInput(ns("n_start"), "Random sets of cluster centers", value = 10))
        ) 
  )
}

#' kmeans Server Functions
#'
#' @param dta The dataset. Need a df with at least 2 rows
#' @param seed Numeric length 1. The seed number for reproducibility
#' @noRd 
mod_kmeans_server <- function(id, dta, seed = reactive(123)){
  
  # stopifnot(length(seed) == 1)
  # stopifnot(is.numeric(seed))
  # stopifnot(inherits(dta(), "data.frame"))
  # stopifnot(nrow(dta()) > 1)
  # 
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(dta(), {
      
      shinyWidgets::updatePickerInput(session, "vars_cluster",
                                      selected = character(0),
                                      choices = names(dta()))

    }, priority = 10)
    
    dta_cleaned <- reactive({
      
      req(dta())
      
      req(any(input$vars_cluster %in% vars_of_type(dta(), "numeric")))
      
      if(!any(input$vars_cluster %in% vars_of_type(dta(), "numeric"))) {
        validate("Please select at least 1 numeric variable for clustering")
      }
      
      # 1. scale
      # 2. only numeric for Kmeans
      
      out <- dta() %>% 
        select(all_of(input$vars_cluster)) %>% 
        purrr::keep(is.numeric) %>% 
        mutate(across(where(is.numeric), scale2, na.rm = TRUE)) 
      
      new_names <- make.names(names(out))
      
      out %>% 
        rename_all(~new_names) %>%
        # as.data.frame keeps the id index of the clustering group
        # It keeps the ommited (in case of NA's) indexes
        # The dissimilarity produces by the cluster::daisy, recognises the
        # ommited cases
        as.data.frame(cut.names = TRUE)
      
    })
    
    # I do it only for the silhouette 
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
            x = dta_cleaned() %>% na.omit(),
            centers = input$n_clust,
            nstart  = input$n_start
          )
          
          res$silhouette <- get_sil_widths(res, diss_matrix())
          res$diss_matrix <- diss_matrix()
          res$vars_cluster <- res$centers %>% dimnames() %>% .[[2]]
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
    
    
    return( k_means )
    
    
  })
}

## To be copied in the UI
# mod_kmeans_ui("kmeans_ui_1")

## To be copied in the server
# mod_kmeans_server("kmeans_ui_1")
