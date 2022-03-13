#' Hierarchical Clustering UI Function
#' 
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hclust_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_4(
      h3("Hierarchical Clustering parameters"),
      fluidRow(
        col_4(
          numericInput(ns("n_clust"), "Number of clusters", value = 3),  
          class = "small-font", style = "margin-top: 15px"),
        col_4(
          selectInput(ns("metric"), "Metric for dissimilarity matrix", 
                      choices = c("Euclidean" = "euclidean",
                                  "Manhattan" = "manhattan",
                                  "Gower's distance" = "gower")
          ),  class = "small-font"),
        col_4(
          selectInput(ns("hc_linkage"), "Linkage method", 
                      choices = c("Ward's method" = "ward", 
                                  "Single linkage" = "single", 
                                  "Complete linkage" = "complete", 
                                  "Average (UPGMA)" = "average",
                                  "Weighted Average linkage (WPGMA)" = "average"
                      )
          ),  class = "small-font", style = "margin-top: 15px")
        
        
        # col_4(
        #   p("Hierarchical clustering needs a ", strong("linkage method.")),
        #   p(strong("Note that"), " Linkage methods may have  strong
        # impact on the cluster formation. See the INFO tab"),
        #   )
      )
    )   
    #,verbatimTextOutput(ns("clust_index"))
  )
}

#' Hierarchical Clustering Server Functions
#'
#' @noRd 
mod_hclust_server <- function(id, dta, vars_cluster, seed = reactive(123)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dta_cleaned <- reactive({
      
      req(dta())
      
      if(!isTruthy(vars_cluster())) {
        validate("Please select at least 1 variable for clustering")
      }
      
      # 1. scale
      dta() %>% 
        select(all_of(vars_cluster())) %>% 
        mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>% 
        # always do it a dataframe. This keeps the id index of the clustering group
        # as the data.frame keeps the ommited (in case of NA's) indexes
        # The dissimilarity produces by the cluster::daisy, recognizes the ommited cases
        as.data.frame()
      
    })
    
    
    diss_matrix <- reactive({
      
      req(dta_cleaned())
      
      all_vars_numeric <- all(purrr::map_lgl(dta_cleaned(), ~inherits(., "numeric")))
      
      
      shinyFeedback::feedback("metric", show = !all_vars_numeric,
                              text = "At least one of your variables is non numeric. 
                              Gower's distance will be used",
                              color = "#068bf8",
                              icon = icon("info-sign", lib = "glyphicon") 
      )
      
      calc_diss_matrix(
        dta = dta_cleaned() %>% na.omit(),
        metric = input$metric
      ) 
      
    })
    
    h_clust <- reactive({
      
      # checks before running
      # 1. k<n_obs, 
      req(diss_matrix())
      
      tryCatch(
        expr = {
          set.seed(seed())
          
          res <- cluster::agnes(
            diss_matrix(), 
            method = input$hc_linkage,
            keep.diss = FALSE,
            keep.data = FALSE
          )
          
          # diss_matrix respect the na.omit. so we know the true (is any missing)
          # of the observation id
          ids <- attr(diss_matrix(), "Labels")
          
          # add a cluster- a named vector 
          res$cluster <- stats::cutree(res, input$n_clust) %>% setNames(ids)
          
          res
        },
        
        error = function(e){
          
          print(e)
          validate("Something has gone wrong with the Hierarchical clustering")
          
        }
      )
      
    })
    
    # return the clustering vector and 
    # The cluster::daisy handles the missing cases
    # clustering_vector <- reactive({
    #   
    #   temp_vec <- stats::cutree(h_clust(), input$n_clust)
    #   get_cluster_indx(dta_cleaned(), temp_vec)
    #   
    # })
    
    output$h_clust <- renderPrint({h_clust()})
    
    output$clust_index <- renderPrint(h_clust()$cluster)
    
    
    return(
      list(
        res = h_clust
      )
      
    )
  })
}

## To be copied in the UI
# mod_hclust_ui("hlust_ui_1")

## To be copied in the server
# mod_hclust_server("hlust_ui_1")
