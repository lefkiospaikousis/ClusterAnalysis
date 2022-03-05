#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  

# Modules -----------------------------------------------------------------

  res_kmeans <- mod_kmeans_server("kmeans_ui_1", dta, vars_for_cluster)
  
  output$cluster <- renderPrint(res_kmeans$res())

# Constants ---------------------------------------------------------------

  # Cluster Methods
  clust_methods <- c("Hierarchical", "k- Medoids (PAM)", "k - means")
  
  # HC methods
  hc_methods  <- c("ward.D", "ward.D2", "single", "complete", "average") #, "mcquitty", "median", "centroid"
  hc_distance <- c("euclidean", "manhattan")
  n_clusters  <- seq(2,7)
  
  # a list of statistics for silhouette summaries
  sil_summary = list(
    ~mean(., na.rm = TRUE),
    ~sd(., na.rm = TRUE),
    ~median(., na.rm = TRUE),
    ~min(., na.rm = TRUE),
    ~max(., na.rm = TRUE)
  )
  
  # Cluster statistics the package {fpc} produces using the fpc::cluster.stats(dissMatrix, cluster_membership)
  stats_per_clust <- c("average.distance" = "within cluster average distances"
                       , "median.distance" = "within cluster distance medians"
                       , "separation" = "minimum distances of a point in the cluster to a point of another cluster"
                       , "average.toother" = "average distances of a point in the cluster to the points of other clusters"
                       , "clus.avg.silwidths" = "average silhouette widths"
                       #,"cluster.size" = "cluster size"
  )
  
  stats_overall <- c("average.between" = "average distance between clusters"
                     , "average.within" = "average distance within clusters"
                     , "avg.silwidth" = "average silhouette width"
                     , "sindex" = "adjusted separation index"
                     # for sindex see the documentation. less sensitive to a single or a few ambiguous points for 
                     #,"within.cluster.ss" = "a generalisation of the within clusters sum of squares"
                     ,"ch"
  )
  

# Server declaration ------------------------------------------------------

  
  dta <- reactive({
    
    if(!isTruthy(input$file)) {
      
      penguins  # Sample file
      
    } else {
      
      #TODO safe_read_file with purrr::safely or possibly
      dta_upld <- read_file(input$file$datapath)
      
      dta_upld
    }
    
    
  })
  
  
  vars_for_cluster <- reactive({
    
    input$vars_cluster
    
    })
  
  observeEvent(dta(), {
    
    shinyWidgets::updatePickerInput(session, "vars_cluster",
                                    choices = names(dta())
    )
  }, priority = 10)
  
  file_info <- reactive({
    req(input$file)
    input$file
  })
  
  
  labels_list <- reactive({
    req(dta())
    get_var_labels(dta(), unlist = FALSE)
  })
  
  output$dta <- reactable::renderReactable({
    
    dta() %>% 
      reactable::reactable(
        searchable = TRUE, defaultPageSize = 15, resizable = TRUE, compact = TRUE,
        wrap = FALSE, highlight = TRUE
      )
    
  })
  
  dta_scaled <- reactive({
    
    dta() %>% 
      mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>%
      # cluster::daisy with character vars not working, need to be factors
      mutate_if(is.character, ~ factor(.))
    
  })
  
  
  
}
