#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function( input, output, session ) {
  
  
  # Modules -----------------------------------------------------------------
  
  res_kmeans <- mod_kmeans_server("kmeans_ui_1", dta, vars_for_cluster, seed = reactive(input$seed))
  
  res_kmeds  <- mod_kmedoids_server("kmedoids_ui_1", dta, vars_for_cluster, seed = reactive(input$seed))
  
  res_hclust <- mod_hclust_server("hlust_ui_1", dta, vars_for_cluster, seed = reactive(input$seed))
  
  
  active_clustering <- reactive({
    
    switch (input$clust_method,
            "k-means" = res_kmeans(),
            'k-meds'  = res_kmeds(),
            'h-clust' = res_hclust(),
            validate(glue::glue("{input$clust_method} Not yet ready"))
    )
    
  })
  

  # Constants ---------------------------------------------------------------
  
  # Cluster Methods
  clust_methods <- c("Hierarchical", "k-Medoids (PAM)", "k-means")
  
  # HC clustering methods in cluster::agnes
  hc_methods  <- c("Ward's method"                    = "ward", 
                   "Single linkage"                   = "single", 
                   "Complete linkage"                 = "complete", 
                   "Average (UPGMA)"                  = "average",
                   "Weighted Average linkage (WPGMA)" = "average"
  ) #, "mcquitty", "median", "centroid"
  
  hc_distance <- c("euclidean", "manhattan")
  n_clusters  <- seq(2,7)
  
  # a list of statistics for silhouette summaries
  silhouette_summary = list(
    mean   = ~mean(., na.rm = TRUE),
    sd     = ~sd(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    min    = ~min(., na.rm = TRUE),
    max    = ~max(., na.rm = TRUE)
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
      
      penguins %>% # Sample file/ or penguins_raw
        slice_sample(n = 60) %>% 
        tibble::rowid_to_column("id")  
      
    } else {
      
      #TODO safe_read_file with purrr::safely or possibly
      dta_upld <- read_file(input$file$datapath)
      
      dta_upld %>% 
        tibble::rowid_to_column("id")
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
  
  
  
  output$res_cluster <- renderPrint({ active_clustering() })
  
  output$cluster_group <- renderPrint({ active_clustering()$cluster })
  
  tbl_silhouette <- reactive({ active_clustering()$silhouette })
  
  output$tbl_silhouette <- DT::renderDT({ tbl_silhouette() })
  
  
  dta_updated <- reactive({
    
    
    # add silhouette information
    # add cluster membership
    
    req(active_clustering())
    tryCatch(

      expr = {

        dta() %>% 
          add_silhouette_to_dta(active_clustering()$silhouette)
        
      },
      error = function(e){

        print(paste("Error at :",Sys.time(), e))
        showModal(modalDialog("Something went terribly wrong"))
      }
    )
    
  })
  
  
  output$dta_updated <- renderTable(
    
    head(dta_updated())
  )
  
  
  hc_plot <- reactive({
    
    req(res_hclust())
    req(input$clust_method == "h-clust")
    
    
    #browser()
    dendro <- stats::as.dendrogram(res_hclust())
    
    #if(!is.null(input$id_var)){
      
    dendextend::set(dendro, "labels", dta()$species[res_hclust()$order]) 
      
    #}
    
    n_clust <- res_hclust()$cluster %>% unique() %>% length()
    
    set.seed(input$seed)
    colrs <- colours()[sample(600, n_clust)]
      
    # c("skyblue", "orange", "grey", "olivedrab", "moccasin", "bisque4")
    
    # $order.lab is the actual ids of the participant cases
    # There is also the .$order but that is just the order of the cases
    # When there are no missing values in the original dataset, then 
    # this is the sameas the .order.lab
    # remember that the na.omit reserves the original case ids
    new_labels <- dta()$species[as.numeric(res_hclust()$order.lab)]
    
    cols <- c("skyblue", "orange", "grey", "chocolate4", "darkmagenta", "darkgreen")
    
    dendro <- 
      dendro %>%
      dendextend::set("labels_colors", value = cols, k = n_clust) %>%
      dendextend::set("branches_k_color", value = cols, k = n_clust) %>%
      #dendextend::set("leaves_pch", 19)  %>%
      dendextend::set("nodes_cex", 0.7) %>%
      dendextend::set("labels", new_labels) %>%
      #dendextend::set("labels", "") %>%
      dendextend::set("labels_cex", 0.8) %>%  # labels  size
      dendextend::set("leaves_cex", 2) %>%  # node point size
      #dendextend::set("leaves_pch", c(17, 18, 19)) %>%  # node point type
      dendextend::set("leaves_col", cols) %>% #node point color
      #plot(horiz=TRUE, axes=FALSE) %>% 
      {.}
    
    ggplot2::ggplot(
      
      dendextend::as.ggdend(dendro),
      horiz = FALSE
    ) + 
      ggplot2::ylim(-3, NA)+
      #dendextend::theme_dendro()+
      NULL
    
  })
  
  output$hc_plot <- renderPlot({ hc_plot() })
  
  
  observe({
    
    shinyjs::toggleElement(id = "show_dendro", condition = input$clust_method == "h-clust")
  })
  
  observeEvent(input$show_dendro, {
    #browser()
    showModal(myModal())
  })
  
  myModal <- function(){
    
    modalDialog(
      tagList(
        plotOutput("hc_plot")
        
      )
      , size = "l"
      
      , easyClose = TRUE
    )
 
  }
  
}
