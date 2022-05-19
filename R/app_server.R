#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @importFrom reactable reactable renderReactable reactableOutput colDef colFormat
#' @importFrom ggplot2 ggplot aes geom_density geom_vline facet_wrap scale_color_manual scale_fill_brewer
#' @importFrom stats complete.cases kmeans median na.omit sd setNames
#' @noRd
app_server <- function( input, output, session ) {
  
  
  # Modules -----------------------------------------------------------------
  
  res_kmeans <- mod_kmeans_server("kmeans_ui_1", dta, seed = reactive(input$seed))
  
  res_kmeds  <- mod_kmedoids_server("kmedoids_ui_1", dta, seed = reactive(input$seed))
  
  res_hclust <- mod_hclust_server("hlust_ui_1", dta, seed = reactive(input$seed))
  
  
  # active_clustering <- reactive({
  #   
  #   updateTabsetPanel(inputId = "switcher", selected = input$cluster_method)
  #   out <- switch (input$cluster_method,
  #           "k-means" = mod_kmeans_server("kmeans_ui_1", dta, seed = reactive(input$seed))(),#res_kmeans(),
  #           'k-meds'  = mod_kmedoids_server("kmedoids_ui_1", dta, seed = reactive(input$seed))(), #res_kmeds(),
  #           'h-clust' = mod_hclust_server("hlust_ui_1", dta, seed = reactive(input$seed))(), #res_hclust(),
  #           validate(glue::glue("{input$cluster_method} Not yet ready"))
  #   )
  #   
  #   
  #   out
  #   
  # }) 
  
  active_clustering <- reactive({
    
    out <- switch (input$cluster_method,
            "k-means" = res_kmeans(),
            'k-meds'  = res_kmeds(),
            'h-clust' = res_hclust(),
            validate(glue::glue("{input$cluster_method} Not yet ready"))
    )
    
    
    
    out
    
  }) 
  
  observeEvent(active_clustering(),{
    
    shinyjs::show("cluster_output", anim = TRUE)
    shinyjs::hide(selector = "div.box")
    
  }
  )
  
  observeEvent(input$cluster_method, {
    updateTabsetPanel(inputId = "switcher", selected = input$cluster_method)
  })
  
  # Constants ---------------------------------------------------------------
  
  # Cluster Methods
  cluster_methods <- c("Hierarchical", "k-Medoids (PAM)", "k-means")
  
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
    
    Size   = ~n(),
    Mean   = ~mean(., na.rm = TRUE),
    SD     = ~sd(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    Min    = ~min(., na.rm = TRUE),
    Max    = ~max(., na.rm = TRUE)
    
  )
  
  # Cluster statistics the package {fpc} produces using the fpc::cluster.stats(dissMatrix, cluster_membership)
  vec_cluster_stats <- c(
    
    "average.distance"     = "Within cluster average distance"
    , "median.distance"    = "Within cluster median distance"
    , "separation"         = "Minimum distance of a point in the cluster to a point of another cluster"
    , "average.toother"    = "Average distance of a point in the cluster to the points of other clusters"
    , "clus.avg.silwidths" = "Average silhouette width"
    #,"cluster.size" = "cluster size"
  )
  
  # statistics for overall cluster solution
  vec_stats_overall <- c(
    
    "average.between"    = "Average distance between clusters"
    , "average.within"   = "Average distance within clusters"
    , "avg.silwidth"     = "Average silhouette width"
    , "sindex"           = "Adjusted separation index"
    # for sindex see the documentation. less sensitive to a single or a few ambiguous points for 
    ,"within.cluster.ss" = "A generalisation of the within clusters sum of squares"
    ,"ch"
  )
  
  # Server declaration ------------------------------------------------------
  
  dta_upld <- mod_upload_file_server("upload_file_ui_1")
  
  dta_error <- reactiveVal(FALSE)
  file_uploaded <- reactiveVal(FALSE)
  
  
  observeEvent(dta_upld(), {
    
    if(inherits(dta_upld(), "error")){
      
      dta_error(TRUE)
      
    } else {
      
      dta_error(FALSE)
      file_uploaded(TRUE)
      shinyjs::hideElement("sample_data_info", anim = TRUE)
      shinyjs::hideElement("sample_data_info1", anim = TRUE)
    }
    
  })
  
  dta <- reactive({
    
    if(isTRUE(file_uploaded())){
      # keep the previously uploaded file, in case the no success with the new
      req(isFALSE(dta_error()), cancelOutput = TRUE)
      dta_upld()
      
    } else {
      # Sample file/ or penguins_raw
      penguins %>% 
        slice_sample(n = 60) %>% 
        {.}
    }
    
  })
  
  
  labels_list <- reactive({
    
    get_var_labels(dta(), unlist = FALSE)
    
  })
  
  
  output$dta <- reactable::renderReactable({
    
    dta() %>% 
      reactable::reactable(
        searchable = TRUE, defaultPageSize = 15, resizable = TRUE, compact = TRUE,
        wrap = FALSE, highlight = TRUE
      )
    
  })
  
  output$dta_labels <- renderPrint({
    
    labels_list() %>% unlist()
    
  })
  
  
  output$res_cluster <- renderPrint({ active_clustering() })
  
  
  output$vars_cluster <- renderText({
    active_clustering()$vars_cluster
  })
  
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
          tibble::rowid_to_column(".rowid") %>% 
          add_silhouette(active_clustering()$silhouette)
        
      },
      error = function(e){
        
        print(paste("Error at :",Sys.time(), e))
        showModal(modalDialog("Something went terribly wrong, refresh and try again"))
      }
    )
    
  })
  
  
  output$dta_updated <- renderTable(
    
    head(dta_updated())
  )
  
  
  # Cluster Statistics ------------------------------------------------------
  
  
  by_cluster_silhouette <- reactive({
    
    req(tbl_silhouette())
    
    by_group <- 
      tbl_silhouette() %>% 
      group_by(cluster = as.character(cluster)) %>% 
      summarise(
        across(sil_width, silhouette_summary, .names = "{.fn}")
      ) %>% 
      ungroup() %>% 
      mutate(Proportion = Size/sum(Size), .after = Size)
    
    overall <- 
      tbl_silhouette() %>% 
      summarise(
        across(sil_width, silhouette_summary, .names = "{.fn}")
      ) %>% 
      mutate(cluster = "ALL", Proportion = 1)
    
    
    bind_rows(by_group, overall) 
    
    
  })
  
  output$by_cluster_silhouette <- renderReactable({
    
    req(tbl_silhouette())
    
    dta <- by_cluster_silhouette()
    
    dta %>% 
      mutate(across(where(is.numeric) & !c(Proportion), round, 2)) %>% 
      reactable(
        rowStyle = function(index) {
          if (index == nrow(dta)) list(fontWeight = "bold",  borderTopStyle = "groove")
        },
        highlight = TRUE, sortable = FALSE, compact = TRUE, fullWidth = FALSE,
        columns = list(
          Proportion = colDef(format = colFormat(percent = TRUE, digits = 1))
        )
      )
  })
  
  
  output$info_sil <- renderUI({
    
    req(active_clustering())
    with_tooltip("The silhouete", info$silhouette, interactive = TRUE)
    
  })
  
  cluster_stats <- reactive({
    
    req(active_clustering())
    
    fpc::cluster.stats(
      d = active_clustering()$diss_matrix, 
      clustering = active_clustering()$cluster)
  })
  
  tbl_cluster_stats <- reactive({
    
    cluster_stats()[names(vec_cluster_stats)] %>%
      as_tibble() %>% 
      tibble::rowid_to_column("cluster") %>% 
      mutate(cluster = as.character(cluster))
    
  })
  
  output$tbl_cluster_stats <- reactable::renderReactable({
    
    req(tbl_cluster_stats())
    
    tbl_cluster_stats() %>% 
      mutate(
        across(where(is.numeric), round, 2)
      ) %>% 
      reactable(
        highlight = TRUE, sortable = FALSE,
        columns = list(
          cluster = colDef(footer = p("Internal validation statistics from ",
                                      a(href = "https://arxiv.org/pdf/1503.02059.pdf", "p.25 here"))),
          average.distance = colDef(header = with_tooltip("Average distance", vec_cluster_stats[["average.distance"]])),
          median.distance = colDef(header = with_tooltip("Median distance", vec_cluster_stats[["median.distance"]])),
          separation = colDef(header = with_tooltip("Separation", vec_cluster_stats[["separation"]])),
          average.toother = colDef(header = with_tooltip("Average toother", vec_cluster_stats[["average.toother"]])),
          clus.avg.silwidths = colDef(header = with_tooltip("Silhouette widths", vec_cluster_stats[["clus.avg.silwidths"]]))
        )
      )
    
  })
  
  
  tbl_sep_matrix <- reactive({
    
    as_tbl_sep_matrix(cluster_stats()$separation.matrix) 
    
  })
  
  output$tbl_sep_matrix <- renderTable({
    
    tbl_sep_matrix()
  }
  , hover = TRUE, caption = "Cluster Separation Index")
  
  plot_sep_matrix <- reactive({
    
    gg_separation_matrix(cluster_stats()$separation.matrix)
    
  })
  
  output$plot_sep_matrix <- renderPlot({plot_sep_matrix()})
  
  
  plot_density <- reactive({
    
    req(dta_updated())
    
    tryCatch({
      vars_cluster <- isolate(active_clustering()$vars_cluster)
      
      if(!anyNumeric(dta_updated()[vars_cluster])) {
        validate("For the density plots, we need at least 1 numeric variable")
      }
      
      dta_updated() %>%
        select(cluster, all_of(vars_cluster)) %>% 
        gg_density_plot()
      
    },
    error = function(e){
      print(e)
      validate("We apologise. Something went wrong!")
    }
    )
    
  })
  
  dim_p <- reactive({
    
    dim <- facet_panel_dimensions(plot_density())
    
    height <- switch (dim$rows,
            "1" = 300,
            "2" = 500,
            "3" = 600,
            600
    )
    
    
    width <- switch (dim$cols,
                      "1" = 600,
                      "2" = 700,
                      "auto"
    )
    
    list(
      height = height,
      width = width
    )
    
  })
  
  output$plot_density <- renderPlot({
    
    plot_density()
    
    
  }, height = function() dim_p()$height, width = function() dim_p()$width, )
  
  # Dendrogram --------------------------------------------------------------
  
  
  hc_plot <- reactive({
    
    req(res_hclust())
    req(input$cluster_method == "h-clust")
    
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
    
    shinyjs::toggleElement(id = "show_dendro", condition = input$cluster_method == "h-clust")
  })
  
  observeEvent(input$show_dendro, {
    
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
  
  
  # END of app_server ----  
}
