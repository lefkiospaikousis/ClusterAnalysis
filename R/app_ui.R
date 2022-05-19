#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      #theme = bslib::bs_theme(bootswatch = "minty"),
      theme = bslib::bs_theme(version = 5,
                              base_font = bslib::font_google("Roboto Condensed"),
                              font_scale = 0.7
                              #,"input-border-color" = "#55595e"
      ),
      titlePanel("Cluster Analysis Web Application"),
      
      sidebarLayout(
        sidebarPanel(
          p(tags$b("Upload a dataset")),
          span(id = "sample_data_info", "Currently installed: ", 
            a(href = "https://allisonhorst.github.io/palmerpenguins/", "palmerpenguins")
            ),
          mod_upload_file_ui("upload_file_ui_1"),
          fluidRow(
            selectInput("cluster_method", tags$b("Select Cluster Method"), 
                        choices = c("K-means" = "k-means",
                                    "K-medoids" = "k-meds",
                                    "Hierarchical Clustering" = "h-clust"
                        )
                        , width = "80%"),
          ),
          fluidRow(
            tabsetPanel(
              id = "switcher",
              type = "hidden",
              tabPanelBody("k-means", mod_kmeans_ui("kmeans_ui_1")),
              tabPanelBody("k-meds", mod_kmedoids_ui("kmedoids_ui_1")),
              tabPanelBody("h-clust", mod_hclust_ui("hlust_ui_1"))
            )
          ),
          # fluidRow(
          #   conditionalPanel(
          #     condition = "input.cluster_method == 'k-means'",
          #     mod_kmeans_ui("kmeans_ui_1")
          #   ),
          #   conditionalPanel(
          #     condition = "input.cluster_method == 'k-meds'",
          #     mod_kmedoids_ui("kmedoids_ui_1")
          #   ),
          #   conditionalPanel(
          #     condition = "input.cluster_method == 'h-clust'",
          #     waiter::autoWaiter(id = "hc_plot", html = tagList(waiter::spin_2(), "Loading...")),
          #     fluidRow(col_2(actionButton("show_dendro", "Show Dendrogram"))),
          #     mod_hclust_ui("hlust_ui_1")
          #   )
          # ),
          # p(tags$b("3. Internal validation")),
          # p("Get descriptives of ", em("Internal validation"), "statistics.",
          #   a(href = "https://arxiv.org/pdf/1503.02059.pdf", "see p.25 here")),
          # p("3. Go to ", em("Evaluate"), " tab and select variables
          #          to see their distribution within the clusters, with graphs and tables"),
          hr(),
          h4("A few words on the app"),
          p("The app is using the R packages: ",
            a(href = "https://cran.r-project.org/web/packages/cluster/index.html", code("cluster")), 
            "for clustering, the",
            a(href = "https://cran.r-project.org/web/packages/fpc/index.html", code("pfc")),
            "for internal validation statistics, and the ", 
            a(href = "https://www.tidyverse.org/", code("tidyverse")),
            "collection of packages for data wrangling", "The graphs are produced with the ", 
            a(href = "https://ggplot2.tidyverse.org/", code("ggplot2")),
            "package.", "The app is (almost) fully",
            a(href = "https://shiny.rstudio.com/articles/modules.html", "modularised")
          ),
          hr(),
          p("This app is created by", 
            a(href = "https://www.linkedin.com/in/lefkios", "Lefkios Paikousis."), 
            br(),
            "The code for the app can be found in my", 
            a(href="https://github.com/lefkiospaikousis/ClusterAnalysis", "Github page"),
            br(), "You can also find me on ", 
            a(href = "https://twitter.com/lpaikousis", "twitter")),
          p("As this is a work in progres, please send me your comments 
                   and suggestions on how to improve this app. Thanks for visiting"),
          width = 3,
        ),
        
        mainPanel(
          
          tabsetPanel(
            selected = "Cluster Solution",
            tabPanel("Data",
                     br(),
                     p("The Data"),
                     span(id = "sample_data_info1", "This is the ", 
                          a(href = "https://allisonhorst.github.io/palmerpenguins/", "palmerpenguins"),
                          " dataset"),
                     hr(),
                     reactable::reactableOutput("dta"),
                     width = 8
            ),
            tabPanel("Cluster Solution",
                     fluidRow(
                       div(class = "info box", "-Select variables to continue-"),
                       #h3(id = "info_output", "-Select variables to continue-", style = "text-align: center"),
                       shinyjs::hidden(
                         div(id = "cluster_output",
                             col_8(
                               div(id = "tbl_sil",
                                   tippy::tippy(h4("Silhouette Information"), 
                                                info()$silhouette),
                                   #with_tooltip("What is this?", info$silhouette),
                                   reactableOutput("by_cluster_silhouette"),
                               ),
                               div(id = "tbl_stats",
                                   h4("Cluster statistics"),
                                   reactableOutput("tbl_cluster_stats")
                               ),
                               div(id = "plot_sep",
                                   h4("Separation between Clusters"),
                                   plotOutput("plot_sep_matrix")
                               )
                             )
                         )
                       )
                     )
            ),
            tabPanel("Plots",
                     div(class = "info box", "-Select variables to continue-"),
                     div(id = "plot_outputs",
                         plotOutput("plot_density")
                     )
                     
            ),
            tabPanel("Options",
                     fluidRow(
                       col_2(
                         numericInput("seed", "Set seed", value = 123, 1, 1000, 1)
                       )
                     ),
            )
          )
          
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Cluster-ShinyApp'
    ),
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

