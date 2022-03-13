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
      #h1("Cluster-ShinyApp"),
      #theme = shinythemes::shinytheme("paper"),
      
      titlePanel("Cluster Analysis"),
      
      sidebarLayout(
        sidebarPanel(
          h3("How to use this app"),
          p("1. First upload a dataset"),
          p("Please note that the app currently accepts only .csv and .tsv files"),
          #br(),
          fileInput("file", NULL, accept = c(".csv", ".tsv")),
          p("2. Go to ", em("Clustering"),
            " tab and select variables to do the clustering. There, you will
                   also get descriptives of ", em("Internal validation"), "statistics.",
            a(href = "https://arxiv.org/pdf/1503.02059.pdf", "see p.25 here")),
          
          p("3. Go to ", em("Evaluate"), " tab and select variables
                   to see their distribution within the clusters, with graphs and tables"),
          hr(),
          h4("A few words on the app"),
          p("The app is using the R packages: ",code("cluster"), "for clustering, the",
            code("fpc"), "for internal validation statistics, and of course 
                   uses the ", code("tidyverse"), "collection of packages
                   for data wrangling", "The graphs are produced with the ", code("ggplot2"),
            "package.", "I also try to improve the UI using the ", code("shinyWidgets"),
            "package. (not there yet... :). Lastly, I managed to incorporate some",
            a(href = "https://shiny.rstudio.com/articles/modules.html", "shiny modules"), 
            "functionality (not much yet, just the download buttons) to manage the code complexity"),
          hr(),
          p("This app is created by", a(href = "https://www.linkedin.com/in/lefkios",
                                        "Lefkios Paikousis."), 
            br(),
            "The code for the app can be found in my", 
            a(href="https://github.com/lefkiospaikousis/clusterShinyApp", "Github page"),
            br(), "You can also find me on ", a(href = "https://twitter.com/lefkiospaik", 
                                                "twitter")),
          p("As this is a work in progres, please send me your comments 
                   and suggestions on how to improve this app. Thanks for visiting"),
          width = 3,
        ),
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel("Data",
                     br(),
                     p("The Data"),
                     hr(),
                     reactable::reactableOutput("dta"),
                     width = 8
            ),
            tabPanel("Cluster",
                     fluidRow(
                       col_3(
                         selectInput("clust_method", "Cluster Method", 
                                     choices = c("K-means" = "k-means",
                                                 "K-medoids" = "k-meds",
                                                 "Hierarchical Clustering" = "h-clust"
                                     )
                         ),
                         shinyWidgets::pickerInput("vars_cluster", "Select variables",
                                                   choices = NULL, selected = NULL,
                                                   multiple = TRUE,
                                                   options = list(`actions-box` = TRUE,
                                                                  `live-Search`  = TRUE,
                                                                  liveSearchStyle = "contains"
                                                   )
                         )
                       )
                     ),
                     fluidRow(
                       col_2(
                         numericInput("seed", "Set seed", value = 123, 1, 1000, 1)
                       )
                     ),
                     #verbatimTextOutput("cluster_group"),
                     fluidRow(
                       conditionalPanel(
                         condition = "input.clust_method == 'k-means'",
                         mod_kmeans_ui("kmeans_ui_1")
                       ),
                       conditionalPanel(
                         condition = "input.clust_method == 'k-meds'",
                         mod_kmedoids_ui("kmedoids_ui_1")
                       ),
                       conditionalPanel(
                         condition = "input.clust_method == 'h-clust'",
                         mod_hclust_ui("hlust_ui_1")
                       )
                     ),
                     fluidRow(
                       verbatimTextOutput("res_cluster")
                     )
                     
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

