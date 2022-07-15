#' Clean .sav (SPSS) files
#'
#' A function to clean the imported SPSS file
#' 
#' @param dta A haven labelled dataset
#' @return A tibble
#'
#' @noRd
clean_sav <- function(dta){
  
  out <- dta %>% 
    mutate(
      across(where(haven::is.labelled), forcats::as_factor)
    ) %>% 
    # haven::zap_labels() %>% 
    {.}
  
  # # So far, the forcats::as_factor keeps the variable labels, so no problem with that
  # labels_list <- get_var_labels(dta, unlist = FALSE)
  # purrr::map(names(out), function(x){
  #   
  #   attr(out[[x]], "label") <<- labels_list[[x]]
  #   
  # })
  
  out
  
}

#' Standardise a numeric vector
#' 
#' The base::scale returns a matrix/ array (?) WTF
#' 
#' @noRd
scale2 <- function(x, na.rm = TRUE) {
  
  stopifnot(is.numeric(x))
  
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  
}

#' Get the variable labels
#' 
#' Adapted from 
#' https://github.com/larmarange/labelled/blob/main/R/var_label.R
#' @noRd
get_var_labels <- function(x, unlist = FALSE) {
  
  r <- lapply(x, function(x) attr(x, "label", exact = TRUE))
  
  if (unlist) {
    r <- lapply(r, function(x){if (is.null(x)) "" else x})
    base::unlist(r, use.names = TRUE)
  } else
    r
}

#' Find variables within a dataset
#'  
#' This function finds variables of a specific type within a dataset
#'  
#' Adapted (i.e. stolen) from 
#' https://mastering-shiny.org/scaling-modules.html#case-study-selecting-a-numeric-variable
#' The \code{type} argument will lead to a function - Usually \code{is.numeric} or \code{is.character}
#' @param data A dataframe
#' @param type The type of the variable. One of c("numeric", "character", "factor")
#' @export
#' @return A character vector of variable names 
vars_of_type <- function(data, type =  c("numeric", "character", "factor")) {
  
  stopifnot(is.data.frame(data))
  
  type = match.arg(type)
  
  filter <- switch (type,
                    numeric   = is.numeric,
                    character = is.character,
                    factor    = is.factor
  )
  
  names(data)[vapply(data, filter, logical(1))]
  
}


#' Wrap titles 
#' 
#' Function to wrap titles, so they show completely when saving plot in ggplot
#' 
#' Stolen from https://github.com/Public-Health-Scotland/scotpho-profiles-tool/blob/master/shiny_app/global.R
#' @noRd
wrap_title <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#' Create an empty plot
#' 
#' This function prints a plot when no data available for ggplot visuals
#' @noRd
plot_nodata_gg <- function() {
  
  ggplot()+
    xlab("No data available")+
    theme(
      panel.background = element_blank(),
      axis.title.x=element_text(size=20, colour ='#555555')
    )
  
}


#' Calculate the dissimilarity matrix
#' 
#' Uses the \code{cluster::daisy} function. This function needs 
#' the character columns as factors to properly function
#' 
#' @param dta A dataframe. The data
#' @param metric The metric to use
#' @export
calc_diss_matrix <- function(dta, metric = c("euclidean", "manhattan", "gower")){
  
  metric <- match.arg(metric)
  
  # default metric = euclidean unless we have categorical data
  # where the metric changes on its own to 'gower'. see ?cluster::daisy
  
  # Also, character columns need to be factor, in order for daisy to work
  
  cluster::daisy(
    dta %>% mutate(across(where(is.character), factor)),
    metric = metric
  )
  
}


#' Get the silhouette widths
#' 
#' This function extracts or calculates the silhouette widths from the clustering objects
#' 
#' \code{cluster::pam} retains a table of sil widths in obj$silinfo$widths
#' For the other cluster methods we calculate them ourselves using
#' cluster::silhouette    
#' 
#' The clustering `obj` must have a named `cluster` vector. Named with the observation .rowid
#' and the value the observation cluster membership  
#' 
#' @return A tibble with the observation .rowid, the cluster membership, 
#' the cluster neighbor and the silhouette width
#' 
#' @param obj A clustering object
#' @param diss_matrix A dissimilarity matrix, of class `dist`
get_sil_widths <- function(obj, diss_matrix){
  
  stopifnot(inherits(obj, c("pam", "kmeans", "agnes")))
  stopifnot(inherits(diss_matrix, "dist"))
  
  if(inherits(obj, c("kmeans", "agnes"))){ # kmeans and HC clustering
    
    stopifnot(!is.null(obj[["cluster"]]))
    
    # cluster::silhoutte does not keep the .rowid indx of the 
    # obj$cluster. I do a workaround.
    
    indx <- names(obj$cluster) %>% as.numeric()
    
    res_sil <- cluster::silhouette(obj$cluster, diss_matrix)
    
    out <- 
      tibble(
        cluster = res_sil[,1],
        neighbor = res_sil[,2],
        sil_width = res_sil[,3]
      ) %>% 
      tibble::add_column(.rowid = indx, .before = 1) 
    
  }
  
  if(inherits(obj, "partition")){ # K medoids (cluster::pam) Clustering
    
    # cluster::pam object holds the sil widths as array
    # where the row ids are the observation ids. The important
    # thing is that it respects the na.omit of the df and we know the true obs .rowid
    out <- 
      obj$silinfo$widths %>% 
      as_tibble(rownames = ".rowid") %>% 
      mutate(.rowid = as.numeric(.rowid)) %>% 
      arrange(.rowid)
  }
  
  # if(inherits(obj, "agnes")){ # Hierarchical Clustering
  #   
  # }
  # 
  
  out
}


#' Add silhouette information on the the dataset
#'
#' It adds not only the `sil_width` but also the cluster membership
#' and the neighbor cluster
#' @param dta The original data
#' @param tbl_silhouette A dataframe. Output of the \code{get_sil_widths}
#' 
#' @export
add_silhouette <- function(dta, tbl_silhouette){
  
  stopifnot({
    inherits(tbl_silhouette, "data.frame")
    all(dim(tbl_silhouette) > 0)
    all(dim(dta) > 0)
    not_null(tbl_silhouette$sil_width)
    nrow(tbl_silhouette) <= nrow(dta)
  })
  
  
  # in case there are similar column names in the data
  dta %>% 
    left_join(
      tbl_silhouette, by = ".rowid", suffix = c("_varOfDF", "")
    ) 
  
}


with_tooltip <- function(value, tooltip, ...) {
  # tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
  #           title = tooltip, value)
  div(style = "text-decoration: underline; cursor: help",
      tippy::tippy(value, tooltip, ..., elementId = NULL)
  )
}



#' Extract the panel dimensions in a faceted ggplot
#' 
#' This function extracts the number if rows and columns of the panel
#' in a faceted ggplot. It can be used to calculate the \code{width} and \code{height}
#' of a \code{shiny::renderPlot()}
#' 
#' Some details here :)
#' 
#' @param p A ggplot2 object
#' @return A named list of the number of $rows and $cols
#' @export
#' @examples 
#' library(ggplot2)
#' p1 <- ggplot(mtcars, aes(mpg, wt)) + facet_wrap(~ gear )
#' facet_panel_dimensions(p1)
#' p2 <- ggplot(mtcars, aes(mpg, wt)) + facet_grid(cyl ~ gear )
#' facet_panel_dimensions(p2)
facet_panel_dimensions <- function(p){
  
  stopifnot(inherits(p, "ggplot"))
  
  layout <- ggplot2::ggplot_build(p)$layout$layout
  
  list(
    rows = length(unique(layout$ROW)),
    cols = length(unique(layout$COL))
  )
  
}


anyNumeric <- function(dta){
  
  any(purrr::map_lgl(dta, ~inherits(., c("numeric", "integer"))))
}

pickVarsInput <- function(.id, label = "Select variables"){
  
  shinyWidgets::pickerInput(.id, "Select variables",
                            choices = NULL, selected = character(0),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE,
                                           `live-Search`  = TRUE,
                                           liveSearchStyle = "contains",
                                           style = 'btn-default'),
                            width = "80%"
  )
}



info <- function() {
  
  list(
    
    silhouette = "The silhouette value is a measure of how similar an object is 
    to its own cluster (cohesion) compared to other clusters (separation). 
    The silhouette ranges from -1 to +1, where a high value indicates that the 
    object is well matched to its own cluster and poorly matched to neighboring 
    clusters. If most objects have a high value, then the clustering configuration 
    is appropriate. If many points have a low or negative value, then the 
    clustering configuration may have too many or too few clusters."
    
    
  )
  
}



#' Get the cluster index
#' 
#' Not used now
#' @noRd
#' @return A named vector of cluster membership. The names are the case id
get_cluster_indx <- function(dta, cluster_indx) {
  
  stopifnot(inherits(dta, "data.frame"))
  stopifnot(inherits(cluster_indx, "integer"))
  stopifnot(length(cluster_indx) > 0)
  
  complete <- complete.cases(dta)
  
  stopifnot(sum(complete) == length(cluster_indx))
  
  dta$cluster <- NA_integer_
  dta$cluster[which(complete)] <- cluster_indx
  
  setNames(dta$cluster, seq_len(nrow(dta)))
  
  
}


#' Get the cluster index
#' 
#' Not used now
#' @noRd
#' @return A named vector of cluster membership. The names are the case id
get_pam_cluster_indx <- function(dta, cluster_indx){
  
  stopifnot(inherits(dta, "data.frame"))
  stopifnot(inherits(cluster_indx, "integer"))
  stopifnot(length(cluster_indx) > 0)
  
  cluster_indx %>% 
    tibble::enframe() %>% 
    tidyr::complete(name = as.character(seq_len(nrow(dta)))) %>% # TODO ugly!!! 
    mutate(name = as.numeric(name)) %>% 
    arrange(name) %>% 
    tibble::deframe()
  
}

