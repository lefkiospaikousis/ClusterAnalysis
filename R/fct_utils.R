#' utils 
#'
#' @description A function to clean the imported SPSS file
#' @param dta A haven labelled dataset
#' @return A tibble
#'
#' @noRd
clean_sav <- function(dta){
  
  dta %>% 
    mutate(
      across(where(haven::is.labelled), forcats::as_factor)
    ) %>% 
    # haven::zap_labels() %>% 
    {.}
}


#' Read the uploaded file
#' 
#' This function safely reads the uploaded file.  
#' 
#' The \code{read_file} function currently reads \code{Excel}, 
#' \code{csv}, \code{tsv}, and \code{SPSS} files. If none of the above file type, 
#' then a \code{shiny::validate} message is propagated throughout
#' 
#' @param path String length 1. The path of the dataset
#' @return A tibble of the dataset
read_file <- function(path) {
  
  stopifnot(length(path) == 1)
  
  type <- tools::file_ext(path)
  
  dta <- switch(type,
                
                csv = vroom::vroom(path, delim = ","), 
                
                tsv = vroom::vroom(path, delim = "\t") ,
                
                sav = haven::read_sav(path) %>% clean_sav(),
                
                xlsx = readxl::read_xlsx(path),
                xls  = readxl::read_xls(path),        
                
                validate(
                  
                  "Invalid file! 
             Please upload a file with the following extensions 
             .csv, .tsv, .sav, .xlsx, .xls"
                  
                )
  )
  
  dta
  
}

#' Adapted from 
#' https://github.com/larmarange/labelled/blob/main/R/var_label.R

get_var_labels <- function(x, unlist = FALSE) {
  
  r <- lapply(x, function(x) attr(x, "label", exact = TRUE))
  
  if (unlist) {
    r <- lapply(r, function(x){if (is.null(x)) "" else x})
    base::unlist(r, use.names = TRUE)
  } else
    r
}

scale2 <- function(x, na.rm = TRUE) {
  
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  
}


#'  Find variables within a dataset
#'  
#'  This function finds variables of a specific type within a dataset
#'  
#'  Adapted (i.e. stolen) from 
#'  https://mastering-shiny.org/scaling-modules.html#case-study-selecting-a-numeric-variable
#'  The \code{type} argument will lead to a function - Usually \code{is.numeric} or \code{is.character}
#'  @param data A dataframe
#'  @param type The type of the variable. One of c("numeric", "character", "factor")
#'  @return A character vector of variable names 
find_vars_of_type <- function(data, type =  c("numeric", "character", "factor")) {
  
  stopifnot(is.data.frame(data))
  
  type = match.arg(type)
  
  filter <- switch (type,
                    numeric   = is.numeric,
                    character = is.character,
                    factor    = is.factor
  )
  
  names(data)[vapply(data, filter, logical(1))]
  
}



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

#Function to wrap titles, so they show completely when saving plot in ggplot
# TODO Stolen from https://github.com/Public-Health-Scotland/scotpho-profiles-tool/blob/master/shiny_app/global.R
title_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Function to create plot when no data available for ggplot visuals
plot_nodata_gg <- function() {
  ggplot()+
    xlab("No data available")+
    theme(panel.background = element_blank(),
          axis.title.x=element_text(size=20, colour ='#555555'))
}


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
#' The clustering `obj` must have a named `cluster` vector. Named with the observation id
#' and the value the observation cluster membership  
#' 
#' @return A tibble with the observation id, the cluster membership, 
#' the cluster neighbor and the silhouette width
#' 
#' @param obj A clustering object
#' @param diss_matrix A dissimilarity matrix, of class `dist`
get_sil_widths <- function(obj, diss_matrix){
  
  stopifnot(inherits(obj, c("pam", "kmeans", "agnes")))
  stopifnot(inherits(diss_matrix, "dist"))
  
  if(inherits(obj, c("kmeans", "agnes"))){ # kmeans and HC clustering
    
    stopifnot(!is.null(obj[["cluster"]]))
    
    # cluster::silhoutte does not keep the id indx of the 
    # obj$cluster. I do a workaround.
    
    indx <- names(obj$cluster) %>% as.numeric()
    
    res_sil <- cluster::silhouette(obj$cluster, diss_matrix)
    
    out <- 
      tibble(
        cluster = res_sil[,1],
        neighbor = res_sil[,2],
        sil_width = res_sil[,3]
      ) %>% 
      tibble::add_column(id = indx, .before = 1) 
    
  }
  
  if(inherits(obj, "partition")){ # K medoids (cluster::pam) Clustering
    
    # cluster::pam object holds the sil widths as array
    # where the row ids are the observation ids. The important
    # thing is that it respects the na.omit of the df and we know the true obs id
    out <- 
      obj$silinfo$widths %>% 
      as_tibble(rownames = "id") %>% 
      mutate(id = as.numeric(id)) %>% 
      arrange(id)
  }
  
  # if(inherits(obj, "agnes")){ # Hierarchical Clustering
  #   
  # }
  # 
  
  out
}


#' Title
#'
#' @param dta 
#' @param cluster_group 
#'
#' @return
#' @export
add_cluster_to_dta <- function(dta, cluster_group){
  
  stopifnot(all(dim(dta) > 0))
  stopifnot(is.numeric(cluster_group))
  stopifnot(length(cluster_group) <= nrow((dta)))
  
}


#' Add silhouette information on the the dataset
#'
#' It adds not only the `sil_width` but also the cluster membership
#' and the neighbor cluster
#' @param dta The original data
#'
#' @param tbl_silhouette A dataframe. Output of the \code{get_sil_widths}
#' 
#' @return
#' @export
add_silhouette_to_dta <- function(dta, tbl_silhouette){
  
  
  stopifnot(is.data.frame(tbl_silhouette))
  stopifnot(all(dim(dta) > 0))
  stopifnot(all(dim(tbl_silhouette) > 0))
  stopifnot(!is.null(tbl_silhouette$sil_width))
  stopifnot(nrow(tbl_silhouette) <= nrow(dta))
  
  # in case there are similar column names in the data
  
  dta %>% 
    left_join(
      tbl_silhouette, by = "id", suffix = c("_varOfDF", "")
      ) 
  
}



