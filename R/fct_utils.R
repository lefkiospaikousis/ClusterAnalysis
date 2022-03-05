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


get_cluster_indx <- function(dta, cluster_indx) {
  
  stopifnot(inherits(dta, "data.frame"))
  stopifnot(inherits(cluster_indx, "integer"))
  stopifnot(length(cluster_indx) > 0)
  
  complete <- complete.cases(dta)
  
  stopifnot(sum(complete) == length(cluster_indx))
  
  dta$cluster <- NA_integer_
  dta$cluster[which(complete)] <- cluster_indx
  
  dta$cluster
  
}
