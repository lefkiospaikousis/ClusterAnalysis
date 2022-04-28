# Functions to facilitate the `mod_upload_file` module
# Remember to also get this file , if you reuse the `mod_upload_file` module


#' Read the uploaded file
#' 
#' This function reads the uploaded file.  
#' 
#' The \code{read_file} function currently reads \code{Excel}, 
#' \code{csv}, \code{tsv}, and \code{SPSS} files. If none of the above file type, 
#' then an \code{error} message is propagated throughout
#' 
#' @param path String length 1. The path of the dataset
#' @export
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
                
                stop("Invalid file")
  )
  
  dta
  
}

#' Process the uploaded file
#' 
#' This function performs some processing on the uplaoded file
#' before it is used by the app  
#' 
#' @param dta A data.frame The data read by the `read_file()`
#' @export
#' @return A tibble of the dataset
process_file <- function(dta){
  
  stopifnot(inherits(dta, "data.frame"))
  
  
  # I need the column name .rowid for my safe matchings later on
  if(".rowid" %in% names(dta)){
    dta$.rowid_old <- dta$.rowid
    dta$.rowid <- NULL
  }
  
  dta
}
