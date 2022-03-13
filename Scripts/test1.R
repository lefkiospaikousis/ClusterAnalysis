

devtools::load_all()

temp <- read_file("SampleData/sample_spss.sav")

get_var_labels(temp)

vars_for_cluster <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")

dta <- 
  temp %>% 
  select(all_of(vars_for_cluster)) %>% 
  mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>% 
  as.data.frame()
#purrr::keep(is.numeric) %>% 
#na.omit()

dta_clust <- dta %>% as.data.frame() %>% na.omit()


# dissimilarity ----
diss_matrix <- calc_diss_matrix( dta_clust )
class(diss_matrix)

str(diss_matrix)


sum(is.na(diss_matrix))

# k means ----
res_kmeans <- kmeans(dta_clust, 3, nstart = 25)
class(res_kmeans)


# k meds ----
res_pam <- cluster::pam(diss_matrix, k = 3)
class(res_pam)

res_pam$clustering %>% length()

plot(res_pam)

get_pam_cluster_indx(
  dta,
  res_pam$clustering
)

# Hierarchical ----

res_agnes <- cluster::agnes(diss_matrix)
class(res_agnes)


# add a cluster- a named vector 
ids <- attr(diss_matrix, "Labels")
res_agnes$cluster <- stats::cutree(res_agnes, 3) %>% setNames(ids)


## silhouette widths ---

get_sil_widths(res_kmeans, diss_matrix)
get_sil_widths(res_pam)
get_sil_widths(res_agnes, diss_matrix)

class(diss_matrix)

res_sil<- cluster::silhouette(res_kmeans$cluster, diss_matrix) 


dta %>% 
  as_tibble()

indx <- res_kmeans$cluster %>% names() %>% as.numeric()

indx



tibble(
  cluster = res_sil[,1],
  neighbor = res_sil[,2],
  sil_width = res_sil[,3]
) %>% 
  tibble::add_column(id = indx, .before = 1) %>% 
  tidyr::complete(id = seq_len(nrow(dta)))


res_sil %>% as.data.frame()

str(res_sil)

summary(res_sil)



ggplot()+
  xlab("No data available")+
  theme(panel.background = element_blank(),
        axis.title.x=element_text(size=20, colour ='#555555'))





#' Check the selected variables for k means clustering
#' 
#' This function does a validation check if all the variables
#' the user selected for K-means, conform to some rules
#' i.e. at least one is numeric, and if there are any
#' 
check_vars_for_kmeans <- function(dta, vars){
  
  stopifnot(inherits(dta, "data.frame"))
  stopifnot(is.character(vars))
  stopifnot(length(vars) > 0)
  
  stopifnot(all(vars %in% names(dta)))
  
  list(
    number <- any(sapply(dta[, vars, drop = FALSE], is.numeric))
  )
  
}


sapply(mtcars, is.numeric)


mtcars %>% get_vars_of_type("factor")

names(iris)[vapply(iris, is.numeric, logical(1))]

check_vars_for_kmeans(penguins, c("island", "bill_length_mm")  )

penguins[, c("island", "bill_length_mm")]


mtcars[, c("hp"), drop = FALSE]

dta <- 
  mtcars %>%
  as_tibble() %>% 
  select(cyl, disp, wt) %>% 
  slice_head(n = 15)

dta_clst <- dta

dta_clst$cyl[sample(15,2)] <- NA
dta_clst$disp[sample(15,2)] <- NA



res <- kmeans(na.omit(dta_clst), 2)

res$cluster

# complete <- complete.cases(dta_clst)
# 
# dta_clst$cluster <- NA_integer_
# 
# dta_clst$cluster[which(complete)] <- res$cluster
# 





