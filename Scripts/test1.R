

devtools::load_all()

temp <- read_file("SampleData/sample_spss.sav")

get_var_labels(temp)

vars_for_cluster <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")

dta <- 
  temp %>% 
  slice_sample(n = 30) %>%
  # add a row with missing values
  add_row(species = "Adelie") %>%
  arrange(species) %>% 
  tibble::rowid_to_column("id") %>% 
  as.data.frame()
#purrr::keep(is.numeric) %>% 
#na.omit()

dta_clust <- 
  dta %>% 
  select(all_of(vars_for_cluster)) %>% 
  mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  na.omit()


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

res_kmeans$silhouette <- get_sil_widths(res_kmeans, diss_matrix)

get_sil_widths(res_agnes, diss_matrix)

tbl_sil <- res_kmeans$silhouette

dta %>% 
  left_join(tbl_sil, by = "id", suffix = c("_varOfDF", "")) 


dta %>% 
  add_silhouette_to_dta(tbl_sil)

# Aggregated Tbl silhouette

# a list of statistics for silhouette summaries
silhouette_summary = list(
  mean = ~mean(., na.rm = TRUE),
  sd = ~sd(., na.rm = TRUE),
  median = ~median(., na.rm = TRUE),
  min = ~min(., na.rm = TRUE),
  max = ~max(., na.rm = TRUE)
)

res_sil <- get_sil_widths(res_pam, diss_matrix)

by_group <- 
  res_sil %>% 
  group_by(cluster = as.character(cluster)) %>% 
  summarise(
    across(sil_width, silhouette_summary, .names = "{.fn}")
  )
  
overall <- 
  res_sil %>% 
  summarise(
    across(sil_width, silhouette_summary, .names = "{.fn}")
  ) %>% 
  mutate(cluster = "ALL")


res_pam$clustering %>% 
  as.character() %>% 
  forcats::fct_count(prop = TRUE) %>% 
  transmute("cluster" = as.character(f), "size" = n, "prop" = p) %>% 
  left_join(by_group, by = "cluster") %>% 
  bind_rows(overall)



# Dendrogram

library(dendextend)

dendro <- as.dendrogram(res_agnes)

plot(dendro)

summary(dendro)

labels(dendro)

ord <- res_agnes$order
lab <- res_agnes$order.lab %>% as.numeric()
dta[ord,]
dta[lab,]

new_labels <- dta$species[as.numeric(res_agnes$order.lab)]

  
dendro %>% 
  # set("leaves_pch", c(19, 19, NA)) %>%
  # set("leaves_cex", c(1, 2)) %>%
  set("labels_colors", value = c("skyblue", "orange", "grey"), k=3) %>% 
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  set("leaves_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  set("labels", new_labels) %>% 
  plot(horiz=TRUE, axes=FALSE)




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





