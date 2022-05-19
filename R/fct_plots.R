# A set of functions for creating ggplots for exploring the cluster solutions


#' Create a density plot 
#'
#' This function creates a faceted density plot to demonstrate the 
#' distribution of the variables within each cluster.
#'
#' @return A ggplot. The result is a faceted plot (facet: each variable)
#'
#' @param dta The data
#' @export
gg_density_plot <- function(dta){
  
  stopifnot("cluster" %in% names(dta))
  
  # Only numeric variables & 'cluster'
  dta <- dta %>% 
    select(where(is.numeric), cluster) %>% 
    mutate(cluster = as.character(cluster))
  
  # at least 1 numeric to do the job
  stopifnot(ncol(dta)>1)
  
  stats <- dta %>%
    tidyr::pivot_longer(cols = -cluster) %>%
    group_by(name) %>%
    summarise(
      avg = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE)
    )
  
  dta %>%
    tidyr::pivot_longer(cols = -cluster) %>%
    ggplot(aes(value, ..scaled.. ,fill = cluster))+
    geom_density(alpha = 0.5, adjust = 1.2)+
    geom_vline(data = stats, 
               aes(xintercept = median, colour = "Median"),
               linetype = "dotted"
    )+
    geom_vline(data = stats, 
               aes(xintercept = avg, colour = "Average"),
               linetype = "dashed"
    )+
    facet_wrap(~name, scales = "free")+
    ggplot2::labs(linetype = "")+
    scale_color_manual(name = "Overall stats", 
                       values = c(Median = "black", Average = "violet")
                       #, guide = guide_legend(reverse = FALSE)
    )+
    scale_fill_brewer(name = "Cluster", type = "qual", palette = 2
                      #, guide = guide_legend(reverse = TRUE)
    )+
    ggplot2::theme_classic(14)+
    ggplot2::theme(legend.position = 'top')
  
}



#' Plot a separation matrix
#' 
#' This function plots a tile plot of the separation matrix of a cluster solution
#' 
#' Assume n clusters, the separation matrix is a `n x n` table, with 
#' cluster separation values for each pair of clusters. This is the result of 
#' fpc::cluster.stats()$separation.matrix
#'
#' @param sep_matrix An nxn matrix. 
#'
#' @return
#' @export
gg_separation_matrix <- function(sep_matrix) {
  
  stopifnot({
    inherits(sep_matrix, "matrix")
    length(unique(dim(sep_matrix))) == 1
  })
  
  # # Make it a tibble with cols and row names
  # n <- dim(sep_matrix)[[1]]
  # 
  # colnames(sep_matrix) <- paste0("Cluster ", seq_len(n))
  # row.names(sep_matrix) <- paste0("Cluster ", seq_len(n))
  
  tibble_matrix <- as_tbl_sep_matrix(sep_matrix) 
  
  tibble_matrix %>% 
    tidyr::gather(key, value, -Cluster) %>%
    arrange(Cluster, key) %>%
    ggplot2::ggplot(ggplot2::aes(Cluster, key))+
    ggplot2::geom_tile(ggplot2::aes(fill = value))+
    ggplot2::scale_fill_gradient2(high = "#018571", low = "#d7191c")+
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)))+
    ggplot2::labs(x = "", y= "", fill = "Separation\nlevel",
                  title = "Separation between clusters",
                  subtitle = "Higher value ~ higher separation between the pair of clusters")+
    ggplot2::theme_light(14)
}

#' Turn separation matrix to a tibble
#' 
#' @param sep_matrix A separation matrix, usually from fpc::cluster.stats()   
as_tbl_sep_matrix <- function(sep_matrix){
  
  stopifnot({
    inherits(sep_matrix, "matrix")
    length(unique(dim(sep_matrix))) == 1
  })
  
  # Make it a tibble with cols and row names
  n <- dim(sep_matrix)[[1]]
  
  colnames(sep_matrix) <- paste0("Cluster ", seq_len(n))
  row.names(sep_matrix) <- paste0("Cluster ", seq_len(n))
  
  as_tibble(sep_matrix, rownames = "Cluster") 
  
}



