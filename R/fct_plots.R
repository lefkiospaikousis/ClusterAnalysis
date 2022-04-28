#' plots 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @param dta The data
#' @noRd
#' @export
density_plot <- function(dta){
  
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
