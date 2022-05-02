test_that("ggplots that visualise clusters work", {
  
  
  sep_matrix <- matrix(
    c(0, 1.06, 2.4, 
      1.06, 0, 2.03, 
      2.4, 2.03, 0), 
    nrow = 3, ncol = 3
  ) 

 tbl <- as_tbl_sep_matrix(sep_matrix) 
    
 p <- gg_separation_matrix(sep_matrix)

 expect_true(ggplot2::is.ggplot(p))
 expect_equal(names(p$data), c("Cluster", "key", "value"))
 expect_equal(nrow(p$data), 9)
 
 expect_equal(nrow(tbl),3)
 expect_named(tbl, c("Cluster", paste0("Cluster ", 1:3)))
  
})
