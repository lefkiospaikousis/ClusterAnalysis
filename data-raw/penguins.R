## code to prepare `penguins` dataset goes here


library(palmerpenguins)




usethis::use_data(
  penguins, 
  penguins_raw,
  internal = TRUE, overwrite = TRUE)
