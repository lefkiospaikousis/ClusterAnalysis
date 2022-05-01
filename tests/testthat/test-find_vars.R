test_that("Find variables in a dataser works", {
  
  expect_equal(
    iris %>% vars_of_type("factor"),
    "Species"
  )
  
  expect_equal(
    iris %>% vars_of_type("numeric"),
    c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )
  )
  
  expect_equal(
    mtcars %>% vars_of_type("factor"),
    character(0)
  )
  
  
})
