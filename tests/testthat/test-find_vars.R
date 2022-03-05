test_that("Find variables in a dataser works", {
  
  expect_equal(
    iris %>% find_vars_of_type("factor"),
    "Species"
  )
  
  expect_equal(
    iris %>% find_vars_of_type("numeric"),
    c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" )
  )
  
  expect_equal(
    mtcars %>% find_vars_of_type("factor"),
    character(0)
  )
  
  
})
