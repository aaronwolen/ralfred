context("Usage data")

test_that("properly loaded", {
  usage <- load_usage()
  expect_that(class(usage), matches("data.frame"))
})
          
test_that("matches expected format", {
  vars <- c("item", "description", "keyword", "filepath", "bundlecontext", "ts")    
  expect_that(colnames(usage), matches(vars))
})
