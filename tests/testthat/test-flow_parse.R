withr::local_package("checkmate")
withr::local_package("mockery")

test_that("flow_parse returns a list of workflows",{
  expect_type(flow_parse(),"list")
})

test_that("flow_parse loads multiple files",{
  expect_length(flow_parse(),2)
  expect_named(flow_parse(),c("dummy.yml","dummy2.yml"))
})

test_that("flow_parse checks that flow names are unique",{
  flows <- readRDS(test_path("flows.Rds")) 
  flows[[2]]$name = flows[[1]]$name
  stub(flow_parse,"setNames",flows)
  
  expect_error(flow_parse(),"flow.+unique")
})

test_that("flow_parse checks that job names are unique",{
  flows <- readRDS(test_path("flows.Rds")) 
  names(flows[[1]]$jobs) <- c("job1","job1","job3")
  stub(flow_parse,"setNames",flows)

  expect_error(flow_parse(),"Job.+unique")
})

test_that("flow_parse checks that step names are unique",{
  flows <- readRDS(test_path("flows.Rds")) 
  flows[[1]]$jobs$job1$steps[[1]]$name = flows[[1]]$jobs$job1$steps[[2]]$name
  stub(flow_parse,"setNames",flows)
  
  expect_error(flow_parse(),"Step.+unique")
})

test_that("flow_parse warns if there are keys we don’t know how to handle at the flow level and removes them",{
  flows <- readRDS(test_path("flows.Rds")) 
  flows[[1]]$weird_key = "weird_value"
  stub(flow_parse,"setNames",flows)
  
  expect_warning(flow_parse(),"flow.+key")
  expect_names(names(flow_parse()[[1]]), disjunct.from = "weird_key")
})

test_that("flow_parse warns if there are keys we don’t know how to handle at the job level and removes them",{
  flows <- readRDS(test_path("flows.Rds")) 
  flows[[1]]$jobs[[1]]$weird_key = "weird_value"
  stub(flow_parse,"setNames",flows)
  
  expect_warning(flow_parse(),"job.$+key")
  expect_names(names(flow_parse()[[1]]$jobs[[1]]), disjunct.from = "weird_key")
})