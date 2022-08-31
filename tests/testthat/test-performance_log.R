withr::local_package("mockery")
withr::local_package("checkmate")
local_log_dir()
withr::defer(performance_log_close())
performance_log_open()

test_that("performance_log_create creates a new database table", {
  stub(performance_log_open, "performance_log_create", TRUE)
  performance_log_create()
  expect_true("performance" %in% DBI::dbListTables(tessiflow$db))
})

test_that("performance_log_create creates an index on the table", {
  expect_equal(DBI::dbGetQuery(tessiflow$db, "pragma index_list(performance);") %>% nrow(), 1)
})


# performance_log_open ----------------------------------------------------------

test_that("performance_log_open errors when the config option isn't set or the directory doesn't exist", {
  expect_error(performance_log_open("dirdoesntexist"), "config.+directory.+log")
  stub(performance_log_open, "config::get", "blah")
  expect_error(performance_log_open(), "config.+directory.+log")
})

test_that("performance_log_open opens a database connection", {
  performance_log_open()
  expect_class(tessiflow$db2, "SQLiteConnection")
})

# performance_poll --------------------------------------------------------

r_session <- callr::r_session$new()
pid <- r_session$get_pid()  

test_that("performance_poll gets ppid information on process",{
  expect_names(names(performance_poll(pid)),
               must.include=c("pid","ppid"))
})

test_that("performance_poll gets cpu information on process",{
  expect_names(names(performance_poll(pid)),
               must.include=c("cpu_times.system",
                              "cpu_times.user"))
  expect_lte(performance_poll(pid)$cpu_times.system,1)
  expect_lte(performance_poll(pid)$cpu_times.user,1)
  r_session$run(eval,list(quote({runif(2^26);1})))
  expect_lte(performance_poll(pid)$cpu_times.system,1)
  expect_gte(performance_poll(pid)$cpu_times.user,1)
})

test_that("performance_poll gets memory information on process",{
  expect_names(names(performance_poll(pid)),
               must.include=c("memory_full_info.rss",
                              "memory_full_info.vms"))
  expect_lte(performance_poll(pid)$memory_full_info.rss,1e9)
  expect_lte(performance_poll(pid)$memory_full_info.vms,1e9)
  r_session$run(eval,list(quote({v<<-rep(1,1e9);1})))
  expect_gte(performance_poll(pid)$memory_full_info.rss,1e9)
  expect_gte(performance_poll(pid)$memory_full_info.vms,1e9)
  r_session$run(eval,list(quote({rm(v);gc()})))
  expect_lte(performance_poll(pid)$memory_full_info.rss,1e9)
  expect_lte(performance_poll(pid)$memory_full_info.vms,1e9)
})

test_that("performance_poll gets disk information on process",{
    expect_names(names(performance_poll(pid)),
                 must.include=c("io_counters.read_bytes",
                                "io_counters.write_bytes"))
  filename <- tempfile()
    expect_lte(performance_poll(pid)$io_counters.read_bytes,2^26)
    expect_lte(performance_poll(pid)$io_counters.write_bytes,2^26)
    r_session$run(eval,list(rlang::expr({writeBin(rep(1,2^26),
                                            !!filename)})))
    expect_lte(performance_poll(pid)$io_counters.read_bytes,2^26)
    expect_gte(performance_poll(pid)$io_counters.write_bytes,2^26)
    r_session$run(eval,list(rlang::expr({readBin(!!filename,
                                                 "integer",
                                                 2^26);1})))
    expect_gte(performance_poll(pid)$io_counters.read_bytes,2^26)
    expect_gte(performance_poll(pid)$io_counters.write_bytes,2^26)
    # ...and disk i/o impacts system times
    expect_gte(performance_poll(pid)$cpu_times.system,1)
})

test_that("all performance_poll variables are in database",{
  expect_names(colnames(tbl(tessiflow$db2,"performance")),
               must.include = names(performance_poll(pid)))
})

# performance <- performance_poll(pid)
# saveRDS(performance,test_path("performance.Rds"))

r_session$kill_tree()

# performance_log_update --------------------------------------------------

performance <- readRDS(test_path("performance.Rds"))
jobs <- readRDS(test_path("jobs.Rds"))
jobs[1,"status"] = "Running"
jobs[1,"pid"] = performance$pid
stub(performance_log_update,"performance_poll",performance)

test_that("performance_log_update updates the performance db table",{
  performance_log_update(performance$pid)
  
  expect_mapequal(tbl(tessiflow$db2,"performance") %>% 
                 collect %>% as.data.table %>%
                 select(-c(flow_name,job_name,step,timestamp)) %>%
                 as.list,
               performance)
})

test_that("performance_log_update includes job info",{
  expect_names(names(performance),
               disjunct.from = c("flow_name","job_name","step"))
  sqlite_upsert("jobs",jobs,tessiflow$db2)

  performance_log_update(performance$pid)
  
  expect_equal(tbl(tessiflow$db2,"performance") %>% 
                 collect %>%
                 .$flow_name,c(NA,"Dummy workflow"))
  expect_equal(tbl(tessiflow$db2,"performance") %>% 
                 collect %>%
                 .$job_name ,c(NA,"Job 1"))
  
})


