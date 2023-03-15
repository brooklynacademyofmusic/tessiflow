#* @endpoint /job_stop
job_force_stop <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)
  
  job <- setDT(flows_log_get_last_run(flow_name, job_name))
  
  sqlite_upsert("jobs", job[,.(flow_name, job_name, start_time, status = "Forced stop")])
  
  job_log_write(flow_name, job_name, paste("Force stopping job, pid:", job$pid), console = TRUE)
}

#* @endpoint /job_start
job_force_start <- function(flow_name, job_name) {
  assert_flow_job_name(flow_name, job_name)
  
  job <- setDT(flows_log_get_last_run(flow_name, job_name))
  
  sqlite_upsert("jobs", job[,.(flow_name, job_name, start_time, status = "Forced start")])
  
  job_log_write(flow_name, job_name, paste("Force starting job, pid:", job$pid), console = TRUE)
}

#* @endpoint /flows_refresh
function(req, res) {}

#* @endpoint /flows_get
function(req, res) {
  
}