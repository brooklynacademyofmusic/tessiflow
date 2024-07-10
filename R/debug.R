preserve_debug_frames <- function(filename) {
  dump <- as.environment("tools:callr")$`__callr_data__`$.Last.dump
  trace <- as.environment("tools:callr")$`__callr_data__`$.Traceback
  saveRDS(list(dump = dump,trace = trace), filename)
}

rehydrate_debug_frames <- function(filename) {
  data <- readRDS(filename)
  assign(".Last.dump",data$dump,as.environment("tools:callr")$`__callr_data__`)
  assign(".Traceback",data$trace,as.environment("tools:callr")$`__callr_data__`)
}

tessiflow_debug <- function(flow_name, job_name, pid = NULL) {
  
  if(is.null(config::get("tessiflow.debug")))
    stop("tessiflow.debug is not set, don't know where to find debug frames")
  
  if (is.null(pid)) {
    files <- dir(config::get("tessiflow.debug"),
                 paste0(flow_name,"_",job_name,".*\\.debug"),
                 full.names = T)
    mtimes <- sapply(files,file.mtime)
    filename <- files[order(mtimes,decreasing=T)[[1]]]
  } else {
    filename <- file.path(config::get("tessiflow.debug"), 
                          paste0(flow_name,"_",job_name,"_",pid,".debug"))
  }
  
  if(!file.exists(filename))
    stop(paste("debug frames for",flow_name,"/",job_name,"not found!"))
  
  r <- r_session$new()
  r$run(rehydrate_debug_frames,list(filename))
  r$debug()
}