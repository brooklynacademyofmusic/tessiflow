preserve_debug_frames <- function(filename) {
  frames <- as.environment("tools:callr")$`__callr_data__`$.Last.dump
  saveRDS(frames, filename)
}

rehydrate_debug_frames <- function(filename) {
  as.environment("tools:callr")$`__callr_data__`$.Last.dump <- readRDS(filename)
}

tessiflow_debug <- function(flow_name, job_name, pid = NULL) {
  
  if (is.null(pid)) {
    files <- dir(config::get("tessiflow.debug"),
                 paste0(flow_name,"_",job_name,"*.debug"),
                 full.names = T)
    mtimes <- sapply(files,file.mtime)
    filename <- files[order(mtimes,decreasing=T)[[1]]]
  } else {
    filename <- file.path(config::get("tessiflow.debug"), 
                          paste0(flow_name,"_",job_name,"_",pid,".debug"))
  }
  
  r <- r_session$new()
  r$run(rehyrdate_debug_frames(filename))
  r$debug()
}