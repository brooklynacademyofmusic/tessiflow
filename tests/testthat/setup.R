# make sure that config::get only looks at the local config file
withr::local_envvar(R_CONFIG_FILE="config-tessitask.yml",.local_envir = teardown_env())
