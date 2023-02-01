# tessiflow 0.1.4

* fix bugs related to dependencies when they haven't run yet, or there are multiple

# tessiflow 0.1.3

* Update to avoid main loop lock
- don't infinite loop in job_poll on output, just poll once per loop and see what's there
- database lock resolved by setting timeout on SQLite database driver, needed for multithreading

# tessiflow 0.1.2

* Updates to avoid database locking
- single thread the flows_main process to avoid database write collisions
- don't write to the database on flows_log_get_last_run -- this is a lot of unnecessary writes and greatly increases the risk of write collision

# tessiflow 0.1.1

* Updates to improve failure resistance
- extend wait timeout on initial worker spin-up to 30 seconds.
- don't call flows_update_job until after other calls are complete so that we will retry on next cycle.
- don't update the local flows table until after sqlite_upsert in case there is a db problem so we will retry on next cycle.

# tessiflow 0.1.0

* Alpha release!
