# tessiflow 0.2.0

* Major update!
- brand new plumber-based API that involves two-way communication with the server and serves its own documentation
- cut off backtrace at calling environment
- log all remaining stdout/stderr when job finalizes
- use a more robust error handling procedure that can handle errors in itself
- bug fix: reset stopped jobs

# tessiflow 0.1.8

* add even richer rlang error information to error output

# tessiflow 0.1.7

* remove tempdir for subprocesses after run in case of failure

# tessiflow 0.1.6

* add even richer rlang error information to error output

# tessiflow 0.1.5

* add rich rlang error information to error output

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
