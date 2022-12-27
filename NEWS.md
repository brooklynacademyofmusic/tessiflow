# tessiflow 0.1.1

* Updates to improve failure resistance
- extend wait timeout on initial worker spin-up to 30 seconds.
- don't call flows_update_job until after other calls are complete so that we will retry on next cycle.
- don't update the local flows table until after sqlite_upsert in case there is a db problem so we will retry on next cycle.

# tessiflow 0.1.0

* Alpha release!
