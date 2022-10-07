# tessiflow

<!-- badges: start -->

[![R-CMD-check](https://github.com/skysyzygy/tessiflow/workflows/R-CMD-check/badge.svg)](https://github.com/skysyzygy/tessiflow/actions)
[![Codecov test
coverage](https://codecov.io/gh/skysyzygy/tessiflow/branch/master/graph/badge.svg)](https://codecov.io/gh/skysyzygy/tessiflow?branch=master)

<!-- badges: end -->

`tessiflow` is a workflow runner that handles scheduling R scripts and other
commands on a given schedule, and with conditional and dependency
checks. The syntax is based on a simplified version of Github Actions
yml files. Each workflow is run in a separate `callr` subprocess so that all 
jobs/steps have access to the previous results. The motivation is to allow 
complex workflow dependencies while maintaining transparency and to encourage 
modular, testable code, because steps are by default, just R function calls.

*Why not use Airflow or a similar existing workflow tool?*

- Because this is an R-native workflow tool there will be less disk i/o required between steps 
because data can remain in memory. It is also more straightforward to collect rich error and trace 
information and offers the possibility of remote debugging. Also, orchestrators are complicated 
and this is designed to be very very simple!

## Installation

Install the latest version of this package by entering the following in
R:

    install.packages("remotes")
    remotes::install github("skysyzygy/tessiflow")
    
Install miniconda using the helpers in the reticulate package:

    reticulate::install_miniconda(PATH_TO_INSTALL)

If miniconda is installed in a non-default location, set an environment variable `RETICULATE_MINICONDA_PATH`
to the location of the install. 

Create a yml file in your R_USER directory called `config.yml` and add
the following keys to it, filling in information for your particular
machine configuration:

    default:
    # tessiflow settings
      tessiflow.d: path to directory of yml files
      tessiflow.log: path to directory for log files
      tessiflow.port: port to use for communicating with the main tessiflow instance
      tessiflow.email: email address where errors will be sent 
                       (first email address will also be the sender)
      tessiflow.smtp: 
        - host.name: smtp host name
        - port: optional port

In the tessiflow.d directory, create yml files (see format info below)

## Usage

``` r
library(tessiflow)

flows_parse() 
# returns a data.table of jobs parsed from the tessiflow.d yml files

tessiflow_run()
# ... starts the scheduler
tessiflow_stop()
# ... stops the scheduler

tessiflow_job_start(flow name,job name)
# immediately starts the job with the given flow and job name 

tessiflow_job_start(flow name,job name)
# immediately stops the job with the given flow and job name 

tessiflow_enable()
# enables a scheduled task for the tessiflow scheduler using schtasks.exe on Windows or cron on *nix/Mac

tessiflow_disable()
# removes the scheduled task for the tessiflow scheduler

```

## tessiflow yml format

-   `name` The name of your task

-   `on.schedule`: You can use on.schedule to define a time schedule for
    your workflows. You can schedule a workflow to run at specific UTC
    times using POSIX cron syntax.

    **Example:** This example triggers the workflow every day at 5:30
    and 17:30 UTC:

    ``` yml
    on:
      schedule:
      # * is a special character in YAML so you have to quote this string
        - cron:  '30 5,17 * * *'
    ```

-   `env`: A `map` of environment variables that are available to the
    steps of all jobs in the workflow.

-   `jobs`: A workflow run is made up of one or more jobs, which run in
    parallel by default. To run jobs sequentially, you can define
    dependencies on other jobs using the `jobs.<job_id>.needs` keyword.

    -   `<job_id>`: Use `<job_id>` to give your job a unique identifier.
        The key `job id` is a string and its value is a map of the job's
        configuration data.

        -   `name`: Use `name` to set a name for the job

        -   `needs`: Use `needs` to identify any jobs that must complete
            successfully before this job will run. It can be a string or
            array of strings.

            **Example:** Requiring successful dependent jobs

            ``` yml
            jobs: 
              job1: 
              job2: 
                needs: job1 
              job3: 
                needs: [job1, job2] 
            ```

            In this example, job1 must complete successfully before job2
            begins, and job3 waits for both job1 and job2 to complete.

            The jobs in this example run sequentially

            **Example:** Not requiring successful dependent jobs

            ``` yml
            jobs: 
              job1: 
              job2: 
                needs: job1 
              job3: 
                if: TRUE
                needs: [job1, job2] 
            ```

            In this example, job3 uses the expression `TRUE`, which can
            be any R statement, so that it always runs after job1 and
            job2 have completed, regardless of whether they were
            successful.

        -   `if` You can use the `if` conditional to prevent a job from
            running unless a condition is met.

        -   `runs-on` Use `runs-on` to define the name of the machine or
            machines to run the job on.

        -   `steps` A job contains a sequence of tasks called steps.

            -   `if` You can use the `if` conditional to prevent a step
                from running unless a condition is met.

            -   `name` A name for your step

            -   `env` Sets environment variables for steps to use

            -   `run` Runs command-line programs using the operating
                system's shell. If you do not provide a name, the step
                name will default to the text specified in the `run`
                command.

            -   `shell` You can set the shell value to a template string
                using `command [options] {0} [more options]`. tessiflow
                interprets the first whitespace-delimited word of the
                string as the command, and inserts the file name for the
                temporary script at `{0}`.


