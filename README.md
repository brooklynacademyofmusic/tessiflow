# tessiflow

<!-- badges: start -->

[![R-CMD-check](https://github.com/skysyzygy/tessiflow/workflows/R-CMD-check/badge.svg)](https://github.com/skysyzygy/tessiflow/actions)
[![Codecov test
coverage](https://codecov.io/gh/skysyzygy/tessiflow/branch/master/graph/badge.svg)](https://codecov.io/gh/skysyzygy/tessiflow?branch=master)

<!-- badges: end -->

tessiflow is a task runner that handles scheduling R scripts and other
commands on a given schedule, and with conditional and dependency
checks. The syntax is based on a simplified version of Github actions
yml files.

## Installation

Install the latest version of this package by entering the following in
R:

    install.packages("remotes")
    remotes::install_github("skysyzygy/tessiflow")

Create a yml file in your R_USER directory called `config.yml` and add
the following keys to it, filling in information for your particular
machine configuration:

    default:
    # tessiflow settings
      tessiflow.d: path_to_directory_of_yml_files

In the tessiflow.d directory, create yml files with the following keys:

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
        The key `job_id` is a string and its value is a map of the job's
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
                if: ${{ TRUE }} 
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
                using `command [options] {0} [more_options]`. tessiflow
                interprets the first whitespace-delimited word of the
                string as the command, and inserts the file name for the
                temporary script at `{0}`.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tessiflow)

taskrunner_start()
# ... runs a bunch of tasks
taskrunner_stop()
```
