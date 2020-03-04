# The Aggregate Job Launcher of Single-core or Single-node Applications on HPC Sites

Author: Victor Anisimov, NCSA Blue Waters, University of Illinois at Urbana-Champaign

Send bug reports and requests for enhancement to victor_anisimov@hotmail.com

## Purpose

The purpose of Scheduler is to help user to submit large number of independent jobs into the queue on HPC sites in the form of a single job. 

On HPC sites, the job submission is managed by resource managers like Torque or PBS, which are batch queuing systems. The job submission is tailored toward submitting parallel MPI jobs that may use from a few to thousands of compute nodes per application. The job is submitted to the queue with help of a qsub command. Each submitted job obtains a jobid.

In the situation when user needs to handle thousands of single-node jobs vs a single job that can use a thousand of nodes, the use of batch queuing system becomes cumbersome. For instance, in order to submit 1000 single node-jobs, a user has to prepare a separate batch script for each individual job and type qsub command 1000 times. This would queue 1000 independed jobs into the queue. However, on many HPC sites there are limits on how many jobs user may submit into the queue. The cap on the number of jobs that can accumulate priority in the queue is typically around 50.

Scheduler allows user to aggregate the independent single-core jobs under a single batch job. That means user can submit a single job bundling up any number of small jobs into a single job with help of a very simple configuration file.

The other limitation of standard batch queue systems is that they do not allow sharing of compute-node between applications. For instance, if user needs to submit 3200 single-core jobs, the standard approach is to submit one job per node despite the node may have more than one compute core. For instance, Blue Waters has 32 cores on XE nodes. When using Blue Waters for this computation user would need 3200 nodes in order to run 3200 single-core jobs that will result in unnecessarily high account charge since 31 out of 32 cores will not be in use. There are ways to circumvent those difficulties but they require complicated solutions.

Scheduler allows users to share the node between applications. For instance, user needs only 100 nodes on BW in order to run 3200 single-core jobs.

Scheduler provides efficient load balancing by placing the jobs in its own queue and executing them after any of the cores completes its previous assignement and becomes available to process a new job. It means if user has 3200 short-running single-core jobs, user is not required to use 100 nodes on BW. One or ten nodes may be sufficient to handle the entire workload.

The user may decide how to handle a crash of an individual job in the bundle. Scheduler may be instructed to ignore the error and proceed to a new job or the Scheduler may be configured to terminate abnormally if further computation is not desirable.

In addition to efficiently managing single-core jobs, Scheduler can also bundle up OpenMP single-node jobs.

Scheduler consists of less than 200 lines of source code. User is free to integrate this code into their workflow in order to further automate the job submission process.

Scheduler is written in Fortran 90 language. This makes the understanding of the algorithm a bliss.

## Limitations

Scheduler cannot bundle up MPI jobs. Using regular batch queuing system would be the best for that purpose.
One core (node) is dedicated to the job management and does not run jobs.

## Content
```
01dir/           job directory
02dir/           job directory
02dir/           job directory
joblist          list of jobs to run
run              PBS script to launch the composite (bundled) job on Cray
scheduler.F90    source code
scheduler.x      job launcher
```

## Distribution

The source code is distribute under the GNU Public License. Please let me know if you 
make any changes to the code so the improvements and the name of the Contributor get 
passed on to the generations.

## Compilation
```
On Cray platform: ftn -o scheduler.x scheduler.F90
Anywhere else: mpif90 -o scheduler.x scheduler.F90
```

## Handling application errors

Should be compiled under GNU programming environment in order for `system()` function 
return error code. (`module swap PrgEnv-cray PrgEnv-gnu`)

## Command-line arguments

`scheduler.x joblist fullPathToExecutable [-nostdout] [-noexit]`

* `scheduler.x` is a name of the job launcher.
* `joblist` is a mandatory argument that should always be specified in the place of the 
   first argument.  It is a file that includes the list of jobs to be executed. See 
   "joblist format" bellow for explanation of the format.
* `fullPathToExecutable` is a name of the application executable that will be invoked 
   by the job launcher.  This executable executes jobs in the joblist. The executable 
   could be a real application executable or bash shell.
* `-nostdout` is an optional program argument. When present it will instruct the job 
   launcher to redirect the job stdout to `/dev/null`. If this option is not present, 
   the job stdout will be saved in .slog file.
* `-noexit` is an optional program argument. It instructs the job launcher to ignore the 
   individual job failure and continue to other jobs in the list. If this argument 
   is absent, job launcher will treat the job failure as a critical error and 
   abnormally terminate.

## Usage

To start the job, type `qsub run`.

## joblist format

Each line in this file defines a single job. The number of jobs to run is determined by 
the number of lines in the joblist file. Each line has two fileds separated by blank space.
First field is directory where the job will be executed. The directory can be given with
absolute path, e.g. `/u/scratch/01dir/` if necessary. Second field is a command-line
argument of the application to be submitted.

## Description

Following is the content of `run` script which is used to start the composite job.
```
#! /bin/sh
#PBS -j oe
#PBS -l nodes=1:ppn=32:xe
#PBS -l walltime=00:01:00
#PBS -N ztest
cd $PBS_O_WORKDIR
aprun -n 3 ./scheduler.x joblist /bin/bash > log
```

In this example, we reserve a single node and use 3 cores (`-n 3`) on the node. 
We will be reading the job list from `joblist` and using `/bin/bash` to execute the 
individual jobs (bash scripts, to be exact).

The content of joblist is the following:
```
01dir job1.sh
02dir job2.sh
```

We want to run two jobs, each placed in a separate directory so that their output results 
will not be accidentally overwritten by another job. First job is located in `01dir/` 
directory. The batch tool will cd to that directory and execute `/bin/bash job1.sh`. 
Second job is located in `02dir/` directory, and the job to execute is `/bin/bash job2.sh`.

One can use absolute path in the job directory name. Use "." directory specificator
in joblist to point to current working directory. Make sure your jobs do not write to the 
same file.

As mentioned above, we will be running two jobs, each on a separate core. One extra core 
is necessary for the batch tool itself. That is why we ask for 3 cores from aprin (`-n 3`).
That extra core will not do any computational work but listen for child processes getting 
ready to start new job. This is done to efficiently handle the case when number of jobs
is greater than the number of compute cores.

If the number of jobs is greater than the number of cores, the jobs will be executed in 
a loop after the cores currently running a job become available to start a new job.

Example of running a bunch of threaded single-node jobs:

`aprun -n 1280 -N 1 -d 32 ./scheduler.x joblist /home/user/bin/myOpenMPjob.x > log`

Out of the 40 nodes used, one node will run the batch tool and thus will be excluded from
computation. Notice that the application `/home/user/bin/myOpenMPjob.x` has to be specified 
with full path or it may not be found during the execution.

In this example, I assume that `myOpenMPjob.x` needs a single input argument that is specified 
in the joblist file in the second column

joblist:
```
myfirstdir inputA.dat
mysecondir inputB.dat
```

Based on this information, the launcher executes the following commands:
```
"cd /cwd/myfirstdir; /home/user/bin/myOpenMPjob.x inputA.dat" and 
"cd /cwd/mysecondir; /home/user/bin/myOpenMPjob.x inputB.dat" 
```

Note, the launcher will construct the full path to the job directory unless the directory 
name already starts from "/". 

If the application requires a more elaborate input/start, e.g. it need two or more
command-line arguments, wrap the job inito a shell script and use `/bin/bash` to run it as
described in the first example.

This tool is particularly handy to run a large number of (non-MPI) jobs on a limited 
number of nodes. To utilize this functionality, put more jobs in the joblist exceeding the
number of requested cores.

Final reminder. The composite job will wait for the last core to complete its job. 
