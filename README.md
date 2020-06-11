# NeuroMap fMRI data Pipeline
An explanation of the pipeline centered around [`preprocess.sh`](preprocess.sh).

## Pipeline Completion Status Information
The completion status of the pipeline is tracked through 2 mechanisms:

1. ".complete" files
1. ACI job stdout/stderr files

### (1) .complete files
Several different jobs are submitted to ACI with qsub in [`preprocess.sh`](preprocess.sh). Within these scripts, data processing commands are run. Typically, there is one main command in a job submission. Thus, based on its exit status, the job can be said to have run successfully or not. The exit status is used to conditionally place a ".complete" file in a location that will later be checked to remember if the job completed correctly. The code looks like this:

```
<data processing command> && date > $special_location/.<job name>.complete
```

This `$special_location` is checked by the status scripts in [pipeline_status](pipeline_status) to determine if the job completed successfully. Note that this location must be updated in several locations. See [`preprocess.sh`](preprocess.sh) for details. If a .complete file for a job is not found, that job will be marked as "INCOMPLETE" in the status report.

### (2) ACI stdout/stderr
Jobs submitted to ACI via qsub place files containing that job's stdout and stderr. It is desirable to have easy access to relevant output data of these jobs. To do this, these filenames and their placement locations are modified to allow the status scripts in [pipeline_status](pipeline_status) to connect an output with the correct job and subject. 

This is done through 2 mechanisms:

#### (a) Subject id to ACI job id mapping
As [`preprocess.sh`](preprocess.sh) runs, it writes each subject and the jobs spawned for it to a file: `.subs_jobs.log`. This file is later read by scripts in [pipeline_status](pipeline_status). Since ACI output files contain the job id in their filename, the subject that script belongs to can be established.

#### (b) Job Naming
To identify the *type* of job that the output belongs to, the name of the job must be set when submitted to qsub. This can be done using the `-N` option, and can be set as a PBS directive by putting the following in the PBS submission script:

```
#PBS -N <job type name>
```

Doing this will modify the default ACI output file name to the following: `<job type name>.[oe][0-9]*`. This allows the parser in [pipeline_status](pipeline_status) to get the job type name and include it in the output.

Additionally, for organization, the location ACI output files should be placed is specified with:

```
#PBS -o <directory>
#PBS -e <directory>
```

This `<directory>` is passed as an argument to the status scripts.

### Report Compilation
Information gained from the above is compiled into a single output file, and is placed in the NeuroMap directory (specified by `loc_root` in [`preprocess.sh`](preprocess.sh)). Also, this report is emailed to an arbitrary number of email addresses, which are specified in [`preprocess.sh`](preprocess.sh) via `emailRecipients`. 

---

## Fidelity Checks
Since the fidelity checking scripts access shared data (the files output in [mri_fidelity_checks](mri_fidelity_checks)), and multiple fidelity jobs could be run concurrently when submitted to qsub, there is potential for data to be lost (race condition). To prevent this possibility, each fidelity job cannot run until the one scheduled before it has completed. This ensures that there is no overlap between the run time of fidelity jobs, and thus no chance at data loss. However, this does slow down the overall execution of the fidelity jobs because the pipeline cannot take advantage of parallelism. Note, though, that this does not slow down the scheduling of *other* job types, which are still scheduled normally and can run concurrently.
