# Purpose
The code in this directory produces status reports on the state of the NeuroMap pipeline. It produces 3 reports:

1. Completion status of each program (mriqc, fmriprep, etc...) across all runs of [`preprocess.sh`](../preprocess.sh)
1. stderr of programs run during the current run of [`preprocess.sh`](../preprocess.sh)
1. the fidelity checks computed during the current run

These reports are compiled into a single file that is dropped in the NeuroMap directory, and it is also emailed to an email address specified in [`preprocess.sh`](../preprocess.sh).

# Usage
The different reports can be combined in different way if desired, but a command is provided that integrates them all:

```
python pipeline_status.py <expectationFilepath> <baseLoc> <outputFileDir> <modeMapYaml>
```

## Arguments

* **expectationFilepath**: the path to a file containing a mapping of subject ID numbers to ACI job id numbers. This is currently `../.subs_jobs.log`. This file is used to figure out which output files belong to which subjects.
* **baseLoc**: the NeuroMap root output directory. The output locations of each program started by the pipeline should be relative to this location. From this location, this program will check for ".complete" files to decide if pipeline programs completed successfully.
* **modeMapYaml**: The path to the yaml file containing a mapping of data modalities to output locations and file names. This is used to figure out where the completion files denoted a successful program run should be located. The current file is [`complete-placement.yaml`](complete-placement.yaml). If directory layouts change, this file should be modified accordingly.
* **outputFileDir**: The directory that contains the ACI job's stdout and stderr files. These files should be of the form `<script name>.[oe][0-9]*`.

# Explanation
The task is broken out into 3 logically divided scripts in two different files.

## Completion Status Detection - [`pipeline_status.py`](pipeline_status.py)
Is is desirable to know if programs run during previous calls to [`preprocess.sh`](../preprocess.sh) completed successfully. Since not every program is run during every call, some persistent marker is needed to remember. In this case, a file is placed if and only if the program returns a non-error status code. The name and location of these files can be changed by modifying [`complete-placement.yaml`](complete-placement.yaml). The script discussed here will check these locations for the completion files and record which programs have completed for which subjects, across all runs of the pipeline.

## Output Organization and Intepretation - [`compile_output.py`](compile_output.py)
Besides completition status, it is also desirable to have an easy-to-access, concise report of program output. This script reads the contents of ACI stdout/stderr files and, based on their filenames, decides what their contents are and how they should be presented. If the filename is of the form `*.e*`, it will be interpreted as a process's stderr, and will only be noted in the report if it is non-empty. If the filename is of the form `fidelity_checks.o*`, it will be intepreted as stdout from the fidelity checks and presented as such. For conciseness, only the output from the currently ran jobs is processed: fidelity check information is persistent and previously run results can be viewed at any time with other programs and the error output is often only relevant for that current run (previous error logs should not be shown in every future report).

## Report Collation - [`pipeline_status.py`](pipeline_status.py)
This last script simply calls the above two routines and splices their output together in a pleasing way, and it provides a command-line interface to access the status report functions.

# Contact
If you have questions that were not answered here or other concerns please contact the developer, Austin Marcus, by email: axm6053@psu.edu
