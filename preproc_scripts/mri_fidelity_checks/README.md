This file explains the purpose of the code in this directory, and how it works. This directory contains code to automate the verification of the fidelity of mri scan data. 

# How to use

## Run the fidelity checks

Usage:

```
python3.* feeder.py <template file> <subject data> <output dir> <range or num> [range or num]...
```

OR

`run_fidelity_checks.sh`: If you use this wrapper script, be sure to change the variables defined in the script and possible environment variables to suit your purposes.

* `<template file>` is the path the the `json` file specifying the correct field values
* `<subject data>` is the path to the BIDS formatted directory containing the fmri data
* `<output dir>` is the path to the directory where the output directory will be created
* `<range or num>` is either a number (e.g. 7) or a range (e.g. 1-5). This range is interpreted as inclusive. Note that at least one must be provided

## View the output of the fidelity checks

Use substatus.py. It will output, for each specified subject, the fidelity checks that it failed. If it didn't fail any, it prints none.

Usage:

```
python3.* substatus.py <fidelity output> <range or num> [range or num]...
```

* `<fidelity output>` is the directory containing the output of running the fidelity checks
* `<range or num>` is either a number (e.g. 7) or a range (e.g. 1-5). This range is interpreted as inclusive. Note that at least one must be provided

## Change the tolerance for floating point comparisons
In [`code/compare.py`](code/compare.py), change the global variable `TOL` to the desired value

---

# Overview of source code
All source code is written for python3

## [feeder.py](code/feeder.py)
This file feeds all the relevant scan data found from the specified directory down to compare.py. It then organizes the resulting output and writes the results of the fidelity checks to output files.

## [compare.py](code/compare.py)
This file reads a json template file and constructs a dictionary of field-value pairs. When passed a relevant fidelity file, it compares the empirical field values in it against this dictionary and returns which tests it passed and which it failed.

## [substatus.py](code/substatus.py)
This file reads the output files of feeder.py and turns this information to be more user-friendly. It will output, by subject, which fields failed to match the correct value to stdout.

## [helper.py](code/helper.py)
This file contains helper functions for parsing data whose functions are needed throughout the other source files.

## Terms in code

These terms are used for determining how to process a given file:

* **task**: Refers to the task done during the relevant scan. For example, "rest_post" or "spotpav".
* **scan type**: Refers to the method of scanning used for the relevant scan file. Either "bold" or "sbref".
* **file suffix**: This is used to keep track of whether a nifti or json file is being processed currently. Several functions need to be handled differently depending on which file type it is.

**fileAnalysis**: A python class that stores the information defined above. One object is generated for every file processed. Provides a method to compare and pair files up that are from the same scan (this is further described below).

**Collate**: Refers to a 2-tuple of fileAnalysis objects. For any given scan parameters (subject, task, scan type), there should be exactly one json file and exactly one nifti file. A collate contains these two paired files. There are cases when only one of the files exists, but this is handled. This term could be more descriptive.

**empirical/experimental**: The field values coming from the actual files in the BIDS data directory, for lack of a better term, are referred to as "empirical" or "experimental" or "actual". This is opposed to the field values from the master json file specifying the correct/desired values. These are referred to as "template" or "target".

# Contact
If you have questions that were not answered here or other concerns please contact the developer, Austin Marcus, by email: axm6053@psu.edu
