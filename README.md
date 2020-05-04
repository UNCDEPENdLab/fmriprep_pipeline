# fmriprep_pipeline
Processing scripts for fmriprep pipeline on torque cluster

## Key ingredients:
 - compute_environment.cfg  : All details of the compute environment for running the pipeline (adjust to your environment!)
 - study_cfg/               : Config files for each study that can be handled by the pipeline
 - preprocess_study.sh      : Overlord script that transfers data for a study, then pushes new data through BIDS conversion mriqc, fmriprep, and fidelity checks
 - fmriprep_wrapper         : Torque-friendly wrapper for bare-metal virtualenv fmriprep installation. Sets up environment and then calls fmriprep with specified arguments
 - qsub_heudiconv.sh        : Convert a single subject dataset into BIDS format using a heuristic file
 - qsub_fmriprep_subject.sh : Process a single BIDS dataset through fmriprep using a torque job with 8 cores
 - qsub_mriqc_subject.sh    : Process a single BIDS dataset through mriqc using a torque job
 - mri_fidelity_checks/	    : In-house scripts for validating MRI scanning parameters against expectations
 - add_intendedfor_bold     : In-house R script for adding IntendedFor field to fieldmap files, defaulting to matching all BOLD files for a subject
 - syncMRCTR_MRRaw          : In-house shell script for incremental copy of imaging data from remote host using rsync and ssh
 