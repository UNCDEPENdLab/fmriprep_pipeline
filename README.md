# fmriprep_pipeline
Processing scripts for fmriprep pipeline on torque cluster

## Key ingredients:
 - environment.cfg          : All details of the compute environment for running the pipeline (adjust to your environment!)
 - study_cfg/               : Config files for each study that can be handled by the pipeline
 - preprocess_study.sh      : Overlord script that transfers data for a study, then pushes new data through BIDS conversion mriqc, fmriprep, and fidelity checks
 - fmriprep_wrapper         : Torque-friendly wrapper for bare-metal virtualenv fmriprep installation. Sets up environment and then calls fmriprep with specified arguments
 - qsub_heudiconv.sh        : Convert a single subject dataset into BIDS format using a heuristic file
 - qsub_fmriprep_subject.sh : Process a single BIDS dataset through fmriprep using a torque job with 8 cores
 - qsub_mriqc_subject.sh    : Process a single BIDS dataset through mriqc using a torque job

