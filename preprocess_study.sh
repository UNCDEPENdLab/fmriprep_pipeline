#!/bin/bash

function printHelp() {
cat <<EndOfHelp
-----------------------------------
preprocess_study.sh is a script that does an incremental pull of raw MRI data (DICOMS) from a remote server, restructures these into BIDS format using heudiconv,
then queues fmriprep, mriqc, and (local) fidelity checks on the data for each subject.

It requires a single argument, which is a config file containing all environment variables that define key choice points in the pipeline like
locations of data on the remote and local machines, the location of key programs (e.g., dcm2niix), and so on.

Optionally, you can pass a second configuration file that specifies all of the details of the computation environment (e.g., fmriprep virtual environment location).
If not provided, the script uses environment.cfg in this directory.

Example:
  preprocess_study.sh neuromap.cfg

  preprocess_study.sh neuromap.cfg alternate_compute_environment.cfg

-----------------------------------
EndOfHelp
}

if [ $# -eq 0 ]; then
    printHelp()
    exit 1
fi

if [[ "$1" == "-help" || "$1" == "--help" ]]; then
    printHelp()
    exit 0
fi

script_dir=$( dirname "$0" ) #directory of this script at execution

study_cfg_file="$1" #config file for study
[ ! -r "$study_cfg_file" ] && "Cannot access config file: $study_cfg_file" && exit 1

#Setup environment variables for processing data
source $study_cfg_file

#allow for alternative environment cfg
env_file="${script_dir}/compute_environment.cfg"

[ $# -gt 1 ] && env_file="$2" #if second argument is passed in, use that instead of environment.cfg

[ ! -r "$env_file" ]  && echo "Cannot access config file ${env_file}" && exit 1
source "$env_file" #setup relevant variables for this processing environment

#defaults, if not specified in $env_file
[ -z "$sync_raw_data" ] && sync_raw_data=1 #default to transferring raw data unless set to 0 in study cfg
[ -z "$run_fmriprep" ] && run_fmriprep=1 #default to including fmriprep in pipeline
[ -z "$run_mriqc" ] && run_mriqc=1 #default to including mriqc in pipeline

#Pull raw MRI data from remote host, if requested
[ ${sync_raw_data} -eq 1 ] && "${script_dir}/syncMRCTR_MRRaw"

#loop over subjects
for sub in $(seq -f "%03g" 2000); do 
    #For each subject with raw data...
    if [ -d "${loc_mrraw_root}/$sub" ]; then
	#If they don't yet have BIDS data, run them through the full pipeline, enforcing dependency on BIDS conversion
	if [ ! -d "${loc_root}/bids/sub-${sub}" ]; then
	    bids_jobid=$( qsub $( build_qsub_string walltime=$heudiconv_walltime ) -l  -v $( envpass sub loc_root loc_mrraw_root ) qsub_heudiconv_subject.sh )
	    depend_string="-W depend=afterok:${bids_jobid}"
	else
	    depend_string=
	fi

	#Run MRIQC, if not already run
	if [[ ${run_mriqc} -eq 1 && ! -d "${loc_root}/mriqc_IQMs/sub-${sub}" ]]; then
	    qsub $depend_string $( build_qsub_string walltime=$mriqc_walltime ) -v $( envpass sub loc_root ) ${script_dir}/qsub_mriqc_subject.sh
	fi

	#Run fmriprep, if not already run
	if [[ ${run_fmriprep} -eq 1 && ! -d "${loc_mrproc_root}/fmriprep/sub-${sub}" ]]; then
	    qsub $depend_string $( build_qsub_string walltime=$fmriprep_walltime ) -v $( envpass sub loc_root loc_mrproc_root ) ${script_dir}/qsub_fmriprep_subject.sh	
	fi

	if [[ ${run_fidelity_checks} -eq 1 ]]; then
	    qsub $depend_string $( build_qsub_string ) -v $( envpass sub output_dir=${loc_root}/fidelity_checks repo_loc=$PWD ) ${script_dir}/mri_fidelity_checks/run_fidelity_checks.pbs   
	fi
	
    fi
	
done
