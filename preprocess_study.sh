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

cfg_file="$1" #config file for study

[ ! -r "$cfg_file" ] && "Cannot access config file: $cfg_file" && exit 1

#Setup environment variables for processing data
source $cfg_file

#allow for alternative environment cfg
env_file="${script_dir}/environment.cfg"

[ $# -gt 1 ] && env_file="$2" #if second argument is passed in, use that instead of environment.cfg

[ ! -r "$env_file" ]  && echo "Cannot access config file ${env_file}" && exit 1
source "$env_file" #setup relevant variables for this processing environment

#Pull raw MRI data from SLEIC
"${script_dir}/syncMRCTR_MRRaw"

#loop over subjects
for sub in $(seq -f "%03g" 2000); do 
    #For each subject with raw data...
    if [ -d "${loc_mrraw_root}/$sub" ]; then
	#If they don't yet have BIDS data, run them through the full pipeline		
	if [ ! -d "${loc_root}/bids/sub-${sub}" ];then
	    heudiconv=$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrraw_root=$loc_mrraw_root heudiconv.sh)
	    qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root mriqc.sh
	    qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh	
	    qsub -W depend=afterok:$heudiconv -v sub=$sub,output_dir=${loc_root}/fidelity_checks,repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs
	else
	    #Run MRIQC, if not already run
	    if [ ! -d "${loc_root}/mriqc_IQMs/sub-${sub}" ];then
		qsub -v sub=$sub,loc_root=$loc_root mriqc.sh
	    fi
	    #Run fmriprep, if not already run
	    if [ ! -d "${loc_mrproc_root}/fmriprep/sub-${sub}" ]; then
		qsub -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh	
	    fi
	    
	    #Run fidelity checks
	    qsub -v sub=$sub,output_dir=${loc_root}/fidelity_checks,repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs
	fi
    fi
done
