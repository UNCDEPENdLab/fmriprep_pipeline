#!/bin/bash
set -e
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
    printHelp
    exit 1
fi

if [[ "$1" == "-help" || "$1" == "--help" ]]; then
    printHelp
    exit 0
fi

#https://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
pipedir="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )" 
source "${pipedir}/pipeline_functions" #helpers for building qsub calls

#allow for alternative environment cfg
env_file="${pipedir}/compute_environment.cfg"

[ $# -gt 1 ] && env_file="$2" #if second argument is passed in, use that instead of environment.cfg

[ ! -r "$env_file" ]  && echo "Cannot access config file ${env_file}" && exit 1
source "$env_file" #setup relevant variables for this processing environment

#source config file for this study, which can override settings in the compute environment file
study_cfg_file="$1"
[ ! -r "$study_cfg_file" ] && "Cannot access config file: $study_cfg_file" && exit 1

#Setup environment variables for processing data
source "$study_cfg_file"

#defaults, if not specified in $env_file
[ -z "$sync_raw_data" ] && sync_raw_data=1 #default to transferring raw data unless set to 0 in study cfg
[ -z "$run_fmriprep" ] && run_fmriprep=1 #default to including fmriprep in pipeline
[ -z "$run_mriqc" ] && run_mriqc=1 #default to including mriqc in pipeline
[ -z "$debug_pipeline" ] && debug_pipeline=0 #whether to just echo all commands instead of executing them

#if debug_pipeline is 2, do not qsub any jobs, just echo all commands in the loop below as comments
[ $debug_pipeline -eq 2 ] && rel_suffix=c
if [ $debug_pipeline -eq 1 ]; then #set one-minute execution times for subsidiary scripts for testing
    heudiconv_walltime=00:01:00
    mriqc_walltime=00:01:00
    fmriprep_walltime=00:01:00
fi
    

#default log file name is <pipedir>/<study_cfg_name>_log.txt
[ -z "$log_file" ] && log_file=$( echo -n "$study_cfg_file" | perl -pe 's/\.\w+$//' | cat <(echo -n "${pipedir}/") - <(echo "_log.txt") )

#Pull raw MRI data from remote host, if requested
[ ${sync_raw_data} -eq 1 ] && rel "${pipedir}/syncMRCTR_MRRaw" $rel_suffix

#loop over subjects by findings all top-level directories in loc_mrraw_root that match the subid_regex (note that spaces in dirnames are a problem)
subdirs=$( find "${loc_mrraw_root}" -mindepth 1 -maxdepth 1 -type d | grep -E "${subid_regex}" )

for sdir in $subdirs; do
    rel "Processing subject directory: $sdir" c

    sub=$( basename $sdir ) #subject id is folder name
    #If they don't yet have BIDS data, run them through the full pipeline, enforcing dependency on BIDS conversion
    if [ ! -d "${loc_root}/bids/sub-${sub}" ]; then
	heudiconv_cmd="qsub $( build_qsub_string walltime=$heudiconv_walltime ) \
			    -v $( envpass sub loc_root loc_mrraw_root log_file pipedir heudiconv_location heudiconv_heuristic debug_pipeline ) qsub_heudiconv_subject.sh"
	
	if [ "$rel_suffix" == "c" ]; then
	    #run in debug mode
	    rel "$heudiconv_cmd" $rel_suffix
	    bids_jobid="dummy12345"
	else
	    #o argument to rel captures command output, which is the jobid
	    bids_jobid=$( rel "$heudiconv_cmd" o )
	fi
	
	depend_string="-W depend=afterok:${bids_jobid}"
    else
	depend_string=
    fi

    #Run MRIQC, if not already run
    if [[ ${run_mriqc} -eq 1 && ! -d "${loc_root}/mriqc_IQMs/sub-${sub}" ]]; then
	rel "qsub $depend_string $( build_qsub_string walltime=$mriqc_walltime ) \
	    -v $( envpass debug_pipeline sub loc_root log_file pipedir ) \
	    ${pipedir}/qsub_mriqc_subject.sh" $rel_suffix
    fi

    #Run fmriprep, if not already run
    if [[ ${run_fmriprep} -eq 1 && ! -d "${loc_mrproc_root}/fmriprep/sub-${sub}" ]]; then
	rel "qsub $depend_string $( build_qsub_string nodes=1:ppn=$fmriprep_nthreads walltime=$fmriprep_walltime ) \
	    -v $( envpass debug_pipeline sub loc_root loc_bids_root loc_mrproc_root fmriprep_nthreads log_file pipedir ) \
	    ${pipedir}/qsub_fmriprep_subject.sh" $rel_suffix
    fi

    if [[ ${run_fidelity_checks} -eq 1 ]]; then
	rel "qsub $depend_string $( build_qsub_string ) \
	    -v $( envpass debug_pipeline sub fidelity_json loc_root loc_bids_root log_file pipedir ) \
	    ${pipedir}/mri_fidelity_checks/qsub_fidelity_checks.sh" $rel_suffix
    fi

    rel "" c #blank line
done
