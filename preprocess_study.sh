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
  preprocess_study.sh study_cfg/neuromap.cfg

  preprocess_study.sh study_cfg/neuromap.cfg alternate_compute_environment.cfg

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
if [ $debug_pipeline -eq 1 ]; then #set one-minute execution times for subsidiary scripts for testing
    heudiconv_walltime=00:01:00
    mriqc_walltime=00:01:00
    fmriprep_walltime=00:01:00

if [ $debug_pipeline -eq 2 ]; then
	rel_suffix=c
else
	rel_suffix=o
fi

#default log file name is <pipedir>/<study_cfg_name>_log.txt
[ -z "$log_file" ] && log_file=$( echo -n "$study_cfg_file" | perl -pe 's/\.\w+$//' | cat <(echo -n "${pipedir}/") - <(echo "_log.txt") )

#Pull raw MRI data from remote host, if requested
[ ${sync_raw_data} -eq 1 ] && rel "${pipedir}/syncMRCTR_MRRaw" $rel_suffix

#loop over subjects by findings all top-level directories in loc_mrraw_root that match the subid_regex (note that spaces in dirnames are a problem)
subdirs=$( find "${loc_mrraw_root}" -mindepth 1 -maxdepth 1 -type d | grep -E "${subid_regex}" )

allJobIds=""
echo "" > $expectation_file
echo "rel_suffix = $rel_suffix"
for sdir in $subdirs; do
    rel "Processing subject directory: $sdir" c

    sub=$( basename $sdir ) #subject id is folder name
	echo $sub
	if [[ $sub != "005" && $sub != "008" ]]; then
		continue
	fi
    #If they don't yet have BIDS data, run them through the full pipeline, enforcing dependency on BIDS conversion
    if [ ! -e "${loc_bids_root}/sub-${sub}/.heudiconv.complete" ]; then
		heudiconvID=$(rel "qsub $( build_qsub_string walltime=$heudiconv_walltime ) \
			-v $( envpass rel_suffix sub loc_bids_root loc_mrraw_root log_file pipedir heudiconv_location heudiconv_heuristic debug_pipeline ) \
			${pipedir}/qsub_heudiconv_subject.sh" $rel_suffix heudiconvID-$sub)
    fi

    #Run MRIQC, if not already run
    if [[ ${run_mriqc} -eq 1 && ! -e "${loc_bids_root}/sub-${sub}/.mriqc.complete" ]]; then
		mriqcID=$(rel "qsub $( build_depend_string afterok "$heudiconvID" ) $( build_qsub_string nodes=1:ppn=$mriqc_nthreads walltime=$mriqc_walltime ) \
			-v $( envpass rel_suffix loc_bids_root debug_pipeline sub loc_root log_file pipedir ) \
			${pipedir}/qsub_mriqc_subject.sh" $rel_suffix mriqcID-$sub)
    fi

    #Run fmriprep, if not already run
    if [[ ${run_fmriprep} -eq 1 && ! -e "${loc_bids_root}/sub-${sub}/.fmriprep.complete" ]]; then
		fmriprepID=$(rel "qsub $( build_depend_string afterok "$heudiconvID" ) $( build_qsub_string nodes=1:ppn=$fmriprep_nthreads walltime=$fmriprep_walltime ) \
			-v $( envpass rel_suffix debug_pipeline sub loc_root loc_bids_root loc_mrproc_root fmriprep_nthreads log_file pipedir ) \
			${pipedir}/qsub_fmriprep_subject.sh" $rel_suffix fmriprepID-$sub)
    fi

    if [[ ${run_fidelity_checks} -eq 1 && ! -e "${loc_bids_root}/sub-${sub}/.fidelity.complete" ]]; then
		fidelityID=$(rel "qsub $( build_depend_string afterok "$heudiconvID" afterany "$fidelityID") $( build_qsub_string ) \
			-v $( envpass rel_suffix debug_pipeline sub fidelity_json loc_root loc_bids_root log_file pipedir ) \
			${pipedir}/mri_fidelity_checks/qsub_fidelity_checks.sh" $rel_suffix fidelityID-$sub)
    fi

	if [[ $debug_pipeline -eq 2 ]]; then
		echo $heudiconvID
		echo $mriqcID
		echo $fmriprepID
		echo $fidelityID
	fi

	# track job ids over run
	currentJobIds=$(link_job_listings $mriqcID $fmriprepID $fidelityID $bids_jobid | sed 's/,*$//')
	echo -e "${sub}\t${currentJobIds}" >> $expectation_file # write subject-job pairing to file for status scripts
	allJobIds="${allJobIds} afterany \"$mriqcID\" afterany \"$fmriprepID\" afterany \"$fidelityID\" afterany \"$heudiconvID\"" # continue building list of all jobs being queued

    rel "" c #blank line
done

# schedule report
if [ ! -z "$allJobIds" ]; then
	allJobIds=$(build_depend_string $allJobIds | sed 's/,*$//')
	echo $allJobIds
	rel "qsub $allJobIds -d $PWD  $( build_qsub_string ) \
		-v $( envpass rel_suffix debug_pipeline aci_output_dir expectation_file loc_root qsub_email loc_yaml ) \
		${pipedir}/report.sh" $rel_suffix
else
	rel "All subjects that have raw DICOM data have been fully processed: not submitting any jobs to qsub" c
fi
