#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 4
#SBATCH --time=24:00:00
#SBATCH --mem=48g

set -e

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure fmriprep SIGTERM' SIGTERM
trap 'trap_job_failure fmriprep SIGINT' SIGINT
trap 'trap_job_failure fmriprep SIGHUP' SIGHUP
trap 'trap_job_failure fmriprep SIGQUIT' SIGQUIT

# default nthreads to however many cores we have $SLURM_SOMETHING
[ -z "$fmriprep_nthreads" ] && echo "No fmriprep_nthreads environment variable. Defaulting to 4" && fmriprep_nthreads=4

####
#verify required arguments
[ -z "$fmriprep_container" ] && echo "fmriprep_container not set. Exiting" && exit 1
[ ! -f "$fmriprep_container" ] && echo "fmriprep_container $fmriprep_container not found. Exiting" && exit 1
[ ! -r "$fmriprep_container" ] && echo "fmriprep_container $fmriprep_container not readable. Exiting" && exit 1
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1
[ -z "$loc_scratch" ] && echo "loc_scratch not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ ! -d "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not found. Exiting" && exit 1
[ ! -r "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not readable. Exiting" && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[[ "$debug_pipeline" == "TRUE" ]] && debug_pipeline=1 || debug_pipeline=0 # set debug_pipeline to 1 if it is set to TRUE, otherwise set it to 0

###
# handle multi-session setup
if [ -n "$ses_id" ]; then
  out_dir=${loc_bids_root}/sub-${sub_id}/ses-${ses_id}
  ses_flag="--ses $ses_id"
  ses_str="session $ses_id"
else
  out_dir=${loc_bids_root}/sub-${sub_id}
  ses_flag=""
  ses_str=""
fi
mkdir -p "$out_dir"
cd $out_dir || { echo "Failed to change directory to $out_dir"; exit 1; }

####
[[ "$debug_pipeline" -eq 1 ]] && rel_flag=c #if debug_pipeline is 1, only echo command to log, don't run it
# rel "${pipedir}/fmriprep_wrapper ${loc_bids_root} ${loc_mrproc_root}/ participant --participant_label $sub --nthreads $fmriprep_nthreads -w ${loc_root}/fmriprep_tempfiles && date \"+%m%d%y@%H:%M\" > $loc_bids_root/sub-$sub/.fmriprep.complete" $rel_flag

# rel $rel_flag $fmriprep_container "$loc_bids_root" "$loc_mrproc_root/" participant --participant-label $sub \
#   --nthreads $fmriprep_nthreads -w 

# BASE_SINGULARITY_CMD = (
#     "unset PYTHONPATH; {templateflow1} singularity run -B {templateflow2}"
#     "{bindPaths} {batchcommands} {fmriprepInstance} {bids_dir} {output_dir} "
#     "participant --participant-label {participantLabels} -w {working_dir} "
#     "--fs-license-file {fslicense} {threads} {useAROMA} {other_opts}"
# )

singularity_cmd="singularity run --cleanenv -B '$loc_bids_root' -B '$loc_scratch' -B '$loc_mrproc_root' '$fmriprep_container'"

rel c $cmd_log Starting fmriprep for subject $sub_id $ses_str at $(date +"%Y-%m-%d %H:%M:%S") # log the start of the fmriprep command

rel $rel_flag $singularity_cmd \
    "$loc_bids_root" "$loc_mrproc_root/" participant \
    $cli_options 
    #--participant-label $sub_id \
    #--nthreads $fmriprep_nthreads \
    #$cli_options 
    
[[ $? -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > ${out_dir}/.fmriprep_complete

rel c $cmd_log Finished fmriprep for subject $sub_id $ses_str at $(date +"%Y-%m-%d %H:%M:%S") # log the end of the fmriprep command
