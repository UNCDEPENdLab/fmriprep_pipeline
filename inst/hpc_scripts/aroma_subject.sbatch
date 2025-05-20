#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 4
#SBATCH --time=24:00:00
#SBATCH --mem=48g

set -eE  # 'E' ensures ERR trap inherits in functions/subshells

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure aroma SIGTERM' SIGTERM
trap 'trap_job_failure aroma SIGINT' SIGINT
trap 'trap_job_failure aroma SIGHUP' SIGHUP
trap 'trap_job_failure aroma SIGQUIT' SIGQUIT
#trap 'traperror $? $LINENO $BASH_LINENO "$BASH_COMMAND" $(printf "::%s" ${FUNCNAME[@]:-})' ERR
trap 'on_error $?' ERR

# default nthreads to however many cores we have $SLURM_SOMETHING
[ -z "$aroma_nthreads" ] && echo "No aroma_nthreads environment variable. Defaulting to 4" && aroma_nthreads=4

ncores=$SLURM_NTASKS

####
#verify required arguments
[ -z "$aroma_container" ] && echo "aroma_container not set. Exiting" && exit 1
[ ! -f "$aroma_container" ] && echo "aroma_container $aroma_container not found. Exiting" && exit 1
[ ! -r "$aroma_container" ] && echo "aroma_container $aroma_container not readable. Exiting" && exit 1
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1
[ -z "$loc_scratch" ] && echo "loc_scratch not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ ! -d "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not found. Exiting" && exit 1
[ ! -r "$loc_bids_root" ] && echo "loc_bids_root $loc_bids_root not readable. Exiting" && exit 1
[ -z "$templateflow_home" ] && echo "templateflow_home not set. Exiting." && exit 1
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
[[ -n "$cmd_log" ]] && cmd_log="-l $cmd_log" # if cmd_log is set, pass it to rel

singularity_cmd="singularity run --cleanenv -B '$loc_bids_root' -B '$loc_scratch' \
  -B '$loc_mrproc_root' -B '$templateflow_home' '$aroma_container'"

rel c $cmd_log Starting aroma for subject $sub_id $ses_str at $(date +"%Y-%m-%d %H:%M:%S") # log the start of the aroma command

rel $rel_flag $cmd_log export APPTAINERENV_TEMPLATEFLOW_HOME="$templateflow_home" # set the environment variable for templateflow in the singularity container

#singularity exec --cleanenv -B "$loc_bids_root" -B "$loc_scratch" \
#    -B "$loc_mrproc_root" -B "$templateflow_home" "$aroma_container" aroma \

rel $rel_flag $cmd_log $singularity_cmd \
    "$loc_bids_root" "$loc_mrproc_root/" participant \
    $cli_options # all options setup by submit_aroma()

[[ $? -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > ${out_dir}/.aroma_complete

rel c $cmd_log Finished aroma for subject $sub_id $ses_str at $(date +"%Y-%m-%d %H:%M:%S") # log the end of the aroma command
