#!/bin/bash
#Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=2:00:00
#SBATCH --mem=16g

set -e

# source bash functions used in pipeline shell scripts
[ -z "$pkg_dir" ] && echo "pkg_dir not set. Cannot locate required helper scripts" && exit 1
source "${pkg_dir}/shell_functions"

# Set trap for common termination signals
trap 'trap_job_failure heudiconv SIGTERM' SIGTERM
trap 'trap_job_failure heudiconv SIGINT' SIGINT
trap 'trap_job_failure heudiconv SIGHUP' SIGHUP
trap 'trap_job_failure heudiconv SIGQUIT' SIGQUIT

# [[ -n "${study_cfg}" && -r "${study_cfg}" ]] && source "${study_cfg}" #source a study config file if passed as environment variable
# [ -n "$torque_modules_root" ] && module use "$torque_modules_root"
# [ -n "$r_module" ] && module load "$r_module"

####
#verify necessary arguments
[ -z "$heudiconv_container" ] && echo "heudiconv_container not set. Exiting" && exit 1
[ ! -f "$heudiconv_container" ] && echo "heudiconv_container $heudiconv_container not found. Exiting" && exit 1
[ ! -r "$heudiconv_container" ] && echo "heudiconv_container $heudiconv_container not readable. Exiting" && exit 1
[ -z "$loc_sub_dicoms" ] && echo "loc_sub_dicoms not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ -z "$heudiconv_heuristic" ] && echo "No heudiconv_heuristic variable set." && exit 1
[ ! -f "$heudiconv_heuristic" ] && echo "Heuristic file $heudiconv_heuristic not found." && exit 1
[ ! -r "$heudiconv_heuristic" ] && echo "Heuristic file $heudiconv_heuristic not readable." && exit 1
[ -z "$sub_id" ] && echo "sub_id not set. Exiting." && exit 1
[[ "$debug_pipeline" == "TRUE" ]] && debug_pipeline=1 || debug_pipeline=0 # set debug_pipeline to 1 if it is set to TRUE, otherwise set it to 0
# [ -z "$cmd_log" ] && cmd_log="${loc_bids_root}/heudiconv_${sub_id}${ses_str}.log"

heuristic_dir=$(dirname "$heudiconv_heuristic")

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
#run heudiconv
[ $debug_pipeline -eq 1 ] && rel_flag=c # if debug_pipeline is 1, only echo command to log, don't run it
[[ -n "$cmd_log" ]] && cmd_log="-l $cmd_log" # if cmd_log is set, pass it to rel

rel c $cmd_log Starting heudiconv for subject $sub_id $ses_str at $(date +"%Y-%m-%d %H:%M:%S") # log the start of the heudiconv command

singularity_cmd="singularity run --cleanenv -B $loc_bids_root -B $loc_sub_dicoms -B $heuristic_dir $heudiconv_container"

# we now use --files to specify the dicoms to convert rather than -d since we don't want to assume
# the dicoms are in a specific directory structure (esp. in terms of level of nesting).
rel $rel_flag $cmd_log $singularity_cmd --files $loc_sub_dicoms -s $sub_id $ses_flag \
  -f $heudiconv_heuristic -c dcm2niix \
  -o $loc_bids_root --bids 

# write complete file if the command was successful and debug_pipeline is not set
[[ $? -eq 0 && $debug_pipeline -eq 0 ]] && date +"%Y-%m-%d %H:%M:%S" > ${out_dir}/.heudiconv_complete

rel c $cmd_log Finished heudiconv for subject $sub_id $ses_str at $(date +"%Y-%m-%d %H:%M:%S") # log the start of the heudiconv command

#######
# example call
# singularity run --cleanenv -B /proj/mnhallqlab/projects/preproc_pipeline_test_data/dicoms \
#   -B /proj/mnhallqlab/projects/preproc_pipeline_test_data/data_BIDS \
#   /proj/mnhallqlab/users/michael/fmriprep_pipeline_setup/heudiconv_latest.sif \
#   --files /proj/mnhallqlab/projects/preproc_pipeline_test_data/dicoms/540311 -f heuristic.py -s 540311 -c dcm2niix --bids \
#   -o /proj/mnhallqlab/projects/preproc_pipeline_test_data/data_BIDS

# old bare-metal command
# ${heudiconv_location} -d ${loc_dicom_root}/{subject}/*/*.dcm -s $sub_id -f ${heudiconv_heuristic} -c dcm2niix -o ${loc_bids_root} -b && date \"+%m%d%y@%H:%M\" > ${loc_bids_root}/sub-${sub_id}/.heudiconv.complete
# ${pipedir}/add_intendedfor_bold ${loc_bids_root}/sub-${sub_id} #add IntendedFor field to FMAP jsons, pointing to all BOLD
