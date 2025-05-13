#!/bin/bash
#Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=2:00:00
#SBATCH --memgb=16gb

set -e
cd $SLURM_SUBMIT_DIR

####
#setup compute environment

# source bash functions used in pipeline shell scripts
pipedir=$( dirname "$0" )
source "${pipedir}/pipeline_functions"

# [ -z "$pipedir" ] && pipedir="$PWD"

#in principle, we could pass all environment variables in with -v
# [ -z "$env_file" ] && env_file="${pipedir}/compute_environment.cfg" #set default
# [ ! -r "$env_file" ]  && echo "Cannot access compute config file ${env_file}" && exit 1
# source "$env_file" #setup relevant variables for this processing environment

# [[ -n "${study_cfg}" && -r "${study_cfg}" ]] && source "${study_cfg}" #source a study config file if passed as environment variable
# [ -n "$torque_modules_root" ] && module use "$torque_modules_root"
# [ -n "$r_module" ] && module load "$r_module"

####
#verify necessary arguments
[ -z "$heudiconv_container" ] && echo "heudiconv_container not set. Exiting" && exit 1
[ -z "$loc_sub_dicoms" ] && echo "loc_sub_dicoms not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ -z "$heudiconv_heuristic" ] && echo "No heudiconv_heuristic variable set." && exit 1
[ -z "$sub" ] && echo "sub not set. Exiting." && exit 1
[ -n "$ses" ] && ses_flag="--ses $ses" || ses_flag=""

# [ -z "${heudiconv_location}" ] && echo "No heudiconv_location variable set." && exit 1
# [ -z "${heudiconv_heuristic}" ] && echo "No heudiconv_heuristic variable set." && exit 1
# [ -z "${loc_sub_dicoms}" ] && echo "No loc_sub_dicoms variable set." && exit 1
# [ -z "${loc_bids_root}" ] && echo "No loc_bids_root variable set." && exit 1



####
#run heudiconv
[[ "$debug_pipeline" -eq 1 ]] && rel_flag=c #if debug_pipeline is 1, only echo command to log, don't run it
# rel "${heudiconv_location} -d ${loc_dicom_root}/{subject}/*/*.dcm -s $sub -f ${heudiconv_heuristic} -c dcm2niix -o ${loc_bids_root} -b && date \"+%m%d%y@%H:%M\" > ${loc_bids_root}/sub-${sub}/.heudiconv.complete" $rel_suffix
# rel "${pipedir}/add_intendedfor_bold ${loc_bids_root}/sub-${sub}" $rel_suffix #add IntendedFor field to FMAP jsons, pointing to all BOLD

singularity_cmd="singularity run --cleanenv -B $loc_bids_root -B $loc_sub_dicoms $heudiconv_container

# we now use --files to specify the dicoms to convert rather than -d since we don't want to assume
# the dicoms are in a specific directory structure (esp. in terms of level of nesting).
rel $rel_flag ${singularity_cmd} --files ${loc_sub_dicoms} -s $sub $ses_flag \
  -f $heudiconv_heuristic -c dcm2niix \
  -o ${loc_bids_root} --bids && date '+%m%d%y@%H:%M' > ${loc_bids_root}/sub-${sub}/.heudiconv_complete"

#singularity run --cleanenv -B /proj/mnhallqlab/projects/preproc_pipeline_test_data/dicoms \
#  -B /proj/mnhallqlab/projects/preproc_pipeline_test_data/processed \
#  /proj/mnhallqlab/users/michael/fmriprep_pipeline_setup/heudiconv_latest.sif \
#  --files dicoms/540294 -f heuristic.py -s 540294 -c dcm2niix --bids \
#  -o /proj/mnhallqlab/projects/preproc_pipeline_test_data/processed
