#!/bin/bash
# Default SLURM requests if not overridden on command line
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --time=4:00:00
#SBATCH --memgb=48gb

set -e
cd $SLURM_SUBMIT_DIR

####
#setup compute environment
# [ -z "$pipedir" ] && pipedir="$PWD"

# source bash functions used in pipeline shell scripts
pipedir=$( dirname "$0" )
source "${pipedir}/pipeline_functions"

#in principle, we could pass all environment variables in with -v
# [ -z "$env_file" ] && env_file="${pipedir}/compute_environment.cfg" #set default
# [ ! -r "$env_file" ]  && echo "Cannot access compute config file ${env_file}" && exit 1
# source "$env_file" #setup relevant variables for this processing environment

[[ -n "${study_cfg}" && -r "${study_cfg}" ]] && source "${study_cfg}" #source a study config file if passed as environment variable

# default nthreads to however many cores we have $SLURM_SOMETHING
[ -z "$fmriprep_nthreads" ] && echo "No fmriprep_nthreads environment variable. Defaulting to 4" && fmriprep_nthreads=4

####
#verify required arguments
[ -z "$fmriprep_container" ] && echo "fmriprep_container not set. Exiting" && exit 1
[ -z "$loc_mrproc_root" ] && echo "loc_mrproc_root not set. Exiting." && exit 1
[ -z "$loc_scratch" ] && echo "loc_scratch not set. Exiting." && exit 1
[ -z "$loc_bids_root" ] && echo "loc_bids_root not set. Exiting." && exit 1
[ -z "$sub" ] && echo "sub not set. Exiting." && exit 1

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

singularity_cmd="singularity run --cleanenv -B $loc_bids_root:/data -B $loc_scratch:/work $fmriprep_container

rel $rel_flag $singularity_cmd \
    "$loc_bids_root" "$loc_mrproc_root/" participant \
    --participant-label $sub \
    --nthreads $fmriprep_nthreads \
    $cli_options && date '+%m%d%y@%H:%M' > $loc_mrproc_root/sub-$sub/.fmriprep.complete"



#add --low-mem?
#add --mem-mb?
