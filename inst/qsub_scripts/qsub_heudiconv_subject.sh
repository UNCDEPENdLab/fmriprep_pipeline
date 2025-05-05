#!/bin/bash
#Default PBS requests if not overridden on qsub command line
#PBS -l nodes=1:ppn=1
#PBS -l walltime=2:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -o aci_output
#PBS -e aci_output
#PBS -N heudiconv

set -e
cd $PBS_O_WORKDIR

####
#setup compute environment
[ -z "$pipedir" ] && pipedir="$PWD"
source "${pipedir}/pipeline_functions"

#in principle, we could pass all environment variables in with -v
[ -z "$env_file" ] && env_file="${pipedir}/compute_environment.cfg" #set default
[ ! -r "$env_file" ]  && echo "Cannot access compute config file ${env_file}" && exit 1
source "$env_file" #setup relevant variables for this processing environment

[[ -n "${study_cfg}" && -r "${study_cfg}" ]] && source "${study_cfg}" #source a study config file if passed as environment variable
[ -n "$torque_modules_root" ] && module use "$torque_modules_root"
[ -n "$r_module" ] && module load "$r_module"

####
#verify necessary arguments
[ -z "${heudiconv_location}" ] && echo "No heudiconv_location variable set." && exit 1
[ -z "${heudiconv_heuristic}" ] && echo "No heudiconv_heuristic variable set." && exit 1
[ -z "${loc_mrraw_root}" ] && echo "No loc_mrraw_root variable set." && exit 1
[ -z "${loc_bids_root}" ] && echo "No loc_bids_root variable set." && exit 1

####
#run heudiconv
[[ "$debug_pipeline" -eq 1 ]] && rel_suffix=c #if debug_pipeline is 1, only echo command to log, don't run it
rel "${heudiconv_location} -d ${loc_mrraw_root}/{subject}/*/*.dcm -s $sub -f ${heudiconv_heuristic} -c dcm2niix -o ${loc_bids_root} -b && date \"+%m%d%y@%H:%M\" > ${loc_bids_root}/sub-${sub}/.heudiconv.complete" $rel_suffix
rel "${pipedir}/add_intendedfor_bold ${loc_bids_root}/sub-${sub}" $rel_suffix #add IntendedFor field to FMAP jsons, pointing to all BOLD
