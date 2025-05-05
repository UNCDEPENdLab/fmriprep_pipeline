#!/bin/bash
#PBS -l nodes=1:ppn=8
#PBS -l walltime=12:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -o aci_output
#PBS -e aci_output
#PBS -N mriqc

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

#load in lab programs, including MRIQC
#I would tend to avoid this since it creates a bit of a blackbox and may setup things you don't want.
#I think mriqc needs ants, fsl, afni
#source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash

#https://stackoverflow.com/questions/51026315/how-to-solve-unicodedecodeerror-in-python-3-6/51027262#51027262
LANG=en_US.UTF-8
export LANG

command -v deactivate >/dev/null 2>&1 && deactivate #exit existing virtual environment if active
command -v module >/dev/null 2>&1 && module unload python #make sure no system python modules are loaded    

#these appear to be the immediate dependencies of mriqc
[ -n "$torque_modules_root" ] && module use "$torque_modules_root"
[ -n "$afni_module" ] && module load "$afni_module"
#[ -n "$fsl_module" ] && module load "$fsl_module"
[ -n "$ants_module" ] && module load "$ants_module"

#[ -n "$freesurfer_module" ] && module load "$freesurfer_module"
module load fsl/5.0.11 #Using version 6 of FSL creates problems

#load version 3 of Python (since the default is version 2, which doesn't work w/MRIQC)
#source /gpfs/group/mnh5174/default/lab_resources/lab_python3/bin/activate

####
#verify necessary arguments
[ -z "${mriqc_location}" ] && echo "No mriqc_location variable set." && exit 1
[ -z "${loc_root}" ] && echo "No loc_root variable set." && exit 1

####
[[ "$debug_pipeline" -eq 1 ]] && rel_suffix=c #if debug_pipeline is 1, only echo command to log, don't run it
rel "${mriqc_location} ${loc_root}/bids/ ${loc_root}/mriqc_IQMs/ participant --participant-label $sub -w ${loc_root}/mriqc_tempfiles && date \"+%m%d%y@%H:%M\" > $loc_bids_root/sub-$sub/.mriqc.complete" $rel_suffix
