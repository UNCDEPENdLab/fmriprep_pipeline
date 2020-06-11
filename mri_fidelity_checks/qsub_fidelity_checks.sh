#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=00:05:00
#PBS -A mnh5174_c_g_sc_default
#PBS -o aci_output
#PBS -e aci_output

# This file encapsulates the fidelity checking process
# This file expects a range of numbers to be passed to it as arguments, specifying which subjects to run the fidelity checks on
# Example usage:
# run_fidelity_checks 1 2 5-9
# or, if none are passed, will run fidelity checks on all subjects

# NOTE: ensure that these paths are not broken when moving things around
# For an explantaion of the terms see README.md

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

####
command -v deactivate >/dev/null 2>&1 && deactivate #exit existing virtual environment if active
command -v module >/dev/null 2>&1 && module unload python #make sure no system python modules are loaded    
[ -n "$torque_modules_root" ] && module use "$torque_modules_root"
[ -n "$fsl_module" ] && module load "$fsl_module"

module load python/3.6.3 #currently hard-coding this

code_dir="code"

[ -z "$loc_bids_root" ] && echo "Environment variable loc_bids_root not set" && exit 1
[ -z "$loc_root" ] && echo "Environment variable loc_root not set" && exit 1
[ -z "$fidelity_json" ] && echo "Environment variable fidelity_json not set" && exit 1

#qsub -v sub="$sub",output_dir="$(pwd)" run_fidelity_checks.sh

# run the fidelity checks

#python ${pipedir}/${code_dir}/test.py ${pipedir}/$fidelity_json $loc_bids_root ${loc_root}/fidelity_checks "$sub"
[[ "$debug_pipeline" -eq 1 ]] && rel_suffix=c #if debug_pipeline is 1, only echo command to log, don't run it
python ${repo_loc}/mri_fidelity_checks/${code_dir}/substatus.py ${loc_root}/fidelity_output_data "$sub" 
rel "python ${pipedir}/mri_fidelity_checks/${code_dir}/feeder.py ${pipedir}/$fidelity_json $loc_bids_root ${loc_root}/fidelity_checks $sub && python ${pipedir}/mri_fidelity_checks/${code_dir}/substatus.py ${loc_root}/fidelity_output_data ${sub} && date \"+%m%d%y@%H:%M\" > $loc_bids_root/sub-$sub/.fidelity.complete" $rel_suffix
