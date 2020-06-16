#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=0:05:00
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -o aci_output
#PBS -e aci_output
#PBS -N report

# NOTE: the mail command used here, when run as a job submitted with qsub, only works when run with the "-v" (verbose) option.

####
#setup compute environment
[ -z "$pipedir" ] && pipedir="$PWD"
source "${pipedir}/pipeline_functions"

#in principle, we could pass all environment variables in with -v
[ -z "$env_file" ] && env_file="${pipedir}/compute_environment.cfg" #set default
[ ! -r "$env_file" ]  && echo "Cannot access compute config file ${env_file}" && exit 1
source "$env_file" #setup relevant variables for this processing environment

[[ -n "${study_cfg}" && -r "${study_cfg}" ]] && source "${study_cfg}" #source a study config file if passed as environment variable

[[ "$debug_pipeline" -eq 1 ]] && rel_suffix=c #if debug_pipeline is 1, only echo command to log, don't run it
reportName=pipeline_report-$(date +%m_%d_%y-%H:%M).log
python pipeline_status/pipeline_status.py $expectation_file $loc_root $aci_output_dir $loc_yaml | tee ${loc_root}/$reportName | mail -v -S smtp=smtp.psu.edu -s "Pipeline Report" -r DEPENdNeuroMapAutomation@psu.edu "$qsub_email"
