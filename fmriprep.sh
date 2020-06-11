#!/bin/bash

#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
#PBS -o aci_output
#PBS -e aci_output
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -N fmriprep


#--use-plugin /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts
source /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts/fmriprep_wrapper.sh ${loc_root}/bids/ ${loc_mrproc_root}/ participant --participant_label $sub --nthreads 8 -w ${loc_root}/fmriprep_tempfiles && date "+%m%d%y@%H:%M" > $loc_root/bids/sub-$sub/.fmriprep.complete

#add --low-mem?
#add --mem-mb?
