#!/bin/bash

#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -j oe


source /gpfs/group/mnh5174/default/NeuroMAP/preproc_scripts/fmriprep_wrapper.sh /gpfs/group/mnh5174/default/NeuroMAP/bids/ /gpfs/group/mnh5174/default/NeuroMAP/MR_Proc/ participant --participant_label $sub --nthreads 8 -w /gpfs/group/mnh5174/default/NeuroMAP/fmriprep_tempfiles

#add --low-mem?
#add --mem-mb?
