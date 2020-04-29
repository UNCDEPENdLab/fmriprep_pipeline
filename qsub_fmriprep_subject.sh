#!/bin/bash

#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -j oe

cd $PBS_O_WORKDIR

source /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts/fmriprep_wrapper.sh ${loc_root}/bids/ ${loc_mrproc_root}/ participant --participant_label $sub --nthreads 8 -w ${loc_root}/fmriprep_tempfiles #--use-plugin /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts

#add --low-mem?
#add --mem-mb?
