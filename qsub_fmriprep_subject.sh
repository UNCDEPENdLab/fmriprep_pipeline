#!/bin/bash
set -e
#Default PBS requests if not overridden on qsub command line
#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -j oe

cd $PBS_O_WORKDIR

./fmriprep_wrapper ${loc_root}/bids/ ${loc_mrproc_root}/ participant --participant_label $sub --nthreads 8 -w ${loc_root}/fmriprep_tempfiles

#add --low-mem?
#add --mem-mb?
