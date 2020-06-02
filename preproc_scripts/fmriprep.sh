#!/bin/bash

#PBS -l nodes=1:ppn=8
#PBS -l walltime=24:00:00
#PBS -o aci_output
#PBS -e aci_output
#PBS -A open
#PBS -N fmriprep


#--use-plugin /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts
#source /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts/fmriprep_wrapper.sh ${loc_root}/bids/ ${loc_mrproc_root}/ participant --participant_label $sub --nthreads 8 -w ${loc_root}/fmriprep_tempfiles && date "+%m%d%y@%H:%M" > $loc_mrproc_root/fmriprep/sub-$sub/.complete
mkdir $loc_mrproc_root/fmriprep/sub-$sub/
date "+%m%d%y@%H:%M" > $loc_mrproc_root/fmriprep/sub-$sub/.complete

#add --low-mem?
#add --mem-mb?
