#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=2:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -j oe


source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash

heudiconv -d ${loc_mrraw_root}/{subject}/*/*.dcm -s $sub -f /gpfs/group/mnh5174/default/Daniel/s4_mri/preproc_scripts/nmap_heuristic.py -c dcm2niix -o ${loc_root}/bids -b


