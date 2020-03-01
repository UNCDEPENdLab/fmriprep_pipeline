#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=2:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -j oe


source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash

heudiconv -d /gpfs/group/mnh5174/default/NeuroMAP/MR_Raw/{subject}/*/*.dcm -s $sub -f /gpfs/group/mnh5174/default/NeuroMAP/preproc_scripts/nmap_heuristic.py -c dcm2niix -o /gpfs/group/mnh5174/default/NeuroMAP/bids -b


