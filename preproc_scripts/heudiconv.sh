#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=2:00:00
#PBS -A open
#PBS -M dpp5430@psu.edu
#PBS -j oe
#PBS -N heudiconv


source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash

#heudiconv -d ${loc_mrraw_root}/{subject}/*/*.dcm -s $sub -f ${repo_loc}/nmap_heuristic.py -c dcm2niix -o ${loc_root}/bids -b
/gpfs/group/mnh5174/default/lab_resources/lab_python3/bin/heudiconv -d ${loc_mrraw_root}/{subject}/*/*.dcm -s $sub -f ${repo_loc}/nmap_heuristic.py -c dcm2niix -o ${loc_root}/bids -b && date "+%m %d %y @ %H:%M" > ${loc_root}/bids/sub-${sub}/.heudiconv.complete


