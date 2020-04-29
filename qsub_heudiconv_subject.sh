#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=${heudiconv_walltime}
#PBS -A ${qsub_allocation}
#PBS -M ${qsub_email}
#PBS -j oe

#temp cache while testing environment passthrough
# -A mnh5174_c_g_sc_default
# -M dpp5430@psu.edu
# -l walltime=2:00:00

#source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash
source $( dirname "$0" )/environment.cfg #need this to be dynamic, or assume things have already been passed in

[ -z "${heudiconv_location}" ] && echo "No heudiconv_location variable set." && exit 1
[ -z "${heudiconv_heuristic}" ] && echo "No heudiconv_heurisic variable set." && exit 1
[ -z "${loc_mrraw_root}" ] && echo "No loc_mrraw_root variable set." && exit 1
[ -z "${loc_root}" ] && echo "No loc_root variable set." && exit 1

"${heudiconv_location}" -d ${loc_mrraw_root}/{subject}/*/*.dcm -s $sub -f ${heudiconv_heuristic} -c dcm2niix -o ${loc_root}/bids -b


