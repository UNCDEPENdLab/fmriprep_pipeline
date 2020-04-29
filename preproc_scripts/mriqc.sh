#!/bin/bash

#PBS -l nodes=1:ppn=8
#PBS -l walltime=12:00:00
#PBS -A mnh5174_c_g_sc_default
#PBS -M dpp5430@psu.edu
#PBS -j oe

#load in lab programs, including MRIQC
#I would tend to avoid this since it creates a bit of a blackbox and may setup things you don't want.
#I think mriqc needs ants, fsl, afni
#source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash

#https://stackoverflow.com/questions/51026315/how-to-solve-unicodedecodeerror-in-python-3-6/51027262#51027262
LANG=en_US.UTF-8
export LANG

command -v deactivate >/dev/null 2>&1 && deactivate #exit default virtual environment if active
module unload python #make sure no system python modules are loaded
#these appear to be the immediate dependencies of mriqc
module use /gpfs/group/mnh5174/default/sw/modules
module load afni/19.0.26
module load fsl/5.0.11 #Using version 6 of FSL creates problems
module load ants/2.2.0

#load version 3 of Python (since the default is version 2, which doesn't work w/MRIQC)
source /gpfs/group/mnh5174/default/lab_resources/lab_python3/bin/activate

mriqc ${loc_root}/bids/ ${loc_root}/mriqc_IQMs/ participant --participant-label $sub -w ${loc_root}/mriqc_tempfiles
