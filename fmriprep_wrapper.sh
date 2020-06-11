#!/bin/bash

set -e
if [ $# -eq 0 ]; then
    echo "fmriprep_wrapper loads the proper python environment and executes fmriprep. It expects a full call to fmriprep."
    echo "Example: fmriprep_wrapper --n_cpus 8 --verbose --output-space template --write-graph --participant_label 040 --use-aroma -w work bids output participant"
    exit 1
fi

LANG=en_US.UTF-8
export LANG

command -v deactivate >/dev/null 2>&1 && deactivate #exit existing virtual environment if active
#module unload python #make sure no system python modules are loaded
#module use /gpfs/group/mnh5174/default/sw/modules
#module load afni/19.0.26
#module load fsl/6.0.1
#module load ants/2.2.0
#module load freesurfer/stable-pub-v6.0.0
#export SUBJECTS_DIR="$G/freesurfer_subjects"

source /gpfs/group/mnh5174/default/lab_resources/ni_path.bash


#activate fmriprep python environment
source /gpfs/group/mnh5174/default/lab_resources/fmriprep_python/bin/activate

fmriprep "$@" #pass all arguments forward


