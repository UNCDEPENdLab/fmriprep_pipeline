#!/bin/bash

#This scripts queues jobs to convert DICOMs into NIFTIs, to quality-check NIFTIs using MRIQC, and to preprocess the NIFTIs using fmriprep.
  
for sub in $(seq -f "%03g" 2000); do 
	if [ -d "/gpfs/group/mnh5174/default/NeuroMAP/MR_Raw/$sub" ] && [ ! -d "/gpfs/group/mnh5174/default/NeuroMAP/MR_Proc/fmriprep/$sub" ]; then
		heudiconv=$(qsub -v sub=$sub heudiconv.sh)
		qsub -W depend=afterok:$heudiconv -v sub=$sub mriqc.sh
		qsub -W depend=afterok:$heudiconv -v sub=$sub fmriprep.sh
		#qsub -W depend=afterok:$heudiconv -v sub=$sub fidelity_check.sh
	fi
done

