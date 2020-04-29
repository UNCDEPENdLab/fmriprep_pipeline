#!/bin/bash

#This script pulls DICOMs from SLEIC, and then queues jobs to convert DICOMs into NIFTIs, to quality-check NIFTIs using MRIQC, to check for correctness of scan parameters using Austin's scripts, and to preprocess the NIFTIs using fmriprep.

#Set environment variables
source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/cfg_files/neuromap_transfer.cfg

#Pull raw MRI data from SLEIC
source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/syncMRCTR_MRRaw

for sub in $(seq -f "%03g" 2000); do 
	#For each subject with raw data...
	if [ -d "${loc_mrraw_root}/$sub" ]; then
		#If they don't yet have BIDS data, run them through the full pipeline		
		if [ ! -d "${loc_root}/bids/sub-${sub}" ];then
			heudiconv=$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrraw_root=$loc_mrraw_root heudiconv.sh)
			qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root mriqc.sh
			qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh	
			qsub -W depend=afterok:$heudiconv -v sub=$sub,output_dir=${loc_root}/fidelity_checks,repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs
		else
			#Run MRIQC, if not already run
			if [ ! -d "${loc_root}/mriqc_IQMs/sub-${sub}" ];then
				qsub -v sub=$sub,loc_root=$loc_root mriqc.sh
			fi
			#Run fmriprep, if not already run
			if [ ! -d "${loc_mrproc_root}/fmriprep/sub-${sub}" ]; then
				qsub -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh	
			fi
			#Run fidelity cheks case not already run
			qsub -v sub=$sub,output_dir=${loc_root}/fidelity_checks,repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs
		fi
	fi
done

#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_neuromap_transfer.cfg
#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_syncMRCTR_MRRaw
#repo_loc=/gpfs/group/mnh5174/default/Daniel/data_automation/mri_fidelity_checks
