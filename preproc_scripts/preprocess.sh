#!/bin/bash

#This script pulls DICOMs from SLEIC, and then queues jobs to convert DICOMs into NIFTIs, to quality-check NIFTIs using MRIQC, to check for correctness of scan parameters using Austin's scripts, and to preprocess the NIFTIs using fmriprep.

#Set environment variables
source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/cfg_files/neuromap_transfer.cfg
source_root=/gpfs/group/mnh5174/default/NeuroMAP
loc_root=/storage/home/axm6053/NeuroMap_testing #local root directory for project
loc_mrraw_root=${source_root}/MR_Raw #local dicom sync directory
loc_mrproc_root=${loc_root}/MR_Proc #local directory for processed data. NB: If not defined, processed data will be placed inside subject directories in loc_mrraw_root

#Pull raw MRI data from SLEIC
#source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/syncMRCTR_MRRaw

for sub in $(seq -f "%03g" 2000); do 
	#For each subject with raw data...
	if [ -d "${loc_mrraw_root}/$sub" ]; then
		echo $sub
		#If they don't yet have BIDS data, run them through the full pipeline		
		if [ ! -d "${loc_root}/bids/sub-${sub}" ];then
			echo -e "\tsubject doesnt have bids: running full pipeline"
			heudiconv=$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrraw_root=$loc_mrraw_root,repo_loc=$PWD heudiconv.sh)
			#qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root mriqc.sh
			#qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh	

			# stagger dependencies to avoid race conditions
			if [ -z "$fidelityjid" ]; then # first fidelity job
				fidelityjid=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			else # another fidelity job was queued first: wait for that one to finish
				fidelityjid=$(qsub -W depend=after:$fidelityjid,afterok:$heudiconv -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			fi
			qsub -W depend=afterok:$fidelityjid -v sub=$sub,fidelity_dir=${loc_root}/fidelity_output_data,repo_loc=$PWD mri_fidelity_checks/run_fidelity_viewer.pbs
		else
			echo -e "\tsubject has bids: running partially"
			#Run MRIQC, if not already run
			if [ ! -d "${loc_root}/mriqc_IQMs/sub-${sub}" ];then
				#qsub -v sub=$sub,loc_root=$loc_root mriqc.sh
				echo -e "\twould have run mriqc"
			fi
			#Run fmriprep, if not already run
			if [ ! -d "${loc_mrproc_root}/fmriprep/sub-${sub}" ]; then
				#qsub -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh	
				echo -e "\twould have run fmriprep"
			fi

			#Run fidelity checks case not already run
			# stagger dependencies to avoid race conditions
			if [ -z "$fidelityjid" ]; then # first fidelity job
				fidelityjid=$(qsub -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			else # another fidelity job was queued first: wait for that one to finish
				fidelityjid=$(qsub -W depend=after:$fidelityjid -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			fi
			qsub -W depend=afterok:$fidelityjid -v sub=$sub,fidelity_dir=${loc_root}/fidelity_output_data,repo_loc=$PWD mri_fidelity_checks/run_fidelity_viewer.pbs
		fi
	fi
done

#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_neuromap_transfer.cfg
#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_syncMRCTR_MRRaw
#repo_loc=/gpfs/group/mnh5174/default/Daniel/data_automation/mri_fidelity_checks
