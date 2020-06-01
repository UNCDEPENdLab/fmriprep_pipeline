#!/bin/bash
# NOTE: this command should be run from its directory, that is, within s4_mri/preproc_scripts. It currently makes the assumption that certain files are within the working directory

#This script pulls DICOMs from SLEIC, and then queues jobs to convert DICOMs into NIFTIs, to quality-check NIFTIs using MRIQC, to check for correctness of scan parameters using Austin's scripts, and to preprocess the NIFTIs using fmriprep.

#Set environment variables
source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/cfg_files/neuromap_transfer.cfg
source_root=/gpfs/group/mnh5174/default/NeuroMAP
loc_root=/storage/home/axm6053/NeuroMap_testing #local root directory for project
loc_mrraw_root=${source_root}/MR_Raw #local dicom sync directory
loc_mrproc_root=${loc_root}/MR_Proc #local directory for processed data. NB: If not defined, processed data will be placed inside subject directories in loc_mrraw_root

#Pull raw MRI data from SLEIC
#source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/syncMRCTR_MRRaw

emailRecipients="axm6053@psu.edu" # the email(s) to which the pipeline status report will be sent. NOTE: delimit multiple emails with a space!
expectationFile=".subs_jobs.log" # temporary file. tracks which subjects should be processed
outputDir="aci_output" # the directory where qsub output files go. this needs to match the "-o" and "-e" in every pbs script

echo > $expectationFile
allJobIds=""
for sub in $(seq -f "%03g" 8); do 
	#For each subject with raw data...
	if [ -d "${loc_mrraw_root}/$sub" ]; then
		echo $sub
		#If they don't yet have BIDS data, run them through the full pipeline		
		if [ ! -d "${loc_root}/bids/sub-${sub}" ];then
			echo -e "\tsubject doesnt have bids: running full pipeline"
			# qsub returns the name of the job scheduled: [0-9]*.torque01.[a-z.]*.edu
			heudiconv=$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrraw_root=$loc_mrraw_root,repo_loc=$PWD heudiconv.sh)
			mriqc=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root mriqc.sh)
			fmriprep=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh)

			# stagger dependencies to avoid race conditions
			if [ -z "$fidelityjid" ]; then # first fidelity job
				fidelityjid=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			else # another fidelity job was queued first: wait for that one to finish
				fidelityjid=$(qsub -W depend=after:$fidelityjid,afterok:$heudiconv -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			fi

			allJobIds=${allJobIds},after:${heudiconv},after:${fidelityjid},after:${mriqc},after:${fmriprep} # construct dependency argument
			currentIds=${heudiconv},${fidelityjid},${mriqc},${fmriprep} # construct list of job ids associated with the current subject
		else
			echo -e "\tsubject has bids: running partially"
			#Run MRIQC, if not already run
			if [ ! -d "${loc_root}/mriqc_IQMs/sub-${sub}" ]; then
				mriqc=,after:$(qsub -v sub=$sub,loc_root=$loc_root mriqc.sh)
				echo -e "\twould have run mriqc"
			fi
			#Run fmriprep, if not already run
			if [ ! -d "${loc_mrproc_root}/fmriprep/sub-${sub}" ]; then
				fmriprep=,after:$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh)
				echo -e "\twould have run fmriprep"
			fi

			#Run fidelity checks case not already run
			# stagger dependencies to avoid race conditions
			if [ -z "$fidelityjid" ]; then # first fidelity job
				fidelityjid=$(qsub -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			else # another fidelity job was queued first: wait for that one to finish
				fidelityjid=$(qsub -W depend=after:$fidelityjid -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			fi

			allJobIds=${allJobIds},after:${fidelityjid}${mriqc}${fmriprep}
			currentIds=${fidelityjid}${mriqc}${fmriprep}
			currentIds=$(echo $currentIds | sed 's/,after:/,/g') # remove the "after" dependency specification
		fi

		# build expectations so status scripts have enough information
		currentIds=$(echo $currentIds | sed 's/\.torque01\.[a-z0-9\.]*edu//g') # isolate the job id number
		echo -e "$sub\t$currentIds" >> $expectationFile
	fi
done

allJobIds=$(echo $allJobIds | sed 's/\.torque01\.[a-z0-9\.]*edu//g' | sed 's/^,*//') # isolate the job id number, while preserving the "after" dependency specification
qsub -W depend=$allJobIds -d $PWD -v outputDir=$outputDir,expectationFile=$expectationFile,loc_root=${loc_root},TOEMAIL="$emailRecipients" report.sh

#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_neuromap_transfer.cfg
#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_syncMRCTR_MRRaw
#repo_loc=/gpfs/group/mnh5174/default/Daniel/data_automation/mri_fidelity_checks
