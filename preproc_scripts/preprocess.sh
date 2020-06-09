#!/bin/bash

# This script pulls DICOMs from SLEIC, and then queues jobs to convert DICOMs
# into NIFTIs, to quality-check NIFTIs using MRIQC, to check for correctness of
# scan parameters using Austin's scripts, and to preprocess the NIFTIs using
# fmriprep.

# NOTE: this command should be run from its directory, that is, within s4_mri/preproc_scripts. It currently makes the assumption that certain files are within the working directory
# NOTE: the location of the pipeline input and output (loc_root, fmriprep output, mriqc output etc..), if changed, should be updated in the following locations:
#	complete-placement.yaml
#	each qsub script that leaves a .complete file after completion (e.g. fmirprep.sh, mriqc.sh)
#	the "if" statements in this script that check to see if the .complete file exists before scheduling a job

#Set environment variables
source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/cfg_files/neuromap_transfer.cfg

#Pull raw MRI data from SLEIC
source /gpfs/group/mnh5174/default/lab_resources/fmri_processing_scripts/autopreproc/syncMRCTR_MRRaw

emailRecipients="axm6053@psu.edu" # the email(s) to which the pipeline status report will be sent. NOTE: delimit multiple emails with a space!
expectationFile=".subs_jobs.log" # temporary file. tracks which subjects should be processed
outputDir="aci_output" # the directory where qsub output files go. this needs to match the "-o" and "-e" in every pbs script
locYaml="pipeline_status/complete-placement.yaml"

echo > $expectationFile
allJobIds=""
dependCom="afterany"
for sub in $(seq -f "%03g" 2000); do 
	#For each subject with raw data...
	if [ -d "${loc_mrraw_root}/$sub" ]; then
		echo $sub
		#If they don't yet have BIDS data, run them through the full pipeline		
		if [ ! -e "${loc_root}/bids/sub-${sub}/.heudiconv.complete" ]; then
			echo -e "\trunning full pipeline"

			# qsub returns the name of the job scheduled: [0-9]*.torque01.[a-z.]*.edu
			heudiconv=$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrraw_root=$loc_mrraw_root,repo_loc=$PWD heudiconv.sh)
			mriqc=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root mriqc.sh)
			fmriprep=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh)

			# stagger dependencies to avoid race conditions
			if [ -z "$fidelityjid" ]; then # first fidelity job
				fidelityjid=$(qsub -W depend=afterok:$heudiconv -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			else # another fidelity job was queued first: wait for that one to finish
				fidelityjid=$(qsub -W depend=afterany:$fidelityjid,afterok:$heudiconv -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
			fi

			allJobIds=${allJobIds},${dependCom}:${heudiconv},${dependCom}:${fidelityjid},${dependCom}:${mriqc},${dependCom}:${fmriprep} # construct dependency argument
			currentIds=${heudiconv},${fidelityjid},${mriqc},${fmriprep} # construct list of job ids associated with the current subject
		else
			# clear out after each iteration
			fmriprep=""
			mriqc=""

			#Run MRIQC, if not already run
			if [ ! -e "${loc_root}/bids/sub-${sub}/.mriqc.complete" ]; then
				mriqc=,${dependCom}:$(qsub -v sub=$sub,loc_root=$loc_root mriqc.sh)
				echo -e "\trunning mriqc"
			fi

			#Run fmriprep, if not already run
			if [ ! -e "${loc_root}/bids/sub-${sub}/.fmriprep.complete" ]; then
				fmriprep=,${dependCom}:$(qsub -v sub=$sub,loc_root=$loc_root,loc_mrproc_root=$loc_mrproc_root fmriprep.sh)
				echo -e "\trunning fmriprep"
			fi

			#Run fidelity checks case not already run
			if [ ! -e "${loc_root}/bids/sub-${sub}/.fidelity.complete" ]; then
				echo -e "\trunning fidelity checks"
				# stagger dependencies to avoid race conditions
				if [ -z "$fidelityjid" ]; then # first fidelity job
					fidelityjid=,${dependCom}:$(qsub -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
				else # another fidelity job was queued first: wait for that one to finish
					fidelityjid=,${dependCom}:$(qsub -W depend=afterany:$fidelityjid -v sub=$sub,loc_root=${loc_root},repo_loc=$PWD mri_fidelity_checks/run_fidelity_checks.pbs)
				fi
				# only add on fidelityjid if it was changed to avoid duplicates
				allJobIds=${allJobIds}${fidelityjid}
				currentIds=${fidelityjid}
			fi

			allJobIds=${allJobIds}${mriqc}${fmriprep}
			currentIds=${currentIds}${mriqc}${fmriprep}
			currentIds=$(echo $currentIds | sed 's/,afterany:/,/g' | sed 's/^,*//') # remove the "after" dependency specification
		fi

		# build expectations so status scripts have enough information
		currentIds=$(echo $currentIds | sed 's/\.torque01\.[a-z0-9\.]*edu//g') # isolate the job id number
		echo -e "$sub\t$currentIds" >> $expectationFile
	fi
done

allJobIds=$(echo $allJobIds | sed 's/\.torque01\.[a-z0-9\.]*edu//g' | sed 's/^,*//') # isolate the job id number, while preserving the "after" dependency specification
if [ ! -z $allJobIds ]; then
	qsub -W depend=$allJobIds -d $PWD -v outputDir=$outputDir,expectationFile=$expectationFile,loc_root=${loc_root},TOEMAIL="$emailRecipients",locYaml=$locYaml report.sh
else
	echo "All subjects that have raw DICOM data have been fully processed: not submitting any jobs to qsub"
fi

#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_neuromap_transfer.cfg
#source /gpfs/group/mnh5174/default/Daniel/OLD_preproc_NeuroMAP/SANDBOX_syncMRCTR_MRRaw
#repo_loc=/gpfs/group/mnh5174/default/Daniel/data_automation/mri_fidelity_checks
