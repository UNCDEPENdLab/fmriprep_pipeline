#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=0:05:00
#PBS -A open
#PBS -j o aci_output
#PBS -j e aci_output
#PBS -N report

reportName=pipeline_report-$(date +%m_%d_%y-%H:%M).log
python pipeline_status.py subs.log $loc_root | tee ${loc_root}/$reportName | mail -v -S smtp=smtp.psu.edu -s "Pipeline Report" -r DEPENdNeuroMapAutomation@psu.edu $TOEMAIL
