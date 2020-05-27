#!/bin/bash

#PBS -l nodes=1:ppn=1
#PBS -l walltime=0:00:01
#PBS -A open
#PBS -j oe
#PBS -N report

reportName=pipeline_report-$(date +%m_%d_%y-%H:%M).log
python pipeline_status.py subs.log $loc_root > $reportName
mail -S smtp=smtp.psu.edu -s "Pipeline Report" -r dependpipeline@psu.edu $TOEMAIL < $reportName
