#!/bin/bash
# test new rel
source "pipeline_functions"

rel c hello this is my comment
rel t sleep 10

# write the output of running the fslmaths to a file
# here, | head needs to be quoted so that the or isn't interpreted within bash
rel -l ~/mylog.txt o "fslmaths | head -5"

