#!/bin/bash

set -e

function envpass() {
    npar=$#
    [ $npar -eq 0 ] && return #nothing to do
    env_names=$@
    local vpass_string=
    for inputName in $env_names; do
	#check whether value contains an equals sign, in which case treat it as a verbatim key-value pair
	if [[ "$inputName" == *"="* ]]; then
	    vpass_string="$vpass_string$inputName,"
	else     
	    inputVal=${!inputName}
	    [ -z "${inputVal}" ] && echo "$inputName is empty" # && exit 1
	    vpass_string="$vpass_string$inputName=$inputVal,"
	fi
    done
    vpass_string=$( echo $vpass_string | sed 's/.$//' ) #trim trailing comma
    echo $vpass_string
}

env1=sdflkj
env2=1
hello=11
heck=55a

echo $( envpass env1 env2 hello test=$heck)

#looks for qsub_email and qsub_allocation in environment
#accepts positional parameters thereafter that all get converted to -l arguments
function build_qsub_string() {
    local qsub_string=
    if [ -n "$qsub_allocation" ]; then
	qsub_string="${qsub_string} -A $qsub_allocation"
    else
	qsub_string="${qsub_string} -A open" #default to open queue if not specified
    fi
    
    if [ -n "$qsub_email" ]; then qsub_string="${qsub_string} -M $qsub_email"; fi

    local nargs=$#
    if [ $nargs -gt 0 ]; then
	local env_names=$@
	for inputName in $env_names; do
	    if [[ "$inputName" == *"="* ]]; then
		qsub_string="$qsub_string -l $inputName"
	    else
		inputVal=${!inputName}
		if [ -n "${inputVal}" ]; then
		    qsub_string="$qsub_string -l $inputVal"
		fi
	    fi
	done
    fi
    echo $qsub_string
}

qsub_allocation=mmm
qsub_email=michael@gmail
wall1=24:00:00
nodes="nodes=1:ppn=1"

echo $( build_qsub_string wall1 nodes walltime=24:00:00 )
