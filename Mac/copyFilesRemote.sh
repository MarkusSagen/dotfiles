#!/bin/bash

workdir=$(pwd)
relpath=${workdir%/*}
projectname=${workdir##*/}

f=$(git status --porcelain) # && echo $f

allfiles=($(echo $f | tr ' ' "\n"))
uniquefiles=($(printf "%s\n" "${allfiles[@]}" | sort -u | tr '\n' ' '))

{ # try
for fname in "${uniquefiles[@]}"; do
    # Check if has name
    # Dont check all edge cases form procelin, say M, ??, and so on...
    if [[ ${#fname} -ge 3 ]]; then
	echo "Adding --> $fname"
	git add "${fname}"
	scp ${fname} markus.sagen@gpugpu:/home/markus.sagen/${projectname}
    fi
done } || { # except 
# could not copy files
echo "Could not copy files"
} 

