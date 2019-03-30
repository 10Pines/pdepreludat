#!/bin/bash

git ls-files the-template |
    while read FILE; do 
        echo "{-# START_FILE ${FILE##the-template/} #-}"; 
        cat $FILE; 
        echo; 
    done | 
        sed 's/the-template/{{name}}/g' |
        sed "/REMOVE THIS LINE/ d" |
        sed 's/^# UNCOMMENT ME \(.*\)$/\1/' |
        sed "s/REPLACE WITH CURRENT COMMIT/$(git log -1 --pretty=format:'%H')/g"
