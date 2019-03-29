#!/bin/bash

find the-template -type f -not -path 'the-template/.stack-work/**' |
    while read FILE; do 
        echo "{-# START_FILE ${FILE##the-template/} #-}"; 
        cat $FILE; 
        echo; 
    done | 
        sed 's/the-template/{{name}}/g'
