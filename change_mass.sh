#!/bin/bash

for f in inlist* rn *.sh
do
    sed -i 's/10\.0/8\.0/g' $f
done
