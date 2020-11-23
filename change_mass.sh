#!/bin/bash

for f in inlist* rn *.sh
do
    sed -i 's/7\.5/8\.0/g' $f
done
