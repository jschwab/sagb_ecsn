#!/bin/bash

#SBATCH --job-name=sagb_ecsn
#SBATCH --partition=cpuq
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --export=ALL
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jwschwab@ucsc.edu

module load mesasdk/20.3.1
export MESA_DIR=${DATA_DIR}/mesa-r12778

export OMP_NUM_THREADS=36

./clean
./mk

rm -rf png

./rn | tee rn.out

images_to_movie 'png/grid1*.png' sagb_ecsn.mp4

