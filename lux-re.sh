#!/bin/bash

#SBATCH --job-name=sagb_ecsn
#SBATCH --partition=cpuq
#SBATCH --qos=cpuq
#SBATCH --account=cpuq
#SBATCH --requeue
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --export=ALL
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jwschwab@ucsc.edu

module load mesasdk/20.12.1
export MESA_DIR=${DATA_DIR}/mesa-r15140

export OMP_NUM_THREADS=36

./re | tee re.out

images_to_movie 'png/grid1*.png' 8.0M_sagb_ecsn_TPs.mp4

