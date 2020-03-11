#!/bin/bash -x
#SBATCH --job-name=c13_pocket
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --export=ALL
#SBATCH --time=12:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jwschwab@ucsc.edu
#SBATCH -A csc116

if [ -n "${SLURM_SUBMIT_DIR}" ]; then
   cd ${SLURM_SUBMIT_DIR}
   module load mesasdk/20.3.1
   export MESA_DIR=${PROJECT}/mesa-r12778
   export OMP_NUM_THREADS=24
fi

# rebuild MESA
./clean
./mk

# run MESA
./rn | tee rn.out

images_to_movie 'png/grid1*.png' movie.mp4
