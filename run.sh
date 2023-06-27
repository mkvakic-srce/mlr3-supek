#!/bin/bash

#PBS -l ncpus=128

cd ${PBS_O_WORKDIR}
apptainer run image.sif run.R
