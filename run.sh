#!/bin/bash

#PBS -l ncpus=128
#PBS -M mkvakic@srce.hr
#PBS -m bae

cd ${PBS_O_WORKDIR}
apptainer run image.sif run.R
