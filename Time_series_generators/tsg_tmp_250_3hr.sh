#!/bin/bash
#SBATCH --time=168:00:00
#SBATCH --account=def-wanghao
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=64
#SBATCH --mem=249G

module restore my_modules
Rscript ~/scratch/250_Length_TS_Generators/TIME_SERIES_GENERATOR_250_TMP_3HR_GAP.R