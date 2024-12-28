#!/bin/bash
#SBATCH --time=168:00:00
#SBATCH --account=def-wanghao
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=64
#SBATCH --mem=249G

module restore my_modules
Rscript ~/scratch/100_Length_TS_Generators/TIME_SERIES_GENERATOR_100_TMP_1HR_GAP.R