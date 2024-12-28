#!/bin/bash
#SBATCH --time=168:00:00
#SBATCH --account=def-wanghao
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --mem=0

module restore my_modules
Rscript ~/scratch/TIME_SERIES_GENERATOR_TMP_1HR_GAP_600.R