#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --account=def-wanghao
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=64
#SBATCH --mem=249G

module restore my_modules
Rscript ~/scratch/TIME_SERIES_GENERATOR_DEW_1HR_GAP.R