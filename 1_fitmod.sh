#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=daniel.weinberger@yale.edu
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=10G
#SBATCH -o ./Report/output.%a.out # STDOUT
#SBATCH --array=1-84   # If k models and J hold out time points this is 1- j*k  J=84, K=15


#Define the number of models being tested

N_models=1

#Load R
module load R/4.2.0-foss-2020b

# J:1-108 time periods
# K 1:N_models models

# Use modulos to iterate through all task IDs
task_id=$SLURM_ARRAY_TASK_ID
j=$(( (task_id-1)  / N_models + 1 )) # $(( )) does arithmetic evaluation; Bash performs integer division so floor() is default
k=$(( task_id  % N_models  + 1 )) # $(( )) does arithmetic evaluation

    # Run your R script with the task-specific J and K
Rscript 1_fitmod.R "$j" "$k"

# done
