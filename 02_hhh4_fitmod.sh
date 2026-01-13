#!/bin/bash
#SBATCH --time=01:04:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=1G
#SBATCH -o ./Report/output02/output02.%a.out # STDOUT
#SBATCH -e ./Report/error02/error02.%a.out 
#SBATCH --array=1-432  # If k models and J hold out time points this is 1- j*k  J=84, K=15


#Define the number of models being tested

N_models=6

#Load R
module load  R/4.2.3-foss-2022b

# J:1-108 time periods
# K 1:N_models models

# Use modulos to iterate through all task IDs
task_id=$SLURM_ARRAY_TASK_ID
j=$(( (task_id-1)  / N_models + 1 )) # $(( )) does arithmetic evaluation; Bash performs integer division so floor() is default
k=$(( task_id  % N_models  + 1 )) # $(( )) does arithmetic evaluation

# Start time
start_time=$(date +"%Y-%m-%d %H:%M:%S")

# Run your R script with the task-specific J and K
Rscript ./R/02_call_hhh4.R "$j" "$k"

# End time
end_time=$(date +"%Y-%m-%d %H:%M:%S")

# Calculate running time
start_seconds=$(date -d "$start_time" +%s)
end_seconds=$(date -d "$end_time" +%s)
running_time=$((end_seconds - start_seconds))

# Output j, k, model number, and running time to a log file
echo "j=$j, k=$k, model_number=$task_id, running_time=$running_time seconds" >> ./Report/log2.txt
# done
