#!/bin/bash
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=10G
#SBATCH -o ./Report/output03/output03.%a.out # STDOUT
#SBATCH -e ./Report/error03/error03.%a.out 
#SBATCH --array=1-8064   # If k models and J hold out time points this is 1- j*k  J=84 times, k=112 districts

#Define the number of models being tested

N_models=1
K=112
J=72


#Load R
module load  R/4.2.3-foss-2022b

# J:1-108 time periods
# K 1:N_districts 

# Use modulos to iterate through all task IDs
# Use modulos to calculate i, j, and k
task_id=$SLURM_ARRAY_TASK_ID
i=$(( (task_id - 1) / (J * K) + 1 )) # Calculate i based on the number of models, J, and K
remainder=$(( (task_id - 1) % (J * K) )) # Calculate the remainder to find j and k
j=$(( remainder / K + 1 )) # Calculate j based on the remainder and K
k=$(( remainder % K + 1 )) # Calculate k based on the remainder

# Start time
start_time=$(date +"%Y-%m-%d %H:%M:%S")

    # Run your R script with the task-specific J and K
Rscript ./R/03_call_lag_district_pca.R "$j" "$k" "$i"

# End time
end_time=$(date +"%Y-%m-%d %H:%M:%S")

# Calculate running time
start_seconds=$(date -d "$start_time" +%s)
end_seconds=$(date -d "$end_time" +%s)
running_time=$((end_seconds - start_seconds))

# Output j, k, model number, and running time to a log file
echo "j=$j, k=$k, i=$i, district_number=$task_id, running_time=$running_time seconds" >> ./Report/log3.txt
# done
