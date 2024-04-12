# Notes:

The code in this repository is intended to be run on an HPC. 

## Set up

1) In an interactive session, Install packages found in R/load.R, particularly INLA and scoringutils. On the shell, run:

> salloc

> module load R/4.2.0-foss-2020b
 
> R

remotes::install_version("INLA", version="23.04.24",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

####choose option 3 to say don't update any packages
> 3

> library(INLA)
>

> options(timeout=300)

> inla.binary.install()
>
> #select option 2 to work on the Yale HPC

>2

> library(INLA)

> install.packages('scoringutils')

if there are other packages that need to be installed, install them here as well.

2) if 1_fitmod.sh is created or modified on a Windows machine, you will get an error. In the terminal on the cluster, run:
> dos2unix mod1.sh

3) Run the program by opening the shell, change directory to the directory containing the .sh file using 
> cd XX

4) run 1_fitmod.sh 

5) more models can be added under0_specify_models.R. Note if you want to run a type 4 spatial model, the formula should be called modXX_type4, where the XX is replace by tehe number for the new model

NOTES: if you add more models to 0_specify_models.R, you need to modify the .sh file to reflect the new range of indices AND the total number of models. You can test a subset of the models by specifying the corresponding indices on the SBATCh line at the top. However, N_mod should correspond to the TOTAL number of models that have been evaluated not just those in the current array call

When running INLA on Linux, use the following. Without this, the model unpredictably fails:
library(INLA)
inla.setOption(mkl=TRUE)

IMPORTANT: on set X to be the number of cores requested from the cluster (e.g., 8): inla(..., num.threads=8) 
