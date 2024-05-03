# Forecasting dengue in the Mekong Delta region

This code fits and evaluate models for forecasting the incidence of dengue cases 2 months ahead at the district level in the Mekong Delta region of Vietnam. There are three main types of models that are fit

1.  Bayesian spatiotemporal models. These models use lags of meteorological variables (e.g., temperature, precipitation), sociodemographic variables (urbanization, income, etc), and lags of observed cases. Models include lags of observed incidence and also include a spatiotemporal random effects with different structures (e.g., uncorrelated AR(1) terms by district, spatial random effects, spatiotemporal random effect).

2.  [hhh4 models](https://cran.r-project.org/web/packages/surveillance/vignettes/hhh4_spacetime.pdf). These semi-mechanistic spatiotemporal models account for baseline variations in incidence, autoregression, and spatial spread. Each of these components can be modeled as a function of covariates

3.  An experimental approach that models the district from each time series independently and uses the lagged cases from the district of interest and all of the other districts. A large matrix of these lagged cases is created, and Y-aware principal components analysis is performed. The top N principal components that explain 85% of the variation is used in the model along with harmonic terms and an AR(1) random intercept

Several variations of each of these models with different covariates and random effects structures is evaluated. Each of these models is evaluated using time series cross validation by moving forward the end of the training period by 1 time unit at a time. Forecasting performance at 2 months is evaluated using continuous ranked probability score (CRPS) and Brier scores. We create an ensemble based on the CRPS scores.

## Getting started.

The code in this repository is intended to be run on an HPC.

1.  In an interactive session, Install packages found in R/load.R, particularly INLA and scoringutils. On the shell, run:

> salloc

> module load R/4.2.0-foss-2020b

> R

remotes::install_version("INLA", version="23.04.24",repos=c(getOption("repos"),INLA="<https://inla.r-inla-download.org/R/testing>"), dep=TRUE)

####choose option 3 to say don't update any packages \> 3

> library(INLA)

> options(timeout=300)

> inla.binary.install()
>
> #select option 2 to work on the Yale HPC

> 2

> library(INLA)

> install.packages('scoringutils')

if there are other packages that need to be installed, install them here as well.

2)  if the .sh scripts are created or modified on a Windows machine, you will get an error. In the terminal on the cluster, run: \> dos2unix mod1.sh

    When running INLA on Linux, use the following. Without this, the model unpredictably fails: library(INLA) inla.setOption(mkl=TRUE)

    IMPORTANT: on set X to be the number of cores requested from the cluster (e.g., 8): inla(..., num.threads=8)

## Modifying the code

2)  More Bayesian spatiotemporal models can be defined under99_define_inla_spacetime_mods.R. More hhh4 models can be defined in fun_hhh4.R

3)  Specify which inla spacetime models and hhh4 models you want to run in 01_call_inla_spacetime.R, 02_call_hhh4.R, and 03_call_lag_district_pca.R

4)  Modify the \*.sh scripts to reflect the number of models and time poitns being tested

    #SBATCH \--array=1-504 \# If k models and J hold out time points this should be 1-J\*K

    N_models = \_\_ #this should be the number of models that were specified in the call\_\*.R files

## Running the models

1.  Each of the 3 model types needs to be called separately. On the command prompt, change directory to the directory containing the .sh file using \> cd XX

<!-- -->

2)  \>salloc

    \> sbatch 1_inlaspacetime.sh

    \> sbatch 1_hhh4.sh

    \> sbatch 1_lag_district_pca.sh
