# tunisia-sharks-satelitte

This repository goes with paper ....

## general instruction
1. download or clone this reo and open the R project to then work on it from your local
2. in the notebooks folder run scripts in sequence the first time as some of them depends on datset generated in previous script


## folder descriptions
- *notebooks:* scripts and notes to run analysis
- *data:* all data for the analyses are stored here - including the ones generated during the analyses
- *figures:* folders to store figures
- *shapefiles:* shapefiles for analyses

## notebooks/scripts descriptions
description of scripts inside notebook folder

1. *prepare_sar_data.rmd:* script to create sar dataset
2. *prepare_elasmo_landings.rmd:* script to load and clean official landing data from tunisia
3. *skater.R:* script to create cluster with skater function and to do number of cluster selection with silhouette function
4. *gravity_model.R:* for now experimental but ideally this would assign the cluster to ports based on gravity
5. *lm_model.R:*: aggregate landing data and detections dataset with cluster to run linear model 
