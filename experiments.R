## ===========================================================
## Greedy and Optimistic Algorithm for Clustering
## Apr. 3rd, 2022
## A. Okuno (ISM, RIKEN AIP: okuno@ism.ac.jp)
## K. Hattori (NAOJ, ISM, U. Michigan: khattori@ism.ac.jp)
## ===========================================================

## ===========================================================

## specify the directory name containing this script 
DIR_NAME = getwd()

## specify the number of CPU cores used in this script
N_CORES = max(1,detectCores()-2)

## ===========================================================

set.seed(123)

## [Preliminaries]
source(paste0(DIR_NAME,"/verbose/dependencies.R"))
source(paste0(DIR_NAME,"/verbose/GOC.R"))
source(paste0(DIR_NAME,"/verbose/evaluation_metrics.R"))
# source(paste0(DIR_NAME,"/verbose/preprocessing.R")) #if necessary

## [Experimental Setting Combinations to be Computed]
setting_combinations = rbind(
  expand.grid(clm_type = "k-means", lambda = c(0,0.01,0.1,1), 
            K0 = seq(30,70,10), seed_id = 1:10),
  expand.grid(clm_type = c("k-medoids","GMM_EII","GMM_VII","GMM_VVV"), lambda = c(0,0.01,0.1,1), 
            K0 = seq(30,70,20), seed_id = 1:10)
)
setting_id_max = nrow(setting_combinations)

## [Preparing Directories for Output]
for(setting_id in 1:setting_id_max){
  clm_type = toString(setting_combinations[setting_id, 1])
  lambda = setting_combinations[setting_id, 2]
  K0 = setting_combinations[setting_id, 3]
  seed_id = setting_combinations[setting_id, 4]
  Destination_To_Save = DIR_NAME
  tmp_DIR_NAME = paste0(DIR_NAME,"/output/")
  tmp_DIR_NAME = paste0(tmp_DIR_NAME,clm_type)
  dir.create(tmp_DIR_NAME,showWarnings=FALSE)
  tmp_DIR_NAME = paste0(tmp_DIR_NAME,"/lambda=",lambda)
  dir.create(tmp_DIR_NAME,showWarnings=FALSE)
  tmp_DIR_NAME = paste0(tmp_DIR_NAME,"/K0=",K0)
  dir.create(tmp_DIR_NAME,showWarnings=FALSE)
  tmp_DIR_NAME = paste0(tmp_DIR_NAME,"/seed",seed_id)
  dir.create(tmp_DIR_NAME,showWarnings=FALSE)
}


## ===========================================================
## [Clustering]
## ===========================================================
registerDoParallel(N_CORES)
foreach(setting_id = 1:setting_id_max, .packages=c("pdist","mclust","ClusterR")) %dopar% {
  clm_type = toString(setting_combinations[setting_id, 1])
  lambda = setting_combinations[setting_id, 2]
  K0 = setting_combinations[setting_id, 3]
  seed_id = setting_combinations[setting_id, 4]
  
  load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
  
  Destination_To_Save = paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id)
  
  GOC(X=X, penalty=penalty, lambda=lambda, K0=K0, clm_type = clm_type, 
      K_varies = TRUE, Destination_To_Save = Destination_To_Save)
}
stopImplicitCluster()


## ===========================================================
## [summarize the experimental results (see "/output")]
## ===========================================================
source(paste0(DIR_NAME,"/verbose/summary_in_LaTeX_form.R"))
source(paste0(DIR_NAME,"/verbose/convergence.R"))