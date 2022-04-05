# Optimistic-Clustering

## Contact Info. 
A. Okuno (okuno@ism.ac.jp) and K. Hattori (khattori@ism.ac.jp)

## dataset
You can find 10 dataset instances in "/dataset/seed**.csv". 
Preprocessed (standardadization + removal of some outliers) can be found in "/dataset/preprocessed_seed**.RData", which was processed by "/verbose/preprocessing.R"

## experiments.R
This script computes GOC for each setting, and output each iteration to "/output/(clustering_oracle)/lambda/K0/seed". 
"/verbose/summary_in_LaTeX_form.R" can summarize these outputs. 

## visualization.R
This script provides visualizations of dataset instances 1-3: 

<img src="/output/visualization/seed1.png" width="300"> <img src="/output/visualization/seed2.png" width="300"> <img src="/output/visualization/seed3.png" width="300">

and the comparison between conventional (left) and proposed GOC (right)

<img src="/output/visualization/conventional.png" width="300"> <img src="/output/visualization/optimistic.png" width="300">

## Verbose
- "/verbose/dependencies.R" lists depended packages (used in experiments.R and visualization.R)
- "/verbose/preprocessing.R" preprocess the raw-dataset instances "/dataset/seed**.csv" to "/dataset/preprocessed_seed**.RData"
- "/verbose/evaluation_metrics.R" provides clustering scores (NMI and F-measure)
- "/verbose/GOC.R" provides GOC function equipped with clustering oracle "clm" therein
- "/verbose/affinity_propagation.R" provides the experiments on affinity propagaion
- "/verbose/summary_in_LaTeX_form.R" outputs the summary of experimental results (in the form of LaTeX tables)
