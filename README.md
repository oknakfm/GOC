# Overview 
This repository provides 10 synthetic datasets of sibling stars, generated by our numerical simulation that mimics the formation process of the Milky Way. 
Each dataset ("/dataset/seed**.csv") contains 3-dim. features of 275 stars, associated to each empirical uncertainty set (consisting of 101 features for each). Overall, each dataset contains 275 x 101 = 27775 features with pre-computed penalties. This repository also provides R source codes to reproduce experimental results in [1]. 

A paper of ours [1] provides a greedy and optimistic approach for clustering (GOC) and applies GOC to the stars associated with uncertain covariates. [2] further applies GOC to real-world observation of sibling stars. See [1] for further details of these datasets and experimental results. 

[1] A. Okuno and K. Hattori. "A Greedy and Optimistic Approach for Clustering with A Specified Uncertainty of Covariates", arXiv:xxxx.xxxx <br>
[2] K. Hattori, A. Okuno, and I. Roederer. "Optimisitic clustering - A new clustering method to find structures in noisy data set: An application to clustering analysis of $r$-II halo stars in the Milky Way", arXiv:xxxx.xxxx <br>

## BiBTeX Citation
For the use of these datasets, prease cite the papers [1] and [2] with the following BiBTeX entries:

```
@article{Okuno2022,
    year      = {2022},
    publisher = {CoRR},
    volume    = {xx},
    number    = {xx},
    pages     = {xxxx},
    author    = {Okuno, Akifumi and Hattori, Kohei},
    title     = {A Greedy and Optimistic Approach for Clustering with A Specified Uncertainty of Covariates},
    journal   = {arXiv:xxxx.xxxx}
}

@article{Hattori2022,
    year      = {2022},
    publisher = {CoRR},
    volume    = {xx},
    number    = {xx},
    pages     = {xxxx},
    author    = {Hattori, Kohei and Okuno, Akifumi and Roederer, Ian},
    title     = {Optimisitic clustering - A new clustering method to find structures in noisy data set: An application to clustering analysis of $r$-II halo stars in the Milky Way},
    journal   = {arXiv:xxxx.xxxx}
}
```


## Contact Information
A. Okuno (ISM and RIKEN AIP, okuno@ism.ac.jp; <a href="https://okuno.net/">https://okuno.net/</a>) <br>
K. Hattori (NAOJ, ISM, and U. Michigan, khattori@ism.ac.jp; <a href="https://koheihattori.github.io/">https://koheihattori.github.io/</a>)

<img src="/output/visualization/seed1.png" width="300"> <img src="/output/visualization/optimistic.png" width="300"> 

# Repository Descriptions

## <a href="https://github.com/oknakfm/GOC/tree/main/dataset">/dataset</a>
You can find 10 dataset instances "seed1.csv"-"seed10.csv". 
See [xxxx] for more detailed descriptions of the dataset generation and [1] for more detailed description of these datasets. 
Also the preprocessed instances (standardadization + removal of some outliers) can be found: they were preprocessed by <a href="https://github.com/oknakfm/GOC/blob/main/verbose/preprocessing.R">"/verbose/preprocessing.R"</a>. 
In <a href="https://github.com/oknakfm/GOC/tree/main/dataset/verbose">"/dataset/verbose"</a>, you can find intermediate products of our numerical simulation (to generate the synthetic datasets). 

## <a href="https://github.com/oknakfm/GOC/blob/main/visualization.R">visualization.R</a>
This script provides visualizations of datasets, and clustering results. The visualization results are saved to <a href="https://github.com/oknakfm/GOC/tree/main/output/visualization">"/output/visualization"</a>.

## <a href="https://github.com/oknakfm/GOC/blob/main/experiments.R">experiments.R</a>
This script applies GOC to our synthetic datasets. The clustering results are saved to <a href="https://github.com/oknakfm/GOC/tree/main/output">"/output"</a>. 

## <a href="https://github.com/oknakfm/GOC/tree/main/verbose">/verbose</a>
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/dependencies.R">"/verbose/dependencies.R"</a> lists depended packages (used in experiments.R and visualization.R)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/preprocessing.R">"/verbose/preprocessing.R"</a> preprocess the dataset instances
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/evaluation_metrics.R">"/verbose/evaluation_metrics.R"</a> provides clustering scores (NMI and F-measure)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/GOC.R">"/verbose/GOC.R"</a> provides GOC function equipped with clustering oracle "clm" therein
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/summary_in_LaTeX_form.R">"/verbose/summary_in_LaTeX_form.R"</a> outputs the summary of experimental results (in the form of LaTeX tables)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/convergence.R">"/verbose/convergence.R"</a> outputs the summary of experimental results (in the form of LaTeX tables)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/affinity_propagation.R">"/verbose/affinity_propagation.R"</a> conducts experiments on affinity propagaion
