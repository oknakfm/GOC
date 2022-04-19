# Overview 
This repository provides 10 synthetic orbital action datasets of stars (sibling stars), generated by our numerical simulation that mimics the formation process of the Milky Way. 
Each dataset ("/dataset/seed**.csv") contains pre-processed 3-dim. orbital action features of 275 stars, associated to each empirical uncertainty set (consisting of 101 feature instances for each). Overall, each dataset contains 275 [stars] x 101 [feature instances in each uncertainty set / each star] = 27775 [feature instances] associated with pre-computed penalties. This repository also provides R source codes to reproduce experimental results in our paper [1]. 

While [1] provides a greedy and optimistic approach to clustering (GOC) and applies GOC to the synthetic datasets (whose true clusters are known), [2] further applies GOC to real-world orbital action datasets of stars. Please see [1] for further details of these synthetic datasets and experimental results. 

[1] A. Okuno and K. Hattori. "A Greedy and Optimistic Approach to Clustering with a Specified Uncertainty of Covariates", <a href="https://arxiv.org/abs/2204.08205">arXiv:2204.08205</a> <br>
[2] K. Hattori et al. "Optimisitic clustering: An application to astronomy", in prep. <br>

## BiBTeX Citation
For the use of these datasets, prease cite our papers [1] and [2] with the following BiBTeX entries:

```
@article{Okuno2022,
    year      = {2022},
    publisher = {CoRR},
    volume    = {},
    number    = {},
    pages     = {},
    author    = {Okuno, Akifumi and Hattori, Kohei},
    title     = {A Greedy and Optimistic Approach to Clustering with a Specified Uncertainty of Covariates},
    journal   = {arXiv preprint arXiv:2204.08205}
    note = {submitted.}
}

@article{Hattori2022,
    year      = {2022},
    publisher = {},
    volume    = {},
    number    = {},
    pages     = {},
    author    = {Hattori, Kohei and Okuno, Akifumi and others},
    title     = {Optimisitic clustering: An application to astronomy},
    journal   = {},
    note = {in prep.}
}
```

## Contact Information
A. Okuno (ISM and RIKEN AIP, okuno@ism.ac.jp; <a href="https://okuno.net/">https://okuno.net/</a>) <br>
K. Hattori (NAOJ, ISM, and U. Michigan, khattori@ism.ac.jp; <a href="https://koheihattori.github.io/">https://koheihattori.github.io/</a>)

<img src="/output/visualization/seed1.png" width="300"> <img src="/output/visualization/optimistic.png" width="300"> <br>
(Left) true classes, (Right) clusters detected by GOC

# Repository Descriptions

## <a href="https://github.com/oknakfm/GOC/tree/main/dataset">/dataset</a>
You can find 10 datasets "seed1.csv"-"seed10.csv". See [1] (Example 1 in Section 2 and Appendix A) for more detailed description of these datasets. The standardized instances (including removal of a few outliers) can be found: they were processed by <a href="https://github.com/oknakfm/GOC/blob/main/verbose/preprocessing.R">"/verbose/preprocessing.R"</a>. In <a href="https://github.com/oknakfm/GOC/tree/main/dataset/verbose">"/dataset/verbose"</a>, you can find intermediate products of our numerical simulation (to generate the synthetic datasets). 

## <a href="https://github.com/oknakfm/GOC/blob/main/visualization.R">visualization.R</a>
This script provides visualizations of datasets, and clustering results. The visualization results are saved to <a href="https://github.com/oknakfm/GOC/tree/main/output/visualization">"/output/visualization"</a>.

## <a href="https://github.com/oknakfm/GOC/blob/main/experiments.R">experiments.R</a>
This script applies GOC to our synthetic datasets. The clustering results are saved to <a href="https://github.com/oknakfm/GOC/tree/main/output">"/output"</a>. 

## <a href="https://github.com/oknakfm/GOC/tree/main/verbose">/verbose</a>
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/dependencies.R">"/verbose/dependencies.R"</a> lists depended packages (used in experiments.R and visualization.R)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/preprocessing.R">"/verbose/preprocessing.R"</a> preprocess (standardize) the dataset instances
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/evaluation_metrics.R">"/verbose/evaluation_metrics.R"</a> provides clustering scores (NMI and F-measure)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/GOC.R">"/verbose/GOC.R"</a> provides GOC function equipped with clustering oracle "clm" therein. Particularly, we employed k-means, k-medoids (in <a href="https://cran.r-project.org/web/packages/ClusterR/index.html">ClusterR</a>), Gaussian mixture model (in <a href="https://cran.r-project.org/web/packages/ClusterR/index.html">ClusterR</a> and <a href="https://cran.r-project.org/web/packages/mclust/index.html">Mclust</a>)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/summary_in_LaTeX_form.R">"/verbose/summary_in_LaTeX_form.R"</a> outputs the summary of experimental results (in the form of LaTeX tables)
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/convergence.R">"/verbose/convergence.R"</a> outputs the summary of experimental results on the convergence of GOC
- <a href="https://github.com/oknakfm/GOC/blob/main/verbose/affinity_propagation.R">"/verbose/affinity_propagation.R"</a> conducts experiments on affinity propagaion
