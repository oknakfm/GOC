score_names = c("NMI","Fmeasure", "K")

scores <- function(true_clusters, predicted_clusters){
  n = length(true_clusters)
  
  cluster_table = table(predicted_clusters,true_clusters)
  n1 = apply(cluster_table,1,sum)
  n2 = apply(cluster_table,2,sum)
  nonzero_id = which(cluster_table!=0,arr.ind=T)
  
  ## NMI
  I = 0 # mutual information
  for(l in 1:nrow(nonzero_id)){
    k = nonzero_id[l,1]; j = nonzero_id[l,2]
    nkj = cluster_table[k,j]
    I = I + (nkj/n) * log( (n*nkj) / (n1[k]*n2[j]) )
  }
  H1 = -sum((n1/n) * log(n1/n))
  H2 = -sum((n2/n) * log(n2/n))
  NMI = as.numeric(2*I/(H1+H2))

  ## F-measure
  G = 2 * cluster_table / (n1 %*% t(rep(1,length(n2))) + rep(1,length(n1)) %*% t(n2) )
  F_measure = sum((n2/n) * apply(G,2,max))

  K = length(unique(predicted_clusters))

  return(c(NMI, F_measure, K))
}