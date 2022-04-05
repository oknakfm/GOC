require(apcluster)

## [Experiments in Discussion (Affinity Propagation)]

dist1 <- function(X1,X2){
  d12 = as.matrix(pdist(X1,X2))
  return(mean(d12))
}

dist2 <- function(X1,X2){
  d12 = as.matrix(pdist(X1,X2))
  return(min(d12))
}

dist3 <- function(X1,X2){
  d12 = as.matrix(pdist(X1,X2))
  return( max(min(apply(d12,1,max)), min(apply(d12,2,max))) )
}
nstars = length(X)

registerDoParallel(N_CORES)

foreach(seed_id = 1:10, .packages=c("pdist")) %dopar% {
  load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
  
  dmat1 = dmat2 = dmat3 = matrix(0,nstars,nstars)
  for(i in 1:(nstars-1)){
    cat(i,"\n")
    for(j in (i+1):nstars){
      dmat1[i,j] = dmat1[j,i] = dist1(X[[i]],X[[j]])
      dmat2[i,j] = dmat2[j,i] = dist2(X[[i]],X[[j]])
      dmat3[i,j] = dmat3[j,i] = dist3(X[[i]],X[[j]])
    }
  }
  SAVE_FILE_NAME = paste0(DIR_NAME,"/dataset/dist_seed",seed_id,".RData")
  save(file=SAVE_FILE_NAME, dmat1, dmat2, dmat3)
}


## [Experiments]

qset = seq(0.5,0.9,0.2)

Destination_To_Save = paste0(DIR_NAME,"/output/AP")
dir.create(Destination_To_Save,showWarnings=FALSE)

for(seed_id in 1:10){
  load(paste0(DIR_NAME,"/dataset/dist_seed",seed_id,".RData"))
  load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))

  current_Destination_To_Save = paste0(Destination_To_Save,"/seed",seed_id)
  dir.create(current_Destination_To_Save,showWarnings=FALSE)
  
  for(q in qset){
    apc1 = apcluster(s=-dmat1, q=q); K1 = length(apc1@exemplars)
    apc2 = apcluster(s=-dmat2, q=q); K2 = length(apc2@exemplars)
    apc3 = apcluster(s=-dmat3, q=q); K3 = length(apc3@exemplars)
    
    clusters1 = clusters2 = clusters3 = rep(0,n_stars)
    for(k in 1:K1) clusters1[apc1@clusters[[k]]] = k
    for(k in 1:K2) clusters2[apc2@clusters[[k]]] = k
    for(k in 1:K3) clusters3[apc3@clusters[[k]]] = k
    
    save(file=paste0(current_Destination_To_Save,"/AP_q=",q,".RData"),clusters1, clusters2, clusters3, apc1, apc2, apc3)
  }
}

## [Summary]
st <- function(table, fn="mean", digits=3) round(apply(table,2,fn), digits=digits)

table_S1 = table_S2 = table_S3 = NULL

for(q in qset){
  score1 = score2 = score3 = NULL
  
  for(seed_id in 1:10){
    load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
    load(file=paste0(Destination_To_Save,"/seed",seed_id,"/AP_q=",q,".RData"))
    
    score1 = rbind(score1, scores(true_clusters = true_clusters, clusters1))
    score2 = rbind(score2, scores(true_clusters = true_clusters, clusters2))
    score3 = rbind(score3, scores(true_clusters = true_clusters, clusters3))  
  }
  
  table_S1 = rbind(table_S1, paste0(st(score1), "\\pm", st(score1, fn="sd")))
  table_S2 = rbind(table_S2, paste0(st(score2), "\\pm", st(score2, fn="sd")))
  table_S3 = rbind(table_S3, paste0(st(score3), "\\pm", st(score3, fn="sd")))
}