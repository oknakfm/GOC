for(seed_id in 1:10){
  
  MC_minimum = read.csv(file=paste0(DIR_NAME,"/dataset/seed",seed_id,".csv"), sep=",",
                        header=TRUE)
  
  star_ids = unique(MC_minimum[,"i"])
  n_stars = length(star_ids)
  
  X = lapply(star_ids, function(j) MC_minimum[
    which(MC_minimum[,"i"]==j),c("Jr","Jz","Jphi")])
  
  true_clusters = sapply(star_ids, function(j)
    MC_minimum[min(which(MC_minimum[,"i"]==j)),"true_cluster_id"])
  
  penalty = lapply(star_ids, function(j) MC_minimum[
    which(MC_minimum[,"i"]==j),c("parallax_penalty")])
  
  ## removal of outliers
  for(i in 1:n_stars){
    ind = which(X[[i]][,"Jr"]==-9999); if(length(ind)>0){ X[[i]] = X[[i]][-ind,]; penalty[[i]] = penalty[[i]][-ind] }
    ind = which(X[[i]][,"Jz"]==-9999); if(length(ind)>0){ X[[i]] = X[[i]][-ind,]; penalty[[i]] = penalty[[i]][-ind] }
    ind = which(X[[i]][,"Jphi"]==-9999); if(length(ind)>0){ X[[i]] = X[[i]][-ind,]; penalty[[i]] = penalty[[i]][-ind] }
  }
  
  X_bind = do.call("rbind",X); X_mean = apply(X_bind,2,mean); X_sd = apply(X_bind,2,sd)
  pen_bind = do.call("c",penalty); pen_max = max(pen_bind)
  
  ## standardization of the feature X and penalty
  X = lapply(1:n_stars, function(j) t((t(X[[j]])-X_mean)/X_sd))
  penalty = lapply(1:n_stars, function(j) penalty[[j]]/pen_max)
  
  star_ids = 1:n_stars ## reassign star_id (so that star_id = 1,2,...,n_stars)
  true_clusters = true_clusters + 1 ## reassign the cluster_id (so that cluster_id = 1,2,...,K*)
  
  save(file=paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"), X, penalty, star_ids, n_stars, true_clusters)
}