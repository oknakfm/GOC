## clustering oracle
clm <- function(x, centers=5, clm_type="k-means"){
  n_centers = if(is.null(nrow(centers))) centers else nrow(centers)
  if(clm_type == "k-means"){
    clusters = kmeans(x=x, centers=centers, nstart=100)$cluster
  }else if(clm_type=="k-medoids"){
    clusters = Cluster_Medoids(data=x,clusters=n_centers)$clusters
  }else if(clm_type=="GMM_EII"){
    if(!is.null(nrow(centers))) centers = nrow(centers)
    clusters = Mclust(data=x,G=1:n_centers,modelNames="EII")$classification
  }else if(clm_type=="GMM_VII"){
    if(!is.null(nrow(centers))) centers = nrow(centers)
    clusters = Mclust(data=x,G=1:n_centers,modelNames="VII")$classification
  }else if(clm_type=="GMM_VVV"){
    if(!is.null(nrow(centers))) centers = nrow(centers)
    clusters = Mclust(data=x,G=1:n_centers,modelNames="VVV")$classification
  }else if(clm_type=="GMM_ClusterR"){
    GMM_likelihood = GMM(data=x, gaussian_comps = n_centers, seed_mode = "random_subset")$Log_likelihood
    clusters = apply(GMM_likelihood,1,which.max)
  }
  centers = update_centers(representatives=x, 
                           clusters = clusters, K_tmp = NULL)$new_cluster_centers
  return(list(clusters = clusters, centers = centers))
}

## clustering center
update_centers <- function(representatives, clusters, K_tmp=NULL){
  if(is.null(K_tmp)) K_tmp = max(clusters)
  new_cluster_centers = NULL; 
  n_nonempty_clusters = 0
  for(k in 1:K_tmp){
    ind = which(clusters == k)
    if(length(ind)==1){
      n_nonempty_clusters = n_nonempty_clusters + 1
      new_cluster_centers = rbind(new_cluster_centers,representatives[ind,])
    }else if(length(ind)>1){
      n_nonempty_clusters = n_nonempty_clusters + 1
      new_cluster_centers = 
        rbind(new_cluster_centers, apply(representatives[ind,],2,mean))
    }
  }
  return(list(new_cluster_centers = new_cluster_centers, 
              K_nonempty = n_nonempty_clusters))
}


## GOC
GOC <- function(X, penalty, lambda=0.01, K0=50, 
                                  clm_type = "k-means",
                                  K_varies = TRUE, Destination_To_Save = DIR_NAME, max_itr=50){
  K_process = K_tmp = K0

  # first iteration 
  itr = 1
  representatives = do.call(rbind, lapply(1:n_stars, function(j) apply(X[[j]],2,mean)))
  rownames(representatives) = paste0("star_id = ",star_ids)
  
  clm_tmp = clm(x=representatives, centers=K_tmp, clm_type=clm_type) 
  clusters = clm_tmp$clusters
  cluster_centers = clm_tmp$centers
  save(file=paste0(Destination_To_Save,"/itr=1.RData"), representatives, clusters, cluster_centers, K_tmp)
  
  matching_rates = NULL
  
  # subsequent iterations
  while(TRUE){
    itr = itr + 1
    new_clusters = new_cluster_centers = new_representatives = NULL
    
    selected_instance_id = NULL
    for(j in 1:n_stars){
      D = as.matrix(pdist(X = X[[j]], Y = cluster_centers,
                          indices.A = NULL, indices.B = NULL)) + lambda * penalty[[j]]
      ind = which(D == min(D), arr.ind=T)
      selected_instance_id = append(selected_instance_id, ind[1])
      new_representatives = rbind(new_representatives, X[[j]][ind[1],])
      new_clusters = append(new_clusters, ind[2])
    }
    
    uc = update_centers(representatives = new_representatives, 
                        clusters = new_clusters, K_tmp=K_tmp)
    
    new_cluster_centers = uc$new_cluster_centers
    if(K_varies) K_tmp = uc$K_nonempty
    K_process = append(K_process,K_tmp)
    
    clm_tmp = clm(x=new_representatives, centers=new_cluster_centers)
    clusters = clm_tmp$clusters
    cluster_centers = clm_tmp$centers
    representatives = new_representatives
    
    save(file=paste0(Destination_To_Save,"/itr=",itr,".RData"), representatives, clusters, cluster_centers, K_tmp, selected_instance_id)
    
    if(itr >= 3){
      matching_rate = sum(previous_instance_id == selected_instance_id)/length(previous_instance_id)
      matching_rates = append(matching_rates, matching_rate)
      if(matching_rate == 1 || itr >= max_itr){
        names(matching_rates) = paste0((3:itr)-1, " to ", 3:itr)
        save(file=paste0(Destination_To_Save,"/matching_rates.RData"), matching_rates, K_process)
        break
      }
    }    
    previous_instance_id = selected_instance_id
  }
  
  return(NULL)
}



