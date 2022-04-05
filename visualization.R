## ===========================================================
## Greedy and Optimistic Algorithm for Clustering
## Apr. 3rd, 2022
## A. Okuno (ISM, RIKEN AIP: okuno@ism.ac.jp)
## K. Hattori (NAOJ, ISM, U. Michigan: khattori@ism.ac.jp)
## ===========================================================

## ===========================================================
## specify the directory name containing this script 
DIR_NAME = getwd()
## ===========================================================

set.seed(123)

Destination_To_Save = paste0(DIR_NAME, "/output/visualization")
dir.create(Destination_To_Save,showWarnings=FALSE)

## [Preliminaries]
source(paste0(DIR_NAME,"/verbose/dependencies.R"))
source(paste0(DIR_NAME,"/verbose/GOC.R"))
source(paste0(DIR_NAME,"/verbose/evaluation_metrics.R"))
# source(paste0(DIR_NAME,"/verbose/preprocessing.R")) #if necessary


## ===========================================================
## [Visualization: conventional vs GOC (Figure 1)]
## ===========================================================

seed_id = 1; 
load(paste0(DIR_NAME, "/dataset/preprocessed_seed", seed_id, ".RData"))

## optimistic clustering
GOC(X=X, penalty=penalty, lambda=0.01, K0=50, clm_type = "k-means", 
    K_varies = TRUE, Destination_To_Save = Destination_To_Save)

load(file=paste0(Destination_To_Save,"/matching_rates.RData"))
n_itr = length(matching_rates)+2 

for(est_type in c("conventional","optimistic")){
  itr = if(est_type=="conventional") 1 else n_itr
  load(file=paste0(Destination_To_Save,"/itr=",itr,".RData"))
  
  png(paste0(Destination_To_Save,"/",est_type,".png"), width = 400, height = 400, pointsize=15) 
  
  X_bind = do.call("rbind",X)
  X_PCA = prcomp(X_bind)
  X_proj = X_bind %*% X_PCA$rotation[,1:2]
  sr_proj = representatives %*% X_PCA$rotation[,1:2]
  
  xl = quantile(X_proj[,1], probs=c(0.01,0.99))
  yl = quantile(X_proj[,2], probs=c(0.01,0.99))
  
  if(est_type=="conventional"){
    plot(0,0, xlim=xl, ylim=yl, xlab="PC1", ylab="PC2",
         type="n", cex=0.5, pch=16, col="grey")
  }else{
    plot(X_proj[,1],X_proj[,2], xlim=xl, ylim=yl, xlab="PC1", ylab="PC2",
         type="p", cex=0.5, pch=16, col=adjustcolor("grey", alpha.f = 0.2))
  }
  
  colors = rainbow(K_tmp) ## cluster colors to be plotted
  for(m in 1:K_tmp){
    ind = which(clusters == m)
    ext_sr_proj = sr_proj[ind,]
    par(new=T)
    if(length(ind)>1){
      plot(ext_sr_proj[,1], ext_sr_proj[,2], type="p", col=colors[m], cex=2, pch=3,
           xlim = xl, ylim = yl, xaxt = "n", yaxt = "n", xlab=" ",ylab=" ")
      dX = as.matrix(dist(ext_sr_proj))
      distant_pair = which(dX == max(dX),arr.ind=T)[1,]
      mu = (ext_sr_proj[distant_pair[1],] + ext_sr_proj[distant_pair[2],]) / 2
      r = max(dX)/2
      draw.circle(mu[1],mu[2],r, col=adjustcolor(colors[m], alpha.f=0.05))
    }else{
      plot(ext_sr_proj[1], ext_sr_proj[2], type="p", col=colors[m], cex=2, pch=3,
           xlim = xl, ylim = yl, xaxt = "n", yaxt = "n", xlab=" ",ylab=" ")
    }
  }
  dev.off()
}


## ===========================================================
## [Visualization: dataset instances (Figure 4)]
## ===========================================================

for(seed_id in 1:3){
  load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData")) # mock data
  
  png(paste0(Destination_To_Save,"/seed",seed_id,".png"), width = 400, height = 400, pointsize=15) 
  
  X_bind = do.call("rbind",X)
  X_PCA = prcomp(X_bind)
  X_proj = X_bind %*% X_PCA$rotation[,1:2]
  
  xl = quantile(X_proj[,1], probs=c(0.01,0.99))
  yl = quantile(X_proj[,2], probs=c(0.01,0.99))

  plot(X_proj[,1],X_proj[,2], xlim=xl, ylim=yl, xlab="PC1", ylab="PC2",
       type="p", cex=0.5, pch=16, col=adjustcolor("grey", alpha.f = 0.2))
  
  cluster_id_set = c(1,10,20,30,40,50)
  colors = rainbow(length(cluster_id_set))
  
  i = 1
  for(cluster_id in cluster_id_set){
    ind = which(true_clusters == cluster_id - 1)
    for(id in ind){
      tmp_X = X[[id]] %*% X_PCA$rotation[,1:2]
      par(new=T)
      plot(tmp_X[,1],tmp_X[,2], xlim=xl, ylim=yl, xlab=" ", ylab=" ",
           type="p", cex=0.5, pch=16, col=colors[i], xaxt="n", yaxt="n")
    }
    i = i + 1
  }
  legend("topleft", pch=rep(16,length(colors)), legend=paste0("k=",cluster_id_set), col=colors)
  
  dev.off()
}
