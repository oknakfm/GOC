clm_type = "k-means"; lambda = 0.01; max_itr = 20

NMI = feature_convergence = cluster_convergence = array(0,dim=c(3,10,max_itr)); id = 0

for(K0 in c(30,50,70)){
  id = id + 1
  for(seed_id in 1:10){
    load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
    load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/matching_rates.RData"))
    n_itr = length(matching_rates + 2)
    
    load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",n_itr,".RData"))
    final_clusters = clusters ## clusters at the last step (converged)
    final_representatives = representatives
    final_NMI = scores(true_clusters = true_clusters, predicted_clusters = final_clusters)[1]
    
    for(itr in 1:max_itr){
      cluster_convergence[id,seed_id,itr] = if(itr <= n_itr){
        load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",itr,".RData"))
        scores(clusters,final_clusters)[1]
      }else 1
      feature_convergence[id,seed_id,itr] = if(itr <= n_itr){
        load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",itr,".RData"))
        sum((representatives - final_representatives)^2)/n_stars
      }else 0
      NMI[id,seed_id,itr] = if(itr <= n_itr){
        load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",itr,".RData"))
        scores(true_clusters=true_clusters, predicted_clusters=clusters)[1]/final_NMI
      }else 1
    }
  }
}


## convergence of cluster assignments
pdf(paste0(DIR_NAME,"/output/convergence_cluster.pdf"), width = 6, height = 5, pointsize=18) 

par(mar = c(3, 2, 1, 1)); par(oma = c(0,0,0,0)); par(mgp = c(2, 1, 0))
xl = c(1,max_itr)
yl = range(cluster_convergence,1)
plot(0,0,type="n",xlim=xl, ylim=yl, xlab="Iteration", ylab=" ")
for(id in 1:3){
  for(seed_id in 1:10){
    par(new=T)
    plot(1:max_itr, cluster_convergence[id,seed_id,], type="l", xlim=xl, ylim=yl, 
         xlab=" ",ylab=" ",xaxt="n",yaxt="n",col=id)
  }
}
abline(h=0.95,col="black", lty=2)
abline(h=1.0,col="black", lty=2)
abline(v=10, col="black", lty=2)
legend("bottomright", legend=c("K(0)=30","K(0)=50","K(0)=70"), lty=c(1,1,1), col=1:3)

dev.off()


## convergence of feature candidates
pdf(paste0(DIR_NAME,"/output/convergence_feature.pdf"), width = 6, height = 5, pointsize=18) 

par(mar = c(3, 2, 1, 1)); par(oma = c(0,0,0,0)); par(mgp = c(2, 1, 0))
xl = c(1,max_itr)
yl = range(feature_convergence,max(feature_convergence),0)
plot(0,0,type="n",xlim=xl, ylim=yl, xlab="Iteration", ylab=" ", xaxt="n")
for(id in 1:3){
  for(seed_id in 1:10){
    par(new=T)
    plot(1:max_itr, feature_convergence[id,seed_id,], type="l", xlim=xl, ylim=yl, 
         xlab=" ",ylab=" ",xaxt="n",yaxt="n",col=id)
  }
}
axis(side=1, at=seq(2,max_itr,2))
abline(h=0.05,col="black", lty=2)
abline(v=10, col="black", lty=2)
legend("topright", legend=c("K(0)=30","K(0)=50","K(0)=70"), lty=c(1,1,1), col=1:3)

dev.off()


## convergence of NMI
pdf(paste0(DIR_NAME,"/output/convergence_NMI.pdf"), width = 6, height = 5, pointsize=18) 

par(mar = c(3, 2, 1, 1)); par(oma = c(0,0,0,0)); par(mgp = c(2, 1, 0))
xl = c(1,max_itr)
yl = range(NMI,1)
plot(0,0,type="n",xlim=xl, ylim=yl, xlab="Iteration", ylab=" ")
for(id in 1:3){
  for(seed_id in 1:10){
    par(new=T)
    plot(1:max_itr, NMI[id,seed_id,], type="l", xlim=xl, ylim=yl, 
         xlab=" ",ylab=" ",xaxt="n",yaxt="n",col=id)
  }
}
abline(h=0.99,col="black", lty=2)
abline(h=1.0,col="black", lty=2)
abline(v=10, col="black", lty=2)
legend("bottomright", legend=c("K(0)=30","K(0)=50","K(0)=70"), lty=c(1,1,1), col=1:3)

dev.off()