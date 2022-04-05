st <- function(table, fn="mean", digits=3) round(apply(table,2,fn), digits=digits)

## ======================================================================
## [Experiment-1]
for(K0 in c(30,50,70)){
  table_K0fixed_conv = table_K0fixed_GOC = NULL
  for(lambda in c(0,0.01,0.1,1)){
    tmp_table_conv = tmp_table_GOC = NULL
    for(seed_id in 1:10){
      load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
      load(paste0(DIR_NAME, "/output/k-means/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/matching_rates.RData"))
      
      load(paste0(DIR_NAME, "/output/k-means/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=1.RData"))
      tmp_table_conv = rbind(tmp_table_conv, scores(true_clusters = true_clusters, predicted_clusters = clusters))
      
      n_itr = length(matching_rates) + 1
      load(paste0(DIR_NAME, "/output/k-means/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",n_itr,".RData"))
      tmp_table_GOC = rbind(tmp_table_GOC, append(scores(true_clusters = true_clusters, predicted_clusters = clusters), n_itr))
    }
    table_K0fixed_conv = cbind(table_K0fixed_conv, paste0("$",st(tmp_table_conv), "\\pm", st(tmp_table_conv, fn="sd"),"$"))
    table_K0fixed_GOC = cbind(table_K0fixed_GOC, paste0("$", st(tmp_table_GOC), "\\pm", st(tmp_table_GOC, fn="sd"), "$"))
  }
  table_LaTeX = NULL
  table_LaTeX = paste0("\\begin{tabular}{llcccc} \n \\toprule \n & & $\\lambda=0$ & $\\lambda=0.01$ & $\\lambda=0.1$ & $\\lambda=1$ \\\\ \n \\midrule \n ")
  
  ## NMI for GOC
  table_LaTeX = paste0(table_LaTeX,"\\multirow{2}{*}{NMI} & GOC ")
  for(id in 1:4) table_LaTeX = paste0(table_LaTeX, " & ", table_K0fixed_GOC[1,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  ## NMI for conventional
  table_LaTeX = paste0(table_LaTeX,"& Baseline ")
  for(id in 1:4) table_LaTeX = paste0(table_LaTeX, " & ", table_K0fixed_conv[1,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n  \\cmidrule{2-6} \n")
  
  ## F-measure for GOC
  table_LaTeX = paste0(table_LaTeX,"\\multirow{2}{*}{$F$-measure} & GOC ")
  for(id in 1:4) table_LaTeX = paste0(table_LaTeX, " & ", table_K0fixed_GOC[2,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  ## F-measure for conventional
  table_LaTeX = paste0(table_LaTeX,"& Baseline ")
  for(id in 1:4) table_LaTeX = paste0(table_LaTeX, " & ", table_K0fixed_conv[2,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n \\cmidrule{2-6} \n")
  
  ## #clusters for GOC
  table_LaTeX = paste0(table_LaTeX,"$\\#$clusters & GOC ")
  for(id in 1:4) table_LaTeX = paste0(table_LaTeX, " & ", table_K0fixed_GOC[3,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")

  ## #iterations for GOC
  ## #clusters for GOC
  table_LaTeX = paste0(table_LaTeX,"$\\#$iterations & GOC ")
  for(id in 1:4) table_LaTeX = paste0(table_LaTeX, " & ", table_K0fixed_GOC[4,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  table_LaTeX = paste0(table_LaTeX, "\\bottomrule \n \\end{tabular}")
  write(x = table_LaTeX, file=paste0(DIR_NAME,"/output/exp1_K0=",K0,".txt"))
}


## ======================================================================
## [Experiment-2]

lambda = 0.01
table_lfixed_conv = table_lfixed_GOC = NULL
for(K0 in seq(30,70,10)){
  tmp_table_conv = tmp_table_GOC = NULL
  for(seed_id in 1:10){
    load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
    load(paste0(DIR_NAME, "/output/k-means/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/matching_rates.RData"))

    load(paste0(DIR_NAME, "/output/k-means/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=1.RData"))
    tmp_table_conv = rbind(tmp_table_conv, scores(true_clusters = true_clusters, predicted_clusters = clusters))
    
    n_itr = length(matching_rates) + 1
    load(paste0(DIR_NAME, "/output/k-means/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",n_itr,".RData"))
    tmp_table_GOC = rbind(tmp_table_GOC, append(scores(true_clusters = true_clusters, predicted_clusters = clusters), n_itr))
  }
  table_lfixed_conv = cbind(table_lfixed_conv, paste0("$",st(tmp_table_conv), "\\pm", st(tmp_table_conv, fn="sd"),"$"))
  table_lfixed_GOC = cbind(table_lfixed_GOC, paste0("$", st(tmp_table_GOC), "\\pm", st(tmp_table_GOC, fn="sd"), "$"))
}
table_LaTeX = NULL
table_LaTeX = paste0("\\begin{tabular}{llccccc} \n \\toprule \n & & $K(0)=30$ & $K(0)=40$ & $K(0)=50$ & $K(0)=60$ & $K(0)=70$ \\\\ \n \\midrule \n ")

## NMI for GOC
table_LaTeX = paste0(table_LaTeX,"\\multirow{2}{*}{NMI} & GOC ")
for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_lfixed_GOC[1,id])
table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")

## NMI for conventional
table_LaTeX = paste0(table_LaTeX,"& Baseline ")
for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_lfixed_conv[1,id])
table_LaTeX = paste0(table_LaTeX, " \\\\ \n \\cmidrule{2-7} \n")

## F-measure for GOC
table_LaTeX = paste0(table_LaTeX,"\\multirow{2}{*}{$F$-measure} & GOC ")
for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_lfixed_GOC[2,id])
table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")

## F-measure for conventional
table_LaTeX = paste0(table_LaTeX,"& Baseline ")
for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_lfixed_conv[2,id])
table_LaTeX = paste0(table_LaTeX, " \\\\ \n \\cmidrule{2-7} \n")

## #clusters for GOC
table_LaTeX = paste0(table_LaTeX,"$\\#$clusters & GOC ")
for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_lfixed_GOC[3,id])
table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")

## #iterations for GOC
## #clusters for GOC
table_LaTeX = paste0(table_LaTeX,"$\\#$iterations & GOC ")
for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_lfixed_GOC[4,id])
table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")

table_LaTeX = paste0(table_LaTeX, "\\bottomrule \n \\end{tabular}")
write(x = table_LaTeX, file=paste0(DIR_NAME,"/output/exp2_lambda=",lambda,".txt"))


## ======================================================================
## [Experiment-3]
for(K0 in c(30,50,70)){
  table_conv = table_GOC = NULL
  for(clm_type in c("k-means","k-medoids","GMM_EII","GMM_VII","GMM_VVV")){
    tmp_table_conv = tmp_table_GOC = NULL
    for(seed_id in 1:10){
      load(paste0(DIR_NAME,"/dataset/preprocessed_seed",seed_id,".RData"))
      load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/matching_rates.RData"))
      
      load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=1.RData"))
      tmp_table_conv = rbind(tmp_table_conv, scores(true_clusters = true_clusters, predicted_clusters = clusters))
      
      n_itr = length(matching_rates) + 1
      load(paste0(DIR_NAME, "/output/",clm_type,"/lambda=",lambda,"/K0=",K0,"/seed",seed_id,"/itr=",n_itr,".RData"))
      tmp_table_GOC = rbind(tmp_table_GOC, append(scores(true_clusters = true_clusters, predicted_clusters = clusters), n_itr))
    }
    table_conv = cbind(table_conv, paste0("$",st(tmp_table_conv), "\\pm", st(tmp_table_conv, fn="sd"),"$"))
    table_GOC = cbind(table_GOC, paste0("$", st(tmp_table_GOC), "\\pm", st(tmp_table_GOC, fn="sd"), "$"))
  }
  table_LaTeX = NULL
  table_LaTeX = paste0("\\begin{tabular}{llccccc} \n \\toprule \n & & $k$-means & $k$-medoids & GMM~(EII) & GMM~(VII) & GMM~(VVV) \\\\ \n \\midrule \n ")
  
  ## NMI for GOC
  table_LaTeX = paste0(table_LaTeX,"\\multirow{2}{*}{NMI} & GOC ")
  for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_GOC[1,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  ## NMI for conventional
  table_LaTeX = paste0(table_LaTeX,"& Baseline ")
  for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_conv[1,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n \\cmidrule{2-7} \n")
  
  ## F-measure for GOC
  table_LaTeX = paste0(table_LaTeX,"\\multirow{2}{*}{$F$-measure} & GOC ")
  for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_GOC[2,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  ## F-measure for conventional
  table_LaTeX = paste0(table_LaTeX,"& Baseline ")
  for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_conv[2,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n \\cmidrule{2-7} \n")
  
  ## #clusters for GOC
  table_LaTeX = paste0(table_LaTeX,"$\\#$clusters & GOC ")
  for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_GOC[3,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  ## #iterations for GOC
  ## #clusters for GOC
  table_LaTeX = paste0(table_LaTeX,"$\\#$iterations & GOC ")
  for(id in 1:5) table_LaTeX = paste0(table_LaTeX, " & ", table_GOC[4,id])
  table_LaTeX = paste0(table_LaTeX, " \\\\ \n ")
  
  table_LaTeX = paste0(table_LaTeX, "\\bottomrule \n \\end{tabular}")
  write(x = table_LaTeX, file=paste0(DIR_NAME,"/output/exp3_K0=",K0,".txt"))
}