#This file generates the ensemble bivariate ALE plots (in the SI of the paper) for all pairs of the DFT features.

library(ggplot2)
library(tidyverse)
library(RColorBrewer)

#generating cruciform bivariate ALE plots
cruciforms <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/cruciform model/cruciform ale results/cruciforms.rds")[1:81,]
results_c_ale <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/cruciform model/cruciform ale results/results_c_ale.rds")
c_ale <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/cruciform model/cruciform ale results/c_ale.rds")

for (k in 1:nrow(c_ale)) {
  ale_mtx =  data.frame(matrix(ncol = 3, nrow = 0))
  for (i in 1:100) {
    for (m in 1:length(results_c_ale[k,i][[1]][[2]][[1]])) {
      for (n in 1:length(results_c_ale[k,i][[1]][[2]][[2]])) {
        ale_mtx = rbind(ale_mtx,c(results_c_ale[k,i][[1]][[2]][[1]][m],results_c_ale[k,i][[1]][[2]][[2]][n],results_c_ale[k,i][[1]][[3]][m,n]))
      }
    }
  }
  ale_mtx[,1] = as.factor(ale_mtx[,1])
  ale_mtx[,2] = as.factor(ale_mtx[,2])
  name1 = c_ale$name1[k]
  name2 = c_ale$name2[k]
  colnames(ale_mtx) = c('name1', 'name2', 'y')
  mean_c_ale =  ale_mtx %>% group_by(ale_mtx[,1], ale_mtx[,2]) %>% summarise(mean_y = mean(y))
  colnames(mean_c_ale) = c('X','Y','Z')
  mean_c_ale$X = as.numeric(as.character(mean_c_ale$X))
  mean_c_ale$Y = as.numeric(as.character(mean_c_ale$Y))
  cruciforms$X = unlist(cruciforms[name1])
  cruciforms$Y = unlist(cruciforms[name2])
  
  #lim <- 10
  #mean_c_ale$Z_trunc <- ifelse(mean_c_ale$Z > lim, lim, mean_c_ale$Z)
  # fig_nate = ggplot(data = mean_c_ale, aes(x = X, y = Y, z = Z_trunc)) + 
  #   stat_contour(geom = "polygon", aes(fill = ..level..)) +
  #   geom_tile(aes(fill = Z_trunc)) + guides(fill = guide_colorbar(title = expression(paste(Delta,'\ Prediction (nm)')))) +
  #   stat_contour(color = "black") +
  #   scale_fill_gradientn(colours = rev(brewer.pal(n = 11, name = 'RdBu')), limits = c(-lim, lim)) +
  #   ggtitle(paste0('Cruciform Model ',name1,' & ',name2)) + theme(plot.title = element_text(hjust = 0.5)) + xlab(paste0(c_ale$name1[k],c_ale$unit1[k])) + ylab(paste0(c_ale$name2[k],c_ale$unit2[k])) +
  #   theme(plot.title = element_text(size = 15), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15))
  # 
  # ggsave(paste0('/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/cruciform model/cruciform ale plots/',c_ale$pairs[k],'_nate.png'), width = 7, height = 5)

  fig = ggplot(data = mean_c_ale, aes(x = X, y = Y)) + stat_contour(geom = 'polygon', aes(z = Z, fill = ..level..)) +
    theme(panel.grid=element_blank()) + coord_cartesian(xlim=c(min(mean_c_ale$X), max(mean_c_ale$X)), ylim=c(min(mean_c_ale$Y),max(mean_c_ale$Y)), expand = F) +
    guides(fill = guide_colorbar(title = expression(paste(Delta,'\ Prediction (nm)')))) +  theme(panel.background=element_rect(fill=brewer.pal(n =15, name = 'RdBu')[4])) +
    scale_fill_gradientn(colours = rev(brewer.pal(n = 15, name = 'RdBu')), limits = c(-30,30)) +
    ggtitle(paste0('Cruciform Model ',name1,' & ',name2)) + theme(plot.title = element_text(hjust = 0.5)) + xlab(paste0(c_ale$name1[k],c_ale$unit1[k])) + ylab(paste0(c_ale$name2[k],c_ale$unit2[k])) +
    theme(plot.title = element_text(size = 15), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15)) +
    geom_point(data = cruciforms, size = 2, aes(x = X, y = Y, color = 'Molecules in\nTraining Set'), show.legend=TRUE) + scale_color_manual(values = 'black') + labs(colour = element_blank())
  
  ggsave(paste0('/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/cruciform model/cruciform ale plots/',c_ale$pairs[k],'.png'), width = 7, height = 5)
  
}



#generating fragment bivariate ALE plots
fragments <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/fragment model/fragment ale results/fragments.rds")[1:81,]
results_f_ale <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/fragment model/fragment ale results/results_f_ale.rds")
f_ale <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/fragment model/fragment ale results/f_ale.rds")

for (k in 1:nrow(f_ale)) {
  ale_mtx = data.frame(matrix(ncol = 3, nrow = 0))
  for (i in 1:100) {
    for (m in 1:length(results_f_ale[k,i][[1]][[2]][[1]])) {
      for (n in 1:length(results_f_ale[k,i][[1]][[2]][[2]])) {
        ale_mtx = rbind(ale_mtx,c(results_f_ale[k,i][[1]][[2]][[1]][m],results_f_ale[k,i][[1]][[2]][[2]][n],results_f_ale[k,i][[1]][[3]][m,n]))
      }
    }
  }
  ale_mtx[,1] = as.factor(ale_mtx[,1])
  ale_mtx[,2] = as.factor(ale_mtx[,2])
  name1 = f_ale$name1[k]
  name2 = f_ale$name2[k]
  colnames(ale_mtx) = c('name1', 'name2', 'y')
  mean_f_ale =  ale_mtx %>% group_by(ale_mtx[,1], ale_mtx[,2]) %>% summarise(mean_y = mean(y))
  colnames(mean_f_ale) = c('X','Y','Z')
  mean_f_ale$X = as.numeric(as.character(mean_f_ale$X))
  mean_f_ale$Y = as.numeric(as.character(mean_f_ale$Y))
  fragments$X = unlist(fragments[name1])
  fragments$Y = unlist(fragments[name2])
  
  #minValue = sapply(mean_f_ale,min)
  #maxValue = sapply(mean_f_ale,max)
  #rg = maxValue - minValue
  #arbitaryValue = 0
  #edge1 = data.frame(X=minValue[1]-0.05*rg[1],Y=minValue[2]:maxValue[2],Z=arbitaryValue)
  #edge2 = data.frame(X=minValue[1]:maxValue[1],Y=minValue[2]-0.05*rg[2],Z=arbitaryValue)
  #edge3 = data.frame(X=maxValue[1]+0.05*rg[1],Y=minValue[2]:maxValue[2],Z=arbitaryValue)
  #edge4 = data.frame(X=minValue[1]:maxValue[1],Y=maxValue[2]+0.05*rg[2],Z=arbitaryValue)
  #data = rbind(mean_f_ale,edge1,edge2,edge3,edge4)
  #fig = ggplot(data, aes(x = X, y = Y ,z = Z)) + stat_contour(geom = 'polygon', aes(fill = ..level..)) + coord_cartesian(xlim=c(min(data$X), max(data$X)), ylim=c(min(data$Y),max(data$Y)), expand = F)
  
  #lim <- 10
  #mean_f_ale$Z_trunc <- ifelse(mean_f_ale$Z > lim, lim, mean_f_ale$Z)
  # fig_nate = ggplot(data = mean_f_ale, aes(x = X, y = Y, z = Z_trunc)) + 
  #   stat_contour(geom = "polygon", aes(fill = ..level..)) +
  #   geom_tile(aes(fill = Z_trunc)) + guides(fill = guide_colorbar(title = expression(paste(Delta,'\ Prediction (nm)')))) +
  #   stat_contour(color = "black") +
  #   scale_fill_gradientn(colours = rev(brewer.pal(n = 11, name = 'RdBu')), limits = c(-lim, lim)) +
  #   ggtitle(paste0('Fragment Model ',name1,' & ',name2)) + theme(plot.title = element_text(hjust = 0.5)) + xlab(paste0(f_ale$name1[k],f_ale$unit1[k])) + ylab(paste0(f_ale$name2[k],f_ale$unit2[k])) +
  #   theme(plot.title = element_text(size = 15), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15))
  # 
  # ggsave(paste0('/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/fragment model/fragment ale plots/',f_ale$pairs[k],'_nate.png'), width = 7, height = 5)
  
  fig = ggplot(data = mean_f_ale, aes(x = X, y = Y)) + stat_contour(geom = 'polygon', aes(z = Z, fill = ..level..)) +
    theme(panel.grid=element_blank()) + coord_cartesian(xlim=c(min(mean_f_ale$X), max(mean_f_ale$X)), ylim=c(min(mean_f_ale$Y),max(mean_f_ale$Y)), expand = F) +
    guides(fill = guide_colorbar(title = expression(paste(Delta,'\ Prediction (nm)')))) + theme(panel.background=element_rect(fill=brewer.pal(n =15, name = 'RdBu')[4])) +
    scale_fill_gradientn(colours = rev(brewer.pal(n = 15, name = 'RdBu')), limits = c(-30,30)) +
    ggtitle(paste0('Fragment Model ',name1,' & ',name2)) + theme(plot.title = element_text(hjust = 0.5)) + xlab(paste0(f_ale$name1[k],f_ale$unit1[k])) + ylab(paste0(f_ale$name2[k],f_ale$unit2[k])) +
    theme(plot.title = element_text(size = 15), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15)) +
    geom_point(data = fragments, size = 2, aes(x = X, y = Y, color = 'Molecules in\nTraining Set'), show.legend=TRUE) + scale_color_manual(values = 'black') + labs(colour = element_blank())
  
  ggsave(paste0('/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/fragment model/fragment ale plots/',f_ale$pairs[k],'.png'), width = 7, height = 5)
}
