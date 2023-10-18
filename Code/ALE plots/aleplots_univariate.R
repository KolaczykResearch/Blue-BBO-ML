#This file generates the ensemble univaraite ALE plots (in the SI of the paper) that are not included in the main text of the paper.

library(ggplot2)
library(tidyverse)
library(xlsx)

results_c <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/results_c.rds")
results_f <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/results_f.rds")
cruciforms <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/cruciform model/cruciform ale results/cruciforms.rds")[1:81,]
fragments <- readRDS("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_bivariate/fragment model/fragment ale results/fragments.rds")[1:81,]

#cruciform HOMO

c_HOMO_ale = data.frame(cbind(results_c[5,1][[1]][[2]],results_c[5,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  c_HOMO_ale = rbind(c_HOMO_ale, data.frame(cbind(results_c[5,i][[1]][[2]],results_c[5,i][[1]][[3]],paste0('component',i))))
}
colnames(c_HOMO_ale) = c('HOMO','y','component')
c_HOMO_ale$y = as.numeric(as.character(c_HOMO_ale$y))
mean_c_HOMO =  c_HOMO_ale %>% group_by(HOMO) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
c_HOMO_ale$HOMO = as.numeric(as.character(c_HOMO_ale$HOMO))
mean_c_HOMO$HOMO = as.numeric(as.character(mean_c_HOMO$HOMO))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/c_HOMO.png", width = 800, height = 500)
ggplot(data = mean_c_HOMO, aes(x = HOMO, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Cruciform HOMO") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('HOMO (eV)') + geom_rug(data = data.frame(x_train = cruciforms$HOMO), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#cruciform EG
c_EG_ale = data.frame(cbind(results_c[6,1][[1]][[2]],results_c[6,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  c_EG_ale = rbind(c_EG_ale, data.frame(cbind(results_c[6,i][[1]][[2]],results_c[6,i][[1]][[3]],paste0('component',i))))
}
colnames(c_EG_ale) = c('EG','y','component')
c_EG_ale$y = as.numeric(as.character(c_EG_ale$y))
mean_c_EG =  c_EG_ale %>% group_by(EG) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_c_EG$EG = as.numeric(as.character(mean_c_EG$EG))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/c_EG.png", width = 800, height = 500)
ggplot(data = mean_c_EG, aes(x = EG, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle(expression(paste('Cruciform ',E[g]))) + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab(expression(paste(E[g],' (eV)')))+ geom_rug(data = data.frame(x_train = cruciforms$EG), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#cruciform Lmax
c_Lmax_ale = data.frame(cbind(results_c[7,1][[1]][[2]],results_c[7,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  c_Lmax_ale = rbind(c_Lmax_ale, data.frame(cbind(results_c[7,i][[1]][[2]],results_c[7,i][[1]][[3]],paste0('component',i))))
}
colnames(c_Lmax_ale) = c('Lmax','y','component')
c_Lmax_ale$y = as.numeric(as.character(c_Lmax_ale$y))
mean_c_Lmax =  c_Lmax_ale %>% group_by(Lmax) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_c_Lmax$Lmax = as.numeric(as.character(mean_c_Lmax$Lmax))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/c_Lmax.png", width = 800, height = 500)
ggplot(data = mean_c_Lmax, aes(x = Lmax, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle(expression(paste('Cruciform ',lambda['max']^'abs'))) + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab(expression(paste(lambda['max']^'abs',' (nm)'))) + geom_rug(data = data.frame(x_train = cruciforms$Lmax), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#cruciform KK
c_KK_ale = data.frame(cbind(results_c[8,1][[1]][[2]],results_c[8,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  c_KK_ale = rbind(c_KK_ale, data.frame(cbind(results_c[8,i][[1]][[2]],results_c[8,i][[1]][[3]],paste0('component',i))))
}
colnames(c_KK_ale) = c('KK','y','component')
c_KK_ale$y = as.numeric(as.character(c_KK_ale$y))
mean_c_KK =  c_KK_ale %>% group_by(KK) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_c_KK$KK = as.numeric(as.character(mean_c_KK$KK))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/c_KK.png", width = 800, height = 500)
ggplot(data = mean_c_KK, aes(x = KK, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Cruciform KK") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('KK')+ geom_rug(data = data.frame(x_train = cruciforms$KK), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#cruciform Solvent
c_Solvent_ale = data.frame(cbind(results_c[9,1][[1]][[2]],results_c[9,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  c_Solvent_ale = rbind(c_Solvent_ale, data.frame(cbind(results_c[9,i][[1]][[2]],results_c[9,i][[1]][[3]],paste0('component',i))))
}
colnames(c_Solvent_ale) = c('Solvent','y','component')
c_Solvent_ale$y = as.numeric(as.character(c_Solvent_ale$y))
mean_c_Solvent =  c_Solvent_ale %>% group_by(Solvent) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_c_Solvent$Solvent = as.numeric(as.character(mean_c_Solvent$Solvent))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/c_Solvent.png", width = 800, height = 500)
ggplot(data = mean_c_Solvent, aes(x = Solvent, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Cruciform Solvent") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('Solvent')
dev.off()



#fragment HOMO1
f_HOMO1_ale = data.frame(cbind(results_f[5,1][[1]][[2]],results_f[5,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_HOMO1_ale = rbind(f_HOMO1_ale, data.frame(cbind(results_f[5,i][[1]][[2]],results_f[5,i][[1]][[3]],paste0('component',i))))
}
colnames(f_HOMO1_ale) = c('HOMO1','y','component')
f_HOMO1_ale$y = as.numeric(as.character(f_HOMO1_ale$y))
mean_f_HOMO1 =  f_HOMO1_ale %>% group_by(HOMO1) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_HOMO1$HOMO1 = as.numeric(as.character(mean_f_HOMO1$HOMO1))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_HOMO1.png", width = 800, height = 500)
ggplot(data = mean_f_HOMO1, aes(x = HOMO1, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Fragment 2,6 HOMO") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('2,6 HOMO (eV)') + geom_rug(data = data.frame(x_train = fragments$`2,6 HOMO`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment EG1
f_EG1_ale = data.frame(cbind(results_f[6,1][[1]][[2]],results_f[6,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_EG1_ale = rbind(f_EG1_ale, data.frame(cbind(results_f[6,i][[1]][[2]],results_f[6,i][[1]][[3]],paste0('component',i))))
}
colnames(f_EG1_ale) = c('EG1','y','component')
f_EG1_ale$y = as.numeric(as.character(f_EG1_ale$y))
mean_f_EG1 =  f_EG1_ale %>% group_by(EG1) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_EG1$EG1 = as.numeric(as.character(mean_f_EG1$EG1))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_EG1.png", width = 800, height = 500)
ggplot(data = mean_f_EG1, aes(x = EG1, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle(expression(paste('Fragment 2,6 ',E[g]))) + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab(expression(paste('2,6 ',E[g],' (eV)'))) + geom_rug(data = data.frame(x_train = fragments$`2,6 EG`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment Lmax1
f_Lmax1_ale = data.frame(cbind(results_f[7,1][[1]][[2]],results_f[7,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_Lmax1_ale = rbind(f_Lmax1_ale, data.frame(cbind(results_f[7,i][[1]][[2]],results_f[7,i][[1]][[3]],paste0('component',i))))
}
colnames(f_Lmax1_ale) = c('Lmax1','y','component')
f_Lmax1_ale$y = as.numeric(as.character(f_Lmax1_ale$y))
mean_f_Lmax1 =  f_Lmax1_ale %>% group_by(Lmax1) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_Lmax1$Lmax1 = as.numeric(as.character(mean_f_Lmax1$Lmax1))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_Lmax1.png", width = 800, height = 500)
ggplot(data = mean_f_Lmax1, aes(x = Lmax1, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle(expression(paste('Fragment 2,6 ',lambda['max']^'abs'))) + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab(expression(paste('2,6 ',lambda['max']^'abs',' (nm)'))) + geom_rug(data = data.frame(x_train = fragments$`2,6 Lmax`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment KK1
f_KK1_ale = data.frame(cbind(results_f[8,1][[1]][[2]],results_f[8,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_KK1_ale = rbind(f_KK1_ale, data.frame(cbind(results_f[8,i][[1]][[2]],results_f[8,i][[1]][[3]],paste0('component',i))))
}
colnames(f_KK1_ale) = c('KK1','y','component')
f_KK1_ale$y = as.numeric(as.character(f_KK1_ale$y))
mean_f_KK1 =  f_KK1_ale %>% group_by(KK1) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_KK1$KK1 = as.numeric(as.character(mean_f_KK1$KK1))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_KK1.png", width = 800, height = 500)
ggplot(data = mean_f_KK1, aes(x = KK1, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Fragment 2,6 KK") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('2,6 KK') + geom_rug(data = data.frame(x_train = fragments$`2,6 KK`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment HOMO2
f_HOMO2_ale = data.frame(cbind(results_f[9,1][[1]][[2]],results_f[9,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_HOMO2_ale = rbind(f_HOMO2_ale, data.frame(cbind(results_f[9,i][[1]][[2]],results_f[9,i][[1]][[3]],paste0('component',i))))
}
colnames(f_HOMO2_ale) = c('HOMO2','y','component')
f_HOMO2_ale$y = as.numeric(as.character(f_HOMO2_ale$y))
mean_f_HOMO2 =  f_HOMO2_ale %>% group_by(HOMO2) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_HOMO2$HOMO2 = as.numeric(as.character(mean_f_HOMO2$HOMO2))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_HOMO2.png", width = 800, height = 500)
ggplot(data = mean_f_HOMO2, aes(x = HOMO2, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Fragment 4,8 HOMO") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('4,8 HOMO (eV)') + geom_rug(data = data.frame(x_train = fragments$`4,8 HOMO`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment EG2
f_EG2_ale = data.frame(cbind(results_f[10,1][[1]][[2]],results_f[10,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_EG2_ale = rbind(f_EG2_ale, data.frame(cbind(results_f[10,i][[1]][[2]],results_f[10,i][[1]][[3]],paste0('component',i))))
}
colnames(f_EG2_ale) = c('EG2','y','component')
f_EG2_ale$y = as.numeric(as.character(f_EG2_ale$y))
mean_f_EG2 =  f_EG2_ale %>% group_by(EG2) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_EG2$EG2 = as.numeric(as.character(mean_f_EG2$EG2))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_EG2.png", width = 800, height = 500)
ggplot(data = mean_f_EG2, aes(x = EG2, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle(expression(paste('Fragment 4,8 ',E[g]))) + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab(expression(paste('4,8 ',E[g],' (eV)'))) + geom_rug(data = data.frame(x_train = fragments$`4,8 EG`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment Lmax2
f_Lmax2_ale = data.frame(cbind(results_f[11,1][[1]][[2]],results_f[11,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_Lmax2_ale = rbind(f_Lmax2_ale, data.frame(cbind(results_f[11,i][[1]][[2]],results_f[11,i][[1]][[3]],paste0('component',i))))
}
colnames(f_Lmax2_ale) = c('Lmax2','y','component')
f_Lmax2_ale$y = as.numeric(as.character(f_Lmax2_ale$y))
mean_f_Lmax2 =  f_Lmax2_ale %>% group_by(Lmax2) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_Lmax2$Lmax2 = as.numeric(as.character(mean_f_Lmax2$Lmax2))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_Lmax2.png", width = 800, height = 500)
ggplot(data = mean_f_Lmax2, aes(x = Lmax2, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle(expression(paste('Fragment 4,8 ',lambda['max']^'abs'))) + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab(expression(paste('4,8 ',lambda['max']^'abs',' (nm)'))) + geom_rug(data = data.frame(x_train = fragments$`4,8 Lmax`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment KK2
f_KK2_ale = data.frame(cbind(results_f[12,1][[1]][[2]],results_f[12,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_KK2_ale = rbind(f_KK2_ale, data.frame(cbind(results_f[12,i][[1]][[2]],results_f[12,i][[1]][[3]],paste0('component',i))))
}
colnames(f_KK2_ale) = c('KK2','y','component')
f_KK2_ale$y = as.numeric(as.character(f_KK2_ale$y))
mean_f_KK2 =  f_KK2_ale %>% group_by(KK2) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_KK2$KK2 = as.numeric(as.character(mean_f_KK2$KK2))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_KK2.png", width = 800, height = 500)
ggplot(data = mean_f_KK2, aes(x = KK2, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Fragment 4,8 KK") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)')))+ xlab('4,8 KK') + geom_rug(data = data.frame(x_train = fragments$`4,8 KK`), aes(x = x_train), sides = "b", inherit.aes = F)
dev.off()



#fragment Solvent
f_Solvent_ale = data.frame(cbind(results_f[13,1][[1]][[2]],results_f[13,1][[1]][[3]],paste0('component',1)))
for (i in 2:100) {
  f_Solvent_ale = rbind(f_Solvent_ale, data.frame(cbind(results_f[13,i][[1]][[2]],results_f[13,i][[1]][[3]],paste0('component',i))))
}
colnames(f_Solvent_ale) = c('Solvent','y','component')
f_Solvent_ale$y = as.numeric(as.character(f_Solvent_ale$y))
mean_f_Solvent =  f_Solvent_ale %>% group_by(Solvent) %>% summarise(mean_y = mean(y), lower = quantile(y, 0.05), upper = quantile(y, 0.95))
mean_f_Solvent$Solvent = as.numeric(as.character(mean_f_Solvent$Solvent))
png("/projectnb/darpa/pyuning/frag modeling/Ensemble models/ALE plots/ale_univariate/f_Solvent.png", width = 800, height = 500)
ggplot(data = mean_f_Solvent, aes(x = Solvent, y = mean_y)) + geom_hline(yintercept=0, color = 'darkgrey', size = 2) + ggtitle("Fragment Solvent") + theme(plot.title = element_text(hjust = 0.5)) + geom_line(col = 'red', size = 2) + ylim(-60, 60) + theme(legend.position = "none",text = element_text(size = 30)) + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "blank",fill = 'darkblue') + ylab(expression(paste(Delta,'\ Prediction (nm)'))) + xlab('Solvent')
dev.off()