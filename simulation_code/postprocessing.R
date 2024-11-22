
setwd('simulation_output/')

scenarios <- c("Baseline", "Scen1", "Scen2", "Scen3", "Scen4", "Scen5", "Scen6", "Scen7", "Scen8", "Scen9")

###-----------------------------------------------------------------------------------
### PROBABILITY OF ONWARD TRANSMISSION

mean.propOnward <- c()
mean.propOnwardNoSeed <- c()
mean.propOnwardAfterIsol <- c()

for(s in scenarios){

  datfile <- paste0(s,"_onward_transmission.csv")
  dat <- read.csv(datfile, header = T)
  dat <- dat[,-1]
  
  mean.propOnward <- c(mean.propOnward, mean(dat$propOnward, na.rm = T))
  mean.propOnwardNoSeed <- c(mean.propOnwardNoSeed, mean(dat$propOnwardNoSeed, na.rm = T))
  mean.propOnwardAfterIsol <- c(mean.propOnwardAfterIsol, mean(dat$propOnwardAfterIsol, na.rm = T))
  
  ## Data for boxplots
  mat.propOnward <- matrix(NA, nrow = 1000, ncol = 4) # 1 = scenario, 2 = propOnward, 3 = propOnwardAfterIsol
  # mat.propOnward <- matrix(NA, nrow = 1000, ncol = 3) # 1 = scenario, 2 = propOnward, 3 = propOnwardAfterIsol
  ss <- ifelse(s %in% c('Baseline','Scen1','Scen2','Scen3','Scen4'), 'No heterogeneity', 'Heterogeneity')
  scen <- s
  if(ss == 'Heterogeneity'){
    if(s == 'Scen5'){
      scen <- 'Baseline'
    }else if(s == 'Scen6'){
      scen <- 'Scen1'
    }else if(s == 'Scen7'){
      scen <- 'Scen2'
    }else if(s == 'Scen8'){
      scen <- 'Scen3'
    }else if(s == 'Scen9'){
      scen <- 'Scen4'
    }
  }
  for(i in 1:dim(dat)[1]){
    mat.propOnward[i, 1] <- scen
    mat.propOnward[i, 2] <- as.numeric(dat$propOnward[i])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
    mat.propOnward[i, 4] <- ss
  }
  
  if(s == 'Baseline'){
    df.all <- mat.propOnward
  }else{
    df.all <- rbind(df.all, mat.propOnward)
  }

}

df.onward <- data.frame(Scenario = scenarios,
                        propOnward = mean.propOnward,
                        propOnwardNoSeed = mean.propOnwardNoSeed,
                        propOnwardAfterIsol = mean.propOnwardAfterIsol
)
df.onward
df.onward[6:10,]

df.all <- as.data.frame(df.all)
df.all[,2] <- as.numeric(df.all[,2])
df.all[,3] <- as.numeric(df.all[,3])
names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'Heterogeneity')
# table(df.all$Scenario, df.all$nSeed)

library(ggplot2)
ggplot(df.all, aes(x = Heterogeneity, y = propOnward, fill = Scenario)) +
  geom_boxplot() +
  xlab('') + ylab('Proportion of all cases causing onward transmission') +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('../Figure1.jpeg', width = 40, height = 20, units = 'cm', dpi = 300)

# ggplot(df.all, aes(x = Scenario, y = propOnward, fill = Scenario)) +
#   geom_boxplot() +
#   xlab('') + ylab('Proportion of cases causing onward transmission')

# summary(df.all$propOnwardAfterIsol[df.all$nSeed=='seed1' & df.all$Scenario=='Scen1'])
# summary(df.all$propOnwardAfterIsol[df.all$nSeed=='seed1' & df.all$Scenario=='Scen2'])
# summary(df.all$propOnwardAfterIsol[df.all$nSeed=='seed1' & df.all$Scenario=='Scen6'])
# ggplot(df.all[df.all$Scenario != 'Baseline',], aes(x = Scenario, y = propOnwardAfterIsol, fill = Scenario)) +
#   geom_boxplot() + #ylim(0,0.02) +
#   xlab('') + ylab('Proportion of exposed individuals causing onward transmission AFTER isolation') 

ggplot(df.all[df.all$Scenario != 'Baseline',], aes(x = Heterogeneity, y = propOnwardAfterIsol, fill = Scenario)) +
  geom_boxplot() + #ylim(0,0.02) +
  xlab('') + ylab('Proportion of exposed individuals causing onward transmission AFTER isolation')+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('../Figure2.jpeg', width = 40, height = 20, units = 'cm', dpi = 300)

sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Baseline' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen1' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen2' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen3' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen4' & df.all$Heterogeneity == 'No heterogeneity']>0.01, na.rm = T)/1000

