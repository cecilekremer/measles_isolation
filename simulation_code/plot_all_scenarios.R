
setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school/')

# scenarios <- c("Baseline", "Scen1", "Scen2", "Scen3", "Scen4", "Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
#                paste0('Sens',1:9,'_baseline'),
#                paste0('Sens',1:9,'_scen1'),
#                paste0('Sens',1:9,'_scen2'),
#                paste0('Sens',1:9,'_scen3'),
#                paste0('Sens',1:9,'_scen4'),
#                paste0('Sens',1:9,'_scen5'),
#                paste0('Sens',1:9,'_scen6'),
#                paste0('Sens',1:9,'_scen7'),
#                paste0('Sens',1:9,'_scen8'),
#                paste0('Sens',1:9,'_scen9')
# )

## Only scenarios including heterogeneity
scenariosHet <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
                  paste0('Sens',1:9,'_scen5'),
                  paste0('Sens',1:9,'_scen6'),
                  paste0('Sens',1:9,'_scen7'),
                  paste0('Sens',1:9,'_scen8'),
                  paste0('Sens',1:9,'_scen9')
)

for(s in scenariosHet){
  
  datfile <- paste0(s,"_onward_transmission.csv") # proportions for each simulation run
  dat <- read.csv(datfile, header = T)
  dat <- dat[,-1]
  
  dat2 <- read.csv(paste0(s,"_outbreak_characteristics.csv"))
  dat2 <- dat2[,-1]
  
  # finSize without index-generated cases
  dat3 <- readRDS(paste0('contact_networks_', s, '.rds'))
  finsize.tert <- c()
  finsize.tert.prop <- c()
  # finsize.class <- c()
  # max.finsize.class <- c()
  for(i in 1:10000){
    dd <- dat3[[i]]
    ind <- which(dd$IsIndex == 1)
    ntert <- length(dd$ID[!is.na(dd$InfectorID) & dd$InfectorID != ind])
    nsec.index <- length(dd$ID[!is.na(dd$InfectorID) & dd$InfectorID == ind])
    
    # if(ntert > 1){
    #   finsize.tert[i] <- (ntert - 1) / (dim(dd)[1])
    # }else{
    #   finsize.tert[i] <- 0
    # }
    finsize.tert[i] <- ntert
    finsize.tert.prop[i] <- ntert / (dim(dd)[1] - nsec.index - 1)
    
    # for(c in unique(dd$StudentClass)){
    #   finsize.class <- c(finsize.class, sum(!is.na(dd$InfectionTime[dd$StudentClass==c]))/(sum(dd$StudentClass==c)-1))
    # }
    # max.finsize.class[i] <- max(finsize.class, na.rm = T)
  }
  
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 12)
  
  if(grepl('cen5', s)){
    scenario <- 'Baseline'
  }else if(grepl('cen6', s)){
    scenario <- 'Scen1'
  }else if(grepl('cen7', s)){
    scenario <- 'Scen2'
  }else if(grepl('cen8', s)){
    scenario <- 'Scen3'
  }else if(grepl('cen9', s)){
    scenario <- 'Scen4'
  }
  
  if(grepl('Sens1', s)){
    analysis <- 'SA1'
  }else if(grepl('Sens2', s)){
    analysis <- 'SA2'
  }else if(grepl('Sens3', s)){
    analysis <- 'SA3'
  }else if(grepl('Sens4', s)){
    analysis <- 'SA4'
  }else if(grepl('Sens5', s)){
    analysis <- 'SA5'
  }else if(grepl('Sens6', s)){
    analysis <- 'SA6'
  }else if(grepl('Sens7', s)){
    analysis <- 'SA7'
  }else if(grepl('Sens8', s)){
    analysis <- 'SA8'
  }else if(grepl('Sens9', s)){
    analysis <- 'SA9'
  }else{
    analysis <- 'Default'
  }
  
  for(i in 1:dim(dat)[1]){
    mat.propOnward[i, 1] <- scenario
    mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[i])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
    mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[i])
    mat.propOnward[i, 5] <- analysis
    mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[i])
    mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[i])
    mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[i])
    mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[i])
    mat.propOnward[i, 10] <- as.numeric(dat$R_index[i])
    mat.propOnward[i, 11] <- as.numeric(finsize.tert[i])
    mat.propOnward[i, 12] <- as.numeric(finsize.tert.prop[i])
    # mat.propOnward[i, 11] <- as.numeric(dat$propOnwardAfterPEV[i])
    # mat.propOnward[i, 12] <- as.numeric(dat$propEffectivePEV[i])
    # mat.propOnward[i, 13] <- as.numeric(dat$propReceivedPEV[i])
    # mat.propOnward[i, 14] <- as.numeric(dat$propSuscReceivedPEV[i])
  }
  
  ## Combine output from all analyses
  if(s == scenariosHet[1]){
    df.all <- mat.propOnward
  }else{
    df.all <- rbind(df.all, mat.propOnward)
  }
  
}

dim(df.all)
df.all <- as.data.frame(df.all)
df.all[,2] <- as.numeric(df.all[,2])
df.all[,3] <- as.numeric(df.all[,3])
df.all[,4] <- as.numeric(df.all[,4])
df.all[,6] <- as.numeric(df.all[,6])
df.all[,7] <- as.numeric(df.all[,7])
df.all[,8] <- as.numeric(df.all[,8])
df.all[,9] <- as.numeric(df.all[,9])
df.all[,10] <- as.numeric(df.all[,10])
df.all[,11] <- as.numeric(df.all[,11])
df.all[,12] <- as.numeric(df.all[,12])
names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'Analysis',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'R_index', 'FinSizeTert', 'FinSizeTertProp')
head(df.all)

library(dplyr)
library(forcats)
df.all <- df.all %>%
  mutate(Scenario = fct_recode(Scenario, "Baseline" = "Baseline", "A1" = "Scen1", "A2" = "Scen2",
                               "A3" = "Scen3", "A4" = "Scen4"))

library(ggplot2)
ggplot(df.all[(df.all$Analysis == 'Default' | df.all$Analysis == 'SA3') & df.all$Scenario != 'Baseline', ],
       aes(x = Scenario, y = pInfectiousAfterIsol, fill = Scenario)) +
  geom_boxplot() +
  facet_wrap(~Analysis) #+
  # ylim(0, 0.2)

ggplot(df.all[df.all$Analysis != 'Default', ], aes(x = Analysis, y = propOnwardAfterIsol, fill = Scenario)) +
  geom_boxplot()

## Add results with post-exposure vaccination

df.all$propOnwardAfterPEV <- NA
df.all$propOnwardNoPEV <- NA
df.all$propEffectivePEV <- NA
df.all$propReceivedPEV <- NA
df.all$propSuscReceivedPEV <- NA

# scenariosHetPEV <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
#                   paste0('Sens',1:5,'_scen5'),
#                   paste0('Sens',1:5,'_scen6'),
#                   paste0('Sens',1:5,'_scen7'),
#                   paste0('Sens',1:5,'_scen8'),
#                   paste0('Sens',1:5,'_scen9')
# )
scenariosHetPEV <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9",
                     paste0('Sens',c(1:4, 6:7),'_scen5'),
                     paste0('Sens',c(1:4, 6:7),'_scen6'),
                     paste0('Sens',c(1:4, 6:7),'_scen7'),
                     paste0('Sens',c(1:4, 6:7),'_scen8'),
                     paste0('Sens',c(1:4, 6:7),'_scen9')
)

setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV/')
for(s in scenariosHetPEV){
  
  datfile <- paste0(s,"_onward_transmission.csv") # proportions for each simulation run
  dat <- read.csv(datfile, header = T)
  dat <- dat[,-1]
  
  dat2 <- read.csv(paste0(s,"_outbreak_characteristics.csv"))
  dat2 <- dat2[,-1]
  
  # finSize without index-generated cases
  dat3 <- readRDS(paste0('contact_networks_', s, '.rds'))
  finsize.tert <- c()
  finsize.tert.prop <- c()
  # finsize.class <- c()
  # max.finsize.class <- c()
  for(i in 1:10000){
    dd <- dat3[[i]]
    ind <- which(dd$IsIndex == 1)
    ntert <- length(dd$ID[!is.na(dd$InfectorID) & dd$InfectorID != ind])
    nsec.index <- length(dd$ID[!is.na(dd$InfectorID) & dd$InfectorID == ind])
    
    # if(ntert > 1){
    #   finsize.tert[i] <- (ntert - 1) / (dim(dd)[1])
    # }else{
    #   finsize.tert[i] <- 0
    # }
    finsize.tert[i] <- ntert
    finsize.tert.prop[i] <- ntert / (dim(dd)[1] - nsec.index - 1)
    
    # for(c in unique(dd$StudentClass)){
    #   finsize.class <- c(finsize.class, sum(!is.na(dd$InfectionTime[dd$StudentClass==c]))/(sum(dd$StudentClass==c)-1))
    # }
    # max.finsize.class[i] <- max(finsize.class, na.rm = T)
  }
  
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 17)
  
  if(grepl('cen5', s)){
    scenario <- 'Baseline'
  }else if(grepl('cen6', s)){
    scenario <- 'Scen1'
  }else if(grepl('cen7', s)){
    scenario <- 'Scen2'
  }else if(grepl('cen8', s)){
    scenario <- 'Scen3'
  }else if(grepl('cen9', s)){
    scenario <- 'Scen4'
  }
  
  if(grepl('Sens1', s)){
    analysis <- 'SA1'
  }else if(grepl('Sens2', s)){
    analysis <- 'SA2'
  }else if(grepl('Sens3', s)){
    analysis <- 'SA3'
  }else if(grepl('Sens6', s)){
    analysis <- 'SA4'
  # }else if(grepl('Sens5', s)){
  #   analysis <- 'SA5'
  }else if(grepl('Sens7', s)){
   analysis <- 'SA5' 
  }else{
    analysis <- 'Default'
  }
  
  for(i in 1:dim(dat)[1]){
    # if(as.numeric(dat$propOnwardNoPEV[i]==12)){print(i); stop()}
    mat.propOnward[i, 1] <- scenario
    mat.propOnward[i, 2] <- as.numeric(dat$propOnwardNoSeed[i])
    mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
    mat.propOnward[i, 4] <- as.numeric(dat$pInfectiousAfterIsol[i])
    mat.propOnward[i, 5] <- analysis
    mat.propOnward[i, 6] <- as.numeric(dat2$propDetected[i])
    mat.propOnward[i, 7] <- as.numeric(dat2$propSuscIsolated[i])
    mat.propOnward[i, 8] <- as.numeric(dat2$FinSize[i])
    mat.propOnward[i, 9] <- as.numeric(dat2$FinSizeProp[i])
    mat.propOnward[i, 10] <- as.numeric(dat$R_index[i])
    mat.propOnward[i, 11] <- as.numeric(dat$propOnwardAfterPEV[i])
    mat.propOnward[i, 12] <- as.numeric(dat$propEffectivePEV[i])
    mat.propOnward[i, 13] <- as.numeric(dat$propReceivedPEV[i])
    mat.propOnward[i, 14] <- as.numeric(dat$propSuscReceivedPEV[i])
    mat.propOnward[i, 15] <- as.numeric(finsize.tert[i])
    mat.propOnward[i, 16] <- as.numeric(finsize.tert.prop[i])
    mat.propOnward[i, 17] <- as.numeric(dat$propOnwardNoPEV[i])
  }
  
  ## Combine output from all analyses
  if(s == scenariosHetPEV[1]){
    df.allPEV <- mat.propOnward
  }else{
    df.allPEV <- rbind(df.allPEV, mat.propOnward)
  }  
  
}

dim(df.allPEV)
df.allPEV <- as.data.frame(df.allPEV)
df.allPEV[,2] <- as.numeric(df.allPEV[,2])
df.allPEV[,3] <- as.numeric(df.allPEV[,3])
df.allPEV[,4] <- as.numeric(df.allPEV[,4])
df.allPEV[,6] <- as.numeric(df.allPEV[,6])
df.allPEV[,7] <- as.numeric(df.allPEV[,7])
df.allPEV[,8] <- as.numeric(df.allPEV[,8])
df.allPEV[,9] <- as.numeric(df.allPEV[,9])
df.allPEV[,10] <- as.numeric(df.allPEV[,10])
df.all[,11] <- as.numeric(df.all[,11])
df.all[,12] <- as.numeric(df.all[,12])
df.all[,13] <- as.numeric(df.all[,13])
df.all[,14] <- as.numeric(df.all[,14])
df.all[,15] <- as.numeric(df.all[,15])
df.all[,16] <- as.numeric(df.all[,16])
df.all[,17] <- as.numeric(df.all[,17])
names(df.allPEV) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'Analysis',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'R_index',
                   'propOnwardAfterPEV', 'propEffectivePEV', 'propReceivedPEV', 'propSuscReceivedPEV', 'FinSizeTert', 'FinSizeTertProp',
                   'propOnwardNoPEV')
head(df.allPEV)

df.all$PEV <- 'No PEV'
df.allPEV$PEV <- 'PEV'

library(dplyr)
library(forcats)
df.allPEV <- df.allPEV %>%
  mutate(Scenario = fct_recode(Scenario, "Baseline" = "Baseline", "A1" = "Scen1", "A2" = "Scen2",
                               "A3" = "Scen3", "A4" = "Scen4"))

# summary(as.numeric(df.allPEV$propOnwardAfterPEV))
# summary(as.numeric(df.allPEV$propOnwardNoPEV))
# table(df.allPEV$Scenario[df.allPEV$propOnwardNoPEV > 1])
# table(df.allPEV$Analysis[df.allPEV$propOnwardNoPEV > 1])

summary(as.numeric(df.allPEV$propEffectivePEV))
summary(as.numeric(df.allPEV$propReceivedPEV)) # proportion of infected children that received PEV

ggplot(df.allPEV[df.allPEV$Scenario != 'Baseline',], 
       aes(x = Analysis, y = as.numeric(propReceivedPEV), fill = Scenario)) +
  geom_boxplot()


df.allPEV <- df.allPEV[,c(1:10,15,16,11,17,12,13,14,15,18)]
df.allPEV <- df.allPEV[,c(1:17,19)]

df.combined <- rbind(df.all, df.allPEV)

library(ggplot2)
ggplot(df.combined[df.combined$Analysis == 'Default', ], aes(x = PEV, y = as.numeric(FinSizeTertProp), fill = Scenario)) +
  geom_boxplot() 
  # facet_wrap(~Analysis)
# ggplot(df.combined[df.combined$PEV=='PEV', ], aes(x = Analysis, y = as.numeric(propOnwardNoPEV), fill = Scenario)) +
#   geom_boxplot()


## Figure 2: PEV (Default, SA5) vs no PEV (Default, SA2)
df.plot <- df.combined[df.combined$Analysis=='Default' | (df.combined$PEV == 'PEV' & df.combined$Analysis == 'SA4') |
                         (df.combined$PEV == 'No PEV' & df.combined$Analysis == 'SA2'), ]
df.plot$Analysis[df.plot$Analysis != 'Default'] <- '70% of children immune'
df.plot$Analysis[df.plot$Analysis == 'Default'] <- '~93% of children immune'

p2 <- ggplot(df.plot,
       aes(x = PEV, y = as.numeric(propOnward), fill = Scenario)) +
  geom_boxplot() +
  facet_wrap(~Analysis) +
  xlab('') + ylab('Proportion of secondary cases causing onward transmission') +
  theme(legend.position = 'top',
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        title = element_text(size = 32),
        strip.text.x = element_text(size = 32))
p2
ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/Figure2_final.jpeg', p2, width = 50, height = 35, units = 'cm', dpi = 300)





p1 <- ggplot(df.combined[df.combined$Analysis=='Default', ],
             aes(x = PEV, y = propOnward, fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(a)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of cases (excl. index) causing onward transmission') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))
p1

p2 <- ggplot(df.combined[df.combined$Analysis=='Default', ],
             aes(x = PEV, y = pInfectiousAfterIsol, fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(b)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of infected contacts that are infectious after quarantine') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

p3 <- ggplot(df.combined[df.combined$Analysis=='Default', ],
       aes(x = PEV, y = FinSizeProp, fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(c)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size (proportion of children that got infected, excl. index)') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

p4 <- ggplot(df.combined[df.combined$Analysis=='Default', ],
             aes(x = PEV, y = as.numeric(FinSizeTertProp), fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(d)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size excluding index-generated cases') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

library(gridExtra); library(ggpubr); library(grid)
# pp <- grid.arrange(p1, p2, ncol = 2)
pp <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = 'top')#, font.label = list(size = 30))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/PEVvsNoPEV.jpeg', pp, width = 70, height = 50, units = 'cm', dpi = 300)

##-----------------------------------------------------------------
## Sens. analysis with 20% immune children

p1 <- ggplot(df.combined[(df.combined$Analysis=='SA3' & df.combined$PEV == 'No PEV') | (df.combined$Analysis == 'SA2' & df.combined$PEV == 'PEV'), ],
             aes(x = PEV, y = propOnward, fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(a)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of cases (excl. index) causing onward transmission') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

p2 <- ggplot(df.combined[(df.combined$Analysis=='SA3' & df.combined$PEV == 'No PEV') | (df.combined$Analysis == 'SA2' & df.combined$PEV == 'PEV'), ],
             aes(x = PEV, y = pInfectiousAfterIsol, fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(b)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of infected contacts that are infectious after quarantine') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

p3 <- ggplot(df.combined[(df.combined$Analysis=='SA3' & df.combined$PEV == 'No PEV') | (df.combined$Analysis == 'SA2' & df.combined$PEV == 'PEV'), ],
             aes(x = PEV, y = FinSizeProp, fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(c)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size (proportion of children that got infected, excl. index)') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

p4 <- ggplot(df.combined[(df.combined$Analysis=='SA3' & df.combined$PEV == 'No PEV') | (df.combined$Analysis == 'SA2' & df.combined$PEV == 'PEV'), ],
             aes(x = PEV, y = as.numeric(FinSizeTertProp), fill = Scenario)) +
  geom_boxplot() +
  ggtitle('(d)') +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Final size excluding index-generated cases') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))

library(gridExtra); library(ggpubr); library(grid)
# pp <- grid.arrange(p1, p2, ncol = 2)
pp <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = 'top')#, font.label = list(size = 30))
ggsave('~/GitHub/measles_isolation/simulation_output/figures/PEVvsNoPEV_sensImm.jpeg', pp, width = 70, height = 50, units = 'cm', dpi = 300)
