
setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_cutoff_delayQ/')

## Only scenarios including heterogeneity
scenariosHet <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9", "Scen10",
                  paste0('Sens',1:9,'_scen5'),
                  paste0('Sens',1:9,'_scen6'),
                  paste0('Sens',1:9,'_scen7'),
                  paste0('Sens',1:9,'_scen8'),
                  paste0('Sens',1:9,'_scen9'),
                  paste0('Sens',1:9,'_scen10')
)

# scenariosHet <- scenariosHet[(grepl('Sens1', scenariosHet))]
# scenariosHet <- c("Scen5", "Scen6", "Scen7", "Scen8", "Scen9", scenariosHet)

# write.csv(scenariosHet, file = '~/GitHub/measles_isolation/simulation_code/simSettingsAll2.csv', quote = F, row.names = F)

# ### Individual profile
# dat <- read.csv("Sens5_scen10_onward_transmission.csv", header = T)
# dat2 <- read.csv("Sens5_scen10_outbreak_characteristics.csv")
# dat2 <- dat2[,-1]
# summary(dat2$FinSizeProp)
# dat2$simID[dat2$FinSizeProp == max(dat2$FinSizeProp)]
# simid = 292
# dat3 <- readRDS('contact_networks_Sens5_scen10.rds')
# contactNet <- dat3[[simid]]
# infectionNet <- contactNet[!is.na(contactNet$InfectionTime), ]
# infectionNet$inf.start <- infectionNet$RashOnsDate - 4
# infectionNet$inf.end <- infectionNet$RashOnsDate + 4
# 
# dat4 <- readRDS('epi_outbreak_Sens5_scen10.rds')
# epi.out <- dat4[[simid]]
# events <- as.data.frame(epi.out$time.events[-1,])
# infectionNet$isolation.contact <- NA
# infectionNet$isolation.inf <- NA
# for(i in 1:dim(infectionNet)[1]){
#   id <- infectionNet$ID[i]
#   inf.day <- as.numeric(events$time[events$event == 1 & events$who == id])
#   isol.day <- as.numeric(events$time[events$event == 'IsolationContact' & events$who == id])
#   if(length(isol.day) > 0){
#     isol.day <- isol.day[which(isol.day > inf.day)]
#     infectionNet$isolation.contact[i] <- isol.day[1]
#   }
#   isol.inf.day <- as.numeric(events$time[events$event == 'Isolation' & events$who == id])
#   if(length(isol.inf.day) > 0){
#     infectionNet$isolation.inf[i] <- isol.inf.day
#   }
# }
# sum(infectionNet$isolation.contact > infectionNet$inf.start, na.rm = T)
# 
# contactNet$vaccinated <- ifelse(contactNet$isImmune == 1 & is.na(contactNet$InfectionTime), 1, 0)
# table(contactNet$vaccinated)
# table(contactNet$InfectorID[contactNet$vaccinated==0])
# contactNet$ID[contactNet$IsIndex==1]
# contactNet$StudentClass[contactNet$ID==103]
# table(contactNet$vaccinated[contactNet$StudentClass==6])
# contactNet$InfectorID[contactNet$ID==103]
# contactNet$StudentClass[contactNet$ID==57]
# 
# events[events$who==91,]
# contactNet$StudentClass[contactNet$ID==91]
# table(contactNet$isImmune[contactNet$StudentClass==5])
# contactNet$ID[contactNet$isImmune==0 & contactNet$StudentClass==5]
# events[events$who==94,] # susceptible contact isolated during infectious period of case?
# epi.out$status.matrix$q[epi.out$status.matrix$id==91]
# infectionNet[infectionNet$ID==91,]
# summary(epi.out$status.matrix$q)
# 
# library(epicontacts)
# epiCases <- infectionNet[,c(1,2,5,6,7,12,13,14,15)]
# epiContacts <- infectionNet[,c(6,1)]
# cons_tree <- make_epicontacts(epiCases, epiContacts, directed = TRUE)
# plot(cons_tree)
# library(ggplot2)
# library(igraph)
# library(adegenet)
# library(EpiEstim)
# library(visNetwork)
# # pNet <- vis_temporal_static(cons_tree)
# 
# # ggplot(epiCases, aes(x = InfectionTime, y = factor(ID))) +
# #   geom_point(size = 3) +
# #   geom_segment(aes(xend = InfectionTime, yend = factor(InfectorID)), 
# #                arrow = arrow(length = unit(0.2, "cm")), color = 'blue', na.rm = T)
# library(ggraph)
# edges <- na.omit(epiContacts)
# library(igraph)
# library(dplyr)
# g <- graph_from_data_frame(edges, vertices = epiCases, directed = T)
# V(g)$infectionTime <- epiCases$InfectionTime#[match(V(g)$ID, epiCases$ID)]
# layout <- data.frame(x = V(g)$InfectionTime, y = seq_along(V(g)))
# # layout <- layout[order(layout$x), ]
# p <- ggraph(g, layout = 'manual', x = layout$x, y = layout$y) +
#   geom_edge_link(arrow = arrow(length = unit(0.2, 'cm')), end_cap = circle(3, 'mm')) +
#   geom_node_point(size = 5, color = 'blue') +
#   geom_node_text(aes(label = name), vjust = -0.5) +
#   geom_segment(data = epiCases, aes(x = inf.start, xend = inf.end, y = seq_along(ID), yend = seq_along(ID)), color = 'red', 
#                linetype = 'solid',
#                linewidth = 1) +
#   geom_point(data = epiCases, aes(x = isolation.contact, y = seq_along(ID)), color = 'purple', shape = 17,
#              size = 4) +
#   geom_point(data = epiCases, aes(x = isolation.inf, y = seq_along(ID)), color = 'red', shape = 17,
#              size = 4) +
#   # geom_segment(data = epiCases, aes(x = inf.start, xend = inf.end, y = seq_along(ID), yend = seq_along(ID), color = 'infectious period', 
#   #                                   linetype = 'infectious period'),
#   #              linewidth = 1) +
#   # geom_point(data = epiCases, aes(x = isolation.contact, y = seq_along(ID), color = 'contact isolation', shape = 'contact isolation'),
#   #            size = 4) +
#   # geom_point(data = epiCases, aes(x = isolation.inf, y = seq_along(ID), color = 'detection/isolation', shape = 'detection/isolation'),
#   #            size = 4) +
#   # scale_color_manual(name = 'Legend',
#   #                    labels = c('infection time', 'detection/isolation', 'contact isolation', 'infectious period'),
#   #                    values = c('blue','red','purple','red'))+
#   #                    # values = c('infection time' = 'blue', 'detection/isolation' = 'red', 'contact isolation' = 'purple',
# #                               # 'infectious period' = 'red')) +
# # scale_shape_manual(name = 'Legend',
# #                    labels = c('infection time', 'detection/isolation', 'contact isolation', 'infectious period'),
# #                    values = c('blue','red','purple','red'))+
# # #                    # values = c('infection time' = 1, 'detection/isolation' = 17, 'contact isolation' = 17, 'infectious period' = NA)) +
# # scale_linetype_manual(name = 'Legend',
# #                       labels = c('infection time', 'detection/isolation', 'contact isolation', 'infectious period'),
# #                       values = c('blue','red','purple','red'))+
# # values = c('infection time' = NA, 'detection/isolation' = NA, 'contact isolation' = NA, 'infectious period' = 'solid')) +
# # guides(color = guide_legend('Legend'), shape = guide_none(), linetype = guide_none()) +
# theme_minimal()
# p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab('') + xlab('Infection time') +
#   ggtitle('60% immune, 1-day isolation of contacts')
# ggsave('~/GitHub/measles_isolation/simulation_output/figures/infectionTimelineSens5Scen10_SA5.jpeg', width = 30, height = 15, units = 'cm')
# 
# grepl('88', contactNet$BetweenClassesCtc[contactNet$ID==57])
# table(contactNet$StudentClass, contactNet$vaccinated)
# infectionNet$ID[infectionNet$StudentClass==3]


###------------------------------------------------------------------------------

mean.propNewWave <- c()

for(s in scenariosHet){
  
  datfile <- paste0(s,"_onward_transmission.csv") # proportions for each simulation run
  dat <- read.csv(datfile, header = T)
  dat <- dat[,-1]
  
  dat2 <- read.csv(paste0(s,"_outbreak_characteristics.csv"))
  dat2 <- dat2[,-1]
  
  # finSize without index-genderated cases
  dat3 <- readRDS(paste0('contact_networks_', s, '.rds'))
  finsize.tert <- c()
  finsize.tert.prop <- c()
  finsize.class <- c()
  max.finsize.class <- c()
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
    
    for(c in unique(dd$StudentClass)){
      finsize.class <- c(finsize.class, sum(!is.na(dd$InfectionTime[dd$StudentClass==c]))/(sum(dd$StudentClass==c)-1))
    }
    max.finsize.class[i] <- max(finsize.class, na.rm = T)
  }
  
  # mean.propNewWave <- c(mean.propNewWave, sum(dat$propOnwardNoSeed > 0, na.rm = T)/1000)
  mat.secondWave <- matrix(NA, nrow = 1, ncol = 3)
  
  mat.propOnward <- matrix(NA, nrow = 10000, ncol = 13)
  
  if(grepl('cen5', s)){
    scenario <- '0 days'
  }else if(grepl('cen6', s)){
    scenario <- '21 days'
  }else if(grepl('cen7', s)){
    scenario <- '18 days'
  }else if(grepl('cen8', s)){
    scenario <- '15 days'
  }else if(grepl('cen9', s)){
    scenario <- '10 days'
  }else if(grepl('cen10', s)){
    scenario <- '1 day'
  }
  
  if(grepl('Sens1', s)){
    analysis <- '90%'
  }else if(grepl('Sens2', s)){
    analysis <- '85%'
  }else if(grepl('Sens3', s)){
    analysis <- '80%'
  }else if(grepl('Sens4', s)){
    analysis <- '70%'
  }else if(grepl('Sens5', s)){
    analysis <- '60%'
  }else if(grepl('Sens6', s)){
    analysis <- '50%'
  }else if(grepl('Sens7', s)){
    analysis <- '40%'
  }else if(grepl('Sens8', s)){
    analysis <- '30%'
  }else if(grepl('Sens9', s)){
    analysis <- '20%'
  }else{
    analysis <- '95%'
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
    # mat.propOnward[i, 11] <- as.numeric(dat$propOnwardAfterPEV[i])
    # mat.propOnward[i, 12] <- as.numeric(dat$propEffectivePEV[i])
    # mat.propOnward[i, 13] <- as.numeric(dat$propReceivedPEV[i])
    # mat.propOnward[i, 14] <- as.numeric(dat$propSuscReceivedPEV[i])
  }
  mat.propOnward[, 11] <- finsize.tert
  mat.propOnward[, 12] <- finsize.tert.prop
  mat.propOnward[, 13] <- max.finsize.class
  
  mat.secondWave[, 1] <- scenario
  mat.secondWave[, 2] <- analysis
  mat.secondWave[, 3] <- sum(dat$propOnwardNoSeed > 0, na.rm = T)/10000
  
  ## Combine output from all analyses
  if(s == scenariosHet[1]){
    df.all <- mat.propOnward
    df.onward <- mat.secondWave
  }else{
    df.all <- rbind(df.all, mat.propOnward)
    df.onward <- rbind(df.onward, mat.secondWave)
  }
  
}

# save.image('WS_cutoff.RData')
load('WS_cutoff.RData')

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
df.all[,13] <- as.numeric(df.all[,13])
names(df.all) <- c('DurationQ', 'propOnward', 'propOnwardAfterIsol', 'pInfectiousAfterIsol', 'VaccCoverage',
                   'pDetected', 'pSuscIsol', 'FinSize', 'FinSizeProp', 'R_index', 'FinSizeTert', 'FinSizeTertProp','FinSizeClass')
head(df.all)

summary(df.all$propOnwardAfterIsol)


## Plot tertiary final size by vaccine coverage (facets) -> more impact of Q at lower vaccine coverage?
library(ggplot2)
ggplot(df.all[df.all$DurationQ!='0 days',], aes(x = DurationQ, y = FinSizeTertProp, fill = DurationQ)) +
  geom_boxplot() +
  facet_wrap(~VaccCoverage)

## Other plots
fig1a <- ggplot(df.all[df.all$DurationQ!='0 days',], aes(x = DurationQ, y = FinSizeTertProp, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('Duration quarantine of exposed susceptible contacts') + ylab('Proportion of later-generation cases') +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 28)) +
  guides(fill = guide_legend(title = "Proportion immune"))
ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/FinSizeTert_VaccQ.jpeg', width = 40, height = 25, units = 'cm', dpi = 100)

fig1aEL <- ggplot(df.all[df.all$DurationQ %in% c('1 day', '18 days', '21 days'),], aes(x = DurationQ, y = FinSizeTertProp, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('Quarantaineduur') + ylab('Uitbraakgrootte') +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 28)) +
  guides(fill = guide_legend(title = "Proportie immuun"))


ggplot(df.all[df.all$DurationQ!='0 days',], aes(x = DurationQ, y = FinSize, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('Duration quarantine of exposed susceptible contacts') + ylab('Final size (number of children that got infected, excl. index)') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))+
  guides(fill = guide_legend(title = "Proportion immune"))
ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/FinSize_VaccQ.jpeg', width = 40, height = 25, units = 'cm', dpi = 100)

ggplot(df.all, aes(x = DurationQ, y = R_index, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('Duration quarantine of exposed susceptible contacts') + ylab('Secondary infections caused by index case') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))+
  guides(fill = guide_legend(title = "Proportion immune"))
ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/R_VaccQ.jpeg', width = 40, height = 25, units = 'cm', dpi = 100)

ggplot(df.all, aes(x = DurationQ, y = pInfectiousAfterIsol, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('Duration quarantine of exposed susceptible contacts') + ylab('Proportion of infected contacts that are infectious after quarantine') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20))+
  guides(fill = guide_legend(title = "Proportion immune"))

ggplot(df.all, aes(x = DurationQ, y = propOnward, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of cases (excl. index) causing onward transmission') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20)) +
  guides(fill = guide_legend(title = "Proportion immune"))
# ggsave('~/GitHub/measles_isolation/simulation_output/figures/pOnward_VaccQ.jpeg', width = 40, height = 25, units = 'cm', dpi = 100)

ggplot(df.all, aes(x = DurationQ, y = propOnwardAfterIsol, fill = VaccCoverage)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', size = 3, color = 'red', shape = 17, position = position_dodge(width = 0.75), show.legend = F) + # mean point
  # geom_boxplot(fatten = NULL) + stat_summary(fun.y = mean, geom = 'errorbar', aes(ymax = ..y.., ymin = ..y.., group = Scenario),
  #                                            width = 0.75, size = 1, linetype = 'solid') +
  xlab('') + ylab('Proportion of cases (excl. index) causing onward transmission') +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20)) +
  guides(fill = guide_legend(title = "Proportion immune"))

###--------------------------------------------------------------------------------------------------------
### Proportion of sims that experience a second wave (i.e. onward transmission not from index)

dim(df.onward)
df.onward <- as.data.frame(df.onward)
df.onward[,3] <- as.numeric(df.onward[,3])
names(df.onward) <- c('DurationQ', 'VaccCoverage', 'pNewWave')
head(df.onward)

ggplot(df.onward[df.onward$DurationQ!='0 days',], aes(x = DurationQ, y = pNewWave, fill = VaccCoverage)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = pNewWave), size = 5, vjust = 0.5, hjust = -0.2, position = position_dodge(width = 0.9), angle = 90) +
  xlab('') + ylab('Proportion simulations with multiple infection waves') +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        legend.position = 'top') +
  guides(fill = guide_legend(title = "Proportion immune"))
# ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/pNewWave_VaccQ.jpeg', width = 40, height = 30, units = 'cm', dpi = 100)
ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/Figure2_v2.pdf', width = 70, height = 50, units = 'cm', dpi = 300)




