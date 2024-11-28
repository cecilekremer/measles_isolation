
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
  
  mean.propOnward <- c(mean.propOnward, median(dat$propOnward, na.rm = T))
  mean.propOnwardNoSeed <- c(mean.propOnwardNoSeed, median(dat$propOnwardNoSeed, na.rm = T))
  mean.propOnwardAfterIsol <- c(mean.propOnwardAfterIsol, median(dat$propOnwardAfterIsol, na.rm = T))
  
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
df.onward[1:5, c(1,2,4)]
df.onward[6:10, c(1,2,4)]

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

sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Baseline' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen1' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen2' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen3' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000
sum(df.all$propOnwardAfterIsol[df.all$Scenario=='Scen4' & df.all$Heterogeneity == 'Heterogeneity']>0.01, na.rm = T)/1000

###--------------------------------------------------------------------------------------------------------------------
### TIME-VARYING EFFECTIVE REPRODUCTION NUMBER

nSim <- 1000
# Rt_out <- matrix(NA, nrow = nSim*length(scenarios), ncol = 6) # cols: Scenario, Heterogeneity, sim, time, Rt_forw, Rt_backw
Rt_out <- data.frame(Scenario = NA, Heterogeneity = NA, sim = NA, time = NA, Rt_forw = NA, Rt_backw = NA)
cnt <- 1
# listplots <- list()
for(s in scenarios){
  
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
  
  datfile <- paste0("epi_outbreak_", s, ".rds")
  out <- readRDS(datfile)
  
  # Rt_forw <- list(); Rt_backw <- list()
  
  selected_sims <- sample(1:nSim, 1000)
  
  # cnt2 <- 1
  for(i_sim in selected_sims){
    
    statusmat <- out[[i_sim]]$status.matrix
    time_range <- seq(min(statusmat$time.of.infection, na.rm = T), max(statusmat$time.of.infection, na.rm = T))
    Rt_res <- data.frame(time = time_range) %>%
      rowwise() %>%
      mutate(
        # count infectors
        infectors = sum(floor(statusmat$time.of.infection) == time, na.rm = T),
        Rt_forw = 0,
        Rt_backw = 0
      )
    for(i in 1:dim(Rt_res)[1]){
      t <- Rt_res$time[i]
      ## Forward Rt
      n.infectors <- sum(floor(statusmat$time.of.infection[!is.na(statusmat$time.of.infection)]) == t)
      infectors <- which(floor(statusmat$time.of.infection) == t)
      # infectees
      n.infectees <- sum(statusmat$infector %in% infectors, na.rm = T)
      if(n.infectors > 0){
        Rt_res$Rt_forw[i] <- n.infectees / n.infectors
      }
      ## Backward Rt
      infectors <- which((statusmat$TimeRashOnset - 4) <= t & (statusmat$TimeRashOnset + 4) >= t)
      n.infectors <- length(infectors)
      n.infectees <- sum(statusmat$infector %in% infectors, na.rm = T)
      if(n.infectors > 0){
        Rt_res$Rt_backw[i] <- n.infectees / n.infectors
      }
    }
    # cnt2 <- cnt2 + 1
    Rt_res$Scenario <- scen
    Rt_res$sim <- i_sim
    Rt_res$Heterogeneity <- ss
    Rt_res <- Rt_res[,c(5,7,6,1,3,4)]
    
    Rt_out <- rbind(Rt_out, Rt_res)
    
  }
  
  # Rt_out[cnt, ] <- c(s, mean(Rt_forw), mean(Rt_backw))
  # plotname <- paste0('p_', s)
  # assign(plotname, p)
  # listplots[[cnt]] <- p
  
  # cnt <- cnt + 1
  
}

Rt_out <- Rt_out[-1, ]

Rt_out <- Rt_out %>%
  group_by(Scenario, Heterogeneity, sim) %>%
  mutate(group_id = cur_group_id()) 
cnts <- Rt_out %>% 
  group_by(group_id) %>%
  count()
head(cnts); dim(cnts)
ids.keep <- cnts$group_id[cnts$n>1]
length(unique(ids.keep))
Rt_out <- Rt_out[Rt_out$group_id %in% ids.keep, ]
## Plot Rt for each scenario
p.wIndex <- Rt_out %>%  
  ggplot(aes(x = time, y = Rt_forw, group = factor(sim))) +
  geom_line(color = 'blue', size = 0.5) +
  geom_smooth(color = 'lightblue', size = 0.5, se = F, method = 'loess', span = 0.3) + # small span = less smooth
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  labs(
    title = '',
    x = 'Time',
    y = 'Effective reproduction number Rt'
  ) +
  facet_grid(Heterogeneity~Scenario) +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size= 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('../FigureRt2.jpeg', plot = p.wIndex, width = 60, height = 40, dpi = 300, units = 'cm')

# excluding index case
p.noIndex <- Rt_out[Rt_out$time > 0, ] %>%  
  ggplot(aes(x = time, y = Rt_forw, group = factor(sim))) +
  geom_line(color = 'blue', size = 0.5) +
  geom_smooth(color = 'lightblue', size = 0.5, se = F, method = 'loess', span = 0.3) + # small span = less smooth
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  labs(
    title = '',
    x = 'Time',
    y = 'Effective reproduction number Rt'
  ) +  
  facet_grid(Heterogeneity~Scenario) +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size= 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('../FigureRt1.jpeg', plot = p.noIndex, width = 60, height = 40, dpi = 300, units = 'cm')



