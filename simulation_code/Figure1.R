
## PEV without quarantine
load('~/GitHub/measles_isolation/simulation_output/FINAL/school_cutoff_PEVnoQ/WS_cutoff_PEV_noQ.RData')
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
df.all$Setting <- 'PEV without Q'
df.all.PEVnoQ <- df.all

df.onward <- as.data.frame(df.onward)
df.onward[,3] <- as.numeric(df.onward[,3])
names(df.onward) <- c('DurationQ', 'VaccCoverage', 'pNewWave')
head(df.onward)
df.onward$Setting <- 'PEV without Q'
df.onward.PEVnoQ <- df.onward

## PEV with quarantine
load('~/GitHub/measles_isolation/simulation_output/FINAL/school_cutoff_PEVwQ/WS_cutoff_PEV.RData')
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
df.all$Setting <- 'PEV with Q'
df.all.PEVwQ <- df.all

df.onward <- as.data.frame(df.onward)
df.onward[,3] <- as.numeric(df.onward[,3])
names(df.onward) <- c('DurationQ', 'VaccCoverage', 'pNewWave')
head(df.onward)
df.onward$Setting <- 'PEV with Q'
df.onward.PEVwQ <- df.onward

## Only quarantine
load('~/GitHub/measles_isolation/simulation_output/FINAL/school_cutoff_delayQ/WS_cutoff.RData')
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
df.all$Setting <- 'Only Q'
df.all.noPEV <- df.all

df.onward <- as.data.frame(df.onward)
df.onward[,3] <- as.numeric(df.onward[,3])
names(df.onward) <- c('DurationQ', 'VaccCoverage', 'pNewWave')
head(df.onward)
df.onward$Setting <- 'Only Q'
df.onward.noPEV <- df.onward

rm(df.all)
rm(df.onward)

## Combined output
df.all <- rbind(df.all.noPEV, df.all.PEVnoQ, df.all.PEVwQ)
dim(df.all); head(df.all)

###--------------------------------------------------------------------------------------------------------
### Proportion of sims that experience a second wave (i.e. onward transmission not from index)

df.onward <- rbind(df.onward.noPEV, df.onward.PEVnoQ, df.onward.PEVwQ)
dim(df.onward); head(df.onward)


df.onward$Setting=as.factor(df.onward$Setting)
# levels(df.onward$Setting) <- c("Enkel quarantaine", "PEV met quarantaine", "PEV zonder quarantaine")

library(ggplot2)
fig1b <- ggplot(df.onward[df.onward$DurationQ!='0 days',], aes(x = DurationQ, y = pNewWave, fill = VaccCoverage)) +
  geom_col(position = 'dodge') +
  # geom_text(aes(label = pNewWave), size = 5, vjust = 0.5, hjust = -0.2, position = position_dodge(width = 0.9), angle = 90) +
  xlab('Quarantine duration') + ylab('Proportie simulaties met meerdere infectiegolven') +
  facet_wrap(~Setting) + 
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 28),
        legend.position = 'top') +
  guides(fill = guide_legend(title = "Proportie immuun"))
# ggsave('~/GitHub/measles_isolation/simulation_output/FINAL/figures/pNewWave_VaccQ.jpeg', width = 40, height = 30, units = 'cm', dpi = 100)
# ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/SUBMISSION/v2/REVISION/final/edit/Figure1_160426.pdf', width = 90, height = 50, units = 'cm', dpi = 300)
fig1b + theme_minimal()
ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/SUBMISSION/v2/REVISION/final/EersteLijn/Figure1.jpeg', width = 90, height = 50, units = 'cm', dpi = 300)

labs <- c("Geen quarantaine", "18 dagen", "21 dagen")
fig_EL <- ggplot(df.onward[df.onward$DurationQ %in% c('1 day', '18 days', '21 days') & df.onward$Setting=='Only Q', ], aes(x = DurationQ, y = pNewWave, fill = VaccCoverage)) +
  geom_col(position = 'dodge') +
  # geom_text(aes(label = pNewWave), size = 5, vjust = 0.5, hjust = -0.2, position = position_dodge(width = 0.9), angle = 90) +
  xlab('Quarantaineduur') + ylab('Kans op nieuwe besmettingsgolf') +
  # facet_wrap(~Setting) + 
  scale_x_discrete(labels = labs) +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 28),
        legend.position = 'top') +
  guides(fill = guide_legend(title = "Proportie immuun"))
fig_EL
# ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/SUBMISSION/v2/REVISION/final/EersteLijn/Figure1.jpeg', width = 60, height = 40, units = 'cm', dpi = 300)

fig_EL <- fig_EL + theme(axis.text = element_text(size = 32),
                         axis.title = element_text(size = 36),
                         legend.text = element_text(size = 32),
                         legend.title = element_text(size = 36))
fig1aEL <- fig1aEL + scale_x_discrete(labels = labs) + xlab('') + theme(axis.text = element_text(size = 32),
                                                                        axis.title = element_text(size = 36),
                                                                        legend.text = element_text(size = 32),
                                                                        legend.title = element_text(size = 36))
library(ggpubr)
ggarrange(fig1aEL, fig_EL, ncol = 1, nrow = 2, common.legend = TRUE)
# ggarrange(fig1a, fig1b, ncol = 1, nrow = 2, common.legend = TRUE)
# ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/SUBMISSION/v2/REVISION/final/edit/Figure1_160426.pdf', width = 90, height = 80, units = 'cm', dpi = 300)
ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/SUBMISSION/v2/REVISION/final/EersteLijn/Infectieziektebulletin/Figuur1.jpeg', width = 90, height = 80, units = 'cm', dpi = 300)


ggplot(df.onward[df.onward$DurationQ != '0 days', ], aes(x = DurationQ, y = pNewWave, fill = Setting)) +
  geom_col(position = 'dodge') +
  facet_wrap(~VaccCoverage, scales = 'free_y') +
  geom_text(aes(label = pNewWave), size = 5, vjust = 0.5, hjust = -0.2, position = position_dodge(width = 0.9), angle = 90) +
  xlab('Quarantine duration') + ylab('Proportion simulations with multiple infection waves') +
  theme(axis.text = element_text(size = 28),
        axis.title = element_text(size = 32),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 32),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 28),
        legend.position = 'top') +
  guides(fill = guide_legend(title = "Setting"))
# ggsave('G:/My Drive/UHasselt/Research/Measles/Incubation/MANUSCRIPT/Figure1_all_v2.pdf', width = 90, height = 50, units = 'cm', dpi = 300)

