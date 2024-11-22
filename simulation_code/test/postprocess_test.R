
setwd('G:/My Drive/UHasselt/Research/Measles/Incubation/simulations/outTEST/')

scenarios <- c("Baseline", "Scen1", "Scen2", "Scen3", "Scen4", "Scen5", "Scen6")

###----------------------------------------------------------------------------------
### Check expectations regarding final size (finalSize/n)

## Baseline
outBase <- read.csv('Baseline_outbreak_characteristics.csv', header = T)[,-1]
outBase$Scenario <- 'Baseline'
summary(outBase$FinSizeProp)
## Scen1: expect more transmission compared to Baseline (higher R0) -> ok
outScen1 <- read.csv('Scen1_outbreak_characteristics.csv', header = T)[,-1]
outScen1$Scenario <- 'Scen1'
summary(outScen1$FinSizeProp)
## Scen2: overdisp: expect fewer but bigger outbreaks compared to Baseline
outScen2 <- read.csv('Scen2_outbreak_characteristics.csv', header = T)[,-1]
outScen2$Scenario <- 'Scen2'
summary(outScen2$FinSizeProp)
sum(outScen2$FinSizeProp > 0.1); sum(outBase$FinSizeProp > 0.1) # 0.1 as threshold for 'not extinct'
sum(outScen2$FinSizeProp > 0.05); sum(outBase$FinSizeProp > 0.05)
## Scen3: overdisp: expect fewer but bigger outbreaks compared to Baseline but less than Scen2
outScen3 <- read.csv('Scen3_outbreak_characteristics.csv', header = T)[,-1]
outScen3$Scenario <- 'Scen3'
summary(outScen3$FinSizeProp)
sum(outScen3$FinSizeProp > 0.1); sum(outBase$FinSizeProp > 0.1) # 0.1 as threshold for 'not extinct'
sum(outScen3$FinSizeProp > 0.05); sum(outBase$FinSizeProp > 0.05)
## Scen4: expect less transmission (faster detection) compared to Baseline 
## -> not really, but there isn't expected much difference between 0 and 8 days because most indivs not infectious yet
outScen4 <- read.csv('Scen4_outbreak_characteristics.csv', header = T)[,-1]
outScen4$Scenario <- 'Scen4'
summary(outScen4$FinSizeProp); summary(outBase$FinSizeProp)
## Scen5: expect less transmission (higher prop immune children) compared to Baseline -> ok
outScen5 <- read.csv('Scen5_outbreak_characteristics.csv', header = T)[,-1]
outScen5$Scenario <- 'Scen5'
summary(outScen5$FinSizeProp)
summary(outScen5$FinSizeProp); summary(outBase$FinSizeProp)
## Scen6: expect more transmission (lower prop immune children) compared to Baseline -> ok
outScen6 <- read.csv('Scen6_outbreak_characteristics.csv', header = T)[,-1]
outScen6$Scenario <- 'Scen6'
summary(outScen6$FinSizeProp)
summary(outScen6$FinSizeProp); summary(outBase$FinSizeProp)

outALL <- rbind(outBase, outScen1, outScen2, outScen3, outScen4, outScen5, outScen6)
library(ggplot2)
ggplot(outALL[outALL$Scenario!='Scen2' & outALL$Scenario != 'Scen3', ], aes(x = Scenario, y = FinSizeProp)) + geom_boxplot()

###-----------------------------------------------------------------------------------
### Check expectations regarding overdispersion

# Scen2: overdisp: expect fewer but bigger outbreaks compared to Baseline
# Scen3: overdisp: expect fewer but bigger outbreaks compared to Baseline but less than Scen2 (higher k)

## Probability of extinction
extinct.base <- sum(outBase$FinSizeProp < 0.1)/1000; extinct.base
extinct.scen2 <- sum(outScen2$FinSizeProp < 0.1)/1000; extinct.scen2
extinct.scen3 <- sum(outScen3$FinSizeProp < 0.1)/1000; extinct.scen3

## Probability of no further transmission? -> ok (expected lowest for Baseline)
noTrans.base <- sum(outBase$FinSize == 1)/1000; noTrans.base
noTrans.scen2 <- sum(outScen2$FinSize == 1)/1000; noTrans.scen2
noTrans.scen3 <- sum(outScen3$FinSize == 1)/1000; noTrans.scen3

## Outbreak size of those that do 'explode'
summary(outBase$FinSizeProp[outBase$FinSize>1])
summary(outScen2$FinSizeProp[outScen2$FinSize>1])
summary(outScen3$FinSizeProp[outScen3$FinSize>1])
ggplot(outALL[outALL$Scenario %in% c('Baseline','Scen2','Scen3'), ], 
       aes(x = Scenario, y = FinSizeProp)) + geom_boxplot()

##-------------------------------------------------------------------------------------
## Test overdispersion this w/ completely susceptible population + no isolation at all
outScen7 <- read.csv('Scen7_outbreak_characteristics.csv', header = T)[,-1]
outScen7$Scenario <- 'Scen7'
outScen8 <- read.csv('Scen8_outbreak_characteristics.csv', header = T)[,-1]
outScen8$Scenario <- 'Scen8'
outScen9 <- read.csv('Scen9_outbreak_characteristics.csv', header = T)[,-1]
outScen9$Scenario <- 'Scen9'
outALL <- rbind(outScen7, outScen8, outScen9)

# Scen2: overdisp: expect fewer but bigger outbreaks compared to Baseline
# Scen3: overdisp: expect fewer but bigger outbreaks compared to Baseline but less than Scen2 (higher k)

## Probability of extinction -> lowest for Baseline ok
extinct.base <- sum(outScen7$FinSizeProp < 0.1)/100; extinct.base
extinct.scen2 <- sum(outScen8$FinSizeProp < 0.1)/100; extinct.scen2
extinct.scen3 <- sum(outScen9$FinSizeProp < 0.1)/100; extinct.scen3
## Probability of no further transmission? -> ok (expected lowest for Baseline)
noTrans.base <- sum(outScen7$FinSize == 1)/100; noTrans.base
noTrans.scen2 <- sum(outScen8$FinSize == 1)/100; noTrans.scen2
noTrans.scen3 <- sum(outScen9$FinSize == 1)/100; noTrans.scen3
## Peak cases instead of outbreak size?
peak.base <- median(outScen7$casesPeak[outScen7$FinSizeProp>0.1]); peak.base
peak.scen2 <- median(outScen8$casesPeak[outScen8$FinSizeProp>0.1]); peak.scen2
peak.scen3 <- median(outScen9$casesPeak[outScen9$FinSizeProp>0.1]); peak.scen3

## Outbreak size of those that do 'explode' -> expected highest for Scen2, followed by Scen3?
summary(outScen7$FinSizeProp[outScen7$FinSizeProp>0.1])
summary(outScen8$FinSizeProp[outScen8$FinSizeProp>0.1])
summary(outScen9$FinSizeProp[outScen9$FinSizeProp>0.1])

ggplot(outALL[outALL$Scenario %in% c('Scen7','Scen8','Scen9') & outALL$FinSizeProp>0.1, ], 
       aes(x = Scenario, y = casesPeak)) + geom_boxplot()


# ###-----------------------------------------------------------------------------------
# ### PROBABILITY OF ONWARD TRANSMISSION
# 
# mean.propOnward <- c()
# mean.propOnwardNoSeed <- c()
# mean.propOnwardAfterIsol <- c()
# 
# for(s in scenarios){
#   
#   datfile <- paste0(s,"_onward_transmission.csv")
#   dat <- read.csv(datfile, header = T)
#   dat <- dat[,-1]
#   
#   mean.propOnward <- c(mean.propOnward, mean(dat$propOnward, na.rm = T))
#   mean.propOnwardNoSeed <- c(mean.propOnwardNoSeed, mean(dat$propOnwardNoSeed, na.rm = T))
#   mean.propOnwardAfterIsol <- c(mean.propOnwardAfterIsol, mean(dat$propOnwardAfterIsol, na.rm = T))
#   
#   ## Data for boxplots
#   mat.propOnward <- matrix(NA, nrow = 100, ncol = 4) # 1 = scenario, 2 = propOnward, 3 = propOnwardAfterIsol
#   sdd <- ifelse(s %in% c('Baseline','Scen1','Scen2','Scen6'), 'seed1', 'seed5')
#   scen <- s
#   if(sdd == 'seed5'){
#     if(s == 'Scen3'){
#       scen <- 'Baseline'
#     }else if(s == 'Scen4'){
#       scen <- 'Scen1'
#     }else if(s == 'Scen5'){
#       scen <- 'Scen2'
#     }else if(s == 'Scen7'){
#       scen <- 'Scen6'
#     }
#   }
#   for(i in 1:dim(dat)[1]){
#     mat.propOnward[i, 1] <- scen
#     mat.propOnward[i, 2] <- as.numeric(dat$propOnward[i])
#     mat.propOnward[i, 3] <- as.numeric(dat$propOnwardAfterIsol[i])
#     mat.propOnward[i, 4] <- sdd
#   }
#   
#   if(s == 'Baseline'){
#     df.all <- mat.propOnward
#   }else{
#     df.all <- rbind(df.all, mat.propOnward)
#   }
#   
# }
# 
# df.onward <- data.frame(Scenario = scenarios,
#                         propOnward = mean.propOnward,
#                         propOnwardNoSeed = mean.propOnwardNoSeed,
#                         propOnwardAfterIsol = mean.propOnwardAfterIsol
# )
# df.onward
# 
# 
# df.all <- as.data.frame(df.all)
# df.all[,2] <- as.numeric(df.all[,2])
# df.all[,3] <- as.numeric(df.all[,3])
# names(df.all) <- c('Scenario', 'propOnward', 'propOnwardAfterIsol', 'nSeed')
# table(df.all$Scenario, df.all$nSeed)
# 
# library(ggplot2)
# ggplot(df.all, aes(x = nSeed, y = propOnward, fill = Scenario)) +
#   geom_boxplot() +
#   xlab('Number of seed cases') + ylab('Proportion of cases causing onward transmission')
# 
# summary(df.all$propOnwardAfterIsol[df.all$nSeed=='seed1' & df.all$Scenario=='Scen1'])
# summary(df.all$propOnwardAfterIsol[df.all$nSeed=='seed1' & df.all$Scenario=='Scen2'])
# summary(df.all$propOnwardAfterIsol[df.all$nSeed=='seed1' & df.all$Scenario=='Scen6'])
# ggplot(df.all[df.all$Scenario != 'Baseline',], aes(x = nSeed, y = propOnwardAfterIsol, fill = Scenario)) +
#   geom_boxplot() + #ylim(0,0.02) +
#   xlab('Number of seed cases') + ylab('Proportion of exposed individuals causing onward transmission AFTER isolation') 
# 
# 
# 
# 
