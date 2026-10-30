
# setwd('~/GitHub/measles_isolation/simulation_output/FINAL/school_PEV/')

setwd('~/GitHub/measles_isolation/simulation_output/school_10000sims_v2/')

mean.propNewWave <- c()
mean.propOnwardNoSeed <- c()

s <- 'Sens3_scen6'

for(i in seq(100,10000,100)){
  
  datfile <- paste0(s,"_onward_transmission.csv") # proportions for each simulation run
  dat <- read.csv(datfile, header = T)
  dat <- dat[c(1:i),-1]
  
  # mean proportions across simulation runs for scenario s
  mean.propOnwardNoSeed <- c(mean.propOnwardNoSeed, mean(dat$propOnwardNoSeed, na.rm = T))
  mean.propNewWave <- c(mean.propNewWave, sum(dat$propOnwardNoSeed > 0, na.rm = T)/i)
 
}
plot(seq(100,10000,100), mean.propNewWave, type = 'b', xlab = "nSim", ylab = "p_NewWave")
     # ylim = c(0.3,0.4))
