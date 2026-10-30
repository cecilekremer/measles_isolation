
n <- 24
# p <- 0.6 # children present 60% of the time (3/5 days)
# draws <- 1000
# track how many times each number is selected
# counts <- rep(0, n)
# weights <- rep(1/n, n) # initialize equal weights
# 
# samples <- list()
# for(i in 1:draws){
#   sampled <- sample(1:n, size = 18, prob = rep(p, n))
#   samples[[i]] <- sampled
#   
#   counts[sampled] <- counts[sampled] + 1
# }
# prop.table(counts)
# summary(counts/1000)

## Set days at which each child is present?
samples <- list()
for(i in 1:100){ # max. 100 days
  sampled <- sample(1:n, size = 18, replace = FALSE)
  samples[[i]] <- sampled
}
table(unlist(samples))

days.present <- list()
for(i in 1:n){
  days.present[[i]] <- which(sapply(samples, function(x) i %in% x))
}
table(unlist(days.present)) # should all be 18
