num_sim <- 10^3

set.seed(1)

p <- 0.05  # probability of infection
k <- 10    # size of pool
n <- 500   # number of pools
N <- n*k   # total number of people

# (A)의 검사 수: N = n*k

# (B) 방식

res <- numeric(num_sim)

for(i in 1:num_sim) {
  disease <- rbinom(N, size=1, prob=p)
  pool_mat <- matrix(disease, nrow=k, ncol=n)
  Y <- apply(pool_mat, 2, sum) >= 1
  res[i] <- n+sum(Y)*k
}

mean(res)
# [1] 2502.36

sd(res)
# [1] 108.3179
