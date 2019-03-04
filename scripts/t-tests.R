#1 sample t-test
set.seed(0)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000) # Ho: mu = 39000

#1-tailed t-test
set.seed(0)
treeVolume <- c(rnorm(75, mean = 36500, sd = 2000))
t.test(treeVolume, mu = 39000, alternative = "greater", conf.level = .99) # Ho: mu = 39000

#paired t-test
set.seed(2820)

preTreat <- c(rnorm(1000, mean = 145, sd = 9))
postTreat <- c(rnorm(1000, mean = 138, sd = 8))

t.test(preTreat, postTreat, paired = TRUE)

#2 sample t-test-continuous
set.seed(0)

ClevelandSpending <- rnorm(50, mean = 250, sd = 75)
NYSpending <- rnorm(50, mean = 300, sd = 80)

t.test(ClevelandSpending, NYSpending, var.equal = TRUE)

#2 sample t-test-categorical
spending <- c(ClevelandSpending, NYSpending)
city <- c(rep("Cleveland", 50), rep("New York", 50))

t.test(spending ~ city, var.equal = TRUE)

#2-sample t-test aka Welch's t-test
spending <- c(ClevelandSpending, NYSpending)
city <- c(rep("Cleveland", 50), rep("New York", 50))

t.test(spending ~ city, var.equal = FALSE)   