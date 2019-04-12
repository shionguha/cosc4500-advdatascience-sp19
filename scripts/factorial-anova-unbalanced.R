#easy factorial anova with unbalanced design
set.seed(123)
preRCBM = round(rnorm(30,30,10),0)
postRCBM = round(rnorm(30,30,10),0)
treatment = c(rep(1,10), rep(0,20))
dataRCBM = as.data.frame(cbind(preRCBM, postRCBM, treatment))
dataReading = reshape(dataRCBM, varying = list(c("preRCBM", "postRCBM")), times = c(1,2), direction = "long")
colnames(dataReading) = c("treatment", "time", "RCBM", "id")
library(car)
result <- Anova(lm(RCBM~treatment*time,data = dataReading), type="III")
result

#more complicated factorial anova with unbalanced design
set.seed(1)
nA <- 2  #number of levels of A
nB <- 3  #number of levels of B
nsample <- 10  #number of reps in each
A <- gl(nA, 1, nA, lab = paste("a", 1:nA, sep = ""))
B <- gl(nB, 1, nB, lab = paste("b", 1:nB, sep = ""))
data <- expand.grid(A = A, B = B, n = 1:nsample)
X <- model.matrix(~A * B, data = data)
eff <- c(40, 15, 5, 0, -15, 10)
sigma <- 3  #residual standard deviation
n <- nrow(data)
eps <- rnorm(n, 0, sigma)  #residuals
data$y <- as.numeric(X %*% eff + eps)
head(data)  #print out the first six rows of the data set


#interaction plots
with(data, interaction.plot(A, B, y))

#boxplots
boxplot(y ~ A * B, data)

#better graphs with ggplot2
library(ggplot2)
ggplot(data, aes(y = y, x = A, fill = B)) + geom_boxplot()

#building the model
data.lm <- lm(y ~ A * B, data)


#visualizing model fit
par(mfrow = c(2, 2))
plot(data.lm)


summary(data.lm)


