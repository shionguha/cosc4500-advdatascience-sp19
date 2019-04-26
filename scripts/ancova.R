
#ancova in R
set.seed(3)
n <- 10
p <- 3
A.eff <- c(40, -15, -20)
beta <- -0.45
sigma <- 4
B <- rnorm(n * p, 0, 15)
A <- gl(p, n, lab = paste("Group", LETTERS[1:3]))
mm <- model.matrix(~A + B)
data <- data.frame(A = A, B = B, Y = as.numeric(c(A.eff, beta) %*% t(mm)) + rnorm(n * p, 0, 4))
data$B <- data$B + 20
head(data)

library(car)
scatterplot(Y ~ B | A, data = data)

# OR via ggplot
library(ggplot2)
ggplot(data, aes(y = Y, x = B, group = A)) + geom_point() + geom_smooth(method = "lm")

ggplot(data, aes(y = Y, x = A)) + geom_boxplot()

#doing anova
anova(lm(Y ~ B * A, data = data))

#interactive plots
plot(data.lm)

#summary of the model
summary(data.lm)

#confidence intervals
confint(data.lm)

#doing anova
anova(data.lm)

#predictions
predict(data.lm, newdata = data.frame(A = levels(data$A), B = mean(data$B, na.rm = TRUE)), interval = "prediction")


#generating effects plots
## generate a prediction data frame
newdata <- data.frame(A = levels(data$A), B = mean(data$B, na.rm = TRUE))
fit <- predict(data.lm, newdata = newdata, interval = "confidence")
fit <- data.frame(newdata, fit)

library(ggplot2)
ggplot(fit, aes(y = fit, x = A)) + geom_pointrange(aes(ymin = lwr, ymax = upr)) +
    scale_y_continuous("Y") + scale_x_discrete("X") + theme_classic() +
    theme(axis.title.y = element_text(vjust = 1, size = rel(1.25)), axis.title.x = element_text(vjust = -1,
        size = rel(1.25)))

#adding observed values
ggplot(fit, aes(y = fit, x = A)) + geom_point(data = data, aes(y = Y),
    col = "grey") + geom_pointrange(aes(ymin = lwr, ymax = upr)) + scale_y_continuous("Y") +
    scale_x_discrete("X") + theme_classic() + theme(axis.title.y = element_text(vjust = 1,
    size = rel(1.25)), axis.title.x = element_text(vjust = -1, size = rel(1.25)))


#adding separate group trends
newdata <- expand.grid(A = levels(data$A), B = seq(min(data$B), max(data$B),
    l = 10))
fit <- predict(data.lm, newdata = newdata, interval = "confidence")
fit <- data.frame(newdata, fit)
part.obs <- cbind(data, part.obs = fitted(data.lm) + resid(data.lm))
ggplot(fit, aes(y = fit, x = B, group = A, linetype = A)) + geom_point(data = part.obs,
    aes(y = part.obs, shape = A), col = "grey") + geom_line() + geom_ribbon(aes(ymin = lwr,
    ymax = upr), fill = "blue", alpha = 0.2) + scale_y_continuous("Y") +
    scale_x_continuous("X") + theme_classic() + theme(axis.title.y = element_text(vjust = 1,
    size = rel(1.25)), axis.title.x = element_text(vjust = -1, size = rel(1.25)))