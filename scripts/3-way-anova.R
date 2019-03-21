url: https://github.com/quantide/qdata

#setup
head(distance)
str(distance)

#summary plot
plot.design(Distance ~ ., data = distance)

#2-way interaction plot	
op <- par(mfrow = c(1, 1))
with(distance, {
  interaction.plot(ABS, Tire, Distance)
  interaction.plot(ABS, Tread, Distance)
  interaction.plot(Tread, Tire, Distance)
  }
)
par(op)

#doing the anova
fm <- aov(Distance ~ ABS * Tire * Tread, data = distance)
summary(fm)

#remove 3 way interaction
fm <- update(fm, . ~ . -ABS:Tire:Tread)
summary(fm)

#remove 2 way interaction
fm1 <- update(fm, .~ABS+Tire+Tread)
summary(fm1)

#comparing full and reduced model
anova(fm, fm1)

#table of means from model
model.tables(fm1,type="effects")
model.tables(fm1,type="means")

#final model with significant effects
fm <- aov(Distance ~ ABS + Tire, data = distance)
summary(fm)

#residual analysis	
op <-  par(mfrow = c(2, 2))
plot(fm)
par(op)

model.tables(fm, type="means")

#predicted values	
df_pred <- data.frame(ABS=c("enabled","disabled"),Tire=c("GT","LS"))
predict(object=fm,newdata=df_pred)