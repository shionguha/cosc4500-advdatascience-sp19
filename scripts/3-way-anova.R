url: https://github.com/quantide/qdata

head(distance)

str(distance)

plot.design(Distance ~ ., data = distance)

	
op <- par(mfrow = c(1, 1))
with(distance, {
  interaction.plot(ABS, Tire, Distance)
  interaction.plot(ABS, Tread, Distance)
  interaction.plot(Tread, Tire, Distance)
  }
)
par(op)

fm <- aov(Distance ~ ABS * Tire * Tread, data = distance)
summary(fm)

fm <- update(fm, . ~ . -ABS:Tire:Tread)
summary(fm)

fm1 <- update(fm, .~ABS+Tire+Tread)
summary(fm1)

anova(fm, fm1)

model.tables(fm1,type="effects")

model.tables(fm1,type="means")

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