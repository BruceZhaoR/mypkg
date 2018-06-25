
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

x1 <- as.numeric(p$prog)
x2 <- p$math
y <- p$num_awards

y_hat <- numeric(200L)

for (i in 1:200) {
  if (x1[i] == 2) {
    x_tmp <- 1.08386
  } else if (x1[i] == 3) {
    x_tmp <- 0.36981
  } else if (x1[i] == 1){
    x_tmp <- 0
  }
  print(x_tmp)
  y_hat[i] <- exp(-5.24712 + x_tmp + x2[i] * 0.07015)
}

y_fit <- fitted.values(m1)


resi_fit <- residuals(m1)
D <- sum(resi_fit^2)


# olog(o/e) = (o-e) + 1/2 (o-e)^2/e + ...  Taylor series expansion

# deviance = sign(o-e)sqrt(2 olog(o/e) - (o-e))

# deviance residuals 计算方法
resi_hat <- numeric(200L)

for (i in 1:200) {
  if (y[i] == 0) {
    resi_hat[i] <- sign(y[i] - y_fit[i]) * sqrt(2 * y_fit[i])
  } else {
    resi_hat[i] <- sign(y[i] - y_fit[i]) * sqrt(2 * (y[i] * log(y[i]/y_fit[i]) - (y[i] - y_fit[i])))
  }
}

# possion deviance residuals plot
plot(x= y_fit, y = resi_fit, type = 'p', pch = 3)


# Pearson residual ri = (oi - ei) / sqrt(ei)
# chisq = sum(ri^2)

pearson_resi <- (y - y_hat)/sqrt(y_hat)


par(mfrow=c(1,2))
plot(x= y_fit, y = resi_fit, type = 'p', pch = 3)
plot(x= y_fit, y = pearson_resi, type = 'p', pch = 3)

par(mfrow=c(1,1))

library(jsonlite)

test<- fromJSON("[[161.2, 51.6], [167.5, 59.0], [159.5, 49.2], [157.0, 63.0], [155.8, 53.6],
            [170.0, 59.0], [159.1, 47.6], [166.0, 69.8], [176.2, 66.8], [160.2, 75.2]]")


residual_data <- matrix(c(y_fit,resi_fit),ncol = 2,byrow = FALSE)

toJSON(residual_data)
 
