# Test on bootstrapping confidence intervals
# Francesca Rosa
# 03/06/2021

data <- rnorm(10000, mean = 4, sd = 0.5)

q97.5 <- quantile(data, probs = 97.5/100)

q2.5 <- quantile(data, probs = 2.5/100)

mean_realdata <- mean(data) + mean(data)*0.05

hist(data, freq = FALSE, xlim = c(1.5,7))
points(x = mean(data), y = 0, col = "black", pch = 19)
points(x = c(q2.5, q97.5), y = c(0,0), col = "red", pch = 19)
points(x = c(2*mean_realdata-q97.5, 2*mean_realdata-q2.5), y = c(0,0), col = "blue", pch = 19)
points(x = mean_realdata, y = 0, col = 3, pch = 19)
#points(x =  2*mean_realdata+q2.5, y = 0, col = 5, pch = 19)

legend("topright", 
       legend = c("Estimate based on bootstrapped data", "Estimate based on real data", "Percentile CI",
                  "Basic bootstrap confidence interval"),
       col = c("black ", 3, "red", "blue"), 
       pt.cex = 1, cex = 0.8, pch = 19)

