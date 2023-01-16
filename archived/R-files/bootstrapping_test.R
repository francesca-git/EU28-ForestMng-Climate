# Bootstrap 95% CI for R-Squared
library(boot)

test_sel <- data.frame(Slost_aggr_sort["PA0429",2,])
test_sel <- data.frame(Slost_aggr_sort["NA0605",6,])

names(test_sel)[1] = "test"

test <- test_sel
c = 10000000

test <- test %>% mutate_if(is.numeric, ~(.*10000000))

foo <- function(data, indices){
  dt<-data[indices,]
    median(dt)
}

# bootstrapping with 1000 replications
set.seed(50)
results <- boot(data=test, statistic=foo,
                R=1000)

# view results
results
plot(results)

# get 95% confidence interval
test_ci <- boot.ci(results)

test_norm <- test %>%
              mutate(test = test/c)
test_norm <- pull(test_norm)

middle <- ((max(test_norm)-min(test_norm))/2 + min(test_norm))

test_norm_simm <- 2*middle - test_norm

hist(test_norm, main = "Histogram of Slost for Permanent crops (ecoregion PA0429)", xlab = "Slost", xlim = c(0,2e-06))
hist(test_norm, main = "Histogram of Slost for Permanent crops (ecoregion PA0429)", xlab = "Slost", xlim = c(1.2e-6,1.4e-06))
hist(test_norm, main = "Histogram of Slost for Clear cut (ecoregion NA0605)", xlab = "Slost")

points(x = middle, y = 0, col = "cyan", pch = 19) # middle of the data range
points(median(test_norm), 0, col = "green", pch = 19) # median
points(mean(test_norm), 0, col = "black", pch = 19) # mean
points(median(test_norm_simm), y = 0, col = "green", pch = 19) # middle of the data range
points(x = (2*middle - median(test_norm_simm)), y = 0, col = "orchid", pch = 19) # middle of the data range

hist(test_norm_simm, main = "Histogram of Slost for Permanent crops (ecoregion PA0429) - reverse", xlab = "Slost", xlim = c(0,2e-06))

# definitions from wikipedia
sd_star_rev <- exp(sd(log(test_norm)))
mu_star_rev <- median(test_norm) # median of the reverse log-normal, which is the distribution of our data
mu_star <- median(test_norm_simm)
sd_star <- exp(sd(log(test_norm_simm)))

sd <- sd(test_norm)

# these expressions come from the calculations I made on my ipad using the explanation from Slob (1994) and the slides from Steffi 

CI_low <- 2*middle*(1-sd_star^2) + mu_star_rev*sd_star^2
CI_up <- (2*middle*(sd_star^2 - 1) + mu_star_rev)/sd_star^2
points(c(CI_up, CI_low), c(0,0), col = "darkorange", pch = 19) 

points(c(CI_up, CI_low), c(0,0), col = "darkorange", pch = 19) 

legend("topleft", 
       legend = c("median", "mean", "CI using percentiles", "CI using multiplicative properties"), 
       col = c("green", "black", "blue", "darkorange", 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = 19)

points(c(test_norm[round(0.025*simulations)], test_norm[simulations - round(0.025*simulations)]), c(0,0), col = "blue", pch = 19) # manually selected
points(x = c(mean(test_norm) - sd, mean(test_norm) + sd), y = c(0,0), col = "plum1", pch = 19) # standard deviation of the mean


CI_low_check1 <- mu_star_rev - (mu_star * sd_star^2 - mu_star)
points(CI_low_check, 0, col = "darkorange2", pch = 19) 

CI_low_check2 <- 2*middle - mu_star*sd_star^2
points(CI_low_check2, 0, col = "darkorange4", pch = 19) 

CI_up_check1 <- mu_star_rev + (mu_star - mu_star/sd_star^2)
points(CI_up_check1, 0, col = "darkorange2", pch = 19) 

CI_up_check2 <- 2*middle - mu_star/sd_star^2
points(CI_up_check2, 0, col = "darkorange4", pch = 19) 


hist(test_norm, main = "Histogram of Slost for Clear cut (ecoregion NA0605)", xlab = "Slost")

fitdist(test_sel, "normal")
points(c(test_ci$percent[4]/10000000, test_ci$percent[5]/10000000), c(0,0), col = "red", pch = 19)   # from bootstrapping
points(c(test_norm[round(0.025*simulations)], test[simulations - round(0.025*simulations)]), c(0,0), col = "blue", pch = 19) # manually selected
points(mean(test_norm), 0, col = "black", pch = 19) # mean
points(median(test_norm), 0, col = "green", pch = 19) # median
legend("topleft", 
       legend = c("CI from bootstrapping", "CI manually selected", "Mean", "Median"), 
       col = c("red", "blue", "black", "green", 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = 19)


library(boot)

foo <- function(data, indices){
  dt<-data[indices,]
  c(
    cor(dt[,1], dt[,2], method='s'),
    median(dt[,1]),
    median(dt[,2])
  )
}

set.seed(12345)
myBootstrap <- boot(iris, foo, R=1000)
boot.ci(myBootstrap)



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# https://www.datacamp.com/community/tutorials/bootstrap-r

foo <- function(data, indices, cor.type){
  dt<-data[indices,]
  c(
        median(dt[,1]), 
        median(dt[,2]),
        cor(dt[,1], dt[,2], method=cor.type)
  )
}

set.seed(12345)
myBootstrap <- boot(iris, foo, R=1000, cor.type='s')

myBootstrap_ci <- boot.ci(myBootstrap, index=1)

iris <- iris %>% arrange(Sepal.Length)
simulations = 150
hist(iris$Sepal.Length)
points(c(myBootstrap_ci$percent[4], myBootstrap_ci$percent[5]), c(0,0), col = "red")
points(c(iris$Sepal.Length[round(0.025*simulations)], iris$Sepal.Length[simulations - round(0.025*simulations)]), c(0,0), col = "blue")
points(mean(iris$Sepal.Length), 0, col = "black")
points(median(iris$Sepal.Length), 0, col = "green")


