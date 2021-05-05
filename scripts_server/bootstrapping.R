# Bootstrap 95% CI for R-Squared
library(boot)

test_sel <- data.frame(Slost_aggr_sort["PA0429",9,])
names(test_sel)[1] = "test"

test <- test_sel

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

hist(test$test/10000000)
points(c(test_ci$percent[4]/10000000, test_ci$percent[5]/10000000), c(0,0), col = "red")
points(c(test[round(0.025*simulations),]/10000000, test[simulations - round(0.025*simulations),]/10000000), c(0,0), col = "blue")
points(mean(test$test)/10000000, 0, col = "black")
points(median(test$test)/10000000, 0, col = "green")


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


