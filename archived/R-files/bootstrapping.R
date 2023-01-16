library(boot)

data <- rweibull(1000, shape = 1.5, scale = 1)

set.seed(1)

bootstrap <- boot(data, sum, 1000)
