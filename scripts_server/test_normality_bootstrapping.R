
load("./slost_static_RCP_MFM_AF100_2100.Rdata")

slost_static <- Slost_lf

load("./slost_bs_RCP_MFM_AF100_2100.Rdata")

slost_bs <- Slost_lf

rm(Slost_lf)

diff <- array(data = NA, dim = dim(slost_bs))

dimnames(diff) <- dimnames(slost_bs)

mean_median <- slost_static

n = dim(slost_bs)[4]
  
for(i in 1:n) {
  
  temp <- slost_static[,,,1] - slost_bs[,,,i] 
  diff[,,,i] <- temp
  
} 

mean_median[,,,1] <- apply(diff, c(1,2,3), mean, na.rm = TRUE)                   
mean_median[,,,2] <- apply(diff, c(1,2,3), median, na.rm = TRUE)                   

test <- mean_median[,,,1]

test[,,] <- (mean_median[,,,1] - mean_median[,,,2])/(mean_median[,,,1] + mean_median[,,,2])
