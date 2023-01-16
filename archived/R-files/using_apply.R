z = c(1001:2000)
e = rnorm(1000, mean =0.5)
f = rnorm(1000, mean =0.5)
  
v <- array(1:300, dim = c(3, 10, 10))

test.fun = function(x, y, k) {
  
  temp = (x*y)^k
  
  return(temp)
  
  
}

lapply(CFloc_group_in, function(x) x == test) == c("TRUE", "TRUE", "TRUE")
lapply(CFloc_group_in, function(x) x == test) 
lapply(CFloc_group_in, function(x) is.element(c(9,5,2), x))

test_sapply = sapply(1:1000, function(a) z[a]^e[a])


apply(v, c(1,2,3), function(b) b+1)



test = lapply(1:1000, function(a) {
  apply(v, c(1,2,3), function(b) (b+1)^e[a])})



test = lapply(seq(dim(CFloc_3d_ecoreg)[2]), function(x) CFloc_3d_ecoreg[ , x, ])

matr = matrix(data = c(1:4), nrow = 2, ncol = 2)

v1 = list(1)
v2 = list(1)
v3 = list(1)

for (i in 1:5) {
  
  v1[[i]] = matr
  v2[[i]] = matrix(data = 2, nrow = 2, ncol = 2)
  v3[[i]] = matrix(data = 2, nrow = 2, ncol = 2)
   
}

#broadcasting - element-wise multiplication of a matrix and an array whose two dimensions equal the dimensions of the matrix

test = array(data = 1:24, dim = c(2,3,4))
test1 = array(data = 2, dim = c(2,3))

#test2 = sapply(1:dim(test)[3], simplify = "array", function(taxa) { apply(test[,,taxa], c(1,2), function(v) v*test1) } )
#test3 = lapply(1:dim(test)[3], function(taxa) { apply(test[,,taxa], c(1,2), function(v) v*test1) } )
test6 <- lapply(1:dim(test)[3], function(x) {test[,,x]})
test7 <- lapply(test6, function(x,y) {x*y}, y = test1)
test8 <- array(sapply(test7, function(x){x}), dim = dim(test))

#### ASK CHRISTOPHER OR RHYTHIMA WHY THE FUNCTION WORKS !!!!!!!!!!!!!!!!!!

#################################################################

  hCalc = function(x, y) {
    
    if (missing(y)) {
      
      y = mean(z)
      
    }
    
    
    hCalc = (1-x)^(1/y)
    
  }


h = apply(CFloc_3d_ecoreg, MARGIN = c(2,3), FUN = hCalc, y = Ecoregions$z_values)  
  
###############################################################

CFloc_MC <- list(1) 

simulations = 1000



v <- vector(mode = "numeric", length = simulations)
v = rnorm(n = 10, mean = 10, sd = 2)

newlist = vector(mode = "list", length = simulations)



for (i in (1:simulations)) {
        
    newlist[[]][i] = v[i]
        
      }




CFloc_MC = lapply(1000, rnorm, mean = CFloc_3d_ecoreg, sd = 1)

h_MC = lapply(1:length(CFloc_Mc), )


  
test = lapply(v1, test.fun, y = v2, k = v3)



test = lapply(1:length(Ecoregions$z_values), function(a) {
  apply(v, c(1,2,3), function(b) (b+1)^Ecoregions$z_values[a])})

