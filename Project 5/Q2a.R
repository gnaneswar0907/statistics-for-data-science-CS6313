library(boot) #importing boot lib
lambda = c(0.01,0.1,1,10) #lambda values 
n = c(5,10,30,100) #n values
alpha = 0.05 #alpha value for 95% CI 
monte = 5000 #no. Of runs
#function we used for boot call
mean_x = function(x, indices){ 
  result = mean(x[indices])
  return(result) 
}
for(i in n) {
  for(j in lambda) {
    count1=0 
    count2=0
    for (k in 1:monte) {
      x = rexp(i, j) #generating x values with exponential distribution 
      mean.x = 1/j #population mean
      var.x = var(x)
      standard.error = sqrt(var.x/i)
      CI.large = mean(x) + c(-1,1) * qnorm(1-alpha/2) * standard.error #large sample z-interval CI 
      if(CI.large[1]<mean.x && CI.large[2]>mean.x)
      {
        count1=count1+1 #increasing count if mean lies in the CI 
      }
      CI.boot = boot(x, mean_x, R=999, sim = "ordinary", stype = "i")
      boot.percentile = sort(CI.boot$t)[c(25,975)] #Bootstrap percentile CI 
      if(boot.percentile[1]<mean.x && boot.percentile[2]>mean.x)
      {
        count2=count2+1 #increasing count if mean lies in the CI 
      }
    }
    coverage.prob.large = count1/monte #coverage prob for large sample z-interal 
    coverage.prob.boot = count2/monte #coverage prob for bootstrap percentile 
    print(paste("Value of n:", i))
    print(paste("Value of lambda", j))
    print(paste("Coverage probability of large sample z-interval", coverage.prob.large)) 
    print(paste("Coverage probability of percentile bootstrap", coverage.prob.boot))
  } 
}