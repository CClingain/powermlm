# Attempt to parallelize
#output <- foreach(k = c(1:Nsim), .combine = rbind) %dopar% function(k)

  boot.power2 <- function(model, n, id, group1, group2, data){
    # Initiate storage for p-values
    p.vals <- NULL
    # Check number of cores
    numcores <- detectCores()
    # Run on the number of cores -1
    registerDoParallel(cores = numcores/2)

    # run the model for each bootstrapped sample
    p.vals <- foreach(nsim = 1:n, .combine = rbind, .packages = c('powermlm','lme4','lmerTest')) %dopar% {
      boot.dat <- bootmlm(data = data, id = id, group1 = group1, group2 = group2)
      mod <- summary(lmer(formula = model, data = boot.dat))
      coefs <- mod$coefficients[,5]
    }
    power <- apply(p.vals, 2, FUN = function(x) return(paste((sum(x<.05)/n)*100,"%", sep = "")))
    return(power)
    stopImplicitCluster()
  }
