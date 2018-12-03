# Single power estimate function

#' Single power estimate
#'
#' Obtain a single power estimate based on bootstrapping
#' @usage boot.power(model, n, id, group1, group2, data)
#' @param model vector containing the lmer formula
#' @param n number of bootstrap samples from which to run the model
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param group2 character of level 3 grouping variable (ex: schools), default is FALSE in case of only 2 levels of nesting.
#' @param data dataset from which to bootstrap
#' @examples boot.power(model = Y ~ X + (1|schoolid), n = 1000, id = "ID", group1 = "schoolid", group2 = FALSE, data = dat)


boot.power <- function(model, n, id, group1, group2, data){
  # Initiate storage for p-values
  p.vals <- NULL
  # run the model for each bootstrapped sample
  for(i in 1:n){
    boot.dat <- bootmlm(data = data, id = id, group1 = group1, group2 = group2)
    mod <- summary(lmer(formula = model, data = boot.dat))
    coefs <- mod$coefficients[,5]
    if(i == 1){
      # Extract matrix dimensions
      p.vals <- matrix(ncol = length(coefs), nrow = n)
      p.vals[i,] <- coefs
    }
    else {
      p.vals[i,] <- coefs
    }
  }
  power <- apply(p.vals, 2, FUN = function(x) return(paste((sum(x<.05)/n)*100,"%", sep = "")))
  return(power)
}




