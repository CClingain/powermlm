# Power estimate with resized sample function

#' Power Estimate with Resized Sample
#'
#' Obtain a single power estimate based on bootstrapping from an increased sample size based on your data.
#' @usage power.resize(model, n, id, group1, group2, data, level, increase = 50)
#' @param model vector containing the lmer formula
#' @param n number of bootstrap samples from which to run the model
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param group2 character of level 3 grouping variable (ex: schools), default is FALSE in case of only 2 levels of nesting.
#' @param data dataset from which to bootstrap
#' @param level character of level variable at which you want to increase sample size
#' @param increase = numerical value of multiplicative increase in sample size, default is 2
#' @examples power.resize(model = Y ~ X + (1|schoolid), n = 1000, id = "ID", group1 = "schoolid", group2 = FALSE, data = dat)



# Increasing size

# Two possible levels to increase at:
# 1. increase # of students in all classrooms
  # resample students from each classroom with new size n
# 2. increase # of classrooms
  # randomly select classrooms, and then randomly select students within that selected classroom

power.resize <- function(model, n, id, group1, group2 = FALSE, data, level, increase = 2){
  # Initiate storage for p-values
  p.vals <- NULL
  # run the model for each bootstrapped sample
  for(i in 1:n){
    boot.dat <- boot.resize(data = data, id = id, group1 = group1, group2 = group2,level = level, increase = increase)
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
  names(power) <- names(coefs)
  return(power)

}
