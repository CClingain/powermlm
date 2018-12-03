# Power Curve function

#' Power Curve Power
#'
#' Obtain a Power Curve based on bootstrapping
#' @usage boot.power(model, n, id, group1, group2, data)
#' @param model vector containing the lmer formula
#' @param n number of bootstrap samples from which to run the model
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param group2 character of level 3 grouping variable (ex: schools), default is FALSE in case of only 2 levels of nesting.
#' @param data dataset from which to bootstrap
#' @examples boot.power(model = Y ~ X + (1|schoolid), n = 1000, id = "ID", group1 = "schoolid", group2 = FALSE, data = dat)


pow.curve <- function(model, n, id, group1, group2, data){
  # Initiate storage for p-values
  p.vals <- NULL
  # run the model for each bootstrapped sample
  for(i in 1:n){
    boot.dat <- bootmlm(data = data, id = id, group1 = group1, group2 = group2)
    mod <- summary(lmer(formula = model, data = boot.dat))
    coefs <- mod$coefficients[,5]
    p.vals <- rbind(p.vals,coefs)

  }

  alphas <- seq(from = .001, to = .05, by = .001)
  power.plots <- matrix(nrow = length(alphas), ncol = nrow(mod$coefficients))
  for (i in 1:length(alphas)) {
    power.plots[i,] <- as.numeric(apply(p.vals, 2, FUN = function(x) return((sum(x<alphas[i])/n))))

  }
return(list(plot(y = power.plots[,1], x = alphas, type = "l", ylab = "Power "),
         plot(y = power.plots[,2], x = alphas, type = "l", ylab = "Power")))

}



# change .05 parameter to get the roc curve
# rbind <- expensive timewise, find dims in first iteration
