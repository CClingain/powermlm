# Power Curve function

#' Power Curve with Varying Alphas
#'
#' Obtain a Power Curve based on bootstrapping with various alpha cut-offs. Displays the relation between power and Type I error rate.
#' @usage boot.power(model, n, id, group1, group2, data)
#' @param model vector containing the lmer formula
#' @param n number of bootstrap samples from which to run the model
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param group2 character of level 3 grouping variable (ex: schools), default is FALSE in case of only 2 levels of nesting.
#' @param data dataset from which to bootstrap
#' @return Returns power curves for all fixed parameters specified in the model.
#' @examples boot.power(model = Y ~ X + (1|schoolid), n = 1000, id = "ID", group1 = "schoolid", group2 = FALSE, data = dat)


pow.curve <- function(model, n, id, group1, group2, data){
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
  # Create a vector of alphas
  alphas <- seq(from = .000, to = .10, by = .001)
  # Initalize storage for power based on each alpha
  power.plots <- matrix(nrow = length(alphas), ncol = nrow(mod$coefficients))
  for (i in 1:length(alphas)) {
    power.plots[i,] <- as.numeric(apply(p.vals, 2, FUN = function(x) return((sum(x<alphas[i])/n))))

  }
# Store and return the power plots for each fixed parameters
p <- par(mfrow=c(1,1))
  for (i in 1:ncol(power.plots)){
    plot(y = power.plots[,i], x = alphas, type = "l", ylab = "Power", main = paste("Power for",rownames(mod$coefficients)[i]), ylim = c(0,1))
    abline(v = .05, col = 2, lty = 2)
    text(x = .05, y = .1, labels = "Alpha = .05", cex = .75, col = 2)
    text(x = .05, y = max(power.plots[,i]-.2), labels = paste("Power =",power.plots[50,i]), cex = .75, col = 3)
  }
return(par(p))

}
