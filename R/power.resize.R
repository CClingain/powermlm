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
#' @param increase = numerical value of percentage increase in sample size, default is 50%
#' @examples power.resize(model = Y ~ X + (1|schoolid), n = 1000, id = "ID", group1 = "schoolid", group2 = FALSE, data = dat)



# Increasing size

# Two possible levels to increase at:
# 1. increase # of students in all classrooms
  # resample students from each classroom with new size n
# 2. increase # of classrooms
  # randomly select classrooms, and then randomly select students within that selected classroom

power.resize <- function(model, n, id, group1, group2 = FALSE, data, level, increase = 50){


  if(group2 == F){ # if we only have 2 levels
    # Find the unique groups
    num.groups <- unique(data[,group1])
    # Initiate bootstrap sample storage
    boot.samp <- NULL

    # If level to increase is Level 1 (Student/ID)
    if(level == id){
      # Get column call ready since can't pass as before
      group1 <- data[,group1]
      # Initialize storage
      #boot.samp <- matrix(ncol = ncol(data), nrow = nrow(data)*(1+(increase/100)))
      # Increase students within each LEVEL 2 Group (i.e, schools)
      for(i in num.groups){
       # Get observations that are in current group
       in.group <- data[group1==i,]
       # sample from within that group
       group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group)*(1+(increase/100)), replace = T),]
       # Save into a data frame
       boot.samp <- rbind.data.frame(boot.samp,group.samp)
       }
       return(as.data.frame(boot.samp))
    }
    # If level to increase is Level 2 (School)
    else if(level == group1){
      # Initialize storage
      #boot.samp <- matrix(ncol = ncol(data), nrow = nrow(data)*(1+(increase/100)))
      # Randomly select x schools to be the new schools
      new <- sample(num.groups, length(num.groups)*(increase/100), replace = T)
      # Get column call ready since can't pass as before
      group1 <- data[,group1]
      for(i in 1:new){
        in.group <- data[group1 == i,]
        group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]
        boot.samp <- rbind.data.frame(boot.samp, group.samp)
      }
      return(boot.samp)
    }


  }

  else if (group2 != F){ # if we have 3 levels
    # Find the unique groups for level 3
    num.groups2 <- unique(data[,group2])
    # Find the unique groups for level 2
    #num.groups1 <- unique(data[,group1])
    # Find the unique IDs
    num.ids <- unique(data[,id])

    # Initiate bootstrap sample storage
    boot.samp <- NULL
    # Get column call ready since can't pass as before
    #group1 <- data[,group1]
    group2col <- data[,group2]
    # Resample within each group
    for(i in num.groups2){
      in.group2 <- data[group2col==i,]

      num.groups1 <- unique(in.group2[,group2])
      for(j in num.groups1){
        # Get observations that are in current group
        in.group1 <- in.group2[group1==j,]
        # sample from within that group
        group.samp <- in.group2[sample(1:nrow(in.group2), nrow(in.group2), replace = T),]
        # Save into a data frame
        boot.samp <- rbind.data.frame(boot.samp,group.samp)
      }
    }
  }
  return(boot.samp)



}
