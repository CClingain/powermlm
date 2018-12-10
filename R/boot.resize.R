# Bootstrapping with resized sample function

#' Bootstrapping with Resized Sample
#'
#' Increase your sample size at level 1 or level 2 by bootstrapping.
#' @usage boot.resize(id, group1, group2, data, level, increase = 50)
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param group2 character of level 3 grouping variable (ex: schools), default is FALSE in case of only 2 levels of nesting.
#' @param data dataset from which to bootstrap
#' @param level character of level variable at which you want to increase sample size
#' @param increase = numerical value of multiplicative increase in sample size, default is 2
#' @examples boot.resize(id = "ID", group1 = "schoolid", group2 = FALSE, data = dat)


boot.resize <- function(id, group1, group2 = FALSE, data, level, increase = 2){

  boot.samp <- NULL

# If data has only 2 levels
if(group2 == F){
  # Find the number of unique groups
  num.groups <- unique(data[,group1])
  # If the increase will be at the first level
  if(level == id){
    group1col <- data[,group1]

    for(i in num.groups){
      in.group <- data[group1col == i,]
      group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group)*increase, replace = T),]
      boot.samp <- rbind.data.frame(boot.samp, group.samp)
    }
    return(boot.samp)
  }
  # If the increase will be at the second level
  else if(level == group1){
    new <- sample(c(num.groups, num.groups), length(num.groups)*(increase-1), replace = T)
    group1col <- data[,group1]
    all <- c(num.groups, new)
    newid <- 1

    for(i in all){
      in.group <- data[group1col == i,]
      group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]

      group.samp$level2id <- newid
      boot.samp <- rbind.data.frame(boot.samp, group.samp)
      newid <- newid+1
    }
  return(boot.samp)
  }

}
# If data has 3 levels
else if(group2 != FALSE){
  # If the increase will be at the first level
  if(level == id){
    # Find the unique groups for level 3
    num.groups2 <- unique(data[,group2])
    group2col <- data[,group2]
    # Resample within each group
    for(i in num.groups2){
      in.group2 <- data[group2col==i,]

      num.groups1 <- unique(in.group2[,group1])
      for(j in num.groups1){
        # Get observations that are in current group
        in.group1 <- in.group2[group1==j,]
        # sample from within that group
        group.samp <- in.group2[sample(1:nrow(in.group2), nrow(in.group2)*increase, replace = T),]
        # Save into a data frame
        boot.samp <- rbind.data.frame(boot.samp,group.samp)
       }
      }
return(boot.samp)
    }
  # If the increase will be at the second level
  else if(level == group1){
    # Find the unique groups for level 3
    num.groups2 <- unique(data[,group2])
    group2col <- data[,group2]

    # Resample within each group
    for(i in num.groups2){
      # Save a new data set for each specific level
      in.group2 <- data[group2col==i,]
      # And find the unique level 2 IDs within each level 3
     # num.groups1 <- unique(in.group2[,group1])
      colname <- names(in.group2)
      level2 <- which(colname == group1)
      num.groups1 <- unique(in.group2[,level2])
      # Sample n new level 2 IDs: Note we cbind the IDs since sample function returns 1:x if only one number in vector, which will cause function to crash
      new <- sample(x=c(num.groups1,num.groups1), size=(length(num.groups1)*(increase-1)), replace = T)
      # Save as subset
      group1col <- in.group2[,level2]
      # Combine original IDs and newly sampled ones
      all <- c(num.groups1, new)
      # Set ID counter for the new IDs
      newid <- 1
      # Initialize storage for nested loop (bootstrap each new level2)
      boot.samp1 <- NULL

      for(j in all){
        # Subset to current level 2 ID (ex. classroom 1)
        in.group <- in.group2[group1col == j,]
        # Sample students
        group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]
        # Generate new level 2 ID
        group.samp$level2id <- newid
        boot.samp1 <- rbind.data.frame(boot.samp1, group.samp)
        # Increase counter. Note: each school will now have level2 IDs 1:x
        newid <- newid+1
      }
      # Combine from each level 2 into total for level 3
      boot.samp <- rbind(boot.samp, boot.samp1)
    }

   }
}
  return(boot.samp)
}
