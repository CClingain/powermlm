
#' Multi-level Bootstrapping
#'
#' Bootstrapping for 2- or 3-level multi-level data
#' @usage bootmlm(id, group1, group2 = FALSE, data)
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param group2 character of level 3 grouping variable (ex: schools), default is FALSE in case of only 2 levels of nesting.
#' @param data dataset from which to bootstrap
#' @examples bootmlm(id = "ID", group1 = "classid", group2 = "schoolid", data = dat)

# BOOTSTRAP FROM EXISTING DATA WHILE PRESERVING GROUP STRUCTURE

bootmlm <- function(id, group1, group2 = F, data){
  # id = character of level 1 grouping variable/identifier
  # group1 = character of level 2 grouping variable (ex: classrooms)
  # group2 =  character of level 3 grouping variable (ex: schools), default is FALSE in case of 2 levels
  # predictors = vector of fixed effects
  # data = dataset from which to bootstrap

  if(group2 == F){ # if we only have 2 levels
    # Find the unique groups
    num.groups <- unique(data[,group1])
    # Find theunique IDs
    num.ids <- unique(data[,id])

    # Initiate bootstrap sample storage
    boot.samp <- NULL
    # Get column call ready since can't pass as before
    group1 <- data[,group1]
    # Resample within each group
    for(i in num.groups){
      # Get observations that are in current group
      in.group <- data[group1==i,]
      # sample from within that group
      group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]
      # Save into a data frame
      boot.samp <- rbind.data.frame(boot.samp,group.samp)
    }
return(boot.samp)


  }

  else if (group2 != F){ # if we have 3 levels
    # Find the unique groups for level 3
    num.groups2 <- unique(data[,group2])
    # Find the unique groups for level 2
    num.groups1 <- unique(data[,group1])
    # Find the unique IDs
    num.ids <- unique(data[,id])

    # Initiate bootstrap sample storage
    boot.samp <- NULL
    # Get column call ready since can't pass as before
    group1 <- data[,group1]
    group2 <- data[,group2]
    # Resample within each group
    for(i in num.groups2){
      for(j in num.groups1){
      # Get observations that are in current group
      in.group <- data[group1==j & group2 == i,]
      # sample from within that group
      group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]
      # Save into a data frame
      boot.samp <- rbind.data.frame(boot.samp,group.samp)
      }
    }
  }
return(boot.samp)

}
