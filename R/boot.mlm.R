# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# BOOTSTRAP FROM EXISTING DATA WHILE PRESERVING GROUP STRUCTURE

bootmlm <- function(id, group1, group2 = F, predictors, data){
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
      in.group <- data[group1==num.groups[i],]
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
      in.group <- data[group1==num.groups1[i] & group2 == num.groups2[j],]
      # sample from within that group
      group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]
      # Save into a data frame
      boot.samp <- rbind.data.frame(boot.samp,group.samp)
      }
    }
  }
return(boot.samp)

}
