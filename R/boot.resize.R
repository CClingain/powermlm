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
    new <- sample(num.groups, length(num.groups)*increase, replace = T)
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
      # Sample n new level 2 IDs
      new <- sample(num.groups1, length(num.groups1)*increase, replace = T)
      group1 <- in.group2[,level2]
      all <- c(num.groups1, new)
      newid <- 1

      for(j in all){
        in.group <- in.group2[group1 == j,]
        group.samp <- in.group[sample(1:nrow(in.group), nrow(in.group), replace = T),]

        group.samp$level2id <- newid
        boot.samp <- rbind.data.frame(boot.samp, group.samp)
        newid <- newid+1
      }
    }

   }
}
  return(boot.samp)
}
