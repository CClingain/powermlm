# 2-level bootstrapping example function

#' Multi-level Bootstrapping
#'
#' Bootstrapping for 2-level multi-level data
#' @usage bootmlm2(id, group1, data)
#' @param id character of level 1 grouping variable/identifier
#' @param group1 character of level 2 grouping variable (ex: classrooms)
#' @param data dataset from which to bootstrap
#' @examples bootmlm2(id = "ID", group1 = "schoolid", data = dat)

bootmlm2 <- function(id, group1, data){
  # Find the unique groups
  num.groups <- unique(data[,group1])
  # Find the unique IDs
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
    boot.samp <- rbind(boot.samp,group.samp)
  }
  return(boot.samp)
}

