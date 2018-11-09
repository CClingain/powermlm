# 2-level bootstrapping example function

bootmlm <- function(id, group1, data){
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

# Test the function
set.seed(123)
dat <- cbind.data.frame(rnorm(100,0,1), rnorm(100,10,2), 1:100, rep(1:5, 20))
colnames(dat) <- c("X","Y","ID","schoolid")
dat2 <-bootmlm(id = "ID", group1 = "schoolid", data = dat)

head(dat2)
