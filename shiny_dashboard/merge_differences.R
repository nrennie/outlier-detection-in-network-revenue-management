# merge_differences <- function(...){
#   k <- data.frame(Reduce(function(x,y) merge(x=x, y=y, by="rn", all=TRUE), list(...))) %>% remove_rownames %>% column_to_rownames(var="rn")
#   return(data.matrix(k))
# }

# merge_differences <- function(l){
#   k <- data.frame(Reduce(function(x,y) merge(x=x, y=y, by="rn", all=TRUE), l)) %>% remove_rownames %>% column_to_rownames(var="rn")
#   return(data.matrix(k))
# }

merge_differences <- function(l){
  #input is a named list of vectors with names
  m <- list()
  for (i in 1:length(l)){
    m[[i]] <- data.frame(rn=names(l[[i]]), l[[i]])
    colnames(m[[i]])[2] <- names(l)[i]
  }
  k <- data.frame(Reduce(function(x,y) merge(x=x, y=y, by="rn", all=TRUE), m)) %>% remove_rownames %>% column_to_rownames(var="rn")
  return(data.matrix(k))
}


# l1 <- data.frame(rn=names(left), left)
# 
# left <- c(1,2,3,4)
# names(left) <- c("A", "B", "C", "D")
# right <- c(5,6,7,8)
# names(right) <- c("A", "B", "C", "E")
# l2 <- list(left, right)
# names(l2) <- c("One", "Two")
# merge_differences(l2)
# l1 <- list(left)
# merge_differences(l1)
# 
# names(l2)
# n <- lapply(1:length(l2), function(x) data.frame(rn=names(l2[[x]]),y=l2[[x]]))
# 
# lapply(n, function(x) colnames(x)[2] <- names(l2)[1])
# 
# l <- lapply(l2, function(x) data.frame(rn=names(x), x))
# 
# merge(l[[1]],l[[2]],by="rn", all=TRUE)
