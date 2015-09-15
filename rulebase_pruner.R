rulebase_pruner <- function(exhaustive_rulebase, exhaustive_rulebase_with_ruledegree){

#rulebase_pruner

# find the consolidated database by removing duplicates from
# exhaustive rulebase based on rules with highest degree
# exhaustive_rulebase_with_ruledegree: contains degree of each rule on
# the last column
# rulebase: exhaustive rulebase

a <- exhaustive_rulebase[,1:(ncol(exhaustive_rulebase)-1)]

ncol_exhaustive_rbwd <-ncol(exhaustive_rulebase_with_ruledegree)
cc <- as.matrix(cbind(exhaustive_rulebase_with_ruledegree[, 1:(ncol_exhaustive_rbwd-2)], exhaustive_rulebase_with_ruledegree[,ncol_exhaustive_rbwd]))


d <- exhaustive_rulebase_with_ruledegree[, (ncol(exhaustive_rulebase_with_ruledegree)-1)]; # consequent part only
frequency <- NULL
i <- 1
consolidated_rulebase_t <- NULL
kept_rule <- NULL
consolidated_rulebase <- NULL

tmp_dim_a <- dim(a)
while (nrow(a)){
  counter <- 0
  temp <- a[1,]
  
  num_row <- nrow(a)
  index_vector <- logical(num_row)
  
  for (j in 1:num_row){
    if (all(temp == a[j, ])){
      counter <- counter+1
      index_vector[j] <- TRUE
      
    }
  } #end for
  
  bb <- as.matrix(cc[index_vector, ])
  
  if (ncol(bb)==1){
    temp_bb <- t(bb)
    bb <- NULL
    bb <- temp_bb
  }

  rule_to_keep <- which.max(bb[, ncol(bb)])

  ttt <- c(bb[rule_to_keep, ], c(d[rule_to_keep]))
  kept_rule <- rbind(kept_rule, ttt)
  
  # remove corresponding rows

  a <- a[!index_vector, ] 
  cc <- cc[!index_vector, ]
  d <- d[!index_vector]
  
  consolidated_rulebase_t <- rbind(consolidated_rulebase_t, temp)
  
  frequency[i] <- counter
  i <- i+1
  
  bb <- NULL
  temp <- NULL
  j <- NULL 
  index_vector <- NULL
  
}

# removing row names "ttt" from the kept rule for asthetic reason
rownames(kept_rule) <- NULL


consolidated_rulebase <- cbind(kept_rule[,1:(ncol(kept_rule)-2)], kept_rule[,ncol(kept_rule)])


# combine consolidated rulebase and frequency of each rule in a list to return
# to the calling syntax
reurtned_list <- list("consolidated_rulebase" = consolidated_rulebase, "freq" = frequency)


return(reurtned_list)
}