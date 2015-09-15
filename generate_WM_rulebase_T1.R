
generate_WM_rulebase_T1 <- function(trn_data, num_mf_array, std_dev_array, std_dev_output, num_mf_output )
{
  
  # loading required library for this function
  library(pracma)
  
# This function creates a type-1 fuzzy rulebase based on the method describe 
# in the following paper:
# Wang, L-X., and Jerry M. Mendel. "Generating fuzzy rules by 
# learning from examples." Systems, Man and Cybernetics, IEEE Transactions
# on 22.6 (1992): 1414-1427.

# It takes training data as a matrix with inputs and outputs in the column. This currently
# works for only one output, but can be easily extended to multiple outputs format. Output is
# placed in the last column, always. All samples are placed in seperate rows. 

# Number of membership functions on each input can be different.

# Description of parameters:
#   
#  Inputs:
# trn_data: matrix containing training samples, last column corresponds to output
# 
# num_mf_array: number of MF on each input (defines number of FUZZY category on
# INPUT e.g. small, average, big), This is an array containing number of 
# MFs on each input. If there is 4 input in the FLS, the length of this array is 4.
# There is no assumption of all inputs having same number of MFs.
# 
# 
# std_dev_array: standard deviation for input and output Gaussian MFs
# num_mf_output: Number of MFs on output domain (defines number of FUZZY 
# category on OUTPUT e.g. small, average, big). This is a vector of length
# equals to number of INPUT in the FLS.
# 
# Outputs:
# consolidated_rulebase: consoldated rulebase which contains only one rule
# from each conflict group (e.g. same antecedent with different consequent)
# based on the maximum degree of rule
# consolidated_rulebase_with_degree: consolidated rulebase with associated
# degree of rules
# rulebase: exhaustive rulebase, without any kind of modification and
# conflict removal
# center_mf = center of MFs on inputs, required to compute membership grade
# on new inputs (e.g. testing data)
# center_mf_output= center of MFs on output, required to compute wang
  
#   
# temp_dimenstion_trndata <- dim(trn_data)
# num_samples <- temp_dimenstion_trndata[1]
# a <- temp_dimenstion_trndata[2]
# 
# 
# num_input <- a-1
# 
# library(pracma)
# 
# for (i in 1:num_input){
#   min_inputs[i] <- min(trn_data[,i])
#   max_inputs[i] <- max(trn_data[,i])
# }
# 
# 
# # generate center of MF in the input range
# center_mf <- list("list", num_input)
# 
# for (j in 1:num_input){
#   center_mf[[j]] <- linspace(min_inputs[j], 0.25*min_inputs[j], max_inputs[j]-0.25*max_inputs[j], num_mf_array[j])
# }
# 
#   
# 






temp_dimenstion_trndata <- dim(trn_data)
num_samples <- temp_dimenstion_trndata[1]
aa <- temp_dimenstion_trndata[2]


num_input <- aa-1


min_inputs <- integer(num_input)
max_inputs <- integer(num_input)

for (i in 1:num_input){
  min_inputs[i] <- min(trn_data[,i])
  max_inputs[i] <- max(trn_data[,i])
}

## INPUT portion
# generate center of MF in the input range
center_mf <- list("list", num_input)

for (j in 1:num_input){
  center_mf[[j]] <- linspace(min_inputs[j]+ 0.25*min_inputs[j], max_inputs[j]-0.25*max_inputs[j], num_mf_array[j])
}

# find MF of each sample of each input
mf <- matrix(data = NA, nrow = num_samples, ncol = num_input)
mf_cat <- matrix(data = NA, nrow = num_samples, ncol = num_input)
i <- 0
for (i in 1:num_samples){
  j <- 0
  for (j in 1:num_input){
    b <- NULL
    tmp_center <- NULL
    tmp_center <- center_mf[[j]]
    # disp(tmp_center)
    for (k in 1:length(tmp_center)){
      b[k] <- compute_membership_grade(trn_data[i,j], tmp_center[k], std_dev_array[j])
    }
    mf[i,j] <- max(b)
    mf_cat[i,j] <- which.max(b)
  }
}

## OUTPUT portion
# assumption: this is one output system
center_mf_output <- linspace(min(trn_data[, ncol(trn_data)]), max(trn_data[, ncol(trn_data)]), num_mf_output)

z <- trn_data[, ncol(trn_data)]

i<- 0
mf_o <- integer(length(center_mf_output))
mf_cat_o <- integer(length(center_mf_output))

for (i in 1:length(z)){
  # init
  b_o <- integer(length(center_mf_output))
  
  for (k in 1:length(center_mf_output)){
    b_o[k] <- compute_membership_grade(z[i], center_mf_output[k], std_dev_output)
  }
  mf_o[i] <- max(b_o)
  mf_cat_o[i] <- which.max(b_o)
}



# exhaustive rulebase - whole numbers represent fuzzy category of input and
# output variables

rulebase <- NULL
rulebase <- cbind(mf_cat, mf_cat_o)


# memerbship degree associated with each input and output of each rule
rulebase_mf <- cbind(mf, mf_o)

## reducing rulebase size

# find degree of each rule
degree_of_rule <- integer(num_samples)

# number of rules = num_samples as this is exhaustive rulebase
for (i in 1:num_samples){
  tmp <- rulebase_mf[i,]
  degree_of_rule[i] <- prod(tmp)
}


# exhaustive rulebase with corresponding degree of rule
exhaustive_rulebase_with_ruledegree <- cbind(rulebase, degree_of_rule)


exhaustive_rulebase <- rulebase

colnames(exhaustive_rulebase) <- NULL

reurtned_list <- rulebase_pruner( exhaustive_rulebase, exhaustive_rulebase_with_ruledegree )
consolidated_rulebase <- reurtned_list$consolidated_rulebase
rule_frequency <- reurtned_list$freq


# create a list which will contain all the required output of this function
returned_list <- list("consolidated_rulebase" = consolidated_rulebase,
                      "exhaustive_rulebase" = exhaustive_rulebase,
                      "exhaustive_rulebase_with_ruledegree" = exhaustive_rulebase_with_ruledegree,
                      "center_mf" = center_mf,
                      "center_mf_output" = center_mf_output,
                      "num_input" = num_input,
                      "frequency" = rule_frequency)


return (returned_list)

}
