# This is the main file to create a fuzzy logic system (FLS) from
# sample data and use that FLS to get a new output. This provides
# a type-1 fuzzy logic based prediction framework for regression problems.
# This can be extended to a classification problem as well.


# load basic building blocks of FLS
source('~/R codes/fuzzy logic system/compute_membership_grade.R')
source('~/R codes/fuzzy logic system/generate_WM_rulebase_T1.R')
source('~/R codes/fuzzy logic system/rulebase_pruner.R')
source('~/R codes/fuzzy logic system/FLS_output_T1.R')

# training data on which FLS will be built
# i.e. this is the synthetic numerical data which 
# will be used to create rulebase for the FLS
# For this example, a 2-input 1-output system is demonstrated.
# Output will always be on the last column
trn_data <- matrix(1:99, nrow = 33, ncol=3)

# number of membership function (MF) (e.g. fuzzy category) on each input
# Input-01 has 10 MFs and input-02 has 5 MF
num_mf_array <- c(10, 5)

# This FLS uses Gaussian MF. Standard deviation of each MF is a design variable
std_dev_array <- c(0.3, 0.4)

# Number of MF and standard deviation on output
num_mf_output <- 7
std_dev_output <- 0.5

# generate the fuzzy logic system - "type-1"
FLS <- generate_WM_rulebase_T1(trn_data, num_mf_array, std_dev_array, std_dev_output, num_mf_output )


## separate the FLS variables

# shortended rulebase according to Wang-Mendel method
consolidated_rulebase <- FLS$consolidated_rulebase

# rulebase containing all rules i.e. conflict group exists here
exhaustive_rulebase <- FLS$exhaustive_rulebase

# exhaustive rulebase with corresponding importance, for removing duplicates
# from conflict group
exhaustive_rulebase_with_ruledegree <- FLS$exhaustive_rulebase_with_ruledegree

# center of memebrship functions of each group. This is a list
center_input_MF <- FLS$center_mf

# center of MFs on output
center_output_MF <- FLS$center_mf_output
num_input <- FLS$num_input

# frequency of each rule in consolidated rulebase
frequency <- FLS$frequency # frequency of each rule in consolidated rulebase


# test input, demo
new_input <- matrix(c(9,19.4, 46, 42.35), nrow=2, ncol=2)

cat("New inputs\n\n")
print(new_input)
# setting the weight of each rule in the consolidated rulebase
weight_matrix <- ones(nrow(consolidated_rulebase),1)

# apply the FLS on test set inputs, using a loop. Pass each row of new input
# matrix in a single iteration 
output <- matrix(data=NA, nrow= nrow(new_input), ncol=1)
for (i in 1:nrow(new_input)){
  output[i,1] <- FLS_output_T1(new_input[i,], consolidated_rulebase, center_input_MF, 
                          center_output_MF, num_input, std_dev_array, weight_matrix) 
}

cat("\n\nDefuzzified outputs are:\n")
print(output)
