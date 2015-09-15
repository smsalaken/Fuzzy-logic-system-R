FLS_output_T1 <- function(new_input, consolidated_rulebase, center_input_MF, 
                          center_output_MF, num_input, std_dev_array, weight_matrix) {

# set all weights to 1 of not supplied by the user
  if (missing(weight_matrix)){
    weight_matrix <- ones(nrow(consolidated_rulebase),1)
      }

combined_mg_new_input <- NULL
corresponding_output_center <- NULL
applicability_of_rule <- NULL

# loop through every rule

for (i in 1:nrow(consolidated_rulebase)){
  membersip_grade_new_input <- NULL
  # find memebrship grade for all inputs (in every rule)
  for (j in 1:num_input){
    tmmmpp_centerInputMF <- NULL
    system_input <-  consolidated_rulebase[i,j]
    tmmmpp_centerInputMF <- center_input_MF[[j]]
    membersip_grade_new_input[j] <- compute_membership_grade(new_input[j], 
                                                             tmmmpp_centerInputMF[system_input],
                                                             std_dev_array[j])
    
  } # end inner for loop
  
  combined_mg_new_input[i] <- prod(membersip_grade_new_input)*weight_matrix[i,1]
  corresponding_output_center[i] <- center_output_MF[consolidated_rulebase[i, ncol(consolidated_rulebase)]]
  applicability_of_rule[i] <- combined_mg_new_input[i]*corresponding_output_center[i]
} # end outer for loop

defuzzified_output <- sum(applicability_of_rule)/sum(combined_mg_new_input)

return(defuzzified_output)

}
