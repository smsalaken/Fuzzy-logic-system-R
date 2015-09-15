# Fuzzy-logic-system-R
R codes for building a fuzzy logic system from numerical data and application of FLS on new inputs.

This repo provide R codes for creating a type-1 fuzzy logic rulebase from numerical data using Wang-Mendel method [1]. 
All basic blocks of FLS has been provided. Call to "generate_WM_rulebase_T1" will create a rulebase and return center 
of membership functions along with the rulebase. These outputs can be used to predict output for new input in the system.
Thus, this repo can be used as a prediction framework using fuzzy logic system in R language. 

For a intro to fuzzy logic system, read "http://sipi.usc.edu/~mendel/publications/Flirtation%20FLA.pdf" 

[1]. Wang, Li-Xin, and Jerry M. Mendel. "Generating fuzzy rules by learning from examples." Systems, Man and Cybernetics, IEEE Transactions on 22.6 (1992): 1414-1427.
