#==================================================
# Error patterns (on first  module only!!)
#=================================================
library(dplyr)
#================
#Lezen
#==============
#Look at which items are in module A

test_design_L %>%
  filter(module_id == "LDA1")

#module_id item_id item_position
#1      LDA1 BL00518             1
#2      LDA1 BL00518             2
#3      LDA1 BL00520             3
#4      LDA1 BL00521             4
#5      LDA1 BL02412             5
#6      LDA1 OP00142             6

# Filter only these items:
patterns_module_A_L <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_A_L[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == c("BL00518", "BL00518", "BL00520", "BL00521", "BL02412",  "OP00142")) 
}

# fpr each student there are 500 items in total: since we have 5 items in module A and 100 repeated measurements
#So we can add a factor variable inicating from which measurement does a particu;lar item come from
# the variable will be called repeat <- indicatingto which of the 100 repeated measurements does the particular 
#item belong to

for( i in 1:length(true.theta_L)){
  patterns_module_A_L[[i]]$occ <- rep(1:100, each = 5)
}

# To be continued
