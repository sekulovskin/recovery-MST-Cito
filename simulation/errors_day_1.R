#==================================================
# Number of errors (On the first day!!)
#=================================================
library(dplyr)
#==============================================================================
#Lezen
#=============================================================================
#Look at which items are administered in day 1
#I will do it seperately just in case
test_design_L %>%
  filter(module_id == "LDA1")
test_design_L %>%
  filter(module_id == "LDB1")
test_design_L %>%
  filter(module_id == "LDC1")
test_design_L %>%
  filter(module_id == "LDD1")
test_design_L %>%
  filter(module_id == "LDE1")
test_design_L %>%
  filter(module_id == "LDF1")

