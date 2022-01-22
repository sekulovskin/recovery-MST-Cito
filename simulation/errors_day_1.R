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

###For Module A
patterns_module_A_L <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_A_L[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142")) 
  
}

#add variable indicating occasion

for( i in 1:length(true.theta_L)){
  patterns_module_A_L[[i]]$occ <- rep(1:100, each = 6)
}

###For Module B (we will do it for each item separately, unfortunately)

#item 1 B

patterns_module_B_L.1 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.1[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00474D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.1[[i]]$occ <-seq(1:nrow(patterns_module_B_L.1[[i]]))
}     #NOTE: IT GIVES AN ERROR BECAUSE SOME STUDENTS HAVE 0 ROWS (DO NOT HAVE THE ITEM
             #HOWEVER, IT WORKS)


#item 2 B

patterns_module_B_L.2 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.2[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL01339") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.2[[i]]$occ <-seq(1:nrow(patterns_module_B_L.2[[i]]))
} 

#item 3 B

patterns_module_B_L.3 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.3[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00341") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.3[[i]]$occ <-seq(1:nrow(patterns_module_B_L.3[[i]]))
} 

#item 4 B

patterns_module_B_L.4 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.4[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00342") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.4[[i]]$occ <-seq(1:nrow(patterns_module_B_L.4[[i]]))
} 

#item 5 B

patterns_module_B_L.5 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.5[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00155") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.5[[i]]$occ <-seq(1:nrow(patterns_module_B_L.5[[i]]))
} 
#item 6 B

patterns_module_B_L.6 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.6[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00204") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.6[[i]]$occ <-seq(1:nrow(patterns_module_B_L.6[[i]]))
} 

#item 7 B

patterns_module_B_L.7 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_B_L.7[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00078") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_B_L.7[[i]]$occ <-seq(1:nrow(patterns_module_B_L.7[[i]]))
} 

# Combine

patterns_module_B_L <- list()

for(i in 1:length(true.theta_L)){
  patterns_module_B_L[[i]] <- rbind(patterns_module_B_L.1[[i]], patterns_module_B_L.2[[i]], patterns_module_B_L.3[[i]], 
                                    patterns_module_B_L.4[[i]], patterns_module_B_L.5[[i]], patterns_module_B_L.6[[i]],
                                    patterns_module_B_L.7[[i]])
}

rm(patterns_module_B_L.1, patterns_module_B_L.2, patterns_module_B_L.3, 
         patterns_module_B_L.4, patterns_module_B_L.5, patterns_module_B_L.6,
         patterns_module_B_L.7)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

## For module C

#item 1 C

patterns_module_C_L.1 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.1[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL02451D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.1[[i]]$occ <-seq(1:nrow(patterns_module_C_L.1[[i]]))
} 

#patterns_module_C_L.1[[41]]$occ <-seq(1:nrow(patterns_module_C_L.1[[41]]))
#item 2 C

patterns_module_C_L.2 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.2[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL02452D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.2[[i]]$occ <-seq(1:nrow(patterns_module_C_L.2[[i]]))
} 

#item 3 C

patterns_module_C_L.3 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.3[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL22453D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.3[[i]]$occ <-seq(1:nrow(patterns_module_C_L.3[[i]]))
} 


#item 4 C

patterns_module_C_L.4 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.4[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL02454D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.4[[i]]$occ <-seq(1:nrow(patterns_module_C_L.4[[i]]))
} 


#item 5 C

patterns_module_C_L.5 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.5[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00089") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.5[[i]]$occ <-seq(1:nrow(patterns_module_C_L.5[[i]]))
} 

#item 6 C

patterns_module_C_L.6 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.6[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00147") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.6[[i]]$occ <-seq(1:nrow(patterns_module_C_L.6[[i]]))
} 


#item 7 C

patterns_module_C_L.7 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_C_L.7[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00258") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_C_L.7[[i]]$occ <-seq(1:nrow(patterns_module_C_L.7[[i]]))
} 


# Combine

patterns_module_C_L <- list()

for(i in 1:length(true.theta_L)){
  patterns_module_C_L[[i]] <- rbind(patterns_module_C_L.1[[i]], patterns_module_C_L.2[[i]], patterns_module_C_L.3[[i]], 
                                    patterns_module_C_L.4[[i]], patterns_module_C_L.5[[i]], patterns_module_C_L.6[[i]],
                                    patterns_module_C_L.7[[i]])
}

rm(patterns_module_C_L.1, patterns_module_C_L.2, patterns_module_C_L.3, 
   patterns_module_C_L.4, patterns_module_C_L.5, patterns_module_C_L.6,
   patterns_module_C_L.7)

## For module D

#item 1 D

patterns_module_D_L.1 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.1[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00383") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.1[[i]]$occ <-seq(1:nrow(patterns_module_D_L.1[[i]]))
} 


#item 2 D

patterns_module_D_L.2 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.2[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00384") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.2[[i]]$occ <-seq(1:nrow(patterns_module_D_L.2[[i]]))
} 
#item 3 D

patterns_module_D_L.3 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.3[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00386") 
  
}

for(i in 1:length(true.theta_L)){
  patterns_module_D_L.3[[i]]$occ <-seq(1:nrow(patterns_module_D_L.3[[i]]))
} 
#item 4 D

patterns_module_D_L.4 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.4[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00440") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.4[[i]]$occ <-seq(1:nrow(patterns_module_D_L.4[[i]]))
} 
#item 5 D

patterns_module_D_L.5 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.5[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00442") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.5[[i]]$occ <-seq(1:nrow(patterns_module_D_L.5[[i]]))
} 


#item 6 D

patterns_module_D_L.6 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.6[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00443") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.6[[i]]$occ <-seq(1:nrow(patterns_module_D_L.6[[i]]))
} 
#item 7 D

patterns_module_D_L.7 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.7[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "V00444") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.7[[i]]$occ <-seq(1:nrow(patterns_module_D_L.7[[i]]))
} 
#item 8 D

patterns_module_D_L.8 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_D_L.8[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00188D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_D_L.8[[i]]$occ <-seq(1:nrow(patterns_module_D_L.8[[i]]))
} 

# Combine

patterns_module_D_L <- list()

for(i in 1:length(true.theta_L)){
  patterns_module_D_L[[i]] <- rbind(patterns_module_D_L.1[[i]], patterns_module_D_L.2[[i]], patterns_module_D_L.3[[i]], 
                                    patterns_module_D_L.4[[i]], patterns_module_D_L.5[[i]], patterns_module_D_L.6[[i]],
                                    patterns_module_D_L.7[[i]], patterns_module_D_L.8[[i]])
}

rm(patterns_module_D_L.1, patterns_module_D_L.2, patterns_module_D_L.3, 
   patterns_module_D_L.4, patterns_module_D_L.5, patterns_module_D_L.6,
   patterns_module_D_L.7, patterns_module_D_L.8)

## For module E

#item 1 E

patterns_module_E_L.1 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.1[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00779") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.1[[i]]$occ <-seq(1:nrow(patterns_module_E_L.1[[i]]))
} 

#item 2 E

patterns_module_E_L.2 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.2[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00780") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.2[[i]]$occ <-seq(1:nrow(patterns_module_E_L.2[[i]]))
} 

#item 3 E

patterns_module_E_L.3 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.3[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00781") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.3[[i]]$occ <-seq(1:nrow(patterns_module_E_L.3[[i]]))
} 

#item 4 E

patterns_module_E_L.4 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.4[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00142") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.4[[i]]$occ <-seq(1:nrow(patterns_module_E_L.4[[i]]))
} 


#item 5 E

patterns_module_E_L.5 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.5[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00143") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.5[[i]]$occ <-seq(1:nrow(patterns_module_E_L.5[[i]]))
} 



#item 6 E

patterns_module_E_L.6 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.6[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV10144") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.6[[i]]$occ <-seq(1:nrow(patterns_module_E_L.6[[i]]))
} 


#item 7 E

patterns_module_E_L.7 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.7[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV10146") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.7[[i]]$occ <-seq(1:nrow(patterns_module_E_L.7[[i]]))
} 


#item 8 E

patterns_module_E_L.8 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_E_L.8[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00096") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_E_L.8[[i]]$occ <-seq(1:nrow(patterns_module_E_L.8[[i]]))
} 


# Combine

patterns_module_E_L <- list()

for(i in 1:length(true.theta_L)){
  patterns_module_E_L[[i]] <- rbind(patterns_module_E_L.1[[i]], patterns_module_E_L.2[[i]], patterns_module_E_L.3[[i]], 
                                    patterns_module_E_L.4[[i]], patterns_module_E_L.5[[i]], patterns_module_E_L.6[[i]],
                                    patterns_module_E_L.7[[i]], patterns_module_E_L.8[[i]])
}

rm(patterns_module_E_L.1, patterns_module_E_L.2, patterns_module_E_L.3, 
   patterns_module_E_L.4, patterns_module_E_L.5, patterns_module_E_L.6,
   patterns_module_E_L.7, patterns_module_E_L.8)

## For module F

#item 1 F

patterns_module_F_L.1 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.1[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00956D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.1[[i]]$occ <-seq(1:nrow(patterns_module_F_L.1[[i]]))
} 


#item 2 F

patterns_module_F_L.2 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.2[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00958D") 
  
}

for(i in 1:length(true.theta_L)){
  patterns_module_F_L.2[[i]]$occ <-seq(1:nrow(patterns_module_F_L.2[[i]]))
} 
#item 3 F

patterns_module_F_L.3 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.3[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "BL00729") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.3[[i]]$occ <-seq(1:nrow(patterns_module_F_L.3[[i]]))
} 

#item 4 F

patterns_module_F_L.4 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.4[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00160D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.4[[i]]$occ <-seq(1:nrow(patterns_module_F_L.4[[i]]))
} 

#item 5 F

patterns_module_F_L.5 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.5[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00162D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.5[[i]]$occ <-seq(1:nrow(patterns_module_F_L.5[[i]]))
} 

#item 6 F

patterns_module_F_L.6 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.6[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00163D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.6[[i]]$occ <-seq(1:nrow(patterns_module_F_L.6[[i]]))
} 

#item 7 F

patterns_module_F_L.7 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.7[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "SV00164D") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.7[[i]]$occ <-seq(1:nrow(patterns_module_F_L.7[[i]]))
} 

#item 8 F

patterns_module_F_L.8 <- list()
for (i in 1:length(true.theta_L)){
  
  patterns_module_F_L.8[[i]] <- students_patterns_L[[i]] %>%
    filter(item_id == "OP00116B") 
  
}

for( i in 1:length(true.theta_L)){
  patterns_module_F_L.8[[i]]$occ <-seq(1:nrow(patterns_module_F_L.8[[i]]))
} 

# Combine

patterns_module_F_L <- list()

for(i in 1:length(true.theta_L)){
  patterns_module_F_L[[i]] <- rbind(patterns_module_F_L.1[[i]], patterns_module_F_L.2[[i]], patterns_module_F_L.3[[i]], 
                                    patterns_module_F_L.4[[i]], patterns_module_F_L.5[[i]], patterns_module_F_L.6[[i]],
                                    patterns_module_F_L.7[[i]], patterns_module_F_L.8[[i]])
}

rm(patterns_module_F_L.1, patterns_module_F_L.2, patterns_module_F_L.3, 
   patterns_module_F_L.4, patterns_module_F_L.5, patterns_module_F_L.6,
   patterns_module_F_L.7, patterns_module_F_L.8)

####Now combine everything to get the scores for the first day of Lezen

patterns_Day.1_L <- list()

for(i in 1:length(true.theta_L)){
  patterns_Day.1_L[[i]] <- rbind(patterns_module_A_L[[i]], patterns_module_B_L[[i]], patterns_module_C_L[[i]], 
                                 patterns_module_D_L[[i]], patterns_module_E_L[[i]], patterns_module_F_L[[i]])
}


