#==================================================
# Number of errors (On the first day!!) for Lezen!!
#=================================================
library(dplyr)

#Look at which items are administered in day 1
#I will do it seperately just in case
test_design_R %>%
  filter(module_id == "RDA1")
test_design_R %>%
  filter(module_id == "RDB1")
test_design_R %>%
  filter(module_id == "RDC1")
test_design_R  %>%
  filter(module_id == "RDD1")
test_design_R  %>%
  filter(module_id == "RDE1")
test_design_R  %>%
  filter(module_id == "RDF1")

###For Module A
patterns_module_A_R.1 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516087") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.1[[i]]$occ <- seq(1:n_sim)
}

#item 2
patterns_module_A_R.2 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516456") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.2[[i]]$occ <- seq(1:n_sim)
}

#item 3
patterns_module_A_R.3 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516A28") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.3[[i]]$occ <- seq(1:n_sim)
}

#item 4
patterns_module_A_R.4 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516663") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.4[[i]]$occ <- seq(1:n_sim)
}

#item 5
patterns_module_A_R.5 <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516375") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_R.5[[i]]$occ <- seq(1:n_sim)
}

##NOW combine
patterns_module_A_R <- list()
for (i in 1:length(true.theta_R)){
  patterns_module_A_R[[i]] <- rbind(patterns_module_A_R.1[[i]], patterns_module_A_R.2[[i]], patterns_module_A_R.3[[i]], patterns_module_A_R.4[[i]], patterns_module_A_R.5[[i]]) 
}

rm(patterns_module_A_R.1, patterns_module_A_R.2, patterns_module_A_R.3, patterns_module_A_R.4, patterns_module_A_R.5)


###For Module B 


patterns_module_B_R.1 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516537") 
  
}

#detour since we cannot add an indicator variable if our lists begins with empty DF's (for whatever reason)
booklet_id <- "placeholder"
person_id <- "placeholder"
item_id <- "placeholder"
item_score <- 1

placeholder.df <- data.frame(booklet_id, person_id, item_id, item_score)

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.1[[i]]) == 0){
    patterns_module_B_R.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.1[[i]]$occ <-seq(1:nrow(patterns_module_B_R.1[[i]]))
}   


#item 2 B

patterns_module_B_R.2 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516B02") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.2[[i]]) == 0){
    patterns_module_B_R.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.2[[i]]$occ <-seq(1:nrow(patterns_module_B_R.2[[i]]))
} 

#item 3 B

patterns_module_B_R.3 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516620") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.3[[i]]) == 0){
    patterns_module_B_R.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.3[[i]]$occ <-seq(1:nrow(patterns_module_B_R.3[[i]]))
} 


#item 4 B

patterns_module_B_R.4 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516A04") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.4[[i]]) == 0){
    patterns_module_B_R.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.4[[i]]$occ <-seq(1:nrow(patterns_module_B_R.4[[i]]))
} 

#item 5 B

patterns_module_B_R.5 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516686") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.5[[i]]) == 0){
    patterns_module_B_R.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.5[[i]]$occ <-seq(1:nrow(patterns_module_B_R.5[[i]]))
} 

#item 6 B

patterns_module_B_R.6 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.6[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516538") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.6[[i]]) == 0){
    patterns_module_B_R.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.6[[i]]$occ <-seq(1:nrow(patterns_module_B_R.6[[i]]))
} 

#item 7 B

patterns_module_B_R.7 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.7[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516160") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.7[[i]]) == 0){
    patterns_module_B_R.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.7[[i]]$occ <-seq(1:nrow(patterns_module_B_R.7[[i]]))
} 

#item 8 B

patterns_module_B_R.8 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.8[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516734") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.8[[i]]) == 0){
    patterns_module_B_R.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.8[[i]]$occ <-seq(1:nrow(patterns_module_B_R.8[[i]]))
} 

#item 9 B

patterns_module_B_R.9 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.9[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516751") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.9[[i]]) == 0){
    patterns_module_B_R.9[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.9[[i]]$occ <-seq(1:nrow(patterns_module_B_R.9[[i]]))
} 


#item 10 B

patterns_module_B_R.10 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_B_R.10[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD517839") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_B_R.10[[i]]) == 0){
    patterns_module_B_R.10[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_B_R.10[[i]]$occ <-seq(1:nrow(patterns_module_B_R.10[[i]]))
} 
# Combine

patterns_module_B_R <- list()

for(i in 1:length(true.theta_R)){
  patterns_module_B_R[[i]] <- rbind(patterns_module_B_R.1[[i]], patterns_module_B_R.2[[i]], patterns_module_B_R.3[[i]], 
                                    patterns_module_B_R.4[[i]], patterns_module_B_R.5[[i]], patterns_module_B_R.6[[i]],
                                    patterns_module_B_R.7[[i]], patterns_module_B_R.8[[i]], patterns_module_B_R.9[[i]],
                                    patterns_module_B_R.10[[i]])
}

rm(patterns_module_B_R.1, patterns_module_B_R.2, patterns_module_B_R.3, 
   patterns_module_B_R.4, patterns_module_B_R.5, patterns_module_B_R.6,
   patterns_module_B_R.7, patterns_module_B_R.8, patterns_module_B_R.9, patterns_module_B_R.10)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module C

# 1 C
patterns_module_C_R.1 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516670") 
  
}


for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.1[[i]]) == 0){
    patterns_module_C_R.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.1[[i]]$occ <-seq(1:nrow(patterns_module_C_R.1[[i]]))
}   


#item 2 C

patterns_module_C_R.2 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516979") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.2[[i]]) == 0){
    patterns_module_C_R.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.2[[i]]$occ <-seq(1:nrow(patterns_module_C_R.2[[i]]))
} 

#item 3 C

patterns_module_C_R.3 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516100") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.3[[i]]) == 0){
    patterns_module_C_R.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.3[[i]]$occ <-seq(1:nrow(patterns_module_C_R.3[[i]]))
} 


#item 4 C

patterns_module_C_R.4 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516339") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.4[[i]]) == 0){
    patterns_module_C_R.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.4[[i]]$occ <-seq(1:nrow(patterns_module_C_R.4[[i]]))
} 

#item 5 C

patterns_module_C_R.5 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516838") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.5[[i]]) == 0){
    patterns_module_C_R.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.5[[i]]$occ <-seq(1:nrow(patterns_module_C_R.5[[i]]))
} 

#item 6 C

patterns_module_C_R.6 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.6[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516830") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.6[[i]]) == 0){
    patterns_module_C_R.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.6[[i]]$occ <-seq(1:nrow(patterns_module_C_R.6[[i]]))
} 

#item 7 C

patterns_module_C_R.7 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.7[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516209") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.7[[i]]) == 0){
    patterns_module_C_R.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.7[[i]]$occ <-seq(1:nrow(patterns_module_C_R.7[[i]]))
} 

#item 8 C

patterns_module_C_R.8 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.8[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516471") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.8[[i]]) == 0){
    patterns_module_C_R.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.8[[i]]$occ <-seq(1:nrow(patterns_module_C_R.8[[i]]))
} 

#item 9 C

patterns_module_C_R.9 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.9[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516138") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.9[[i]]) == 0){
    patterns_module_C_R.9[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.9[[i]]$occ <-seq(1:nrow(patterns_module_C_R.9[[i]]))
} 


#item 10 C

patterns_module_C_R.10 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_C_R.10[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516770") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_C_R.10[[i]]) == 0){
    patterns_module_C_R.10[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_C_R.10[[i]]$occ <-seq(1:nrow(patterns_module_C_R.10[[i]]))
} 
# Combine

patterns_module_C_R <- list()

for(i in 1:length(true.theta_R)){
  patterns_module_C_R[[i]] <- rbind(patterns_module_C_R.1[[i]], patterns_module_C_R.2[[i]], patterns_module_C_R.3[[i]], 
                                    patterns_module_C_R.4[[i]], patterns_module_C_R.5[[i]], patterns_module_C_R.6[[i]],
                                    patterns_module_C_R.7[[i]], patterns_module_C_R.8[[i]], patterns_module_C_R.9[[i]],
                                    patterns_module_C_R.10[[i]])
}

rm(patterns_module_C_R.1, patterns_module_C_R.2, patterns_module_C_R.3, 
   patterns_module_C_R.4, patterns_module_C_R.5, patterns_module_C_R.6,
   patterns_module_C_R.7, patterns_module_C_R.8, patterns_module_C_R.9, patterns_module_C_R.10)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module D

#item 1 D

patterns_module_D_R.1 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516333") 
  
}



for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.1[[i]]) == 0){
    patterns_module_D_R.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.1[[i]]$occ <-seq(1:nrow(patterns_module_D_R.1[[i]]))
}   


#item 2 D

patterns_module_D_R.2 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516802") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.2[[i]]) == 0){
    patterns_module_D_R.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.2[[i]]$occ <-seq(1:nrow(patterns_module_D_R.2[[i]]))
} 

#item 3 D

patterns_module_D_R.3 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516704") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.3[[i]]) == 0){
    patterns_module_D_R.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.3[[i]]$occ <-seq(1:nrow(patterns_module_D_R.3[[i]]))
} 


#item 4 D

patterns_module_D_R.4 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516687") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.4[[i]]) == 0){
    patterns_module_D_R.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.4[[i]]$occ <-seq(1:nrow(patterns_module_D_R.4[[i]]))
} 

#item 5 D

patterns_module_D_R.5 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516B04") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.5[[i]]) == 0){
    patterns_module_D_R.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.5[[i]]$occ <-seq(1:nrow(patterns_module_D_R.5[[i]]))
} 

#item 6 D

patterns_module_D_R.6 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.6[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516094") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.6[[i]]) == 0){
    patterns_module_D_R.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.6[[i]]$occ <-seq(1:nrow(patterns_module_D_R.6[[i]]))
} 

#item 7 D

patterns_module_D_R.7 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.7[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516933") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.7[[i]]) == 0){
    patterns_module_D_R.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.7[[i]]$occ <-seq(1:nrow(patterns_module_D_R.7[[i]]))
} 

#item 8 D

patterns_module_D_R.8 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.8[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516788") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.8[[i]]) == 0){
    patterns_module_D_R.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.8[[i]]$occ <-seq(1:nrow(patterns_module_D_R.8[[i]]))
} 

#item 9 D

patterns_module_D_R.9 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.9[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516731") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.9[[i]]) == 0){
    patterns_module_D_R.9[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.9[[i]]$occ <-seq(1:nrow(patterns_module_D_R.9[[i]]))
} 


#item 10 D

patterns_module_D_R.10 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.10[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516097") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.10[[i]]) == 0){
    patterns_module_D_R.10[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.10[[i]]$occ <-seq(1:nrow(patterns_module_D_R.10[[i]]))
} 

#item 11 D

patterns_module_D_R.11 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.11[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516404") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.11[[i]]) == 0){
    patterns_module_D_R.11[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.11[[i]]$occ <-seq(1:nrow(patterns_module_D_R.11[[i]]))
} 

#item 12 D

patterns_module_D_R.12 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.12[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516527") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.12[[i]]) == 0){
    patterns_module_D_R.12[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.12[[i]]$occ <-seq(1:nrow(patterns_module_D_R.12[[i]]))
} 


#item 13 D

patterns_module_D_R.13 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_D_R.13[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516086") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_D_R.13[[i]]) == 0){
    patterns_module_D_R.13[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_D_R.13[[i]]$occ <-seq(1:nrow(patterns_module_D_R.13[[i]]))
} 
# Combine

patterns_module_D_R <- list()

for(i in 1:length(true.theta_R)){
  patterns_module_D_R[[i]] <- rbind(patterns_module_D_R.1[[i]], patterns_module_D_R.2[[i]], patterns_module_D_R.3[[i]], 
                                    patterns_module_D_R.4[[i]], patterns_module_D_R.5[[i]], patterns_module_D_R.6[[i]],
                                    patterns_module_D_R.7[[i]], patterns_module_D_R.8[[i]], patterns_module_D_R.9[[i]],
                                    patterns_module_D_R.10[[i]], patterns_module_D_R.11[[i]], patterns_module_D_R.12[[i]],
                                    patterns_module_D_R.13[[i]])
}

rm(patterns_module_D_R.1, patterns_module_D_R.2, patterns_module_D_R.3, 
   patterns_module_D_R.4, patterns_module_D_R.5, patterns_module_D_R.6,
   patterns_module_D_R.7, patterns_module_D_R.8, patterns_module_D_R.9, 
   patterns_module_D_R.10, patterns_module_D_R.11, patterns_module_D_R.12,
   patterns_module_D_R.13)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module E

#item 1 E

patterns_module_E_R.1 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516966") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.1[[i]]) == 0){
    patterns_module_E_R.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.1[[i]]$occ <-seq(1:nrow(patterns_module_E_R.1[[i]]))
}   


#item 2 E

patterns_module_E_R.2 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516381") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.2[[i]]) == 0){
    patterns_module_E_R.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.2[[i]]$occ <-seq(1:nrow(patterns_module_E_R.2[[i]]))
} 

#item 3 E

patterns_module_E_R.3 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516331") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.3[[i]]) == 0){
    patterns_module_E_R.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.3[[i]]$occ <-seq(1:nrow(patterns_module_E_R.3[[i]]))
} 


#item 4 E

patterns_module_E_R.4 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516604") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.4[[i]]) == 0){
    patterns_module_E_R.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.4[[i]]$occ <-seq(1:nrow(patterns_module_E_R.4[[i]]))
} 

#item 5 E

patterns_module_E_R.5 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516762") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.5[[i]]) == 0){
    patterns_module_E_R.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.5[[i]]$occ <-seq(1:nrow(patterns_module_E_R.5[[i]]))
} 

#item 6 E

patterns_module_E_R.6 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.6[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516457") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.6[[i]]) == 0){
    patterns_module_E_R.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.6[[i]]$occ <-seq(1:nrow(patterns_module_E_R.6[[i]]))
} 

#item 7 E

patterns_module_E_R.7 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.7[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516142") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.7[[i]]) == 0){
    patterns_module_E_R.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.7[[i]]$occ <-seq(1:nrow(patterns_module_E_R.7[[i]]))
} 

#item 8 E

patterns_module_E_R.8 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.8[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516470") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.8[[i]]) == 0){
    patterns_module_E_R.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.8[[i]]$occ <-seq(1:nrow(patterns_module_E_R.8[[i]]))
} 

#item 9 E

patterns_module_E_R.9 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.9[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516940") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.9[[i]]) == 0){
    patterns_module_E_R.9[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.9[[i]]$occ <-seq(1:nrow(patterns_module_E_R.9[[i]]))
} 


#item 10 E

patterns_module_E_R.10 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.10[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516261") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.10[[i]]) == 0){
    patterns_module_E_R.10[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.10[[i]]$occ <-seq(1:nrow(patterns_module_E_R.10[[i]]))
} 

#item 11 E

patterns_module_E_R.11 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.11[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516A80") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.11[[i]]) == 0){
    patterns_module_E_R.11[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.11[[i]]$occ <-seq(1:nrow(patterns_module_E_R.11[[i]]))
} 

#item 12 E

patterns_module_E_R.12 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.12[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516232") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.12[[i]]) == 0){
    patterns_module_E_R.12[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.12[[i]]$occ <-seq(1:nrow(patterns_module_E_R.12[[i]]))
} 


#item 13 E

patterns_module_E_R.13 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_E_R.13[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516522") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_E_R.13[[i]]) == 0){
    patterns_module_E_R.13[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_E_R.13[[i]]$occ <-seq(1:nrow(patterns_module_E_R.13[[i]]))
} 
# Combine

patterns_module_E_R <- list()

for(i in 1:length(true.theta_R)){
  patterns_module_E_R[[i]] <- rbind(patterns_module_E_R.1[[i]], patterns_module_E_R.2[[i]], patterns_module_E_R.3[[i]], 
                                    patterns_module_E_R.4[[i]], patterns_module_E_R.5[[i]], patterns_module_E_R.6[[i]],
                                    patterns_module_E_R.7[[i]], patterns_module_E_R.8[[i]], patterns_module_E_R.9[[i]],
                                    patterns_module_E_R.10[[i]], patterns_module_E_R.11[[i]], patterns_module_E_R.12[[i]],
                                    patterns_module_E_R.13[[i]])
}

rm(patterns_module_E_R.1, patterns_module_E_R.2, patterns_module_E_R.3, 
   patterns_module_E_R.4, patterns_module_E_R.5, patterns_module_E_R.6,
   patterns_module_E_R.7, patterns_module_E_R.8, patterns_module_E_R.9, 
   patterns_module_E_R.10, patterns_module_E_R.11, patterns_module_E_R.12,
   patterns_module_E_R.13)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module F

#item 1 F

patterns_module_F_R.1 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.1[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516141") 
  
}


for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.1[[i]]) == 0){
    patterns_module_F_R.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.1[[i]]$occ <-seq(1:nrow(patterns_module_F_R.1[[i]]))
}   


#item 2 F

patterns_module_F_R.2 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.2[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516014") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.2[[i]]) == 0){
    patterns_module_F_R.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.2[[i]]$occ <-seq(1:nrow(patterns_module_F_R.2[[i]]))
} 

#item 3 E

patterns_module_F_R.3 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.3[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516067") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.3[[i]]) == 0){
    patterns_module_F_R.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.3[[i]]$occ <-seq(1:nrow(patterns_module_F_R.3[[i]]))
} 


#item 4 F

patterns_module_F_R.4 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.4[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516194") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.4[[i]]) == 0){
    patterns_module_F_R.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.4[[i]]$occ <-seq(1:nrow(patterns_module_F_R.4[[i]]))
} 

#item 5 F

patterns_module_F_R.5 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.5[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516137") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.5[[i]]) == 0){
    patterns_module_F_R.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.5[[i]]$occ <-seq(1:nrow(patterns_module_F_R.5[[i]]))
} 

#item 6 F

patterns_module_F_R.6 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.6[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516203") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.6[[i]]) == 0){
    patterns_module_F_R.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.6[[i]]$occ <-seq(1:nrow(patterns_module_F_R.6[[i]]))
} 

#item 7 F

patterns_module_F_R.7 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.7[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516896") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.7[[i]]) == 0){
    patterns_module_F_R.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.7[[i]]$occ <-seq(1:nrow(patterns_module_F_R.7[[i]]))
} 

#item 8 E

patterns_module_F_R.8 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.8[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516065") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.8[[i]]) == 0){
    patterns_module_F_R.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.8[[i]]$occ <-seq(1:nrow(patterns_module_F_R.8[[i]]))
} 

#item 9 F

patterns_module_F_R.9 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.9[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516251") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.9[[i]]) == 0){
    patterns_module_F_R.9[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.9[[i]]$occ <-seq(1:nrow(patterns_module_F_R.9[[i]]))
} 


#item 10 F

patterns_module_F_R.10 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.10[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516109") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.10[[i]]) == 0){
    patterns_module_F_R.10[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.10[[i]]$occ <-seq(1:nrow(patterns_module_F_R.10[[i]]))
} 

#item 11 F

patterns_module_F_R.11 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.11[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516532") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.11[[i]]) == 0){
    patterns_module_F_R.11[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.11[[i]]$occ <-seq(1:nrow(patterns_module_F_R.11[[i]]))
} 

#item 12 F

patterns_module_F_R.12 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.12[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516964") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.12[[i]]) == 0){
    patterns_module_F_R.12[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.12[[i]]$occ <-seq(1:nrow(patterns_module_F_R.12[[i]]))
} 


#item 13 F

patterns_module_F_R.13 <- list()
for (i in 1:length(true.theta_R)){
  
  patterns_module_F_R.13[[i]] <- students_patterns_R[[i]] %>%
    filter(item_id == "RD516227") 
  
}

for(i in 1:length(true.theta_R)){
  if(nrow(patterns_module_F_R.13[[i]]) == 0){
    patterns_module_F_R.13[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_R)){
  patterns_module_F_R.13[[i]]$occ <-seq(1:nrow(patterns_module_F_R.13[[i]]))
} 
# Combine

patterns_module_F_R <- list()

for(i in 1:length(true.theta_R)){
  patterns_module_F_R[[i]] <- rbind(patterns_module_F_R.1[[i]], patterns_module_F_R.2[[i]], patterns_module_F_R.3[[i]], 
                                    patterns_module_F_R.4[[i]], patterns_module_F_R.5[[i]], patterns_module_F_R.6[[i]],
                                    patterns_module_F_R.7[[i]], patterns_module_F_R.8[[i]], patterns_module_F_R.9[[i]],
                                    patterns_module_F_R.10[[i]], patterns_module_F_R.11[[i]], patterns_module_F_R.12[[i]],
                                    patterns_module_F_R.13[[i]])
}

rm(patterns_module_F_R.1, patterns_module_F_R.2, patterns_module_F_R.3, 
   patterns_module_F_R.4, patterns_module_F_R.5, patterns_module_F_R.6,
   patterns_module_F_R.7, patterns_module_F_R.8, patterns_module_F_R.9, 
   patterns_module_F_R.10, patterns_module_F_R.11, patterns_module_F_R.12,
   patterns_module_F_R.13)

####Now combine everything to get the scores for the first day of Lezen

patterns_Day.1_R <- list()

for(i in 1:length(true.theta_R)){
  patterns_Day.1_R[[i]] <- rbind(patterns_module_A_R[[i]], patterns_module_B_R[[i]], patterns_module_C_R[[i]], 
                                 patterns_module_D_R[[i]], patterns_module_E_R[[i]], patterns_module_F_R[[i]])
}

rm(patterns_module_A_R, patterns_module_B_R, patterns_module_C_R, patterns_module_D_R, patterns_module_E_R, patterns_module_F_R, 
   placeholder.df, booklet_id, item_id, item_score)

# Continue to counting the number of errors

for( i in 1:length(true.theta_R)){
  patterns_Day.1_R[[i]]$error <- ifelse(patterns_Day.1_R[[i]]$item_score == 0, 1, 0)
}

#Exclude all other variables

for(i in 1:length(true.theta_R)){
  
  patterns_Day.1_R[[i]] <- patterns_Day.1_R[[i]] %>%
    select(occ, error)
}

# Separate each measurement in a separate data frame (or in this case tibble)
errors_Day.1_R <- list()
for(i in 1:length(true.theta_R)){
  errors_Day.1_R[[i]] <- split(patterns_Day.1_R[[i]], patterns_Day.1_R[[i]]$occ)
}

#Calculate the sum of errors for each repeated measurement across all students
n_errors_Day.1_R <-list()
for(i in 1:length(true.theta_R)){
  n_errors_Day.1_R[[i]] <- lapply(errors_Day.1_R[[i]], function(x){sum(x$error)})
}

#Unlist
for(i in 1:length(true.theta_R)){
  n_errors_Day.1_R[[i]] <- unlist( n_errors_Day.1_R[i])
}

#put in separate once columnt (df) i.e., vector
for(i in 1:length(true.theta_R)){
  n_errors_Day.1_R[i] <- as.data.frame(n_errors_Day.1_R[i])
}

#Now combine it with each repeated theta re-estimate
for(i in 1:length(true.theta_R)){
  students_abilities_R[[i]]$errors.Day1 <-  n_errors_Day.1_R[[i]]
}


###Analysis#####

#students_abilities_R_split.Day.1 <- list()
#for(i in 1:length(true.theta_R)){
#  students_abilities_R_split.Day.1[[i]] <-  split(students_abilities_R[[i]], students_abilities_R[[i]]$errors.Day1)
#}
#
#summaries_R.Day1 <- list()
#for(i in 1:length(true.theta_R)){
#  summaries_R.Day1[[i]] <- lapply(students_abilities_R_split.Day.1[[i]], summary)
#}

