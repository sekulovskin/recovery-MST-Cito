#==================================================
# Number of errors (On the first day!!) for Lezen!!
#=================================================
library(dplyr)

#Look at which items are administered in day 1
#I will do it seperately just in case
test_design_T %>%
  filter(module_id == "TDA1")
test_design_T %>%
  filter(module_id == "TDB1")
test_design_T %>%
  filter(module_id == "TDC1")
test_design_T  %>%
  filter(module_id == "TDD1")
test_design_T  %>%
  filter(module_id == "TDE1")
test_design_T  %>%
  filter(module_id == "TDF1")

###For Module A
patterns_module_A_T.1 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00003D") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.1[[i]]$occ <- seq(1:100)
}

#item 2
patterns_module_A_T.2 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00038") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.2[[i]]$occ <- seq(1:100)
}

#item 3
patterns_module_A_T.3 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00090D") 
}

for(i in 1:length(true.theta_T)){
  patterns_module_A_T.3[[i]]$occ <- seq(1:100)
}

#item 4
patterns_module_A_T.4 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00074") 
}


for(i in 1:length(true.theta_T)){
  patterns_module_A_T.4[[i]]$occ <- seq(1:100)
}

#item 5
patterns_module_A_T.5 <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00148") 
}

for(i in 1:length(true.theta_R)){
  patterns_module_A_T.5[[i]]$occ <- seq(1:100)
}

##NOW combine
patterns_module_A_T <- list()
for (i in 1:length(true.theta_T)){
  patterns_module_A_T[[i]] <- rbind(patterns_module_A_T.1[[i]], patterns_module_A_T.2[[i]], patterns_module_A_T.3[[i]], patterns_module_A_T.4[[i]], patterns_module_A_T.5[[i]]) 
}

rm(patterns_module_A_T.1, patterns_module_A_T.2, patterns_module_A_T.3, patterns_module_A_T.4, patterns_module_A_T.5)


###For Module B 


patterns_module_B_T.1 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00174") 
  
}

#detour since we cannot add an indicator variable if our lists begins with empty DF's (for whatever reason)
booklet_id <- "placeholder"
person_id <- "placeholder"
item_id <- "placeholder"
item_score <- 1

placeholder.df <- data.frame(booklet_id, person_id, item_id, item_score)

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.1[[i]]) == 0){
    patterns_module_B_T.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.1[[i]]$occ <-seq(1:nrow(patterns_module_B_T.1[[i]]))
}   


#item 2 B

patterns_module_B_T.2 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00145") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.2[[i]]) == 0){
    patterns_module_B_T.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.2[[i]]$occ <-seq(1:nrow(patterns_module_B_T.2[[i]]))
} 

#item 3 B

patterns_module_B_T.3 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "RSW00138") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.3[[i]]) == 0){
    patterns_module_B_T.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.3[[i]]$occ <-seq(1:nrow(patterns_module_B_T.3[[i]]))
} 


#item 4 B

patterns_module_B_T.4 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00280") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.4[[i]]) == 0){
    patterns_module_B_T.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.4[[i]]$occ <-seq(1:nrow(patterns_module_B_T.4[[i]]))
} 

#item 5 B

patterns_module_B_T.5 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00079") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.5[[i]]) == 0){
    patterns_module_B_T.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.5[[i]]$occ <-seq(1:nrow(patterns_module_B_T.5[[i]]))
} 

#item 6 B

patterns_module_B_T.6 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.6[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00113") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.6[[i]]) == 0){
    patterns_module_B_T.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.6[[i]]$occ <-seq(1:nrow(patterns_module_B_T.6[[i]]))
} 

#item 7 B

patterns_module_B_T.7 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.7[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00099") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.7[[i]]) == 0){
    patterns_module_B_T.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.7[[i]]$occ <-seq(1:nrow(patterns_module_B_T.7[[i]]))
} 

#item 8 B

patterns_module_B_T.8 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_B_T.8[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00105") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_B_T.8[[i]]) == 0){
    patterns_module_B_T.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_B_T.8[[i]]$occ <-seq(1:nrow(patterns_module_B_T.8[[i]]))
} 


# Combine

patterns_module_B_T <- list()

for(i in 1:length(true.theta_T)){
  patterns_module_B_T[[i]] <- rbind(patterns_module_B_T.1[[i]], patterns_module_B_T.2[[i]], patterns_module_B_T.3[[i]], 
                                    patterns_module_B_T.4[[i]], patterns_module_B_T.5[[i]], patterns_module_B_T.6[[i]],
                                    patterns_module_B_T.7[[i]], patterns_module_B_T.8[[i]])
}

rm(patterns_module_B_T.1, patterns_module_B_T.2, patterns_module_B_T.3, 
   patterns_module_B_T.4, patterns_module_B_T.5, patterns_module_B_T.6,
   patterns_module_B_T.7, patterns_module_B_T.8)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module C

# 1 C
patterns_module_C_T.1 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00204") 
  
}

#detour since we cannot add an indicator variable if our lists begins with empty DF's (for whatever reason)
booklet_id <- "placeholder"
person_id <- "placeholder"
item_id <- "placeholder"
item_score <- 1

placeholder.df <- data.frame(booklet_id, person_id, item_id, item_score)

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.1[[i]]) == 0){
    patterns_module_C_T.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.1[[i]]$occ <-seq(1:nrow(patterns_module_C_T.1[[i]]))
}   


#item 2 C

patterns_module_C_T.2 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00252") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.2[[i]]) == 0){
    patterns_module_C_T.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.2[[i]]$occ <-seq(1:nrow(patterns_module_C_T.2[[i]]))
} 

#item 3 C

patterns_module_C_T.3 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00127") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.3[[i]]) == 0){
    patterns_module_C_T.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.3[[i]]$occ <-seq(1:nrow(patterns_module_C_T.3[[i]]))
} 


#item 4 C

patterns_module_C_T.4 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00306") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.4[[i]]) == 0){
    patterns_module_C_T.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.4[[i]]$occ <-seq(1:nrow(patterns_module_C_T.4[[i]]))
} 

#item 5 C

patterns_module_C_T.5 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00115") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.5[[i]]) == 0){
    patterns_module_C_T.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.5[[i]]$occ <-seq(1:nrow(patterns_module_C_T.5[[i]]))
} 

#item 6 C

patterns_module_C_T.6 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.6[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00112") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.6[[i]]) == 0){
    patterns_module_C_T.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.6[[i]]$occ <-seq(1:nrow(patterns_module_C_T.6[[i]]))
} 

#item 7 C

patterns_module_C_T.7 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.7[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00139") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.7[[i]]) == 0){
    patterns_module_C_T.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.7[[i]]$occ <-seq(1:nrow(patterns_module_C_T.7[[i]]))
} 

#item 8 C

patterns_module_C_T.8 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_C_T.8[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00126") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_C_T.8[[i]]) == 0){
    patterns_module_C_T.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_C_T.8[[i]]$occ <-seq(1:nrow(patterns_module_C_T.8[[i]]))
} 


# Combine

patterns_module_C_T <- list()

for(i in 1:length(true.theta_T)){
  patterns_module_C_T[[i]] <- rbind(patterns_module_C_T.1[[i]], patterns_module_C_T.2[[i]], patterns_module_C_T.3[[i]], 
                                    patterns_module_C_T.4[[i]], patterns_module_C_T.5[[i]], patterns_module_C_T.6[[i]],
                                    patterns_module_C_T.7[[i]], patterns_module_C_T.8[[i]])
}

rm(patterns_module_C_T.1, patterns_module_C_T.2, patterns_module_C_T.3, 
   patterns_module_C_T.4, patterns_module_C_T.5, patterns_module_C_T.6,
   patterns_module_C_T.7, patterns_module_C_T.8)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module D

#item 1 D

patterns_module_D_T.1 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00135") 
  
}

#detour since we cannot add an indicator variable if our lists begins with empty DF's (for whatever reason)
booklet_id <- "placeholder"
person_id <- "placeholder"
item_id <- "placeholder"
item_score <- 1

placeholder.df <- data.frame(booklet_id, person_id, item_id, item_score)

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.1[[i]]) == 0){
    patterns_module_D_T.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.1[[i]]$occ <-seq(1:nrow(patterns_module_D_T.1[[i]]))
}   


#item 2 D

patterns_module_D_T.2 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00142") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.2[[i]]) == 0){
    patterns_module_D_T.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.2[[i]]$occ <-seq(1:nrow(patterns_module_D_T.2[[i]]))
} 

#item 3 D

patterns_module_D_T.3 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00706D") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.3[[i]]) == 0){
    patterns_module_D_T.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.3[[i]]$occ <-seq(1:nrow(patterns_module_D_T.3[[i]]))
} 


#item 4 D

patterns_module_D_T.4 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00136") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.4[[i]]) == 0){
    patterns_module_D_T.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.4[[i]]$occ <-seq(1:nrow(patterns_module_D_T.4[[i]]))
} 

#item 5 D

patterns_module_D_T.5 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00134") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.5[[i]]) == 0){
    patterns_module_D_T.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.5[[i]]$occ <-seq(1:nrow(patterns_module_D_T.5[[i]]))
} 

#item 6 D

patterns_module_D_T.6 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.6[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00104") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.6[[i]]) == 0){
    patterns_module_D_T.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.6[[i]]$occ <-seq(1:nrow(patterns_module_D_T.6[[i]]))
} 

#item 7 D

patterns_module_D_T.7 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.7[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00315") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.7[[i]]) == 0){
    patterns_module_D_T.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.7[[i]]$occ <-seq(1:nrow(patterns_module_D_T.7[[i]]))
} 

#item 8 D

patterns_module_D_T.8 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_D_T.8[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00119") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_D_T.8[[i]]) == 0){
    patterns_module_D_T.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_D_T.8[[i]]$occ <-seq(1:nrow(patterns_module_D_T.8[[i]]))
} 


# Combine

patterns_module_D_T <- list()

for(i in 1:length(true.theta_T)){
  patterns_module_D_T[[i]] <- rbind(patterns_module_D_T.1[[i]], patterns_module_D_T.2[[i]], patterns_module_D_T.3[[i]], 
                                    patterns_module_D_T.4[[i]], patterns_module_D_T.5[[i]], patterns_module_D_T.6[[i]],
                                    patterns_module_D_T.7[[i]], patterns_module_D_T.8[[i]])
}

rm(patterns_module_D_T.1, patterns_module_D_T.2, patterns_module_D_T.3, 
   patterns_module_D_T.4, patterns_module_D_T.5, patterns_module_D_T.6,
   patterns_module_D_T.7, patterns_module_D_T.8)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module E

#item 1 E

patterns_module_E_T.1 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00092D") 
  
}

#detour since we cannot add an indicator variable if our lists begins with empty DF's (for whatever reason)
booklet_id <- "placeholder"
person_id <- "placeholder"
item_id <- "placeholder"
item_score <- 1

placeholder.df <- data.frame(booklet_id, person_id, item_id, item_score)

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.1[[i]]) == 0){
    patterns_module_E_T.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.1[[i]]$occ <-seq(1:nrow(patterns_module_E_T.1[[i]]))
}   


#item 2 E

patterns_module_E_T.2 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00243") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.2[[i]]) == 0){
    patterns_module_E_T.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.2[[i]]$occ <-seq(1:nrow(patterns_module_E_T.2[[i]]))
} 

#item 3 E

patterns_module_E_T.3 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00750") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.3[[i]]) == 0){
    patterns_module_E_T.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.3[[i]]$occ <-seq(1:nrow(patterns_module_E_T.3[[i]]))
} 


#item 4 E

patterns_module_E_T.4 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00764") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.4[[i]]) == 0){
    patterns_module_E_T.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.4[[i]]$occ <-seq(1:nrow(patterns_module_E_T.4[[i]]))
} 

#item 5 E

patterns_module_E_T.5 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00121") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.5[[i]]) == 0){
    patterns_module_E_T.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.5[[i]]$occ <-seq(1:nrow(patterns_module_E_T.5[[i]]))
} 

#item 6 E

patterns_module_E_T.6 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.6[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00116") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.6[[i]]) == 0){
    patterns_module_E_T.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.6[[i]]$occ <-seq(1:nrow(patterns_module_E_T.6[[i]]))
} 

#item 7 E

patterns_module_E_T.7 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.7[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00333") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.7[[i]]) == 0){
    patterns_module_E_T.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.7[[i]]$occ <-seq(1:nrow(patterns_module_E_T.7[[i]]))
} 

#item 8 E

patterns_module_E_T.8 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_E_T.8[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00147") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_E_T.8[[i]]) == 0){
    patterns_module_E_T.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_E_T.8[[i]]$occ <-seq(1:nrow(patterns_module_E_T.8[[i]]))
} 


# Combine

patterns_module_E_T <- list()

for(i in 1:length(true.theta_T)){
  patterns_module_E_T[[i]] <- rbind(patterns_module_E_T.1[[i]], patterns_module_E_T.2[[i]], patterns_module_E_T.3[[i]], 
                                    patterns_module_E_T.4[[i]], patterns_module_E_T.5[[i]], patterns_module_E_T.6[[i]],
                                    patterns_module_E_T.7[[i]], patterns_module_E_T.8[[i]])
}

rm(patterns_module_E_T.1, patterns_module_E_T.2, patterns_module_E_T.3, 
   patterns_module_E_T.4, patterns_module_E_T.5, patterns_module_E_T.6,
   patterns_module_E_T.7, patterns_module_E_T.8)
#As we can see, there are a few students on the positive extreme that do not have any items from module B (as expected)

###For Module F

#item 1 F

patterns_module_F_T.1 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.1[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00201") 
  
}

#detour since we cannot add an indicator variable if our lists begins with empty DF's (for whatever reason)
booklet_id <- "placeholder"
person_id <- "placeholder"
item_id <- "placeholder"
item_score <- 1

placeholder.df <- data.frame(booklet_id, person_id, item_id, item_score)

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.1[[i]]) == 0){
    patterns_module_F_T.1[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.1[[i]]$occ <-seq(1:nrow(patterns_module_F_T.1[[i]]))
}   


#item 2 F

patterns_module_F_T.2 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.2[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SN00250") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.2[[i]]) == 0){
    patterns_module_F_T.2[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.2[[i]]$occ <-seq(1:nrow(patterns_module_F_T.2[[i]]))
} 

#item 3 E

patterns_module_F_T.3 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.3[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW00261") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.3[[i]]) == 0){
    patterns_module_F_T.3[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.3[[i]]$occ <-seq(1:nrow(patterns_module_F_T.3[[i]]))
} 


#item 4 F

patterns_module_F_T.4 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.4[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "SW02989") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.4[[i]]) == 0){
    patterns_module_F_T.4[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.4[[i]]$occ <-seq(1:nrow(patterns_module_F_T.4[[i]]))
} 

#item 5 F

patterns_module_F_T.5 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.5[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00044") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.5[[i]]) == 0){
    patterns_module_F_T.5[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.5[[i]]$occ <-seq(1:nrow(patterns_module_F_T.5[[i]]))
} 

#item 6 F

patterns_module_F_T.6 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.6[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "IP00109") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.6[[i]]) == 0){
    patterns_module_F_T.6[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.6[[i]]$occ <-seq(1:nrow(patterns_module_F_T.6[[i]]))
} 

#item 7 F

patterns_module_F_T.7 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.7[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00067") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.7[[i]]) == 0){
    patterns_module_F_T.7[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.7[[i]]$occ <-seq(1:nrow(patterns_module_F_T.7[[i]]))
} 

#item 8 E

patterns_module_F_T.8 <- list()
for (i in 1:length(true.theta_T)){
  
  patterns_module_F_T.8[[i]] <- students_patterns_T[[i]] %>%
    filter(item_id == "GR00143") 
  
}

for(i in 1:length(true.theta_T)){
  if(nrow(patterns_module_F_T.8[[i]]) == 0){
    patterns_module_F_T.8[[i]] <- placeholder.df
  }
}

for( i in 1:length(true.theta_T)){
  patterns_module_F_T.8[[i]]$occ <-seq(1:nrow(patterns_module_F_T.8[[i]]))
} 


# Combine

patterns_module_F_T <- list()

for(i in 1:length(true.theta_T)){
  patterns_module_F_T[[i]] <- rbind(patterns_module_F_T.1[[i]], patterns_module_F_T.2[[i]], patterns_module_F_T.3[[i]], 
                                    patterns_module_F_T.4[[i]], patterns_module_F_T.5[[i]], patterns_module_F_T.6[[i]],
                                    patterns_module_F_T.7[[i]], patterns_module_F_T.8[[i]])
}

rm(patterns_module_F_T.1, patterns_module_F_T.2, patterns_module_F_T.3, 
   patterns_module_F_T.4, patterns_module_F_T.5, patterns_module_F_T.6,
   patterns_module_F_T.7, patterns_module_F_T.8)

####Now combine everything to get the scores for the first day of Lezen

patterns_Day.1_T <- list()

for(i in 1:length(true.theta_T)){
  patterns_Day.1_T[[i]] <- rbind(patterns_module_A_T[[i]], patterns_module_B_T[[i]], patterns_module_C_T[[i]], 
                                 patterns_module_D_T[[i]], patterns_module_E_T[[i]], patterns_module_F_T[[i]])
}

rm(patterns_module_A_T, patterns_module_B_T, patterns_module_C_T, patterns_module_D_T, patterns_module_E_T, patterns_module_F_T, 
   placeholder.df, booklet_id, item_id, item_score)

# Continue to counting the number of errors

for( i in 1:length(true.theta_T)){
  patterns_Day.1_T[[i]]$error <- ifelse(patterns_Day.1_T[[i]]$item_score == 0, 1, 0)
}

#Exclude all other variables

for(i in 1:length(true.theta_T)){
  
  patterns_Day.1_T[[i]] <- patterns_Day.1_T[[i]] %>%
    select(occ, error)
}

# Separate each measurement in a separate data frame (or in this case tibble)
errors_Day.1_T <- list()
for(i in 1:length(true.theta_T)){
  errors_Day.1_T[[i]] <- split(patterns_Day.1_T[[i]], patterns_Day.1_T[[i]]$occ)
}

#Calculate the sum of errors for each repeated measurement across all students
n_errors_Day.1_T <-list()
for(i in 1:length(true.theta_T)){
  n_errors_Day.1_T[[i]] <- lapply(errors_Day.1_T[[i]], function(x){sum(x$error)})
}

#Unlist
for(i in 1:length(true.theta_T)){
  n_errors_Day.1_T[[i]] <- unlist(n_errors_Day.1_T[i])
}

#put in separate once columnt (df) i.e., vector
for(i in 1:length(true.theta_T)){
  n_errors_Day.1_T[i] <- as.data.frame(n_errors_Day.1_T[i])
}

#Now combine it with each repeated theta re-estimate
for(i in 1:length(true.theta_T)){
  students_abilities_T[[i]]$errors.Day1 <-  n_errors_Day.1_T[[i]]
}


###Analysis#####

students_abilities_T_split.Day.1 <- list()
for(i in 1:length(true.theta_T)){
  students_abilities_T_split.Day.1[[i]] <-  split(students_abilities_T[[i]], students_abilities_T[[i]]$errors.Day1)
}

summaries_T.Day1 <- list()
for(i in 1:length(true.theta_T)){
  summaries_T.Day1[[i]] <- lapply(students_abilities_T_split.Day.1[[i]], summary)
}

