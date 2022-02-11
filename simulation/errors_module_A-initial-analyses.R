#=========================
#Initial analyses for the number of mistakes made in module A
#========================
#Note the data has to be run through the `errors_module_A.R` script first
#------------------------------------------------------
# LEZEN
#------------------------------------------------------

for(i in c(1,   2,   3,   4,   5,   6,   7,   8,  41,  59,  78, 162, 166, 196, 197, 198, 199, 200)){
  print(unique(students_abilities_L[[i]]$errors))
}


print(mean(students_abilities_L_split[[1]]$`6`$theta))
a <- c(0,0,0,0,0,0,-1.097771)  #the last value is the true theta

for (i in c( 2,   3,   4,   5)){
  print(mean(students_abilities_L_split[[i]]$`5`$theta))
  print(mean(students_abilities_L_split[[i]]$`6`$theta))
}

b <- c(0,0,0,0,0,  -0.805334, -1.075197)
c <- c(0,0,0,0,0, -0.7875042, -1.061197)
d <- c(0,0,0,0,0, -0.799533, -1.026014)
e <- c(0,0,0,0,0, -0.7528137, -1.000861)

for (i in c( 6,   7,   8)){
  print(mean(students_abilities_L_split[[i]]$`4`$theta))
  print(mean(students_abilities_L_split[[i]]$`5`$theta))
  print(mean(students_abilities_L_split[[i]]$`6`$theta))
}

f <- c(0,0,0,0,-0.6200134, -0.7171452, -0.9265702)

g <- c(0,0,0,0,-0.4958055, -0.6858482, -0.8748593)

h <- c(0,0,0,0,-0.6022569, -0.6557785, -0.8010665)

for (i in c(41,  59,  78)){
  print(mean(students_abilities_L_split[[i]]$`1`$theta))
  print(mean(students_abilities_L_split[[i]]$`2`$theta))
  print(mean(students_abilities_L_split[[i]]$`3`$theta))
  print(mean(students_abilities_L_split[[i]]$`4`$theta))
  print(mean(students_abilities_L_split[[i]]$`5`$theta))
  print(mean(students_abilities_L_split[[i]]$`6`$theta))
}

i <- c(0, 0.06738997, 0.02938897,-0.000210555, -0.02094283, -0.05599033, -0.0868597)
j <- c(0, 0.1285401, 0.1043499, 0.07667284,  0.05197364, 0.03176222, -0.03640953)
k <- c(0, 0.1683823, 0.145977, 0.1108515, 0.07896659, 0.04963115, 0.02229565)


for (i in c(162, 166)){
  print(mean(students_abilities_L_split[[i]]$`0`$theta))
  print(mean(students_abilities_L_split[[i]]$`1`$theta))
  print(mean(students_abilities_L_split[[i]]$`2`$theta))
  print(mean(students_abilities_L_split[[i]]$`3`$theta))
  print(mean(students_abilities_L_split[[i]]$`4`$theta))
}

l <- c(0.4408603, 0.4042815, 0.3526681, 0.3147573, 0.2475633, 0, 0)
m <- c(0.4715094, 0.4129982, 0.3730442, 0.3465735, 0.4329932, 0, 0)

print(mean(students_abilities_L_split[[196]]$`0`$theta))
print(mean(students_abilities_L_split[[196]]$`1`$theta))
print(mean(students_abilities_L_split[[196]]$`2`$theta))

n <- c(1.197479, 0.9607391, 0.9077544, 0, 0, 0, 0)

for (i in c(197, 198, 199)){
  print(mean(students_abilities_L_split[[i]]$`0`$theta))
  print(mean(students_abilities_L_split[[i]]$`1`$theta))
}

o <- c(1.308786, 1.019476, 0, 0, 0, 0, 0)
p <- c(1.428722, 1.107104, 0, 0, 0, 0, 0)
q <- c(1.517053, 1.140868, 0, 0, 0, 0, 0)

print(mean(students_abilities_L_split[[200]]$`0`$theta))

r <- c(1.54481, 0, 0, 0, 0, 0, 0)

#data for tables (in appendix)
mistakes.modA.lezen.table <- data.frame(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
mistakes.modA.lezen.table <- as.data.frame(t(mistakes.modA.lezen.table))
mistakes.modA.lezen.table[mistakes.modA.lezen.table == 0] <- NA
mistakes.modA.lezen.table$avg.theta <- apply(mistakes.modA.lezen.table, 1, mean, na.rm = TRUE)
mistakes.modA.lezen.table[is.na(mistakes.modA.lezen.table)] <- 0
#add the true thetas
mistakes.modA.lezen.table$true.theta <- c(-2.54, -1.8, -1.5, -1.3, -1.2, -1, -0.9, -0.8, -0.007,  0.08,  0.13, 0.4,  0.43,  1.2,  1.4, 1.7, 2.2, 3.4)

#data for plots
mistakes.modA.lezen <- c(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
mistakes.modA.lezen.plot <- data.frame(mistakes.modA.lezen, rep(c(-2.54, -1.8, -1.5, -1.3, -1.2, -1, -0.9, -0.8, -0.007,  0.08,  0.13, 0.4,  0.43,  1.2,  1.4, 1.7, 2.2,  3.4), each = 7))
mistakes.modA.lezen.plot$mistakes <- rep(0:6, 18)
names(mistakes.modA.lezen.plot) <- c("Re-estimates", "True theta", "Mistakes")

#save the data
library(foreign)
write_csv(mistakes.modA.lezen.table, "mistakes.modA.lezen.table.csv")
write_csv(mistakes.modA.lezen.plot, "mistakes.modA.lezen.plot.csv")

#Obtain the plot
library(ggplot2)
mistakes.modA.lezen.plot$`True theta` <- as.factor(mistakes.modA.lezen.plot$`True theta`)

ggplot(mistakes.modA.lezen.plot[which(mistakes.modA.lezen.plot$`Re-estimates`!=0),], aes(x = Mistakes, y = `Re-estimates`, group = `True theta`)) +  
  geom_line(aes(color=`True theta`), size = 1, linetype = 3)+
  geom_point(aes(color=`True theta`)) +
  ylim(-1.2, 1.6) #+
  #scale_y_continuous(breaks =  c(-2.54, -1.8, -1.5, -1.3, -1.2, -1, -0.9, -0.8, -0.007,  0.08,  0.13, 0.4,  0.43,  1.2,  1.4, 1.7, 2.2, 3.4))
  #labs(x = "Total number of mistakes in modules A (for all three subjects)",
      # y = "Classification error", title = "Classification errors vs (total) number of mistakes in Module A")


#------------------------------------------------------
# REKENEN
#------------------------------------------------------
for (i in c( 1,   2,   3,  22,  45,  81,  89,  98, 141, 187, 200)){
  print(unique(students_abilities_R[[i]]$errors))
}


print(mean(students_abilities_R_split[[1]]$`4`$theta))
print(mean(students_abilities_R_split[[1]]$`5`$theta))
a <- c(0, 0, 0, 0, -1.106521, -1.383891)

for(i in c(2,3)){
  print(mean(students_abilities_R_split[[i]]$`3`$theta))
  print(mean(students_abilities_R_split[[i]]$`4`$theta))
  print(mean(students_abilities_R_split[[i]]$`5`$theta))
}
b <- c(0, 0, 0, -0.9658731, -1.052734, -1.283875)
c <- c(0, 0, 0, -0.90781, -0.9838026, -1.150232)

for(i in c(22,  45)){
  print(mean(students_abilities_R_split[[i]]$`0`$theta))
  print(mean(students_abilities_R_split[[i]]$`1`$theta))
  print(mean(students_abilities_R_split[[i]]$`2`$theta))
  print(mean(students_abilities_R_split[[i]]$`3`$theta))
  print(mean(students_abilities_R_split[[i]]$`4`$theta))
  print(mean(students_abilities_R_split[[i]]$`5`$theta))
}

d <- c(-0.07594316, -0.1317397, -0.1474175, -0.1679615, -0.1904007, -0.1844534)
e <- c(0.08405695, 0.06533081, 0.03807081, 0.02298288, -0.002579274, -0.003964214)


for(i in c(81,  89,  98)){
  print(mean(students_abilities_R_split[[i]]$`0`$theta))
  print(mean(students_abilities_R_split[[i]]$`1`$theta))
  print(mean(students_abilities_R_split[[i]]$`2`$theta))
  print(mean(students_abilities_R_split[[i]]$`3`$theta))
  print(mean(students_abilities_R_split[[i]]$`4`$theta))
}

f <- c(0.2271753, 0.2020615, 0.1777914, 0.1407536, 0.1065037, 0)
g <- c(0.2563725, 0.2255479, 0.2036752, 0.1480875, 0.1592695, 0)
h <- c(0.301739, 0.2628649, 0.2376078,0.1929994, 0.1670854, 0)


print(mean(students_abilities_R_split[[141]]$`0`$theta))
print(mean(students_abilities_R_split[[141]]$`1`$theta))
print(mean(students_abilities_R_split[[141]]$`2`$theta))
print(mean(students_abilities_R_split[[141]]$`3`$theta))

i <- c(0.5094521, 0.4663315, 0.4244001, 0.3497867, 0, 0)

print(mean(students_abilities_R_split[[187]]$`0`$theta))
print(mean(students_abilities_R_split[[187]]$`1`$theta))
print(mean(students_abilities_R_split[[187]]$`2`$theta))

j <- c( 0.9233876, 0.8655755, 0.765423, 0, 0, 0)

print(mean(students_abilities_R_split[[200]]$`0`$theta))
print(mean(students_abilities_R_split[[200]]$`1`$theta))

k <- c(1.74426, 1.350973, 0, 0, 0, 0)

#data for tables
mistakes.modA.rekenen.table <- data.frame(a, b, c, d, e, f, g, h, i, j, k)
mistakes.modA.rekenen.table <- as.data.frame(t(mistakes.modA.rekenen.table))
mistakes.modA.rekenen.table[mistakes.modA.rekenen.table == 0] <- NA
mistakes.modA.rekenen.table$avg.theta <- apply(mistakes.modA.rekenen.table, 1, mean, na.rm = TRUE)
mistakes.modA.rekenen.table[is.na(mistakes.modA.rekenen.table)] <- 0
#add the true thetas
mistakes.modA.rekenen.table$true.theta <- c(-1.97, -1.43, -1.17, -0.16, 0.042,  0.21,  0.23,  0.27, 0.5,  0.93, 1.85) 

#Data for plots
mistakes.modA.rekenen <- c(a, b, c, d, e, f, g, h, i, j, k)
mistakes.modA.rekenen.plot <- data.frame(mistakes.modA.rekenen, rep(c(-1.97, -1.43, -1.17, -0.16, 0.042,  0.21,  0.23,  0.27, 0.5,  0.93, 1.85), each = 6))
mistakes.modA.rekenen.plot$mistakes <- rep(0:5, 11)
names(mistakes.modA.rekenen.plot) <- c("Re-estimates", "True theta", "Mistakes")

#save
write_csv(mistakes.modA.rekenen.table, "mistakes.modA.rekenen.table.csv")
write_csv(mistakes.modA.rekenen.plot, "mistakes.modA.rekenen.plot.csv")

#plot

mistakes.modA.rekenen.plot$`True theta` <- as.factor(mistakes.modA.rekenen.plot$`True theta`)

ggplot(mistakes.modA.rekenen.plot[which(mistakes.modA.rekenen.plot$`Re-estimates`!=0),], aes(x = Mistakes, y = `Re-estimates`, group = `True theta`)) +  
  geom_line(aes(color=`True theta`), size = 1, linetype = 3)+
  geom_point(aes(color=`True theta`)) +
  ylim(-1.9, 1.9)

#------------------------------------------------------
# TAAL
#------------------------------------------------------
for (i in c(1,   2,  35,  40,  50,  80, 114, 126, 151, 173, 197, 198, 199, 200)){
  print(unique(students_abilities_T[[i]]$errors))
}

print(mean(students_abilities_T_split[[1]]$`3`$theta))
print(mean(students_abilities_T_split[[1]]$`4`$theta))
print(mean(students_abilities_T_split[[1]]$`5`$theta))

a <- c(0, 0, 0, -1.080217, -1.208618, -1.486848)

print(mean(students_abilities_T_split[[2]]$`2`$theta))
print(mean(students_abilities_T_split[[2]]$`3`$theta))
print(mean(students_abilities_T_split[[2]]$`4`$theta))
print(mean(students_abilities_T_split[[2]]$`5`$theta))

b <- c(0, 0, -0.8361318, -1.021674, -1.110697, -1.322772)

for (i in c(35,  40,  50)){
  print(mean(students_abilities_T_split[[i]]$`0`$theta))
  print(mean(students_abilities_T_split[[i]]$`1`$theta))
  print(mean(students_abilities_T_split[[i]]$`2`$theta))
  print(mean(students_abilities_T_split[[i]]$`3`$theta))
  print(mean(students_abilities_T_split[[i]]$`4`$theta))
  print(mean(students_abilities_T_split[[i]]$`5`$theta))
}

c <- c(0.02232902, -0.01262994, -0.03228247, -0.05830431, -0.06439781, -0.1245654)
d <- c(0.07396241, 0.039990181, 0.003881742, -0.02454378, -0.02944609, -0.05069841)
e <- c(0.1273112, 0.09075322, 0.06889079, 0.02958782, 0.004247461, -0.1126466)

for (i in c(80, 114, 126, 151)){
  print(mean(students_abilities_T_split[[i]]$`0`$theta))
  print(mean(students_abilities_T_split[[i]]$`1`$theta))
  print(mean(students_abilities_T_split[[i]]$`2`$theta))
  print(mean(students_abilities_T_split[[i]]$`3`$theta))
  print(mean(students_abilities_T_split[[i]]$`4`$theta))
}

f <- c(0.2039156, 0.17475, 0.1488729, 0.1039887, 0.06937735, 0)
g <- c(0.3164847, 0.2892954,  0.2656929, 0.2369992, 0.2195473, 0)
h <- c(0.3796649, 0.3471875,  0.3084062, 0.2702077, 0.262165, 0)
i <- c(0.4783319, 0.4432322, 0.3889314, 0.2972278, 0.1225794, 0)


print(mean(students_abilities_T_split[[173]]$`0`$theta))
print(mean(students_abilities_T_split[[173]]$`1`$theta))
print(mean(students_abilities_T_split[[173]]$`2`$theta))
print(mean(students_abilities_T_split[[173]]$`3`$theta))

j <- c(0.6089161, 0.566516, 0.5489141, 0.2598443, 0, 0)

for (i in c(197, 198)){
  print(mean(students_abilities_T_split[[i]]$`0`$theta))
  print(mean(students_abilities_T_split[[i]]$`1`$theta))
  print(mean(students_abilities_T_split[[i]]$`2`$theta))
}

k <- c(1.391322, 1.148818, 1.001995, 0, 0, 0)
l <- c(1.536291, 1.205486, 1.075224, 0, 0, 0)


for (i in c(199, 200)){
  print(mean(students_abilities_T_split[[i]]$`0`$theta))
  print(mean(students_abilities_T_split[[i]]$`1`$theta))
}

m <- c(1.621503, 1.237984, 0, 0, 0, 0)
n <- c(1.730166, 1.262784, 0, 0, 0, 0)

#data for tables
mistakes.modA.taal.table <- data.frame(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
mistakes.modA.taal.table <- as.data.frame(t(mistakes.modA.taal.table))
mistakes.modA.taal.table[mistakes.modA.taal.table == 0] <- NA
mistakes.modA.taal.table$avg.theta <- apply(mistakes.modA.taal.table, 1, mean, na.rm = TRUE)
mistakes.modA.taal.table[is.na(mistakes.modA.taal.table)] <- 0
#add the true thetas
mistakes.modA.taal.table$true.theta <- c(-1.7, -1.3, -0.03,  0.012,  0.07,  0.18,  0.304, 0.36,  0.46, 0.6, 1.4, 1.6,  1.8,  2.12) 

#Data for plots
mistakes.modA.taal <- c(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
mistakes.modA.taal.plot <- data.frame(mistakes.modA.taal, rep(c(-1.7, -1.3, -0.03,  0.012,  0.07,  0.18,  0.304, 0.36,  0.46, 0.6, 1.4, 1.6,  1.8,  2.12), each = 6))
mistakes.modA.taal.plot$mistakes <- rep(0:5, 14)
names(mistakes.modA.taal.plot) <- c("Re-estimates", "True theta", "Mistakes")

#save
write_csv(mistakes.modA.taal.table, "mistakes.modA.taal.table.csv")
write_csv(mistakes.modA.taal.plot, "mistakes.modA.taal.plot.csv")

#plot

mistakes.modA.taal.plot$`True theta` <- as.factor(mistakes.modA.taal.plot$`True theta`)

ggplot(mistakes.modA.taal.plot[which(mistakes.modA.taal.plot$`Re-estimates`!=0),], aes(x = Mistakes, y = `Re-estimates`, group = `True theta`)) +  
  geom_line(aes(color=`True theta`), size = 1, linetype = 3)+
  geom_point(aes(color=`True theta`)) +
  ylim(-1.9, 1.9)


