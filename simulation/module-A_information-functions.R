###Information functions for module A######

#Lezen
inf_fun_modA_L <- dexter::information(pars_L,items = c("BL00518", "BL00519", "BL00520", "BL00521", "BL02412",  "OP00142"))
plot(inf_fun_modA_L, from = -2, to= 2, main = "Lezen, Module A")


#Rekenen
inf_fun_modA_R <- dexter::information(pars_R, items = c("RD516087", "RD516456", "RD516A28", "RD516663", "RD516375"))
plot(inf_fun_modA_R, from = -2, to= 2, main = "Rekenen, Module A")


#Taal
inf_fun_modA_T <- dexter::information(pars_T, items = c("SN00003D", "SN00038", "SW00090D", "IP00074", "GR00148"))
plot(inf_fun_modA_T, from = -2, to= 2, main = "Taal, Module A")


#Full test (booklet) information functions would be too many to derive, and I do not not
#how informative they will be. So I am leaving this as an open question to be discussed on Wednesday.