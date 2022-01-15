# dexterMST source https://github.com/dexter-psychometrics/dexterMST
# dexter is on CRAN
setwd("C:/Users/nikol/Desktop/MSc MSBBSS/Year-2_2021-2022/Internship")

library(dexterMST)
#library(dexter) #(dexterMST loads dexter automatically)

options(warn = 1)

test_design = read.csv('dexter_mst_design_dov.csv')
routing_rules = read.csv('dexter_mst_rules_dov.csv')
pars = read.csv('parameters_dov.csv')

# Select only "reading" (starts with L)
test_design = test_design[grepl('^L', test_design$module_id), ]
routing_rules = routing_rules[grepl('^L', routing_rules$module_id), ]
pars = pars[pars[, 'item_id'] %in% test_design$item_id, ]

theta = c(-1, 2)

results = dexterMST::sim_mst(pars, theta, test_design, routing_rules, routing = 'all')

abilities = dexter::ability(results, parms = pars, method = 'WLE')

