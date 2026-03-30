rm(list = ls())
graphics.off()

library(haven)
library(labelled)
library(prettyR)
library(tidyverse)
library(readr)
library(dplyr)

load("crimeall.RData")
adjacency2010 <- read_csv("county_adjacency2010.csv", 
                          col_types = cols(fipscounty = col_number(), 
                                           fipsneighbor = col_number()))

crimeALL$CR<-crimeALL$GRNDTOT/crimeALL$CPOPARST
crimeALL$POPR<-crimeALL$CPOPARST/crimeALL$SPOPARST
crimeALL$WCR<-crimeALL$CR*crimeALL$POPR

crimeALL_T<-crimeALL
crimeALL_T$FIPS_CTY<-formatC(crimeALL$FIPS_CTY, digits = 3, flag = "0",
                             width = 0)
crimeALL_T$FIPS_ST<-formatC(crimeALL$FIPS_ST, digits = 2, flag = "0",width = 0)
crimeALL_T<-crimeALL_T %>% unite(fipsneighbor, FIPS_ST, FIPS_CTY, sep = "",
                                 remove = FALSE)
crimeALL_T$fipsneighbor<-as.numeric(crimeALL_T$fipsneighbor)