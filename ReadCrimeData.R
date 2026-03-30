library(haven)
library(labelled)
library(prettyR)
library(tidyverse)
library(readr)
library(dplyr)
a<-c(4,1,1,4,2,3,8,3,1,8,6,6,5,5,4,4,5,5,5,5,5,3,5,4,5,3,4,5,5,4,4,5,5,5,4,4,
     5,5,5,5,4,5,4,3,3,4,4,5,5,5,5,4,6,4,5,5)
b<-c("STUDYNO",'EDITION','PART','IDNO','FIPS_ST','FIPS_CTY','CPOPARST',
     'AG_ARRST','JURFLAG','COVIND','GRNDTOT','P1TOT','P1VLNT','P1PRPTY',
     'MURDER','RAPE','ROBBERY','AGASSLT','BURGLRY','LARCENY','MVTHEFT','ARSON',
     'OTHASLT','FRGYCNT','FRAUD','EMBEZL','STLNPRP','VANDLSM','WEAPONS',
     'COMVICE','SEXOFF','DRUGTOT','DRGSALE','COCSALE','MJSALE','SYNSALE',
     'OTHSALE','DRGPOSS','COCPOSS','MJPOSS','SYNPOSS','OTHPOSS','GAMBLE',
     'BOOKMKG','NUMBERS','OTGAMBL','OFAGFAM','DUI','LIQUOR','DRUNK','DISORDR',
     'VAGRANT','ALLOTHR','SUSPICN','CURFEW','RUNAWAY','SPOPARST','YEAR')
aa<-c(4,1,1,4,2,3,8,8,3,1,8,6,6,5,5,4,4,5,5,5,5,5,3,5,4,5,3,4,5,5,4,4,5,5,5,4,4,
      4,5,5,4,3,5,4,3,3,4,4,5,5,5,5,4,6,4,4,5)
bb<-c("STUDYNO",'EDITION','PART','IDNO','FIPS_ST','FIPS_CTY','SPOPARST', 'CPOPARST',
      'AG_ARRST','JURFLAG','COVIND','GRNDTOT','P1TOT','P1VLNT','P1PRPTY',
      'MURDER','RAPE','ROBBERY','AGASSLT','BURGLRY','LARCENY','MVTHEFT','ARSON',
      'OTHASLT','FRGYCNT','FRAUD','EMBEZL','STLNPRP','VANDLSM','WEAPONS',
      'COMVICE','SEXOFF','DRUGTOT','DRGSALE','COCSALE','MJSALE','SYNSALE',
      'OTHSALE','DRGPOSS','COCPOSS','MJPOSS','SYNPOSS','OTHPOSS','GAMBLE',
      'BOOKMKG','NUMBERS','OTGAMBL','OFAGFAM','DUI','LIQUOR','DRUNK','DISORDR',
      'VAGRANT','ALLOTHR','SUSPICN','CURFEW','RUNAWAY')
x<-read.fwf("27644_0001_data.txt", width = a, col.names = b)
length(b)
xx<-read.fwf("27644_0005_data.txt", width = aa, col.names = bb)
z<-rep(2008, length(x$STUDYNO))
x$SPOPARST<-xx$SPOPARST
x$YEAR<-z
save(x, file = "2008RawCrime.RData")

crime1999<- read_dta("03167-0001-Data.dta")
crime1999_2<- read_dta("03167-0005-Data.dta")
crime1999<-crime1999[,-1]
crime1999$SPOPARST<-crime1999_2$SPOPARST
crime1999<-as.data.frame(crime1999)
crimeALL<-as.data.frame(crimeALL)
crime1999$YEAR<-rep(1999, length(crime1999$STUDYNO))
colnames(crime1999)<-b
crimeALL<-rbind(crime1999,crimeALL)

crime2000<- read_dta("03451-0001-Data.dta")
crime2000_2<- read_dta("03451-0005-Data.dta")
crime2000<-crime2000[,-1]
crime2000$SPOPARST<-crime2000_2$SPOPARST
crime2000<-as.data.frame(crime2000)
crime2000$YEAR<-rep(2000, length(crime2000$STUDYNO))
colnames(crime2000)<-b
val_labels(crime2000)<- NULL
crimeALL<-rbind(crime2000,crimeALL)

crime2001<- read_dta("03721-0001-Data.dta")
crime2001_2<- read_dta("03721-0005-Data.dta")
crime2001<-crime2001[,-1]
crime2001$SPOPARST<-crime2001_2$SPOPARST
crime2001<-as.data.frame(crime2001)
crime2001$YEAR<-rep(2001, length(crime2001$STUDYNO))
colnames(crime2001)<-b
val_labels(crime2001)<- NULL
crimeALL<-rbind(crime2001,crimeALL)

crime2002<- read_dta("04009-0001-Data.dta")
crime2002_2<- read_dta("04009-0005-Data.dta")
crime2002<-crime2002[,-1]
crime2002$SPOPARST<-crime2002_2$SPOPARST
crime2002<-as.data.frame(crime2002)
crime2002$YEAR<-rep(2002, length(crime2002$STUDYNO))
colnames(crime2002)<-b
val_labels(crime2002)<- NULL
crimeALL<-rbind(crime2002,crimeALL)

crime2009<-da30763.0001
crime2009_2<-da30763.0005
crime2009$SPOPARST<-crime2009_2$SPOPARST
crime2009<-as.data.frame(crime2009)
crimeALL<-as.data.frame(crimeALL)
crime2009$YEAR<-rep(2009, length(crime2009$STUDYNO))
colnames(crime2009)<-b
crimeALL<-rbind(crime2009,crimeALL)

crime2010<-da33523.0001
crime2010_2<-da33523.0005
crime2010$SPOPARST<-crime2010_2$SPOPARST
crime2010<-as.data.frame(crime2010)
crimeALL<-as.data.frame(crimeALL)
crime2010$YEAR<-rep(2010, length(crime2010$STUDYNO))
colnames(crime2010)<-b
crimeALL<-rbind(crime2010,crimeALL)

crime2011<-da34582.0001
crime2011_2<-da34582.0005
crime2011$SPOPARST<-crime2011_2$SPOPARST
crime2011<-as.data.frame(crime2011)
crimeALL<-as.data.frame(crimeALL)
crime2011$YEAR<-rep(2011, length(crime2011$STUDYNO))
colnames(crime2011)<-b
crimeALL<-rbind(crime2011,crimeALL)

crime2012<-da35019.0001
crime2012_2<-da35019.0005
crime2012$SPOPARST<-crime2012_2$SPOPARST
crime2012<-as.data.frame(crime2012)
crimeALL<-as.data.frame(crimeALL)
crime2012$YEAR<-rep(2012, length(crime2012$STUDYNO))
colnames(crime2012)<-b
crimeALL<-rbind(crime2012,crimeALL)

lbls <- sort(levels(da36117.0001$STUDYNO))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36117.0001$STUDYNO <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36117.0001$STUDYNO))
da36117.0001$STUDYNO <- add.value.labels(da36117.0001$STUDYNO, lbls)

lbls <- sort(levels(da36117.0001$EDITION))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36117.0001$EDITION <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36117.0001$EDITION))
da36117.0001$EDITION <- add.value.labels(da36117.0001$EDITION, lbls)

lbls <- sort(levels(da36117.0001$PART))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36117.0001$PART <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36117.0001$PART))
da36117.0001$PART <- add.value.labels(da36117.0001$PART, lbls)

lbls <- sort(levels(da36117.0001$JURFLAG))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36117.0001$JURFLAG <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36117.0001$JURFLAG))
da36117.0001$JURFLAG <- add.value.labels(da36117.0001$JURFLAG, lbls)

crime2013<-da36117.0001
crime2013_2<-da36117.0005
val_labels(crime2013)<- NULL
crime2013$SPOPARST<-crime2013_2$SPOPARST
crime2013<-as.data.frame(crime2013)
crimeALL<-as.data.frame(crimeALL)
crime2013$YEAR<-rep(2013, length(crime2013$STUDYNO))
colnames(crime2013)<-b
crimeALL<-rbind(crime2013,crimeALL)

lbls <- sort(levels(da36399.0001$STUDYNO))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36399.0001$STUDYNO <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36399.0001$STUDYNO))
da36399.0001$STUDYNO <- add.value.labels(da36399.0001$STUDYNO, lbls)

lbls <- sort(levels(da36399.0001$EDITION))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36399.0001$EDITION <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36399.0001$EDITION))
da36399.0001$EDITION <- add.value.labels(da36399.0001$EDITION, lbls)

lbls <- sort(levels(da36399.0001$PART))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36399.0001$PART <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36399.0001$PART))
da36399.0001$PART <- add.value.labels(da36399.0001$PART, lbls)

lbls <- sort(levels(da36399.0001$JURFLAG))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da36399.0001$JURFLAG <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da36399.0001$JURFLAG))
da36399.0001$JURFLAG <- add.value.labels(da36399.0001$JURFLAG, lbls)

crime2014<-da36399.0001
crime2014_2<-da36399.0005
val_labels(crime2014)<- NULL
crime2014$SPOPARST<-crime2014_2$SPOPARST
crime2014<-as.data.frame(crime2014)
crimeALL<-as.data.frame(crimeALL)
crime2014$YEAR<-rep(2014, length(crime2014$STUDYNO))
colnames(crime2014)<-b
crimeALL<-rbind(crime2014,crimeALL)

lbls <- sort(levels(da37059.0001$STUDYNO))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da37059.0001$STUDYNO <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da37059.0001$STUDYNO))
da37059.0001$STUDYNO <- add.value.labels(da37059.0001$STUDYNO, lbls)

lbls <- sort(levels(da37059.0001$EDITION))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da37059.0001$EDITION <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da37059.0001$EDITION))
da37059.0001$EDITION <- add.value.labels(da37059.0001$EDITION, lbls)

lbls <- sort(levels(da37059.0001$PART))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da37059.0001$PART <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da37059.0001$PART))
da37059.0001$PART <- add.value.labels(da37059.0001$PART, lbls)

lbls <- sort(levels(da37059.0001$JURFLAG))
lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
da37059.0001$JURFLAG <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", da37059.0001$JURFLAG))
da37059.0001$JURFLAG <- add.value.labels(da37059.0001$JURFLAG, lbls)

adjacency2010 <- read_csv("county_adjacency2010.csv", 
                                 col_types = cols(fipscounty = col_number(), 
                                                  fipsneighbor = col_number()))

crime2016<-da37059.0001
crime2016_2<-da37059.0005
val_labels(crime2016)<- NULL
crime2016$SPOPARST<-crime2016_2$SPOPARST
crime2016<-as.data.frame(crime2016)
crimeALL<-as.data.frame(crimeALL)
crime2016$YEAR<-rep(2016, length(crime2016$STUDYNO))
colnames(crime2016)<-b
crimeALL<-rbind(crime2016,crimeALL)

save(crimeALL, file = "crimeall.RData")

crimeALL$CR<-crimeALL$GRNDTOT/crimeALL$CPOPARST
crimeALL$POPR<-crimeALL$CPOPARST/crimeALL$SPOPARST
crimeALL$WCR<-crimeALL$CR*crimeALL$POPR

crimeALL_T<-crimeALL
crimeALL_T$FIPS_CTY<-formatC(crimeALL$FIPS_CTY, digits = 3, flag = "0",width = 0)
crimeALL_T<-crimeALL_T %>% unite(fipsneighbor, FIPS_ST, FIPS_CTY, sep = "",remove = FALSE)
str(crimeALL_T)
crimeALL_T$fipsneighbor<-as.numeric(crimeALL_T$fipsneighbor)
str(crimeALL_T)

adjacency2010$YEAR<-rep(1999, length(adjacency2010$fipsneighbor))
rm(adjacency2010)

adjacency2010 <- read_csv("county_adjacency2010.csv", 
                                 col_types = cols(fipscounty = col_number(), 
                                                  fipsneighbor = col_number()))
rm(county_adjacency2010)
adjacency_99<-adjacency2010
adjacency_00<-adjacency2010
adjacency_01<-adjacency2010
adjacency_02<-adjacency2010
adjacency_03<-adjacency2010
adjacency_04<-adjacency2010
adjacency_05<-adjacency2010
adjacency_06<-adjacency2010
adjacency_07<-adjacency2010
adjacency_08<-adjacency2010
adjacency_09<-adjacency2010
adjacency_10<-adjacency2010
adjacency_11<-adjacency2010
adjacency_12<-adjacency2010
adjacency_13<-adjacency2010
adjacency_14<-adjacency2010
adjacency_16<-adjacency2010
adjacency_99$YEAR<-rep(1999, length(adjacency_99$fipsneighbor))
adjacency_00$YEAR<-rep(2000, length(adjacency_00$fipsneighbor))
adjacency_01$YEAR<-rep(2001, length(adjacency_01$fipsneighbor))
adjacency_02$YEAR<-rep(2002, length(adjacency_02$fipsneighbor))
adjacency_03$YEAR<-rep(2003, length(adjacency_03$fipsneighbor))
adjacency_04$YEAR<-rep(2004, length(adjacency_04$fipsneighbor))
adjacency_05$YEAR<-rep(2005, length(adjacency_05$fipsneighbor))
adjacency_06$YEAR<-rep(2006, length(adjacency_06$fipsneighbor))
adjacency_07$YEAR<-rep(2007, length(adjacency_07$fipsneighbor))
adjacency_08$YEAR<-rep(2008, length(adjacency_08$fipsneighbor))
adjacency_09$YEAR<-rep(2009, length(adjacency_09$fipsneighbor))
adjacency_10$YEAR<-rep(2010, length(adjacency_10$fipsneighbor))
adjacency_11$YEAR<-rep(2011, length(adjacency_11$fipsneighbor))
adjacency_12$YEAR<-rep(2012, length(adjacency_12$fipsneighbor))
adjacency_13$YEAR<-rep(2013, length(adjacency_13$fipsneighbor))
adjacency_14$YEAR<-rep(2014, length(adjacency_14$fipsneighbor))
adjacency_16$YEAR<-rep(2016, length(adjacency_16$fipsneighbor))
adjacency<-rbind(adjacency_99, adjacency_00, adjacency_01, adjacency_02,
                 adjacency_03, adjacency_04, adjacency_05, adjacency_06,
                 adjacency_07, adjacency_08, adjacency_09, adjacency_10,
                 adjacency_11, adjacency_12, adjacency_13, adjacency_14,
                 adjacency_16)
rm(adjacency_00, adjacency_01, adjacency_02, adjacency_03, adjacency_04,
   adjacency_05, adjacency_06, adjacency_07, adjacency_08, adjacency_09,
   adjacency_10, adjacency_11, adjacency_12, adjacency_13, adjacency_14,
   adjacency_16)
rm(adjacency_99)
rm(crime)
crime<-crimeALL_T[c("fipsneighbor","YEAR","WCR")]
x<-full_join(adjacency, crime, by="fipsneighbor","YEAR")
x<-inner_join(adjacency, crime, by="YEAR", "fipsneighbor", relationship = "many-to-many")
