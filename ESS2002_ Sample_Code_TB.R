# Tomine Bergseth
# Sample code for Research Project: Professor Hajar Yazdiha
# Working with data from the European Social Survey, 2002
# This sample is only for Sweden, other countries worked on include Spain, France, Germany, and the United Kingdom
# Last worked on: January - April, 2022


########################################################################


#clear global environment 
rm(list=ls())

#set working directory
setwd("C:/Users/tberg_cl8rsaz/OneDrive/Dokumenter/R/Training Data 412_413 F21")

#loading in data packages for use today
library(tidyverse)
library(dplyr)
library(tidyr)
library(foreign)
library(car)
library(countrycode)

# Starting by cleaning up the overall dataset before working on specific countries 

ESS02 <- read.spss("ESS1e06_6.sav") #load in full 2002 dataset

ESS2002_dat <- as.data.frame(ESS02) %>% #converting to dataframe and selecting needed variables
  select(eimpcnt, qfimlng, qfimchr, qfimwht, 
         qfimcmt, imtcjob, imbleco, imwbcrm,
         pplstrd, lwdscwp, noimbro, imueclt, brncntr, cntry, facntr, mocntr, cntbrth)


dat3 <- ESS2002_dat # changed it do dat3 to allow me to reuse code I already did in a different context

# using gsub to change values for responses, creating numeric values

dat3$eimpcnt <- gsub("Allow a few", "3", as.character(dat3$eimpcnt))
dat3$eimpcnt <- gsub("Allow many to come and live here", "1", as.character(dat3$eimpcnt))
dat3$eimpcnt <- gsub("Allow some", "2", as.character(dat3$eimpcnt))
dat3$eimpcnt <- gsub("Allow none", "4", as.character(dat3$eimpcnt))
dat3$eimpcnt <- gsub("Refusal", "0", as.character(dat3$eimpcnt))
dat3$eimpcnt <- gsub("Don't know", "0", as.character(dat3$eimpcnt))
dat3$eimpcnt <- gsub("No answer", "0", as.character(dat3$eimpcnt))

dat3$eimpcnt <-  as.numeric(dat3$eimpcnt) # making sure all values are numeric

dat4 <- dat3 %>%
  filter(eimpcnt != 0) # removing all 0s - (changed refusal, dont knows, and no answers to 0 to remove them)

# repating the process of gsubing and removing undesired responses from all variables in the dataset

da1 <- dat3

da1$qfimlng <- gsub("Extremely unimportant", "0", as.character(da1$qfimlng))
da1$qfimlng <- gsub("Extremely important", "10", as.character(da1$qfimlng))
da1$qfimlng <- gsub("Refusal", "G", as.character(da1$qfimlng))
da1$qfimlng <- gsub("Don't know", "G", as.character(da1$qfimlng))
da1$qfimlng <- gsub("No answer", "G", as.character(da1$qfimlng))

da12 <- da1 %>%
  filter(qfimlng != "G")

da12$qfimlng <-  as.numeric(da12$qfimlng)

a1 <- da12

a1$qfimchr <- gsub("Extremely unimportant", "0", as.character(a1$qfimchr))
a1$qfimchr <- gsub("Extremely important", "10", as.character(a1$qfimchr))
a1$qfimchr <- gsub("Refusal", "G", as.character(a1$qfimchr))
a1$qfimchr <- gsub("Don't know", "G", as.character(a1$qfimchr))
a1$qfimchr <- gsub("No answer", "G", as.character(a1$qfimchr))

a12 <- a1 %>%
  filter(qfimchr != "G")

a12$qfimchr <-  as.numeric(a12$qfimchr)

c1 <- a12

c1$qfimwht <- gsub("Extremely unimportant", "0", as.character(c1$qfimwht))
c1$qfimwht <- gsub("Extremely important", "10", as.character(c1$qfimwht))
c1$qfimwht <- gsub("Refusal", "G", as.character(c1$qfimwht))
c1$qfimwht <- gsub("Don't know", "G", as.character(c1$qfimwht))
c1$qfimwht <- gsub("No answer", "G", as.character(c1$qfimwht))

c12 <- c1 %>%
  filter(qfimwht != "G")

c12$qfimwht <-  as.numeric(c12$qfimwht) 

f1 <- c12

f1$qfimcmt <- gsub("Extremely unimportant", "0", as.character(f1$qfimcmt))
f1$qfimcmt <- gsub("Extremely important", "10", as.character(f1$qfimcmt))
f1$qfimcmt <- gsub("Refusal", "G", as.character(f1$qfimcmt))
f1$qfimcmt <- gsub("Don't know", "G", as.character(f1$qfimcmt))
f1$qfimcmt <- gsub("No answer", "G", as.character(f1$qfimcmt))

f12 <- f1 %>%
  filter(qfimcmt != "G")

f12$qfimcmt <-  as.numeric(f12$qfimcmt)

h1 <- f12


h1$imtcjob <- gsub("Take jobs away", "10", as.character(h1$imtcjob))#flipping values assigned
h1$imtcjob <- gsub("1", "9", as.character(h1$imtcjob))
h1$imtcjob <- gsub("2", "8", as.character(h1$imtcjob))
h1$imtcjob <- gsub("3", "7", as.character(h1$imtcjob))
h1$imtcjob <- gsub("4", "6", as.character(h1$imtcjob))
h1$imtcjob <- gsub("5", "5", as.character(h1$imtcjob))
h1$imtcjob <- gsub("6", "4", as.character(h1$imtcjob))
h1$imtcjob <- gsub("7", "3", as.character(h1$imtcjob))
h1$imtcjob <- gsub("8", "2", as.character(h1$imtcjob))
h1$imtcjob <- gsub("9", "1", as.character(h1$imtcjob))
h1$imtcjob <- gsub("Create new jobs", "0", as.character(h1$imtcjob))
h1$imtcjob <- gsub("Refusal", "G", as.character(h1$imtcjob))
h1$imtcjob <- gsub("Don't know", "G", as.character(h1$imtcjob))
h1$imtcjob <- gsub("No answer", "G", as.character(h1$imtcjob))


h12 <- h1 %>%
  filter(imtcjob != "G")

h12$imtcjob <-  as.numeric(h12$imtcjob) 



j1 <- h12

j1$imbleco <- gsub("Generally take out more", "10", as.character(j1$imbleco))#flipping values
j1$imbleco <- gsub("1", "9", as.character(j1$imbleco))
j1$imbleco <- gsub("2", "8", as.character(j1$imbleco))
j1$imbleco <- gsub("3", "7", as.character(j1$imbleco))
j1$imbleco <- gsub("4", "6", as.character(j1$imbleco))
j1$imbleco <- gsub("5", "5", as.character(j1$imbleco))
j1$imbleco <- gsub("6", "4", as.character(j1$imbleco))
j1$imbleco <- gsub("7", "3", as.character(j1$imbleco))
j1$imbleco <- gsub("8", "2", as.character(j1$imbleco))
j1$imbleco <- gsub("9", "1", as.character(j1$imbleco))
j1$imbleco <- gsub("Generally put in more", "0", as.character(j1$imbleco))
j1$imbleco <- gsub("Refusal", "G", as.character(j1$imbleco))
j1$imbleco <- gsub("Don't know", "G", as.character(j1$imbleco))
j1$imbleco <- gsub("No answer", "G", as.character(j1$imbleco))

j12 <- j1 %>%
  filter(imbleco != "G")

j12$imbleco <-  as.numeric(j12$imbleco) 

l1 <- j12

l1$imwbcrm <- gsub("Crime problems made worse", "10", as.character(l1$imwbcrm)) # flip values
l1$imwbcrm <- gsub("1", "9", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("2", "8", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("3", "7", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("4", "6", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("5", "5", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("6", "4", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("7", "3", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("8", "2", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("9", "1", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("Crime problems made better", "0", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("Refusal", "G", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("Don't know", "G", as.character(l1$imwbcrm))
l1$imwbcrm <- gsub("No answer", "G", as.character(l1$imwbcrm))

l12 <- l1 %>%
  filter(imwbcrm != "G")

l12$imwbcrm <-  as.numeric(l12$imwbcrm)

n1 <- l12

n1$pplstrd <- gsub("Agree strongly", "5", as.character(n1$pplstrd)) #flip values
n1$pplstrd <- gsub("Agree", "4", as.character(n1$pplstrd))
n1$pplstrd <- gsub("Neither agree nor disagree", "3", as.character(n1$pplstrd))
n1$pplstrd <- gsub("Disagree", "2", as.character(n1$pplstrd))
n1$pplstrd <- gsub("4 strongly", "1", as.character(n1$pplstrd))
n1$pplstrd <- gsub("Refusal", "G", as.character(n1$pplstrd))
n1$pplstrd <- gsub("Don't know", "G", as.character(n1$pplstrd))
n1$pplstrd <- gsub("No answer", "G", as.character(n1$pplstrd))


n2 <- n1 %>%
  filter(pplstrd != "G") 

n2$pplstrd <-  as.numeric(n2$pplstrd)

p1 <- n2

p1$lwdscwp <- gsub("Extremely bad", "10", as.character(p1$lwdscwp)) #flip values
p1$lwdscwp <- gsub("1", "9", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("2", "8", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("3", "7", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("4", "6", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("5", "5", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("6", "4", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("7", "3", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("8", "2", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("9", "1", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("Extremely good", "0", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("Refusal", "G", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("Don't know", "G", as.character(p1$lwdscwp))
p1$lwdscwp <- gsub("No answer", "G", as.character(p1$lwdscwp))

p12 <- p1 %>%
  filter(lwdscwp != "G")

p12$lwdscwp <-  as.numeric(p12$lwdscwp)

r1 <- p12

r1$noimbro <- gsub("Refusal", "G", as.character(r1$noimbro))
r1$noimbro <- gsub("Don't know", "G", as.character(r1$noimbro))
r1$noimbro <- gsub("No answer", "G", as.character(r1$noimbro))

r12 <- r1 %>%
  filter(noimbro != "G")

r12$noimbro  <-  as.numeric(r12$noimbro) 

t1 <- r12

t1$imueclt <- gsub("Cultural life undermined", "10", as.character(t1$imueclt)) #flip values
t1$imueclt <- gsub("1", "9", as.character(t1$imueclt))
t1$imueclt <- gsub("2", "8", as.character(t1$imueclt))
t1$imueclt <- gsub("3", "7", as.character(t1$imueclt))
t1$imueclt <- gsub("4", "6", as.character(t1$imueclt))
t1$imueclt <- gsub("5", "5", as.character(t1$imueclt))
t1$imueclt <- gsub("6", "4", as.character(t1$imueclt))
t1$imueclt <- gsub("7", "3", as.character(t1$imueclt))
t1$imueclt <- gsub("8", "2", as.character(t1$imueclt))
t1$imueclt <- gsub("9", "1", as.character(t1$imueclt))
t1$imueclt <- gsub("Cultural life enriched", "0", as.character(t1$imueclt))
t1$imueclt <- gsub("Refusal", "G", as.character(t1$imueclt))
t1$imueclt <- gsub("Don't know", "G", as.character(t1$imueclt))
t1$imueclt <- gsub("No answer", "G", as.character(t1$imueclt))

t12 <- t1 %>%
  filter(imueclt != "G")

t12$imueclt <-  as.numeric(t12$imueclt) 

ESS2002 <- t12

ESS2002$CountryCode <- ESS2002$cntbrth #changing cntbrth to countrycode


#change CountryCode variables for simplified use later
ESS2002$CountryCode<-gsub("Refusal","77",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Don't know","88",as.character(ESS2002$CountryCode))	
ESS2002$CountryCode<-gsub("Andorra","AD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("United Arab Emirates","AE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Afghanistan","AF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Antigua and Barbuda","AG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Anguilla","AI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Albania","AL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Armenia","AM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Netherlands Antilles","AN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Angola","AO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Antarctica","AQ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Argentina","AR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("American Samoa","AS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Austria","AT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Australia","AU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Aruba","AW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Azerbaijan","AZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bosnia and Herzegovina","BA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Barbados","BB",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bangladesh","BD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Belgium","BE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Burkina Faso","BF",as.character(ESS2002$CountryCode))	
ESS2002$CountryCode<-gsub("Bulgaria","BG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bahrain","BH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Burundi","BI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Benin","BJ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bermuda","BM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Brunei Darussalam","BN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bolivia","BO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Brazil","BR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bahamas","BS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bhutan","BT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Bouvet Island","BV",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Botswana","BW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Belarus","BY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Belize","BZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Canada","CA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cocos (Keeling) Islands","CC",as.character(ESS2002$CountryCode))	
ESS2002$CountryCode<-gsub("Congo, The Democratic Republic of the","CD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Central African Republic","CF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Congo","CG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Switzerland","CH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("C?te d'Ivoire","CI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cook Islands","CK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Chile","CL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cameroon","CM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("China","CN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Colombia","CO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Costa Rica","CR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Czechoslovakia","CS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cuba","CU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cape Verde","CV",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Christmas Island","CX",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cyprus","CY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Czechia","CZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Germany","DE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Djibouti","DJ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Denmark","DK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Dominica","DM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Dominican Republic","DO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Algeria","DZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Ecuador","EC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Estonia","EE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Egypt","EG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Western Sahara","EH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Eritrea","ER",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Spain","ES",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Ethiopia","ET",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Finland","FI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Fiji","FJ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Falkland Islands (Malvinas)","FK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Micronesia, Federated States of","FM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Faroe Islands","FO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("France","FR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Gabon","GA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("United Kingdom","GB",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Grenada","GD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Georgia","GE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("French Guiana","GF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Ghana","GH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Gibraltar","GI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Greenland","GL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Gambia","GM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Guinea","GN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Guadeloupe","GP",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Equatorial Guinea","GQ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Greece","GR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("South Georgia and the South Sandwich Islands","GS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Guatemala","GT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Guam","GU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Guinea-Bissau","GW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Guyana","GY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Hong Kong","HK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Heard Island and McDonald Islands","HM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Honduras","HN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Croatia","HR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Haiti","HT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Hungary","HU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Indonesia","ID",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Ireland","IE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Israel","IL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("India","IN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("British Indian Ocean Territory","IO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Iraq","IQ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Iran, Islamic Republic of","IR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Iceland","IS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Italy","IT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Jamaica","JM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Jordan","JO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Japan","JP",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Kenya","KE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Kyrgyzstan","KG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cambodia","KH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Kiribati","KI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Comoros","KM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Saint Kitts and Nevis","KN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Korea, Democratic People's Republic of","KP",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Korea, Republic of","KR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Kuwait","KW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Cayman Islands","KY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Kazakhstan","KZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Lao People's Democratic Republic","LA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Lebanon","LB",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Saint Lucia","LC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Liechtenstein","LI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Sri Lanka","LK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Liberia","LR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Lesotho","LS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Lithuania","LT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Luxembourg","LU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Latvia","LV",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Libyan Arab Jamahiriya","LY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Morocco","MA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Monaco","MC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Moldova, Republic of","MD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Madagascar","MG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Marshall Islands","MH",as.character(ESS2002$CountryCode))	
ESS2002$CountryCode<-gsub("Macedonia","MK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mali","ML",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Myanmar","MM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mongolia","MN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Macao","MO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Northern Mariana Islands","MP",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Martinique","MQ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mauritania","MR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Montserrat","MS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Malta","MT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mauritius","MU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Maldives","MV",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Malawi","MW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mexico","MX",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Malaysia","MY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mozambique","MZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Namibia","NA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("New Caledonia","NC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Niger","NE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Norfolk Island","NF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Nigeria","NG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Nicaragua","NI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Netherlands","NL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Norway","NO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Nepal","NP",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Nauru","NR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Niue","NU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("New Zealand","NZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Oman","OM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Panama","PA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Peru","PE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("French Polynesia","PF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Papua New Guinea","PG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Philippines","PH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Pakistan","PK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Poland","PL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Saint Pierre and Miquelon","PM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Pitcairn","PN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Puerto Rico","PR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Palestinian Territory, Occupied","PS",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Portugal","PT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Palau","PW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Paraguay","PY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Qatar","QA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("R?union","RE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Romania","RO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Russian Federation","RU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Rwanda","RW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Saudi Arabia","SA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Solomon Islands","SB",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Seychelles","SC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Sudan","SD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Sweden","SE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Singapore","SG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Saint Helena","SH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Slovenia","SI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Svalbard and Jan Man","SJ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Slovakia","SK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Sierra Leone","SL",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("San Marino","SM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Senegal","SN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Somalia","SO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Suriname","SR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Sao Tome and Principe","ST",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("USSR","SU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("El Salvador","SV",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Syrian Arab Republic","SY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Swaziland","SZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Turks and Caicos Islands","TC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Chad","TD",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("French Southern Territories","TF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Togo","TG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Thailand","TH",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Tajikistan","TJ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Tokelau","TK",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Turkmenistan","TM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Tunisia","TN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Tonga","TO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("East Timor","TP",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Turkey","TR",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Trinidad and Tobago","TT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Tuvalu","TV",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Taiwan, Province of China","TW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Tanzania, United Republic of","TZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Ukraine","UA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Uganda","UG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("United States Minor Outlying Islands","UM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("United States","US",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Uruguay","UY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Uzbekistan","UZ",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Holy See (Vatican City State)","VA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Saint Vincent and the Grenadines","VC",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Venezuela","VE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Virgin Islands, British","VG",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Virgin Islands, U.S.","VI",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Viet Nam","VN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Vanuatu","VU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Wallis and Futuna","WF",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Samoa","WS",as.character(ESS2002$CountryCode))
#ESS2002$CountryCode<-gsub("Yemen","YE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Mayotte","YT",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Yugoslavia","YU",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("South Africa","ZA",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Zambia","ZM",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Zimbabwe","ZW",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("Not applicable","66",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("No answer","99",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("DMn Republic	","DO",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("NEia	","NG",as.character(ESS2002$CountryCode))
#ESS2002$CountryCode<-gsub("YeYE","YE",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("PY	","PY",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("AN","AN",as.character(ESS2002$CountryCode))
ESS2002$CountryCode<-gsub("GN-Bissau","GW",as.character(ESS2002$CountryCode))


#create Immigrant Status variable

ESS2002.immigrant <- ESS2002 %>%
  filter(brncntr == "No")

ESS2002.immigrant$ImmigrantStatus <- if_else(ESS2002.immigrant$CountryCode %in% c("AD","AL","AT","BA","BE","BG","BY","CH","CS",
                                                                                  "CZ","DE","DK","EE","ES","FI","FO","FR","GB",
                                                                                  "GE","GI","GR","HR","HU","IE","IS","IT","LI","LK",
                                                                                  "LT","LU","LV","MC","MD","MK","MT","NL","NO","PL",
                                                                                  "PT","RO","RU","SE","SI","SJ","SK","SM","SU","UA","VA","YU"), "European Immigrant", "Non-European Immigrant")

ESS2002.nonimmigrant <- ESS2002 %>%
  filter(brncntr == "Yes")

ESS2002.nonimmigrant$ImmigrantStatus <- "Non-Immigrant"

ESS2002.all <- full_join(ESS2002.immigrant, ESS2002.nonimmigrant)

#create ESS2002 dataset with only european immigrants
European_imm <- ESS2002 %>%
  filter(brncntr == "No") %>%
  filter(CountryCode %in% c("AD","AL","AT","BA","BE","BG","BY","CH","CS",
                            "CZ","DE","DK","EE","ES","FI","FO","FR","GB",
                            "GE","GI","GR","HR","HU","IE","IS","IT","LI","LK",
                            "LT","LU","LV","MC","MD","MK","MT","NL","NO","PL",
                            "PT","RO","RU","SE","SI","SJ","SK","SM","SU","UA","VA","YU"))

#create ESS2002 dataset with all other immigrants
NonEuropean_imm <- ESS2002 %>%
  filter(brncntr == "No") %>%
  filter(CountryCode %in% c("AO","BF","BI","BJ","BW","CD","CF",
                            "CG","CI","CM","CV","DJ","DZ","EH","ER",
                            "ET","GA","GH","GM","GN","GQ","GW","KE","KM",
                            "LR","LS","LY","MA","MG","ML","MR","MU","MW",
                            "MZ","NA","NE","NG","RE","RW","SC","SD","SH","SL",
                            "SN","SO","ST","SZ","TD","TG","TN","TZ","UG","YT",
                            "ZA","ZM","ZW","NEia","GN-Bissau",                      #Africa
                            "AR","BO","BR","BZ","CL","CO","EC",
                            "FK","GF","GY","PE","PY","SR","UY","VE",                #South America
                            "AF","AM","AZ","BD","BN","BT","CC","CN","CX",
                            "HK","ID","IN","IO","JP","KG","KH","KP","KR","KZ",
                            "LA","MM","MN","MO","MY","NP","PH","PK","SG","TH",
                            "TJ","TM","TP","TW","UZ","VN",                           #Asia
                            "AS","AU","CK","FJ","FM","GU","KI","MH",
                            "MP","NC","NF","NR","NU","NZ","PF","PG",
                            "PN","PW","SB","TK","TO","TV","UM","VU","WF","WS",       #Oceania
                            "AG","AI","AN","AW","BB","BM","BS","CU",
                            "DM","DO","GD","GP","HT","JM","KN","KY",
                            "LC","MQ","MS","TC","TT","VC","VG","VI","DMn Republic",  #Caribbean
                            "CA","CR","GL","GT","HN","MX","PM","NI",
                            "PA","PM","PR","SV","US",                                #North America
                            "AQ","BV","GS","HM","TF",                                #Antartica
                            "BH","CY","EG","IR","IQ","IL","JO",
                            "KW","LB","OM","PS","QA2","SA",
                            "SY","TR","AE","Yemen"))                                 #Middle East



#Sweden - other countries worked on include Spain, France, Germany, and the UK.

#starting with non-immigrants

Sweden_nonimm1 <- ESS2002 %>%
  filter(cntry == "Sweden") %>% # filtering country responses
  unite("FM", facntr:mocntr) %>% # joining the father and mother born in country variables
  filter(FM == "Yes_Yes") %>% # filtering for both parents born in country
  filter(brncntr == "Yes") %>% #filtering for respondent born in country
  group_by(cntry) %>%       # group by country
  summarise_at(vars(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                    imwbcrm,pplstrd, lwdscwp, noimbro, imueclt), tibble::lst(mean, median), na.rm = T) # getting the mean and median of all variables

#repeated but used count() to get the count for responses for each variable
Sweden_nonimm2 <- ESS2002 %>%
  filter(cntry == "Sweden") %>%
  unite("FM", facntr:mocntr) %>%
  filter(FM == "Yes_Yes") %>%
  filter(brncntr == "Yes") %>%
  group_by(cntry) %>%
  count(cntry)

#joined the two non-immigrant data frames 
Sweden_nonimm <- full_join(Sweden_nonimm2, Sweden_nonimm1, by = c("cntry"))

# repeated for European-immigrants
Sweden_EurImm1 <- European_imm %>% #used the european immigrant dataset for 2002 that I created earlier 
  filter(cntry == "Sweden") %>%
  group_by(cntry) %>%
  summarise_at(vars(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                    imwbcrm,pplstrd, lwdscwp, noimbro, imueclt), tibble::lst(mean, median), na.rm = T) 

Sweden_EurImm2 <- European_imm %>%
  filter(cntry == "Sweden") %>%
  group_by(cntry) %>%
  count(cntry)

Sweden_EurImm <- full_join(Sweden_EurImm2, Sweden_EurImm1, by = c("cntry"))

# repeated for non-European immigrants
Sweden_NonEurImm1 <- NonEuropean_imm %>%
  filter(cntry == "Sweden") %>%
  group_by(cntry) %>%
  summarise_at(vars(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                    imwbcrm,pplstrd, lwdscwp, noimbro, imueclt), tibble::lst(mean, median), na.rm = T)

Sweden_NonEurImm2 <- NonEuropean_imm %>%
  filter(cntry == "Sweden") %>%
  group_by(cntry) %>%
  count(cntry)

Sweden_NonEurImm <- full_join(Sweden_NonEurImm2, Sweden_NonEurImm1, by = c("cntry"))

#cleaning up and pivoting before joining

#Starting with non-immigrants
nonimmSwedenmean <- Sweden_nonimm %>%
  select(ends_with("mean"), n) %>%
  rename("eimpcnt" = eimpcnt_mean,
         "qfimlng" = qfimlng_mean, 
         "qfimchr" = qfimchr_mean,
         "qfimwht" = qfimwht_mean,
         "qfimcmt" = qfimcmt_mean,
         "imtcjob" = imtcjob_mean,
         "imbleco" = imbleco_mean,
         "imwbcrm" = imwbcrm_mean,
         "pplstrd" = pplstrd_mean, 
         "lwdscwp" = lwdscwp_mean,
         "noimbro" = noimbro_mean,
         "imueclt" = imueclt_mean) %>%
  pivot_longer(names_to = "Variables",
               values_to = "Non-Immigrant Averages",
               c(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                 imwbcrm,pplstrd, lwdscwp, noimbro, imueclt)) 

nonimmSwedenmedian <- Sweden_nonimm %>%
  select(ends_with("median")) %>%
  rename("eimpcnt" = eimpcnt_median,
         "qfimlng" = qfimlng_median, 
         "qfimchr" = qfimchr_median,
         "qfimwht" = qfimwht_median,
         "qfimcmt" = qfimcmt_median,
         "imtcjob" = imtcjob_median,
         "imbleco" = imbleco_median,
         "imwbcrm" = imwbcrm_median,
         "pplstrd" = pplstrd_median, 
         "lwdscwp" = lwdscwp_median,
         "noimbro" = noimbro_median,
         "imueclt" = imueclt_median) %>%
  pivot_longer(names_to = "Variables",
               values_to = "Non-Immigrant Median",
               c(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                 imwbcrm,pplstrd, lwdscwp, noimbro, imueclt)) 

nonimmSweden <- full_join(nonimmSwedenmean, nonimmSwedenmedian, by = c("Variables")) %>%
  select(-cntry.y) %>%
  rename("cntry" = cntry.x,
         "Total Non-Immigrant Responses" = "n")

#European immigrants

eurimmSwedenmean <- Sweden_EurImm %>% 
  select(ends_with("mean"), n) %>%
  rename("eimpcnt" = eimpcnt_mean,
         "qfimlng" = qfimlng_mean, 
         "qfimchr" = qfimchr_mean,
         "qfimwht" = qfimwht_mean,
         "qfimcmt" = qfimcmt_mean,
         "imtcjob" = imtcjob_mean,
         "imbleco" = imbleco_mean,
         "imwbcrm" = imwbcrm_mean,
         "pplstrd" = pplstrd_mean, 
         "lwdscwp" = lwdscwp_mean,
         "noimbro" = noimbro_mean,
         "imueclt" = imueclt_mean) %>%
  pivot_longer(names_to = "Variables",
               values_to = "European-Immigrant Averages",
               c(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                 imwbcrm,pplstrd, lwdscwp, noimbro, imueclt)) 

eurimmSwedenmedian <- Sweden_EurImm %>%
  select(ends_with("median")) %>%
  rename("eimpcnt" = eimpcnt_median,
         "qfimlng" = qfimlng_median, 
         "qfimchr" = qfimchr_median,
         "qfimwht" = qfimwht_median,
         "qfimcmt" = qfimcmt_median,
         "imtcjob" = imtcjob_median,
         "imbleco" = imbleco_median,
         "imwbcrm" = imwbcrm_median,
         "pplstrd" = pplstrd_median, 
         "lwdscwp" = lwdscwp_median,
         "noimbro" = noimbro_median,
         "imueclt" = imueclt_median) %>%
  pivot_longer(names_to = "Variables",
               values_to = "European-Immigrant Median",
               c(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                 imwbcrm,pplstrd, lwdscwp, noimbro, imueclt)) 

eurimmSweden <- full_join(eurimmSwedenmean, eurimmSwedenmedian, by = c("Variables")) %>%
  select(-cntry.y) %>%
  rename("cntry" = cntry.x,
         "Total European-Immigrant Responses" = "n")

#Non-European Immigrants

immSwedenmean <- Sweden_NonEurImm %>% 
  select(ends_with("mean"), n) %>%
  rename("eimpcnt" = eimpcnt_mean,
         "qfimlng" = qfimlng_mean, 
         "qfimchr" = qfimchr_mean,
         "qfimwht" = qfimwht_mean,
         "qfimcmt" = qfimcmt_mean,
         "imtcjob" = imtcjob_mean,
         "imbleco" = imbleco_mean,
         "imwbcrm" = imwbcrm_mean,
         "pplstrd" = pplstrd_mean, 
         "lwdscwp" = lwdscwp_mean,
         "noimbro" = noimbro_mean,
         "imueclt" = imueclt_mean) %>%
  pivot_longer(names_to = "Variables",
               values_to = "Non-European-Immigrant Averages",
               c(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                 imwbcrm,pplstrd, lwdscwp, noimbro, imueclt)) 

immSwedenmedian <- Sweden_NonEurImm %>%
  select(ends_with("median")) %>%
  rename("eimpcnt" = eimpcnt_median,
         "qfimlng" = qfimlng_median, 
         "qfimchr" = qfimchr_median,
         "qfimwht" = qfimwht_median,
         "qfimcmt" = qfimcmt_median,
         "imtcjob" = imtcjob_median,
         "imbleco" = imbleco_median,
         "imwbcrm" = imwbcrm_median,
         "pplstrd" = pplstrd_median, 
         "lwdscwp" = lwdscwp_median,
         "noimbro" = noimbro_median,
         "imueclt" = imueclt_median) %>%
  pivot_longer(names_to = "Variables",
               values_to = "Non-European-Immigrant Median",
               c(eimpcnt, qfimlng, qfimchr, qfimwht,qfimcmt,imtcjob,imbleco,
                 imwbcrm,pplstrd, lwdscwp, noimbro, imueclt)) 

immSweden <- full_join(immSwedenmean, immSwedenmedian, by = c("Variables")) %>%
  select(-cntry.y) %>%
  rename("cntry" = cntry.x,
         "Total Immigrant Responses" = "n")

#joining the groups in one data frame

SwedenDF1 <- full_join(eurimmSweden, nonimmSweden, by = c("Variables"))

SwedenDF2 <- full_join(SwedenDF1, immSweden, by = c("Variables")) %>%
  select(-cntry.x, -cntry.y)

#creating a dataset for Sweden with all values for each of the three variables

Sweden1 <- ESS2002 %>%
  filter(cntry == "Sweden") %>%
  unite("FM", facntr:mocntr) %>%
  filter(FM == "Yes_Yes") %>%
  filter(brncntr == "Yes")

Sweden2 <- NonEuropean_imm %>%
  filter(cntry == "Sweden")

Sweden3 <- European_imm %>%
  filter(cntry == "Sweden")

Sweden.all <- ESS2002.all %>%
  filter(cntry == "Sweden")

#test + p-value + confidence interval 

#eimpcnt

Sw.sig <- c()
Sw.pval <- c()
Sw.EUnonEU <- c()
Sw.EUvNat <- c()
Sw.nonEUvNat <- c()
Sw.pval.EUnonEU <- c()
Sw.pval.EUvNat <- c()
Sw.pval.nonEUvNat <- c()
Sw.conf.EUnonEU <- c()
Sw.conf.EUvNat <- c()
Sw.conf.nonEUvNat <- c()
#odd <- c(1,3,5,7,9,11,13,15,17,19,21,23)
#even <- c(2,4,6,8,10,12,14,16,18,20,22,24)
Sw.count <- c()
Sw.count.EU <- c()
Sw.count.nonEU <- c()
Sw.count.nat <- c()


#kruskal wall test + p-value

# eimpcnt

Sw.sig <- kruskal.test(Sweden.all$eimpcnt ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)



#getting correct count variable
Sw.summary.eimpcnt <- Sweden.all %>%
  select(eimpcnt, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(eimpcnt, na.rm = TRUE),
    median = median(eimpcnt, na.rm = TRUE),
    IQR = IQR(eimpcnt, na.rm = TRUE)
  )

#appending counts
Sw.count <- append(Sw.count, sum(Sw.summary.eimpcnt$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.eimpcnt$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.eimpcnt$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.eimpcnt$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$eimpcnt, Sweden2$eimpcnt), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$eimpcnt,Sweden1$eimpcnt), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$eimpcnt, Sweden1$eimpcnt), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)



#qfimlng

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$qfimlng ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)


#getting correct count variable
Sw.summary.qfimlng <- Sweden.all %>%
  select(qfimlng, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(qfimlng, na.rm = TRUE),
    median = median(qfimlng, na.rm = TRUE),
    IQR = IQR(qfimlng, na.rm = TRUE)
  )

#appending counts
Sw.count <- append(Sw.count, sum(Sw.summary.qfimlng$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.qfimlng$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.qfimlng$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.qfimlng$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$qfimlng, Sweden2$qfimlng), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$qfimlng,Sweden1$qfimlng), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$qfimlng, Sweden1$qfimlng), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#qfimchr

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$qfimchr ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.qfimchr <- Spain.all %>%
  select(qfimchr, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(qfimchr, na.rm = TRUE),
    median = median(qfimchr, na.rm = TRUE),
    IQR = IQR(qfimchr, na.rm = TRUE)
  )
Sw.count <- append(Sw.count, sum(Sw.summary.qfimchr$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.qfimchr$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.qfimchr$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.qfimchr$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$qfimchr, Sweden2$qfimchr), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$qfimchr,Sweden1$qfimchr), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$qfimchr, Sweden1$qfimchr), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#qfimwht

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$qfimwht ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.qfimwht <- Sweden.all %>%
  select(qfimwht, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(qfimwht, na.rm = TRUE),
    median = median(qfimwht, na.rm = TRUE),
    IQR = IQR(qfimwht, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.qfimwht$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.qfimwht$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.qfimwht$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.qfimwht$count[3])


#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$qfimwht, Sweden2$qfimwht), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$qfimwht,Sweden1$qfimwht), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$qfimwht, Sweden1$qfimwht), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#qfimcmt

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$qfimcmt ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.qfimcmt <- Sweden.all %>%
  select(qfimcmt, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(qfimcmt, na.rm = TRUE),
    median = median(qfimcmt, na.rm = TRUE),
    IQR = IQR(qfimcmt, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.qfimcmt$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.qfimcmt$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.qfimcmt$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.qfimcmt$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$qfimcmt, Sweden2$qfimcmt), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$qfimcmt,Sweden1$qfimcmt), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$qfimcmt, Sweden1$qfimcmt), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#imtcjob

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$imtcjob ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.imtcjob <- Sweden.all %>%
  select(imtcjob, ImmigrantStatus) %>%
  group_by(ImmigrantStatus)%>%
  summarise(
    count = n(),
    sd = sd(imtcjob, na.rm = TRUE),
    median = median(imtcjob, na.rm = TRUE),
    IQR = IQR(imtcjob, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.imtcjob$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.imtcjob$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.imtcjob$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.imtcjob$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$imtcjob, Sweden2$imtcjob), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$imtcjob,Sweden1$imtcjob), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$imtcjob, Sweden1$imtcjob), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#imbleco

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$imbleco ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.imbleco <- Sweden.all %>%
  select(imbleco, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(imbleco, na.rm = TRUE),
    median = median(imbleco, na.rm = TRUE),
    IQR = IQR(imbleco, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.imbleco$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.imbleco$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.imbleco$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.imbleco$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$imbleco, Sweden2$imbleco), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$imbleco,Sweden1$imbleco), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$imbleco, Sweden1$imbleco), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#imwbcrm

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$imwbcrm ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.imwbcrm <- Sweden.all %>%
  select(imwbcrm, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(imwbcrm, na.rm = TRUE),
    median = median(imwbcrm, na.rm = TRUE),
    IQR = IQR(imwbcrm, na.rm = TRUE)
  )
Sw.count <- append(Sw.count, sum(Sw.summary.imwbcrm$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.imwbcrm$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.imwbcrm$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.imwbcrm$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$imwbcrm, Sweden2$imwbcrm), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$imwbcrm,Sweden1$imwbcrm), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$imwbcrm, Sweden1$imwbcrm), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#pplstrd

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$pplstrd ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.pplstrd <- Sweden.all %>%
  select(pplstrd, ImmigrantStatus) %>%
  group_by(ImmigrantStatus)%>%
  summarise(
    count = n(),
    sd = sd(pplstrd, na.rm = TRUE),
    median = median(pplstrd, na.rm = TRUE),
    IQR = IQR(pplstrd, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.pplstrd$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.pplstrd$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.pplstrd$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.pplstrd$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$pplstrd, Sweden2$pplstrd), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$pplstrd,Sweden1$pplstrd), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$pplstrd, Sweden1$pplstrd), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#lwdscwp

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$lwdscwp ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

Sw.summary.lwdscwp <- Sweden.all %>%
  select(lwdscwp, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(lwdscwp, na.rm = TRUE),
    median = median(lwdscwp, na.rm = TRUE),
    IQR = IQR(lwdscwp, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.lwdscwp$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.lwdscwp$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.lwdscwp$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.lwdscwp$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$lwdscwp, Sweden2$lwdscwp), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$lwdscwp,Sweden1$lwdscwp), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$lwdscwp, Sweden1$lwdscwp), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)

#noimbro

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$noimbro ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.noimbro <- Sweden.all %>%
  select(noimbro, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(noimbro, na.rm = TRUE),
    median = median(noimbro, na.rm = TRUE),
    IQR = IQR(noimbro, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.noimbro$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.noimbro$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.noimbro$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.noimbro$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$noimbro, Sweden2$noimbro), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$noimbro,Sweden1$noimbro), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$noimbro, Sweden1$noimbro), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)


#imueclt

#kruskal wall test + p-value
Sw.sig <- kruskal.test(Sweden.all$imueclt ~ ImmigrantStatus, data = Sweden.all)
Sw.pval <- append(Sw.pval, Sw.sig$p.value)

#count
Sw.summary.imueclt <- Sweden.all %>%
  select(imueclt, ImmigrantStatus) %>%
  group_by(ImmigrantStatus) %>%
  summarise(
    count = n(),
    sd = sd(imueclt, na.rm = TRUE),
    median = median(imueclt, na.rm = TRUE),
    IQR = IQR(imueclt, na.rm = TRUE)
  )

Sw.count <- append(Sw.count, sum(Sw.summary.imueclt$count))
Sw.count.EU <- append(Sw.count.EU, Sw.summary.imueclt$count[1])
Sw.count.nonEU <- append(Sw.count.nonEU, Sw.summary.imueclt$count[2])
Sw.count.nat <- append(Sw.count.nat, Sw.summary.imueclt$count[3])

#WSR test (EU vs non-EU)
Sw.EUnonEU <- wilcox.test(as.numeric(Sweden3$imueclt, Sweden2$imueclt), paired = FALSE, conf.int = TRUE, exact = FALSE)
#WSR Test (EU vs. Native Born)
Sw.EUvNat <- (wilcox.test(as.numeric(Sweden3$imueclt,Sweden1$imueclt), paired = FALSE, conf.int = TRUE, exact = FALSE))
#WSR Test (Non EU vs Native Born)
Sw.nonEUvNat <- wilcox.test(as.numeric(Sweden2$imueclt, Sweden1$imueclt), paired = FALSE, conf.int = TRUE, exact = FALSE)

#append WRS p-values
Sw.pval.EUnonEU <- append(Sw.pval.EUnonEU, Sw.EUnonEU$p.value)
Sw.pval.EUvNat <- append(Sw.pval.EUvNat, Sw.EUvNat$p.value)
Sw.pval.nonEUvNat <- append(Sw.pval.nonEUvNat, Sw.nonEUvNat$p.value)

#append WSR conf int
Sw.conf.EUnonEU <- append(Sw.conf.EUnonEU, Sw.EUnonEU$conf.int)
Sw.conf.EUvNat <- append(Sw.conf.EUvNat, Sw.EUvNat$conf.int)
Sw.conf.nonEUvNat <- append(Sw.conf.nonEUvNat, Sw.nonEUvNat$conf.int)


#adding all values to Sweden dataframe

SwedenDF2$"P-Value of Kruskal-Wallis Test" <- Sw.pval
SwedenDF2$"P-Value WRS Test (EU vs. Non-Imm)" <- Sw.pval.EUvNat
SwedenDF2$"P-Value WRS Test (EU vs. Non-EU)" <- Sw.pval.EUnonEU
SwedenDF2$"P-Value of WRS Test (Non-EU vs. Non-Imm)" <- Sw.pval.nonEUvNat
SwedenDF2$"WRS Conf.Int - Lowerbound (EU vs. Non-EU)" <- Sw.conf.EUnonEU[odd]
SwedenDF2$"WRS Conf.Int - Upperbound (EU vs. Non-EU)" <- Sw.conf.EUnonEU[even]
SwedenDF2$"WRS Conf.Int - Lowerbound (EU vs. Non-Imm)" <- Sw.conf.EUvNat[odd]
SwedenDF2$"WRS Conf.Int - Upperbound (EU vs. Non-Imm)" <- Sw.conf.EUvNat[even]
SwedenDF2$"WRS Conf.Int - Lowerbound (Non-EU vs. Non-Imm)" <- Sw.conf.nonEUvNat[odd]
SwedenDF2$"WRS Conf.Int - Upperbound (Non-EU vs. Non-Imm)" <- Sw.conf.nonEUvNat[even]
SwedenDF2$"Total Responses" <- Sw.count
SwedenDF2$"European Immigrants - Total Responses" <- Sw.count.EU
SwedenDF2$"Non-European Immigrants - Total Responses" <- Sw.count.nonEU
SwedenDF2$"Non-Immigrants - Total Responses" <- Sw.count.nat



#reordering variables

SwedenDF_Final <- SwedenDF2 %>%
  select("Variables","Total Responses", "Non-Immigrants - Total Responses", "Non-European Immigrants - Total Responses",
         "European Immigrants - Total Responses", "Non-European-Immigrant Averages", "Non-Immigrant Averages", "European-Immigrant Averages",
         "Non-European-Immigrant Median", "Non-Immigrant Median", "European-Immigrant Median",
         "P-Value of Kruskal-Wallis Test", "P-Value of WRS Test (Non-EU vs. Non-Imm)", "P-Value WRS Test (EU vs. Non-EU)",
         "P-Value WRS Test (EU vs. Non-Imm)", "WRS Conf.Int - Lowerbound (EU vs. Non-EU)", "WRS Conf.Int - Upperbound (EU vs. Non-EU)",
         "WRS Conf.Int - Lowerbound (EU vs. Non-Imm)", "WRS Conf.Int - Upperbound (EU vs. Non-Imm)",
         "WRS Conf.Int - Lowerbound (Non-EU vs. Non-Imm)", "WRS Conf.Int - Upperbound (Non-EU vs. Non-Imm)")

#save file

#write.csv(SwedenDF_Final,"ESS2002Sweden_1.csv", row.names = FALSE)