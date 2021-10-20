#top-----
#Hello. Following are codes related to my Assignment #1.
#I have already done the tasks 1 to 5 and here is the rest of the tasks.
#You may access to different sections of this script from the created list.
#Thank you,
#Hamed Ghanati

#...................................................................................
#...................................................................................
#...................................................................................
#6.1: Importing each sheet as a separate dataset------
#...................................................................................
#...................................................................................
#...................................................................................

#I will need the package "readxl" and "here" for reading the data from an 
#Excel file:
library(readxl)
library(here)

oh2018 <- read_xlsx(path = here("Data/nhanes_ohx_12_18.xlsx"),
                    sheet = "oh2018")
oh2016 <- read_xlsx(path = here("Data/nhanes_ohx_12_18.xlsx"),
                    sheet = "oh2016")
oh2014 <- read_xlsx(path = here("Data/nhanes_ohx_12_18.xlsx"),
                    sheet = "oh2014")
oh2012 <- read_xlsx(path = here("Data/nhanes_ohx_12_18.xlsx"),
                    sheet = "oh2012")

demo2012 <- read_xlsx(path = here("Data/nhanes_demo_12_18.xlsx"),
                      sheet = "demo2012")
demo2014 <- read_xlsx(path = here("Data/nhanes_demo_12_18.xlsx"),
                      sheet = "demo2014")
demo2016 <- read_xlsx(path = here("Data/nhanes_demo_12_18.xlsx"),
                      sheet = "demo2016")
demo2018 <- read_xlsx(path = here("Data/nhanes_demo_12_18.xlsx"),
                      sheet = "demo2018")
#...................................................................................
#...................................................................................
#...................................................................................
#6.2: Creating a single dataset for oral examination-----
#...................................................................................
#...................................................................................
#...................................................................................

#I will need to check the sanity before appending the sheets.
#First off, checking the variable names to be identical between the sheets:
identical(names(oh2018), names(oh2016))
identical(names(oh2016), names(oh2014))
identical(names(oh2014), names(oh2012))
#R returns that:
#oh2018 and oh2016 have identical variable names.
#oh2016 and oh2014 have some differences in variable names.
#oh2014 and oh2012 have identical variable names.
#I will explore these differences:
oh2016_vs_oh2014 <- setdiff(names(oh2016), names(oh2014))
oh2016_vs_oh2012 <- setdiff(names(oh2016), names(oh2012))
#Now, I want to check if the difference between oh2016_vs_oh2014 and 
#oh2016_vs_oh2012 is the same?
identical(oh2016_vs_oh2014, oh2016_vs_oh2012)
#R returns True. Now, I will check to see how many variables are 
#same between the sheets. For this purpose, I will need to load
#the tidyverse library first:
library(tidyverse)
intersect(names(oh2018), names(oh2016)) %>% length()
#R returns that there are 170 variables in common.
intersect(names(oh2014), names(oh2016)) %>% length()
#R returns that there are 110 variables in common.
intersect(names(oh2014), names(oh2012)) %>% length()
#R returns that there are 110 variables in common.
#We may conclude that all the variables of oh2014 and oh2012 are 
#included in the sheets of oh2016 and oh2018.

#Now, I need to make sure that the common variables are coded in 
#the same way. To do this, I will choose some variables randomly 
#and check them.
oh2018 %>% select(ends_with("STS")) %>% sapply(FUN=unique)
oh2016 %>% select(ends_with("STS")) %>% sapply(FUN=unique)
oh2014 %>% select(ends_with("STS")) %>% sapply(FUN=unique)
oh2012 %>% select(ends_with("STS")) %>% sapply(FUN=unique)
#According to the R returns, the values' codes are the same inside
#the variables chosen randomly. So, we may append the sheets.
#Now, I will choose the participants who have completed the oral
#examination and include the variables related to id (SEQN) and
#the ones ended in "CTC".
#After looking at the variable descriptions on the NaHNES website,
#I will choose the participants whose dentition status code is 1 
#which means that the participant has teeth. I don't want to consider
#edentulous participants. Also, according to the label descriptions,
#the code of participants who has passed the oral examination is 1.

#Selecting the participants and the asked variables:
oh2018_2 <- oh2018 %>% filter(OHDDESTS==1, OHDEXSTS==1) %>%
  select("SEQN",ends_with("CTC"))
oh2016_2 <- oh2016 %>% filter(OHDDESTS==1, OHDEXSTS==1) %>%
  select("SEQN",ends_with("CTC"))
oh2014_2 <- oh2014 %>% filter(OHDDESTS==1, OHDEXSTS==1) %>%
  select("SEQN",ends_with("CTC"))
oh2012_2 <- oh2012 %>% filter(OHDDESTS==1, OHDEXSTS==1) %>%
  select("SEQN",ends_with("CTC"))

#Now, I will append the above datasets:
ohx <- bind_rows(oh2018_2,oh2016_2,oh2014_2,oh2012_2)

#As a final step here, I will check to see if there is any duplicates:
table(duplicated(ohx$SEQN))
#Hopefully, R returns False to all, which means that there is no duplicates
#and the dataset "ohx" is the neat dataset asked in this part of assignment.


#...................................................................................
#...................................................................................
#...................................................................................
#6.3: Adding the "year" variable to the demographic dataset------
#...................................................................................
#...................................................................................
#...................................................................................
#First off, I will check the number of rows (participants) in the demographic
#datasets:

nrow(demo2012)
#R returns 9756 rows.
year <- rep(2012,9756)
demo2012_wy <- data.frame(demo2012,year)
ncol(demo2012)
ncol(demo2012_wy)
head(demo2012_wy$year)
#R returns that the year has added to the demo2012 sheet as 49th variable
#named "year".

nrow(demo2014)
#R returns 10175 rows.
year <- rep(2014,10175)
demo2014_wy <- data.frame(demo2014,year)
ncol(demo2014)
ncol(demo2014_wy)
head(demo2014_wy$year)
#R returns that the year has added to the demo2014 sheet as 48th variable
#named "year".

nrow(demo2016)
#R returns 9971 rows.
year <- rep(2016,9971)
demo2016_wy <- data.frame(demo2016,year)
ncol(demo2016)
ncol(demo2016_wy)
head(demo2016_wy$year)
#R returns that the year has added to the demo2014 sheet as 48th variable
#named "year".

nrow(demo2018)
#R returns 9254 rows.
year <- rep(2018,9254)
demo2018_wy <- data.frame(demo2018,year)
ncol(demo2018)
ncol(demo2018_wy)
head(demo2018_wy$year)
#R returns that the year has added to the demo2014 sheet as 47th variable
#named "year".


#...................................................................................
#...................................................................................
#...................................................................................
#6.4: Creating a single dataset with demographic information------
#...................................................................................
#...................................................................................
#...................................................................................
#I will do the same procedure as before:
#I will also need to check the sanity before appending the sheets.
#First off, checking the variable names to be identical between the sheets:
identical(names(demo2012_wy), names(demo2014_wy))
identical(names(demo2014_wy), names(demo2016_wy))
identical(names(demo2016_wy), names(demo2018_wy))
#R returns that:
#demo2012_wy and demo2014_wy have some differences in variable names.
#demo2014_wy and demo2016_wy have identical variable names.
#demo2016_wy and demo2018_wy have some differences in variable names.
#I will explore these differences:
demo2012_wy_vs_demo2014_wy <- setdiff(names(demo2012_wy), names(demo2014_wy))
demo2016_wy_vs_demo2018_wy <- setdiff(names(demo2016_wy), names(demo2018_wy))
#Now, I want to check if the difference between demo2012_vs_demo2014 and 
#demo2016_vs_demo2018 is the same?
identical(demo2012_wy_vs_demo2014_wy, demo2016_wy_vs_demo2018_wy)
#R returns False. 

#Now, I need to check and see how many variables are same between the sheets.
intersect(names(demo2012_wy), names(demo2014_wy)) %>% length()
#R returns that there are 48 variables in common.
intersect(names(demo2014_wy), names(demo2016_wy)) %>% length()
#R returns that there are 48 variables in common.
intersect(names(demo2016_wy), names(demo2018_wy)) %>% length()
#R returns that there are 43 variables in common.
#We may conclude that all the variables of demo2018_wy and demo2016_wy are 
#included in the sheets of demo2014_wy and demo2012_wy

#Now, I need to make sure that the common variables are coded in 
#the same way. To do this, I will choose some variables randomly 
#and check them.
demo2012_wy %>% select(RIDRETH1) %>% sapply(FUN=unique)
demo2014_wy %>% select(RIDRETH1) %>% sapply(FUN=unique)
demo2016_wy %>% select(RIDRETH1) %>% sapply(FUN=unique)
demo2018_wy %>% select(RIDRETH1) %>% sapply(FUN=unique)


demo2012_wy %>% select(ends_with("SIZ")) %>% sapply(FUN=unique)
demo2014_wy %>% select(ends_with("SIZ")) %>% sapply(FUN=unique)
demo2016_wy %>% select(ends_with("SIZ")) %>% sapply(FUN=unique)
demo2018_wy %>% select(ends_with("SIZ")) %>% sapply(FUN=unique)
#According to the R returns, the values' codes are the same inside
#the variables chosen randomly. So, we may append the sheets.

#Selecting the participants and the asked variables:
demo2012_2 <- demo2012_wy %>% select("SEQN","year","RIDAGEYR")
demo2014_2 <- demo2014_wy %>% select("SEQN","year","RIDAGEYR")
demo2016_2 <- demo2016_wy %>% select("SEQN","year","RIDAGEYR")
demo2018_2 <- demo2018_wy %>% select("SEQN","year","RIDAGEYR")

#Now, I will append the above datasets:
demodata <- bind_rows(demo2012_2,demo2014_2,demo2016_2,demo2018_2)

#As a final step here, I will check to see if there is any duplicates:
table(duplicated(demodata$SEQN))
#Hopefully, R returns False to all 39156 rows of the demodata, which means that 
#there is no duplicate and the dataset "demodata" is the neat dataset asked in 
#this part of assignment.

#...................................................................................
#...................................................................................
#...................................................................................
#6.5: Merging the datasets demodata and ohx------
#...................................................................................
#...................................................................................
#...................................................................................
#To merge two datasets while ignoring the participants who are not present 
#in both the data sets, I will need to see the datasets' number of participants
#(number of rows) and consider a key dataset to be merged with the other one.
nrow(demodata)
#R returns 39156
nrow(ohx)
#R returns 33662
#It seems that the participants of "ohx" are existed in the demodata. So, I consider
#"ohx" as the key data set.To merge two data sets, I will use functions from the 
#package "dplyr".
library(dplyr)
oh_demo_data <- left_join(ohx,demodata)
ncol(ohx)
#R returns 29
ncol(demodata)
#R returns 3
ncol(oh_demo_data)
#R returns 31 which means it has merged two datasets' columns.
#(one column of "demodata" will not be included because it is SEQN)
nrow(ohx)
#R returns 33662
nrow(demodata)
#R returns 39156
nrow(oh_demo_data)
#R returns 33662 which means it has collected all participants of "ohx"
#and the matched ones from "demodata".
#...................................................................................
#...................................................................................
#...................................................................................
#7: Saving the final merged data set as a .csv file in the Data subfolder------
#...................................................................................
#...................................................................................
#...................................................................................

write.csv(oh_demo_data,here("Data/2021-10-13_oh-demo-data.csv"),
          row.names = FALSE)

#...................................................................................
#...................................................................................
#...................................................................................
#8,9,10 is done outside of this script 
#Number of participants by the year-----
#...................................................................................
#...................................................................................
#...................................................................................

table(oh_demo_data$year)
#R returns the following information:
# 2012 2014 2016 2018 
# 8073 8633 8857 8099 


