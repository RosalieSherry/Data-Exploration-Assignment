
#Libraries
library(tidyverse)
library(jtools)
library(car)
library(readr)

'''The College Scorecard was released at the start of September 2015. 
Among colleges that predominantly grant bachelor’s degrees, did it result in 
more student interest in high-earnings colleges relative to low-earnings ones 
(as proxied by Google searches for keywords associated with those colleges)?'''

#Data
Latest_Scorecard <- read_csv("Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")

files <- list.files(path = 'Lab3_Rawdata', pattern = 'trends_up_to_')
print(files)

prepend <- function(fname) {
  paste('Lab3_Rawdata/', fname, sep = '')
}

trends <- files %>%
  map(prepend) %>%
  map(read_csv) %>%
  reduce(rbind)


#Playing with Data
Latest_Scorecard <- Latest_Scorecard %>% rename(schname = INSTNM)
  
scorecard_all <- merge(x = Latest_Scorecard, y = trends, by = "schname", all.x = TRUE)  

Data_To_Play_With <- scorecard_all %>%
  select('UNITID', 'OPEID', ,'schname', 'PREDDEG', 'keyword', 'monthorweek', 'keynum', 'index', 
         'md_earn_wne_p10-REPORTED-EARNINGS') %>%
  rename(med_earn = 'md_earn_wne_p10-REPORTED-EARNINGS') %>%
  filter(PREDDEG == 3) %>%
  filter(med_earn != 'NULL') %>%
  filter(med_earn != 'PrivacySuppressed') %>%
  mutate(med_earn = as.numeric(med_earn))
  
median(Data_To_Play_With$med_earn)
