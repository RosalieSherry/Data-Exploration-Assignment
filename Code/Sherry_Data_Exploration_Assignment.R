
#Libraries
library(tidyverse)
library(jtools)
library(car)
library(readr)
library(purrr)
library(lubridate)


'''The College Scorecard was released at the start of September 2015. 
Among colleges that predominantly grant bachelor’s degrees, did it result in 
more student interest in high-earnings colleges relative to low-earnings ones 
(as proxied by Google searches for keywords associated with those colleges)?'''

#Data
Latest_Scorecard <- read_csv("Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")

name_link <- read_csv("Lab3_Rawdata/id_name_link.csv") %>% rename(OPEID = opeid) %>%
  rename(UNITID = unitid) %>% distinct(schname, .keep_all = TRUE)


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
ID_Scorecard <- merge(x = name_link, y = Latest_Scorecard, by = c('UNITID', 'OPEID'), all.x = TRUE)
  
scorecard_all <- merge(x = ID_Scorecard, y = trends, by = 'schname', all.x = TRUE)

scorecard_all <- scorecard_all %>% select(-INSTNM)

scorecard_all <- scorecard_all %>% na.omit()

Data_To_Play_With <- scorecard_all %>%
  rename(med_earn = 'md_earn_wne_p10-REPORTED-EARNINGS') %>%
  filter(PREDDEG == 3) %>%
  filter(med_earn != 'NULL') %>%
  filter(med_earn != 'PrivacySuppressed') %>%
  mutate(med_earn = as.numeric(med_earn))

Data_To_Play_With <- Data_To_Play_With %>%
  select('UNITID', 'OPEID', 'schname', 'PREDDEG', 'keyword', 'monthorweek', 'keynum', 'index',
         'med_earn')
  
median_earnings <- median(Data_To_Play_With$med_earn)

##The data has a median of 41,800 which I will use to filter the high and low earnings.

Standardized_Play <- Data_To_Play_With %>%
  group_by(keynum) %>%
  mutate(sd_index = (index - mean(index, na.rm = TRUE) / sd(index))) %>%
  summarise(schname, keyword, monthorweek, keynum, med_earn, sd_index)

B4Sept15 <- Standardized_Play %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE < '2015-09-01')

AfterSept15 <- Standardized_Play %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE >= '2015-09-01')  

m1 <- lm(data = B4Sept15, med_earn ~ sd_index)
m2 <- lm(data = AfterSept15, med_earn ~ sd_index)

export_summs(m1, m2)
