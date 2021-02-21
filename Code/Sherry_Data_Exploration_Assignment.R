
#Libraries
library(tidyverse)
library(jtools)
library(car)
library(readr)
library(purrr)
library(lubridate)
library(estimatr)


###The College Scorecard was released at the start of September 2015. 
###Among colleges that predominantly grant bachelor’s degrees, did it result in 
###more student interest in high-earnings colleges relative to low-earnings ones 
###(as proxied by Google searches for keywords associated with those colleges)?

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
         'med_earn', 'CONTROL')
  
median_earnings <- median(Data_To_Play_With$med_earn)

##The data has a median of 41,800 which I will use to filter the high and low earnings.

Standardized_Play <- Data_To_Play_With %>%
  group_by(keynum) %>%
  summarise(sd_index = (index - mean(index)) / sd(index), schname, CONTROL, 
            keyword, monthorweek, keynum, med_earn, sd_index)

#High versus Low Earnings

Standardized_Play$High_Earn <- ifelse(Standardized_Play$med_earn >= median_earnings, 1, 0)

B4Sept15 <- Standardized_Play %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE < '2015-09-01')


AfterSept15 <- Standardized_Play %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE >= '2015-09-01')  

m1 <- lm(data = B4Sept15, med_earn ~ sd_index + factor(CONTROL))
m2 <- lm(data = AfterSept15, med_earn ~ sd_index + factor(CONTROL))

export_summs(m1, m2)

#Keeping this because this was pretty funny
scatter.smooth(x=B4Sept15$sd_index, y=B4Sept15$med_earn, main="med_earn ~ sd_index") 

###This section was a massive Rosalie needs to visualize to know where to go next, 
###all these models are saved but ultimately they are not worth your time.'''

ggplot(data = B4Sept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("All School Before Sept 15")

ggplot(data = AfterSept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("All School After Sept 15")

NoPublicB4Sept15 <- B4Sept15 %>% filter(CONTROL != 1)  

NoPublicAfterSept15 <- AfterSept15 %>% filter(CONTROL != 1)

ggplot(data = NoPublicB4Sept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Private School Before Sept 15")

ggplot(data = NoPublicAfterSept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Private School After Sept 15")

PublicB4Sept15 <- B4Sept15 %>% filter(CONTROL == 1) 
PublicAfterSept15 <- AfterSept15 %>% filter(CONTROL == 1)

ggplot(data = PublicB4Sept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Public School Before Sept 15")

ggplot(data = PublicAfterSept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Public School After Sept 15")

PNPB4Sept15 <- B4Sept15 %>% filter(CONTROL == 2) 
PNPAfterSept15 <- AfterSept15 %>% filter(CONTROL == 2)

ggplot(data = PNPB4Sept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Private Non-Profit School Before Sept 15")

ggplot(data = PublicAfterSept15, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Private Non-Profit School After Sept 15")

#Before September 15
hist(B4Sept15$med_earn)
hist(NoPublicB4Sept15$med_earn)
hist(PublicB4Sept15$med_earn)
hist(PNPB4Sept15$med_earn)

#Sept15 & After

hist(AfterSept15$med_earn)
hist(NoPublicB4Sept15$med_earn)
hist(PublicAfterSept15$med_earn)
hist(PNPAfterSept15$med_earn)


#A Little More Play Time - aka back to business
ModelTime <- Standardized_Play
ModelTime <- ModelTime %>%
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE))

ModelTime$AfterS15 <- ifelse(ModelTime$DATE >= '2015-09-01', 1, 0)

#WOOHOO LETS SEE WHAT WE CAN DO

HighEarnBeforeSept <- ModelTime %>%
  filter(High_Earn == 1) %>%
  filter(AfterS15 == 0)

LowEarnBeforeSept <- ModelTime %>%
  filter(High_Earn == 0) %>%
  filter(AfterS15 == 0)

m3 <- lm(data = HighEarnBeforeSept, med_earn ~ sd_index)
m4 <- lm(data = LowEarnBeforeSept, med_earn ~ sd_index)

export_summs(m3, m4)

HighEarnAfterSept <- ModelTime %>%
  filter(High_Earn == 1) %>%
  filter(AfterS15 == 1)

LowEarnAfterSept <- ModelTime %>%
  filter(High_Earn == 0) %>%
  filter(AfterS15 == 1)

m5 <- lm(data = HighEarnAfterSept, med_earn ~ sd_index)
m6 <- lm(data = LowEarnAfterSept, med_earn ~ sd_index)

export_summs(m3, m4, m5, m6)

moo1 <- lm(data = HighEarnBeforeSept, med_earn ~ sd_index + factor(CONTROL))
moo2 <- lm(data = LowEarnBeforeSept, med_earn ~ sd_index + factor(CONTROL))
moo3 <- lm(data = HighEarnAfterSept, med_earn ~ sd_index + factor(CONTROL))
moo4<- lm(data = LowEarnAfterSept, med_earn ~ sd_index + factor(CONTROL))

export_summs(moo1, moo2, moo3, moo4)
plot_coefs(moo1, moo2, moo3, moo4)

linearHypothesis(moo3, 'sd_index = 0', white.adjust = TRUE)

#What Do I WANT
Earn_High <- ModelTime %>%
  filter(High_Earn == 1) 

Earn_Low <- ModelTime %>%
  filter(High_Earn == 0)

ggplot(data = Earn_High, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Just High Earning")

ggplot(data = Earn_Low, aes(sd_index, med_earn)) +
  geom_smooth() + ggtitle("Just Low Earning")

ggplot(data = LowEarnBeforeSept, aes(sd_index)) +
  geom_histogram(fill = 'blue') +
  facet_grid(CONTROL ~ .) + ggtitle('Low Earn Before September')

ggplot(data = HighEarnBeforeSept, aes(sd_index)) +
  geom_histogram(fill = 'red') +
  facet_grid(CONTROL ~ .) + ggtitle('High Earn Before September')

ggplot(data = LowEarnAfterSept, aes(sd_index)) +
  geom_histogram(fill = 'purple') +
  facet_grid(CONTROL ~ .) + ggtitle('Low Earn After September')

ggplot(data = HighEarnAfterSept, aes(sd_index)) +
  geom_histogram(fill = 'pink') +
  facet_grid(CONTROL ~ .) + ggtitle('High Earn After September')

ah1 <- lm(data = HighEarnBeforeSept, sqrt(med_earn) ~ sd_index + factor(CONTROL))
ah2 <- lm(data = LowEarnBeforeSept, sqrt(med_earn) ~ sd_index + factor(CONTROL))
ah3 <- lm(data = HighEarnAfterSept, sqrt(med_earn) ~ sd_index + factor(CONTROL))
ah4 <- lm(data = LowEarnAfterSept, sqrt(med_earn) ~ sd_index + factor(CONTROL))

export_summs(ah1, ah2, ah3, ah4)
plot_coefs(ah1, ah2, ah3, ah4)

ggplot(data = HighEarnBeforeSept, aes(sd_index, sqrt(med_earn))) +
  geom_smooth() + ggtitle("Sqrt High Earning Before September")

ggplot(data = LowEarnBeforeSept, aes(sd_index, sqrt(med_earn))) +
  geom_smooth() + ggtitle("Sqrt Low Earning Before September")

ggplot(data = HighEarnAfterSept, aes(sd_index, sqrt(med_earn))) +
  geom_smooth() + ggtitle("Sqrt High Earning After September")

ggplot(data = LowEarnAfterSept, aes(sd_index, sqrt(med_earn))) +
  geom_smooth() + ggtitle("Sqrt Low Earning After September")

woohoo1 <- lm(data = HighEarnBeforeSept, log(med_earn) ~ sd_index + factor(CONTROL))
woohoo2 <- lm(data = LowEarnBeforeSept, log(med_earn) ~ sd_index + factor(CONTROL))
woohoo3 <- lm(data = HighEarnAfterSept, log(med_earn) ~ sd_index + factor(CONTROL))
woohoo4 <- lm(data = LowEarnAfterSept, log(med_earn) ~ sd_index + factor(CONTROL))

export_summs(woohoo1, woohoo2, woohoo3, woohoo4)
plot_coefs(woohoo1, woohoo2, woohoo3, woohoo4)

ggplot(data = HighEarnBeforeSept, aes(sd_index, log(med_earn))) +
  geom_smooth() + ggtitle("Sqrt High Earning Before September")

ggplot(data = LowEarnBeforeSept, aes(sd_index, log(med_earn))) +
  geom_smooth() + ggtitle("Sqrt Low Earning Before September")

ggplot(data = HighEarnAfterSept, aes(sd_index, log(med_earn))) +
  geom_smooth() + ggtitle("Sqrt High Earning After September")

ggplot(data = LowEarnAfterSept, aes(sd_index, log(med_earn))) +
  geom_smooth() + ggtitle("Sqrt Low Earning After September")

yahoo1 <- lm(data = HighEarnBeforeSept, med_earn ~ sd_index + (sd_index^2) + factor(CONTROL))
yahoo2 <- lm(data = LowEarnBeforeSept, med_earn ~ sd_index + (sd_index^2) + factor(CONTROL))
yahoo3 <- lm(data = HighEarnAfterSept, med_earn ~ sd_index + (sd_index^2) + factor(CONTROL))
yahoo4<- lm(data = LowEarnAfterSept, med_earn ~ sd_index + (sd_index^2) + factor(CONTROL))

export_summs(yahoo1, yahoo2, yahoo3, yahoo4)
plot_coefs(yahoo1, yahoo2, yahoo3, yahoo4)

last1 <- lm(data = HighEarnBeforeSept, med_earn ~ sd_index^3 + factor(CONTROL))
last2 <- lm(data = LowEarnBeforeSept, med_earn ~ sd_index^3 + factor(CONTROL))
last3 <- lm(data = HighEarnAfterSept, med_earn ~ sd_index^3 + factor(CONTROL))
last4<- lm(data = LowEarnAfterSept, med_earn ~ sd_index^3 + factor(CONTROL))

export_summs(last1, last2, last3, last4)
plot_coefs(last1, last2, last3, last4)

ggplot(data = HighEarnBeforeSept, aes(sd_index^(1/3), med_earn)) +
  geom_smooth() + ggtitle("Cubed Root High Earning Before September")

ggplot(data = LowEarnBeforeSept, aes(sd_index^(1/3), med_earn)) +
  geom_smooth() + ggtitle("Cubed Root Low Earning Before September")

ggplot(data = HighEarnAfterSept, aes(sd_index^(1/3), med_earn)) +
  geom_smooth() + ggtitle("Cubed Root High Earning After September")

ggplot(data = LowEarnAfterSept, aes(sd_index^(1/3), med_earn)) +
  geom_smooth() + ggtitle("Cubed Root Low Earning After September")


#I think the answer section
NoForProfitB4September15 <- B4Sept15 %>%
  filter(CONTROL != 3)

NoForProfitAfterSeptember15 <- AfterSept15 %>%
  filter(CONTROL !=3)

final1 <- lm(data = NoForProfitB4September15, med_earn ~ sd_index + High_Earn)
final2 <- lm(data = NoForProfitAfterSeptember15, med_earn ~ sd_index + High_Earn)

export_summs(final1, final2)
plot_coefs(final1, final2)

linearHypothesis(final1, 'sd_index = 0', white.adjust = TRUE)
linearHypothesis(final2, 'sd_index = 0', white.adjust = TRUE)
