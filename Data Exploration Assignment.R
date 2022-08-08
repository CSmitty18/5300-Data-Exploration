#Libraries
library(tidyverse)
library(huxtable)
library(readr)
library(purrr)
library(lubridate)
library(estimatr)
library(fixest)
library(jtools)
library(ggstance)


#Google Trends
trends <- list.files(path = "Data", pattern = 'trends_up_to_')

gtrend <- map_df(trends, read_csv)

#College Scorecard
college_score <- read_csv("Data/Most+Recent+Cohorts+(Scorecard+Elements).csv")

rm_dup <- read_csv("Data/id_name_link.csv") %>% rename(OPEID = opeid) %>%
  rename(UNITID = unitid) %>% distinct(schname, .keep_all = TRUE)

gtrend <- gtrend %>% 
  mutate(Day = substr(monthorweek, 1, 10)) %>%
  mutate(Day = as.Date(Day))


#Aggregating Google Trends Data
id <- merge(x = rm_dup, y = college_score, by = c('UNITID', 'OPEID'), all.x = TRUE)
  
combine <- merge(x = id, y = gtrend, by = 'schname', all.x = TRUE)

teat <- combine %>% na.omit()

teat <- teat %>%
  rename(median_earn = 'md_earn_wne_p10-REPORTED-EARNINGS') %>%
  filter(median_earn != 'NULL') %>%
  mutate(median_earn = as.numeric(median_earn))


gray <- teat %>%
  group_by(keynum) %>%
  summarise(sdindex = (index - mean(index)) / sd(index), schname, CONTROL, 
            keyword, monthorweek, keynum, sdindex, median_earn, Day)

#I like to name my stuff in all coding as random names or players on my teams
# so if you see names like teat, I am just trying to remember them
# a little better and make it fun

teat <- teat %>%
  select('UNITID', 'OPEID', 'schname', 'CITY', 'STABBR', 'PREDDEG', 'LOCALE', 'keyword', 'monthorweek', 'keynum', 'index', 'median_earn', 'Day')

gray2 <- na.omit(gray)
median <- median(gray2$median_earn)
mean <- mean(gray2$median_earn)

##There is a median of 37,800 and mean of 39,036 for earnings

#Split out High and Low based on these numbers, will set it at 38,400 based on splitting
# the difference of both
split <- 38400
atlas <- gray2

atlas$High <- ifelse(atlas$median_earn >= split, 1, 0)

Pre <- atlas %>% 
  filter(Day < '2015-09-01')

Post <- atlas %>%
  filter(Day >= '2015-09-01')

uno <- lm(data = Pre, median_earn ~ sdindex + factor(CONTROL))
dos <- lm(data = Post, median_earn ~ sdindex + factor(CONTROL))

export_summs(uno, dos)

#Graphs
ggplot(data = PreHigh, aes(sdindex, median_earn)) +
  geom_smooth() + ggtitle("Pre Sept 2015")
ggplot(data = PostHigh, aes(sdindex, median_earn)) +
  geom_smooth() + ggtitle("Post Sept 2015")

ggplot(data = PreLow, aes(Day, median_earn)) +
  geom_smooth() + ggtitle("Pre Sept 2015")
ggplot(data = PostLow, aes(Day, median_earn)) +
  geom_smooth() + ggtitle("Post Sept 2015")


hist(Pre$median_earn, xlim = c(10000, 100000))
hist(Post$median_earn, xlim = c(10000, 100000))
mean(Pre$median_earn)
mean(Post$median_earn)


PostHigh <- atlas %>%
  filter(High == 1) %>%
  filter(Day >= '2015-09-01')
PostLow <- atlas %>%
  filter(High == 0) %>%
  filter(Day >= '2015-09-01')


PreHigh <- atlas %>%
  filter(High == 1) %>%
  filter(Day < '2015-09-01')
PreLow <- atlas %>%
  filter(High == 0) %>%
  filter(Day < '2015-09-01')

#Percentage of high earners before - 47.7% | after - 47.6%

high_pre <- feols(median_earn ~ sdindex, data = PreHigh)
low_pre <- feols(median_earn ~ sdindex, data = PreLow)
high_post <- feols(median_earn ~ sdindex, data = PostHigh)
low_post <- feols(median_earn ~ sdindex, data = PostLow)

export_summs(low_pre, high_pre, low_post, high_post)


teat7 <- feols(median_earn ~ sdindex + CONTROL, data = PreHigh)
teat7_ <- feols(median_earn ~ sdindex + CONTROL, data = PreLow)
gray24  <- feols(median_earn ~ sdindex + CONTROL, data = PostHigh)
gray24_  <- feols(median_earn ~ sdindex + CONTROL, data = PostLow)

export_summs(teat7_, teat7, gray24_, gray24)


t7 <- feols(median_earn ~ sdindex + CONTROL, data = Pre)
t77 <- feols(median_earn ~ sdindex + High, data = Pre)
g24  <- feols(median_earn ~ sdindex + CONTROL, data = Post)
g2 <- feols(median_earn ~ sdindex + High, data = Post)

another <- feols(median_earn ~ sdindex + CONTROL * High, data = Pre)
another2 <- feols(median_earn ~ sdindex + CONTROL * High, data = Post)


etable(t77, g2)
etable(t7, g24)
etable(another, another2)
summary(t77)
summary(g2)
summary(another)
summary(another2)


ggplot(data = Pre, aes(Day, median_earn)) +
  geom_smooth() + ggtitle("Pre Sept 2015")

ggplot(data = Post, aes(Day, median_earn)) +
  geom_smooth() + ggtitle("Post Sept 2015")

wald(another, c('sdindex','CONTROL', 'High'))
wald(another2, c('sdindex','CONTROL', 'High'))
wald(t77, 'sdindex')
wald(g2, 'sdindex')
