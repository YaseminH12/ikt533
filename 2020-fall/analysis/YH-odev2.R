library(tidyverse)
library(dplyr)
library(haven)
library(ggplot2)
library(survey)
library(readxl)

setwd("C:/Users/Bilgisayar/Documents/YH-ödev1/2020-fall")

deg<- c("year", "S1" , "S3" , "S6", "S6_GRUP", "S7", "S13", "S8A", "S8B", "S24", "DURUM", "KIRKENT", 
        "income", "S75", "S42", "S43A", "S45", "S37A", "S33KOD" , "S39", "S93", "S97", "NUTS2", "FAKTOR", "tufe")

hia10 <- read_dta("data/raw/hia2010.dta") %>% mutate(year = 2010, tufe= 0.857, income = S69)
hia10 <- subset(hia10, select = c(deg)) 

hia11 <- read_dta("data/raw/hia2011.dta") %>% mutate(year = 2011, tufe= 0.647, S71= replace_na(S71,0), income = S69+S71)
hia11 <- subset(hia11, select = c(deg))

hia12 <- read_dta("data/raw/hia2012.dta") %>% mutate(year = 2012, tufe= 0.889, S71= replace_na(S71,0), income = S69+S71)
hia12 <- subset(hia12, select = c(deg))

hia13 <- read_dta("data/raw/hia2013.dta") %>% mutate(year = 2013, tufe= 0.749, S71= replace_na(S71,0), income = S69+S71)
hia13 <- subset(hia13, select = c(deg))

trade <- read_excel("data/raw/trade.xlsx")

#Creating various dummy variables
hia <- rbind(hia10, hia11, hia12, hia13) %>% mutate(one=1, base_t = tufe / 0.857) 

hia <- hia %>% mutate(tre_st = ifelse((year %in% c(2012,2013)),1,0),
                      are_st = ifelse((NUTS2 %in% c(12,13,24,25,26)),1,0),
                      inf_emp = ifelse((DURUM=="1" & S42=="2"),1,0),
                      for_emp = ifelse((DURUM=="1" & S42=="1"),1,0),
                      un_emp = ifelse((DURUM=="2" & S75=="1"),1,0),
                      lab_force = ifelse((DURUM=="2" | S42 %in% c(1,2)),1,0),
                      job_sep = ifelse((S97 %in% c(1,2) & DURUM=="2"),1,0),
                      job_find = ifelse((S97=="4" & DURUM=="1"),1,0),
                      marital = ifelse(S24=="2",1,0),
                      low_educ = ifelse(S13 %in% c(0,1,2,3),1,0),
                      high_educ = ifelse(S13 %in% c(4,5,6),1,0),
                      urb_st = ifelse(KIRKENT=="1",1,0),
                      full_time = ifelse((DURUM=="1" & S45=="1"),1,0))

#Native Population
n_pop <- hia %>% filter(S6_GRUP %in% c(4:13)) %>% filter(NUTS2 %in% c(12,13,24,25,26,20,21,22,23)) %>% 
  filter(!(S7=="2" & S8B>2009))

#Create reel wage variable
n_pop <- n_pop %>% mutate(re_w = income/base_t,reel_wage= log(income/base_t)) %>% mutate(reel_wage= ifelse(is.finite(reel_wage),reel_wage,NA), one=1)

#Adding treade variable by year and region
n_pop <- left_join(n_pop, trade) 

write_dta(n_pop, "data/clean/n_pop.dta")

#Table 3: Summary Statistics Demographic Characteristics for Natives
#Treatment Area
tab3_a <- n_pop %>% filter(are_st=="1") %>% group_by(S3,year) %>% summarise(total = n(), mean_age= mean(S6)) %>%
  ungroup() %>% group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(S3=="1") %>% 
  mutate(g_rate= round(total/tot1, digits = 3)) %>% select(year, tot1, g_rate, mean_age)

tab3_b <- n_pop %>% filter(are_st=="1") %>% group_by(marital,year) %>% summarise(total = n()) %>%ungroup() %>% 
  group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(marital=="1") %>% 
  mutate(m_rate= round(total/tot1, digits = 3)) %>% select(year, m_rate)

tab3_c <- n_pop %>% filter(are_st=="1") %>% group_by(high_educ,year) %>% summarise(total = n()) %>%ungroup() %>% 
  group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(high_educ=="1") %>% 
  mutate(high_rate= round(total/tot1, digits = 3)) %>% select(year, high_rate)

tab3_d <- n_pop %>% filter(are_st=="1") %>% group_by(urb_st,year) %>% summarise(total = n()) %>%ungroup() %>% 
  group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(urb_st=="0") %>% 
  mutate(urb_rate= round(total/tot1, digits = 3)) %>% select(year, urb_rate)

tab3_e <- n_pop %>% filter(are_st=="1") %>% group_by(S3) %>% summarise(total = n(), aget= mean(S6)) %>% 
  mutate(gt_rate = round(total/sum(total), digits = 3), aget= round(aget, digits = 1)) %>% filter(S3=="1") %>% 
  select(gt_rate, aget)

tab3_f <- n_pop %>% filter(are_st=="1") %>% group_by(marital) %>% summarise(total = n()) %>% 
  mutate(mt_rate = round(total/sum(total), digits = 3)) %>% filter(marital=="1") %>% select(mt_rate) 

tab3_g <- n_pop %>% filter(are_st=="1") %>% group_by(high_educ) %>% summarise(total = n()) %>% 
  mutate(he_rate = round(total/sum(total), digits = 3)) %>% filter(high_educ=="1") %>% select(he_rate)

tab3_h <- n_pop %>% filter(are_st=="1") %>% group_by(urb_st) %>% summarise(total = n()) %>% 
  mutate(ut_rate = round(total/sum(total), digits = 3)) %>% filter(urb_st=="0") %>% select(ut_rate) 

tab3_obs1 <- n_pop %>% group_by(are_st) %>% summarise(obst= n()) %>% filter(are_st=="1") %>% select(obst)

tab3_x1 <- cbind(tab3_e, tab3_f, tab3_g, tab3_h, tab3_obs1)

tab3_t<- cbind(tab3_a, tab3_b, tab3_c, tab3_d, tab3_x1)
names(tab3_t)[1] <- "year"

tab3_t <- subset(tab3_t, select = c("year","g_rate","mean_age","m_rate","high_rate","urb_rate", "tot1")) %>% t()

#Control Area
tab3_a2 <- n_pop %>% filter(are_st=="0") %>% group_by(S3,year) %>% summarise(total = n(), mean_age= mean(S6)) %>%
  ungroup() %>% group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(S3=="1") %>% 
  mutate(g_rate= round(total/tot1, digits = 3)) %>% select(year, tot1, g_rate, mean_age)

tab3_b2 <- n_pop %>% filter(are_st=="0") %>% group_by(marital,year) %>% summarise(total = n()) %>%ungroup() %>% 
  group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(marital=="1") %>% 
  mutate(m_rate= round(total/tot1, digits = 3)) %>% select(year, m_rate)

tab3_c2 <- n_pop %>% filter(are_st=="0") %>% group_by(high_educ,year) %>% summarise(total = n()) %>%ungroup() %>% 
  group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(high_educ=="1") %>% 
  mutate(high_rate= round(total/tot1, digits = 3)) %>% select(year, high_rate)

tab3_d2 <- n_pop %>% filter(are_st=="0") %>% group_by(urb_st,year) %>% summarise(total = n()) %>%ungroup() %>% 
  group_by(year) %>% mutate(tot1 = sum(total)) %>% filter(urb_st=="0") %>% 
  mutate(urb_rate= round(total/tot1, digits = 3)) %>% select(year, urb_rate)

tab3_e2 <- n_pop %>% filter(are_st=="0") %>% group_by(S3) %>% summarise(total = n(), aget= mean(S6)) %>% 
  mutate(gt_rate = round(total/sum(total), digits = 3), aget= round(aget, digits = 1)) %>% filter(S3=="1") %>% 
  select(gt_rate, aget)

tab3_f2 <- n_pop %>% filter(are_st=="0") %>% group_by(marital) %>% summarise(total = n()) %>% 
  mutate(mt_rate = round(total/sum(total), digits = 3)) %>% filter(marital=="1") %>% select(mt_rate) 

tab3_g2 <- n_pop %>% filter(are_st=="0") %>% group_by(high_educ) %>% summarise(total = n()) %>% 
  mutate(he_rate = round(total/sum(total), digits = 3)) %>% filter(high_educ=="1") %>% select(he_rate)

tab3_h2 <- n_pop %>% filter(are_st=="0") %>% group_by(urb_st) %>% summarise(total = n()) %>% 
  mutate(ut_rate = round(total/sum(total), digits = 3)) %>% filter(urb_st=="0") %>% select(ut_rate) 

tab3_obs2 <- n_pop %>% group_by(are_st) %>% summarise(obst= n()) %>% filter(are_st=="1") %>% select(obst)

tab3_x2 <- cbind(tab3_e2, tab3_f2, tab3_g2, tab3_h2, tab3_obs2)

tab3_cnt<- cbind(tab3_a2, tab3_b2, tab3_c2, tab3_d2, tab3_x2)
names(tab3_cnt)[1] <- "year"

tab3_cnt <- subset(tab3_cnt, select = c("year","g_rate","mean_age","m_rate","high_rate","urb_rate", "tot1")) %>% t()

table3_end <- rbind(tab3_t, tab3_cnt)

write.table(table3_end, "output/table3.csv", row.names = T) 

#Table 4: Summary Statistics Labor Market for Natives

npopsvy <- svydesign(ids=~1, weights = ~FAKTOR, data= n_pop)

table4_a <- svyby(~one, ~year+are_st, npopsvy, svytotal) %>% rename(year_are_tot = one) %>% 
  select("year","are_st","year_are_tot")

table4_b <- svyby(~one, ~year+DURUM+are_st, npopsvy, svytotal) %>% rename(durum_tot = one) %>% 
  select("year","are_st","DURUM","durum_tot")

table4_c <- svyby(~one, ~year+lab_force+are_st, npopsvy, svytotal) %>% rename(lab_f_tot = one) %>% 
  select("year","are_st","lab_force","lab_f_tot")

table4_d <- svyby(~one, ~year+for_emp+are_st, npopsvy, svytotal) %>% rename(inf_for_tot = one) %>% 
  select("year","are_st","for_emp", "inf_for_tot")

table4_e <- svyby(~one, ~year+job_sep+are_st, npopsvy, svytotal) %>% rename(job_sep_tot = one) %>% 
  select("year","are_st","job_sep","job_sep_tot")

table4_f <- svyby(~one, ~year+job_find+are_st, npopsvy, svytotal) %>% rename(job_find_tot = one) %>% 
  select("year","are_st","job_find","job_find_tot")

table4_g <- svyby(~one, ~year+S42+are_st, npopsvy, svytotal) %>% rename(inf_tot = one) %>% 
  select("year","are_st","S42","inf_tot")


table4 <- left_join(table4_a, table4_b, by= c("year", "are_st"))
table4 <- left_join(table4, table4_c, by= c("year", "are_st"))
table4 <- left_join(table4, table4_d, by= c("year", "are_st"))
table4 <- left_join(table4, table4_e, by= c("year", "are_st"))
table4 <- left_join(table4, table4_f, by= c("year", "are_st"))
table4 <- left_join(table4, table4_g, by= c("year", "are_st"))


table4 <- table4 %>% mutate(emp_unemp = round(durum_tot/ year_are_tot, digits = 3) ,
                            for_inf = round(inf_for_tot/ year_are_tot, digits = 3),
                            lab_for = round(lab_f_tot / year_are_tot, digits = 3),
                            job_s = round(job_sep_tot / year_are_tot, digits = 3),
                            job_f = round(job_sep_tot / year_are_tot, digits = 3),
                            inft = round(inf_tot/ year_are_tot, digits = 3)) 


te <- table4 %>% filter(DURUM=="1") %>% select("year","are_st","DURUM","emp_unemp") %>% distinct() %>% 
  group_by(are_st) %>% mutate(tep_are= mean(emp_unemp)) %>% select("year","are_st","emp_unemp","tep_are") %>%
  rename(tep = emp_unemp)

ue <- table4 %>% filter(DURUM=="2") %>% select("year","are_st","DURUM","emp_unemp") %>% distinct() %>% 
  group_by(are_st) %>% mutate(up_are= mean(emp_unemp)) %>% select("year","are_st","emp_unemp","up_are") %>%
  rename(uep = emp_unemp)

fe <- table4 %>% filter(for_emp=="1") %>% select("year","are_st","for_inf") %>% distinct() %>% 
  group_by(are_st) %>% mutate(fep_are= mean(for_inf)) %>% select("year","are_st","for_inf","fep_are") %>%
  rename(fep = for_inf)

ie <- table4 %>% filter(S42=="2") %>% select("year","are_st","inft") %>% distinct() %>% 
  group_by(are_st) %>% mutate(iep_are= mean(inft)) %>% select("year","are_st","inft","iep_are") %>%
  rename(iep = inft)

lfp <- table4 %>% filter(lab_force=="1") %>% select("year","are_st","lab_for") %>% distinct() %>% 
  group_by(are_st) %>% mutate(lfp_are= mean(lab_for)) %>% select("year","are_st","lab_for","lfp_are") %>%
  rename(lfp = lab_for)

sep <- table4 %>% filter(job_sep=="1") %>% select("year","are_st","job_s") %>% distinct() %>% 
  group_by(are_st) %>% mutate(sep_are= mean(job_s)) %>% select("year","are_st","job_s","sep_are") %>%
  rename(sep = job_s)

jfp <- table4 %>% filter(job_find=="1") %>% select("year","are_st","job_find","job_f") %>% distinct() %>% 
  group_by(are_st) %>% mutate(jfp_are= mean(job_f)) %>% select("year","are_st","job_find","job_f","jfp_are") %>%
  rename(jfp = job_f)

a <- left_join(te,ue, by=c("year","are_st"))
a <- left_join(a,fe, by=c("year","are_st"))
a <- left_join(a,ie, by=c("year","are_st"))
a <- left_join(a,lfp, by=c("year","are_st"))
a <- left_join(a,sep, by=c("year","are_st"))
table4_end <- left_join(a,jfp, by=c("year","are_st"))

write.csv(table4_end, "output/table4.csv", row.names = T)

# Not calculated average reel wage...




