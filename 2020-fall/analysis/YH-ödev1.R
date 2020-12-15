library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("C:/Users/Bilgisayar/Documents/YH-ödev1/2020-fall/analysis")

data <- read_dta("~/YH-ödev1/2020-fall/data/raw/NEW7080.dta") %>% rename(EDUC = v4,  QOB = v18, YOB= v27  , CENSUS = v16 )

deg<- c("QOB", "EDUC", "YOB", "CENSUS")
data80 <- subset(data, CENSUS==80 & YOB<40, select = c(deg)) %>%  unite(YEARQ, c(YOB, QOB), sep = "/q", remove = FALSE) %>%
  group_by(YEARQ, QOB) %>% summarise(EDUCMEAN= mean(EDUC)) %>% mutate(a= format(YEARQ, format = "%y/0%q"))

ggplot(data80, aes(x = a, y = EDUCMEAN, group= 1, size=1.2))  + geom_point(aes(shape = factor(QOB), size=1.5)) + geom_line() +
  labs(title = "1930- 1939 Döneminde Doðanlar Ýçin Doðum Çeyreklerine Göre Ortalama Eðitim Süresi", 
       x="1930-1939", y="Ortalama Eðitim Süresi") + theme_light()+ 
  theme_light() + scale_size(guide = 'none') + 
  theme(axis.text.x = element_text(face = "bold", color="black", size = 9, angle = 90))+
  theme(axis.text.y = element_text(face = "bold", color="black", size = 9, angle = 0)) +
  scale_shape(labels = c("1.Doðum Çeyreði", "2.Doðum Çeyreði", "3.Doðum Çeyreði", "4. Doðum Çeyreði"), name = "Doðum Çeyrekleri") +
  theme(legend.title = element_text(face="bold")) + theme(legend.position = "bottom")
