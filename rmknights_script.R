### 12/02/2021 - Darwin data

library(tidyverse)
library(skimr)
library(rstatix)

### Null Hypothesis - the maize plants being fertilised by their own
### pollen rather than another plant's bears no significant effect on
### their growth

darwin <- read.csv("Data/darwin.csv")

### not tidy, no missing data, needs to be pivoted as there are two data 
### entries on each row

darwin <- darwin %>% 
  pivot_longer(cols = c("Self":"Cross"), 
               names_to = "type", 
               values_to = "height")

darwin %>% 
  ggplot(aes(x = height))+
  geom_histogram()
### doesn't necessarily show outliers - sub groups needed

darwin %>% 
  ggplot(aes(x = type, y = height))+
  geom_jitter(width = 0.1, 
              pch = 21, 
              aes(fill = type))+
  theme_classic()
### shows that height can be higher in cross bred plants

model1 <- lm(height~1, data = darwin)
### ~1 means that we do not have an independent variable, we just want
### to estimate a total mean, lm = linear model, height = dependent 
### variable

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_abline(intercept=18.88, 
              slope=0, 
              linetype="dashed")

darwin %>% 
  summarise(mean=mean(height))

model2 <- lm(height~type, data = darwin)

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), 
               linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

darwin_relevel <- darwin %>% 
  mutate(type=factor(type, 
                     levels=c("Self",
                              "Cross")))
lm(height~type, data=darwin_relevel)

summary(model2)

anova_test(height~type, data=darwin)
pf(q=5.9395, df1=1, df2=28, lower.tail=FALSE)


### Write up attempt:
### The average height of the cross pollinated plants (20.2 inches)
### is significantly taller than the height of the self pollinated 
### plants (17.6 inches). F1,28 = 5.94, P = 0.02.
