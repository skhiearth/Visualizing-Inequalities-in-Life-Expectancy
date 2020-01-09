# Loading packages
library(dplyr)
library(tidyr)
library(ggplot2)
options(repr.plot.width = 6, repr.plot.height = 6)

# Loading data
life_expectancy <- read.csv("datasets/UNdata.csv")
head(life_expectancy)

# Subsetting and reshaping the life expectancy data
subdata <- life_expectancy  %>% filter (Year == '2000-2005') %>% 
  subset(select=c(Country.or.Area, Subgroup, Value)) %>%
  spread(Subgroup, Value)

head(subdata)

# Subseting data to obtain countries of interest
top_male <- subdata %>% arrange(Male-Female) %>% head(3)
top_female <- subdata %>% arrange(Female-Male) %>% head(3)

# Plotting male and female life expectancy
ggplot(subdata, aes(x=Male, y=Female, label=Country.or.Area)) +
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=3) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_x_continuous(limits=c(35,85)) +
  scale_y_continuous(limits=c(35,85)) +
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females") + 
  geom_text(data=top_male, size=3) + geom_text(data=top_female, size=3) +
  theme_bw()

# Subsetting, mutating and reshaping the life expectancy data
subdata2 <- life_expectancy %>% 
  filter(Year %in% c("1985-1990", "2000-2005")) %>% 
  mutate(Sub_Year=paste(Subgroup, Year, sep="_")) %>% 
  mutate(Sub_Year=gsub("-", "_", Sub_Year)) %>% 
  select(-Subgroup, -Year) %>% spread(Sub_Year, Value) %>%
  mutate(diff_Female = Female_2000_2005 - Female_1985_1990) %>%
  mutate(diff_Male = Male_2000_2005 - Male_1985_1990)

head(subdata2)

# Subseting data to obtain countries of interest
top <- subdata2 %>% arrange(diff_Male+diff_Female) %>% head(3)
bottom <- subdata2 %>% arrange(-(diff_Male+diff_Female)) %>% head(3)

# Adding text to the previous plot to label countries of interest
ggplot(subdata2, aes(x=diff_Male, y=diff_Female, label=Country.or.Area), guide=FALSE)+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits=c(-25,25))+
  scale_y_continuous(limits=c(-25,25))+
  geom_hline(yintercept=0, linetype=2)+
  geom_vline(xintercept=0, linetype=2)+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Difference between 1985-1990 and 2000-2005; Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
  geom_text(data=top, size=3) + geom_text(data=bottom, size=3) +
  theme_bw()
