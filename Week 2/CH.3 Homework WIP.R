getwd()
#get rid of absolute path

#Read the data. Set strings as factors to identify trends and patterns
weight_data <- read.csv("~/School/DSE5001 Intro to Data Science and Stats/Week 2/weight.csv")
library(dplyr)
library(tidyr) 
library(ggplot2) 



#Chapter 3 HW: selecting variables, filtering observations and reshaping.
weight_data %>% 
  select(subjectid, gender, height, height_selfreport, 
                       weight, weight_selfreport, age, race) %>%
  rename(subject_id=subjectid) %>%
  filter(gender=='Female') %>%
  mutate(age_boolean = ifelse(age<30,'under30','30orolder')) %>%
  arrange(desc(height))

#Practicing Slicing the data
weight_data %>%
  slice(40:100)

#Group By and Summarize
weight_data %>%
  group_by(gender, race) %>%
  summarise(median_weight = median(weight))

#Chapter 4 HW: As with the previous chapter on data wrangling, 
#a valuable exercise based on this chapter is for the reader to use their own 
#data-sets to practice with all the plotting methods that are described in 
#the chapter. It may be that different data sets may be required for different 
#types of plots. See additional datasets below 

FatRats <- read.csv("~/School/DSE5001 Intro to Data Science and Stats/Week 2/FatRats.csv")
nominal_gdp_per_capita <- read.csv("~/School/DSE5001 Intro to Data Science and Stats/Week 2/nominal_gdp_per_capita.csv")
quartet <- read.csv("~/School/DSE5001 Intro to Data Science and Stats/Week 2/quartet.csv")
sleepstudy <- read.csv("~/School/DSE5001 Intro to Data Science and Stats/Week 2/sleepstudy.csv")
TitanicSurvival <- read.csv("~/School/DSE5001 Intro to Data Science and Stats/Week 2/TitanicSurvival.csv")

#FatRats data vis
FatRats %>%
  ggplot() +
  geom_boxplot(aes(x=Source, y=Gain, color=Protein))

#nominal_gdp_per_capita vis
nominal_gdp_per_capita %>%
  ggplot() +
  geom_histogram(aes(x=gdp))

#quartet
quartet %>%
  ggplot() + 
  geom_point(aes(x=x, y=y))
#Set size by set
quartet %>%
  ggplot() + 
  geom_point(aes(x=x, y=y, size=set))

#Sleepstudy
#geom_smooth showed a fairly linear behavior correlated between days and reaction
sleepstudy %>%
  ggplot() + 
  geom_smooth(aes(x=Days, y=Reaction))

#Histogram shows that most people tend to react the most between 225-300
sleepstudy %>%
  ggplot() + 
  geom_histogram(aes(x=Reaction))

#TitanicSurvival

#Use of bar and facet plot to break down survival.
TitanicSurvival %>%
  ggplot()+
  geom_bar(aes(x=survived))+
  facet_grid(cols = vars(sex), rows = vars(passengerClass))

#Chapter 5 HW: Using Fat Rats Data

#Univariate Exploration


#univariate exploration by source
gbs <- FatRats %>% 
  group_by(Source)

gbs %>%
  summarise(mean_Gain = mean(Gain),variance_Gain = var(Gain),
            stddeviation_Gain = sd(Gain),
            median_Gain = median(Gain),
            maximum_value = max(Gain),
            minimum_value = min(Gain),
            InterQuartileRange = IQR(Gain))

#Boxplot showing source vs. Gain
FatRats %>%
  ggplot() + 
  geom_boxplot(aes(x=Source, y=Gain))

#Histogram showing count by Gain
FatRats %>%
  ggplot()+
  geom_histogram(aes(x=Gain))
  
#Group Data by Protein
gbp <- FatRats %>%
  group_by(Protein)

gbp %>%
  summarise(mean_Gain = mean(Gain),variance_Gain = var(Gain),
            stddeviation_Gain = sd(Gain),
            median_Gain = median(Gain),
            maximum_value = max(Gain),
            minimum_value = min(Gain),
            InterQuartileRange = IQR(Gain))
FatRats %>%
  ggplot() + 
  geom_boxplot(aes(x=Protein, y=Gain))

#Measures of Central Tendency
trimmed_mean <- function(x,trim=0.1){
  n <- length(x)
  lo <- floor(n*trim)+1
  hi <- n+1-lo
  sort(x)[lo:hi] %>%
    mean()
}
print(paste("The trimmed mean of the Gain of the Fatrats dataset is: ",trimmed_mean(FatRats$Gain)))

iqr_mean <- function(x){
  q1 <- quantile(x,probs=0.25)
  q2 <- quantile(x,probs=0.75)
  x[x>q1 & x < q2] %>%
    mean()
}

print(paste("The IQR Mean of the Gain of the Fatrats dataset is: ",iqr_mean(FatRats$Gain)))

winsorized_mean <- function(x,trim=0.1){
  low <- quantile(x,probs=trim)
  high <- quantile(x,probs=1-trim)
  x[x<low]<- low
  x[x>high]<-high
  mean(x)
}

print(paste("The winsorized_mean of the Gain of the Fatrats dataset is: ",winsorized_mean(FatRats$Gain)))

#Skewness
skewness <- function(x,dof=1){
  xbar <- mean(x)
  s <- sd(x)
  mean((x-xbar)^3)/s^3
}

print(paste("The skewness of the Gain of the Fatrats dataset is: ",skewness(FatRats$Gain)))

#Kurtosis
kurtosis <- function(x){
  z <- (x-mean(x))/sd(x)
  mean(z^4)
}

print(paste("The kurtosis of the Gain of the Fatrats dataset is: ",kurtosis(FatRats$Gain)))
