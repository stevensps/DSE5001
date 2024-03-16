## Week 2 review

library(ggplot2)
library(dplyr)

FatRats %>%
  ggplot() +
  geom_boxplot(aes(x=Source, y=Gain, color=Protein))

nominal_gdp_per_capita %>%
  ggplot() +
  geom_histogram(aes(x=gdp))

quartet....



(.37*.53)/((.37*.53)+(.44*.47))

