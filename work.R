library(tidyverse)
# library(tidygraph)
library(RColorBrewer)

##### FUNCTIONS

##### MAIN

rawData <- read_csv("Raw_data.csv")
sd <- rawData %>% select(2:11) %>% summarise_each(sd)
d <- gather(sd) %>% mutate(year = as.numeric(key)) %>% select(year, value) %>% rename(sd_rainfall = value)

f <- ggplot(d, aes(year, sd_rainfall)) +
     geom_point()
f

dMax <- (rawData %>% select(2:11) 
         %>% summarise_each(max)
         %>% gather()
         %>% mutate(year = as.numeric(key))
         %>% select(year, value)
         %>% rename(max = value))

dMin <- (rawData %>% select(2:11) 
         %>% summarise_each(min)
         %>% gather()
         %>% mutate(year = as.numeric(key))
         %>% select(year, value)
         %>% rename(min = value))

d <- left_join(dMax, dMin, by = "year") %>% mutate(range = max - min)

f <- ggplot(d, aes(year, range)) +
  geom_point()
f

month <- factor(rawData$Year, levels = rawData$Year)
tbl <- as_tibble(data.frame(month, rawData[,2:11])) %>% mutate(x = as.numeric(as.factor(month)))
ggplot(tbl, aes(x, X2011)) +
        geom_smooth() +
        geom_smooth(aes(X2012), colour = "red")





