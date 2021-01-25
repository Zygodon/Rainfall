library(tidyverse)
library(tidygraph)
library(RColorBrewer)


##### FUNCTIONS
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

##### MAIN

rawData <- read_csv("Raw_data.csv") 

# Look for outliers: do these occur later in the series?
tbl <- (gather(rawData, "2011":"2020", key = "year", value = "cases")
        %>% rename("month" = 1, "records" = 3)
        %>% mutate(outlier=ifelse(is_outlier(records), paste(month, " ", year), NA)))


p <- ggplot(tbl, aes(y=records)) +
  geom_boxplot(outlier.colour = "red") +
  geom_text(aes(x=0, label=outlier),na.rm=TRUE, position = position_dodge(width=NULL),  size=2)
p

