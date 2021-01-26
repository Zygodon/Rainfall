library(tidyverse)
library(RColorBrewer)

##### FUNCTIONS
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

is_whisker <- function(x)
{
  return(x < quantile(x, 0.25) | x > quantile(x, 0.75))
}
##### MAIN

rawData <- read_csv("Raw_data.csv")

# Get months as factor ready for plotting
month <- factor(rawData$Year, levels = rawData$Year) # Column "Year" in rawData is actually month
# Substitute the "month" column for the mis-labelled "Year" column
tbl0 <- (as_tibble(data.frame(month, rawData[,2:11])) )

# Manipulate into long form and group by month ready for analysis by month
tbl1 <- (gather(tbl0, "year", "rainfall", 2:11)
        %>% group_by(month)
        %>% mutate(whisker = is_whisker(rainfall))
        %>% mutate(whisker_txt = ifelse(whisker, paste(month, sub("X", " ", year)), NA)))

# Boxplot of monthly rainfall over the decade
b1 <- ggplot(tbl1, aes(month, rainfall, group=month)) +
  geom_point(colour="green") +
  geom_boxplot(outlier.colour = "red") +
  geom_text(aes(x=month, label=whisker_txt),na.rm=TRUE, position = position_dodge(width=NULL),  size=2)
print(b1)

# Manipulate the data into long form and group by year ready for analysis by year
tbl2 <- (gather(tbl0, "year", "rainfall", 2:11)
         %>% group_by(year)
         %>% mutate(whisker = is_whisker(rainfall))
         %>% mutate(whisker_txt = ifelse(whisker, paste(month, sub("X", " ", year)), NA)))

# Boxplot of yearly rainfall over the decade
b2 <- ggplot(tbl2, aes(year, rainfall, group=year)) +
  geom_point(colour="green") +
  geom_boxplot(outlier.colour = "red") +
  geom_text(aes(x=year, label=whisker_txt),na.rm=TRUE, position = position_dodge(width=NULL),  size=2)
print(b2)

# Manipulate data into long form and generate a month number
# ready to plot monthly rainfall over the decade
tbl3 <- gather(tbl0, "year", "rainfall", 2:11) %>%  mutate(month_number = 1:n())

# Scatterplot with linear fit, negligible trend.
h <- ggplot(tbl3, aes(month_number, rainfall)) +
    geom_point() +
    geom_smooth(method = "lm")
print(h)






