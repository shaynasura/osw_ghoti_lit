
library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)

#filter the df to include only rows where "Pass abstract screening?" is "Yes"
#this returns 276 observations
ghotilitreview  <- ghotilitreview %>%
  filter(ghotilitreview$`Pass abstract screening?` == "Yes")

#tally up each year
year_tally_AP <- as.data.frame(table(ghotilitreview$`Publication Year`))

#rename the columns for clarity
colnames(year_tally_AP) <- c("Year", "Count")

#convert the "Year" column from factor to character
year_tally_AP$Year <- as.character(year_tally_AP$Year)

#convert the "Year" column from character to numeric
year_tally_AP$Year <- as.numeric(year_tally_AP$Year)

#define breaks for custom fill colors
breaks <- c(0, 7, 15, Inf)

#create a new column for the fill color based on Count
year_tally_AP$fill_color <- cut(year_tally_AP$Count, breaks = breaks, 
                                labels = c("lightblue", "skyblue2", "deepskyblue4"), 
                                include.lowest = TRUE)

#create plot
ggplot(year_tally_AP, aes(x = Year, y = Count, fill = fill_color)) + 
  geom_bar(stat = "identity") + 
  scale_fill_identity() +
  theme_bw() + 
  xlab("Year") +
  ylab("Number of Studies") +
  scale_y_continuous(breaks = seq(0, 27, by = 5)) +
  scale_x_continuous(breaks = seq(1970, 2024, by = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

ggsave("ghotiplot.png", ghotiplot, dpi = 100, bg = "white",
       width = 800,
       height = 1000,
       units = "px") 

#count and print observations where either emperical production column has a value of 1
#this returns 17 observations
count_either_1 <- sum(rowSums(ghotiproductionstudies[c("Empirical Secondary Production", "Empirical Primary Production")] == 1, na.rm = TRUE) > 0, na.rm = TRUE)
print(count_either_1)

#sum and print values in the 'Empirical Secondary Production' column
#this returns 12 observations
total_secondary_production <- sum(ghotiproductionstudies$`Empirical Secondary Production`, na.rm = TRUE)
print(total_secondary_production)

#sum and print values in the 'Empirical Primary Production' column
#this returns 5 observations
total_primary_production <- sum(ghotiproductionstudies$`Empirical Primary Production`, na.rm = TRUE)
print(total_primary_production)

#count and print observations where either emperical production column has a value of 1
#this returns 10 observations
count_either_2 <- sum(rowSums(ghotiproductionstudies[c("Simulation Secondary Production", "Simulation Primary Production")] == 1, na.rm = TRUE) > 0, na.rm = TRUE)
print(count_either_2)

#sum and print values in the 'Simulated Secondary Production' column
#this returns 10 observations
simulated_secondary_production <- sum(ghotiproductionstudies$`Simulation Secondary Production`, na.rm = TRUE)
print(simulated_secondary_production)

#sum and print values in the 'Simulated Primary Production' column
#this returns 9 observations
simulated_primary_production <- sum(ghotiproductionstudies$`Simulation Primary Production`, na.rm = TRUE)
print(simulated_primary_production)

#calculate and print the percentage of observations in ghotiproductionstudies relative to ghotilitreview
percentage <- (nrow(ghotiproductionstudies) / nrow(ghotilitreview)) * 100
print(percentage)

