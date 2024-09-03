

library(gridExtra)
library(grid)
library(ggplot2)
library(dplyr)

#######
#filter the df to include only rows where "Any relevance?" is "Yes"
#this returns 276 observations
ghotilitreview_relev  <- ghotilitreview %>%
  filter(ghotilitreview$`Any relevance?` == "Yes")

#tally up each year
year_tally_AP_relev <- as.data.frame(table(ghotilitreview_relev$`Publication Year`))

#rename the columns for clarity
colnames(year_tally_AP_relev) <- c("Year", "Count")

#convert the "Year" column from factor to character
year_tally_AP_relev$Year <- as.character(year_tally_AP_relev$Year)

#convert the "Year" column from character to numeric
year_tally_AP_relev$Year <- as.numeric(year_tally_AP_relev$Year)

#make plot
ghotiplot_relev<-ggplot(year_tally_AP_relev, aes(x = Year, y = Count)) + 
  geom_bar(stat = "identity",fill = "skyblue1", color = "skyblue4", alpha = 0.3) + 
  scale_fill_identity() +
  theme_bw() + 
  xlab(NULL) +
  ylab("\nNumber of Relevant Studies") +
  scale_x_continuous(breaks = seq(1969, 2024, by = 2),limits = c(1969, 2024), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "gray80"),
        panel.grid.major.y = element_line(size = 0.1, color = "gray80"),
        legend.position = "none",
        text = element_text(size = 7))+
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 60), expand = c(0, 0))
ghotiplot_relev

######
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

#make plot
ghotiplot<-ggplot(year_tally_AP, aes(x = Year, y = Count)) + 
  geom_bar(stat = "identity",fill = "skyblue1", color = "skyblue4", alpha = 0.3) + 
  scale_fill_identity() +
  theme_bw() + 
  xlab("Year") +
  ylab("Number of Studies Related to\nArtificial Reef Fish Production") +
  scale_y_continuous(breaks = seq(0, 27, by = 5),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1969, 2024, by = 2),limits = c(1969, 2024), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "gray80"),
        panel.grid.major.y = element_line(size = 0.1, color = "gray80"),
        legend.position = "none",
        text = element_text(size = 7))+
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0))
ghotiplot



#combine two plots to one panel
GhotiPlot<-grid.arrange(ghotiplot_relev, ghotiplot,
                         ncol = 1, nrow = 2)

#save and export
ggsave("GhotiPlot.png", GhotiPlot, dpi = 200, bg = "white",
       width = 800,
       height = 1000,
       units = "px") 





####### below are exploratory calculations/tallies

#count and print observations where either empirical production column has a value of 1
#this returns 18 observations
count_either_1 <- sum(rowSums(ghotiproductionstudies[c("Empirical Secondary Production", "Empirical Primary Production")] == 1, na.rm = TRUE) > 0, na.rm = TRUE)
print(count_either_1)

#sum and print values in the 'Empirical Secondary Production' column
#this returns 13 observations
total_secondary_production <- sum(ghotiproductionstudies$`Empirical Secondary Production`, na.rm = TRUE)
print(total_secondary_production)

#sum and print values in the 'Empirical Primary Production' column
#this returns 5 observations
total_primary_production <- sum(ghotiproductionstudies$`Empirical Primary Production`, na.rm = TRUE)
print(total_primary_production)

#count and print observations where either empirical production column has a value of 1
#this returns 9 observations
count_either_2 <- sum(rowSums(ghotiproductionstudies[c("Simulation Secondary Production", "Simulation Primary Production")] == 1, na.rm = TRUE) > 0, na.rm = TRUE)
print(count_either_2)

#sum and print values in the 'Simulated Secondary Production' column
#this returns 9 observations
simulated_secondary_production <- sum(ghotiproductionstudies$`Simulation Secondary Production`, na.rm = TRUE)
print(simulated_secondary_production)

#sum and print values in the 'Simulated Primary Production' column
#this returns 8 observations
simulated_primary_production <- sum(ghotiproductionstudies$`Simulation Primary Production`, na.rm = TRUE)
print(simulated_primary_production)

#calculate and print the percentage of observations in ghotiproductionstudies relative to ghotilitreview
percentage <- (nrow(ghotiproductionstudies) / nrow(ghotilitreview)) * 100
print(percentage)
