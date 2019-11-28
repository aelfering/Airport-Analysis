# This script is designed to visualize the change in passenger numbers since 1999

# Package and dataframe loading
library(bbplot)
library(ggplot2)
library(dplyr)

passengers <- read.csv('Eppley Airfield Total Passengers.csv')

# Manipulate the dataframe to compare each year's passengers to 1999's passenger levels
passengers <- passengers %>%
  mutate( Passengers.2000 = passengers$Total.Passengers[1]) %>%
  mutate( Raw.Chg = Total.Passengers - Passengers.2000,
          Chg.2000 = (Total.Passengers - Passengers.2000)/Passengers.2000)

# Visualization
eppley.graph <- ggplot(passengers, aes(x = Year, y = Chg.2000*100)) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  geom_line(colour = "#1380A1", size = 1) +
  geom_point(size = 3, colour = "#1380A1") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(limits = c(1999, 2018)) +
  theme(
    axis.ticks.x = element_line(colour = "#333333"), 
    axis.ticks.length =  unit(0.26, "cm")) +
  bbc_style()  +
  # Title
  labs(title="Eppley Passengers Taking Off",
       subtitle = "The Percent Change in Total Passengers\nat Eppley Airfield since 1999") +
  # Annotation for the passenger decline after Sept. 11
  geom_label(aes(x = 1999, y = 16, label = "Passenger numbers\ndeclined after Sept. 11"), 
               hjust = 0, vjust = 0.5, colour = "#555555", fill = "white", 
               label.size = NA, family="Helvetica", size = 4) +
  geom_curve(aes(x = 2002, y = 14, xend = 2002, yend = -3), 
             colour = "#555555", size=0.5, curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
  # Annotation for the passenger increase in 2018
  geom_label(aes(x = 2009, y = 25, label = "Over 1 million more\npassengers flew than\nin 1999"), 
             hjust = 0, vjust = 0.5, colour = "#555555", fill = "white", 
             label.size = NA, family="Helvetica", size = 4) +
  geom_curve(aes(x = 2015, y = 27, xend = 2017.5, yend = 33), 
             colour = "#555555", size=0.5, curvature = -0.2, arrow = arrow(length = unit(0.01, "npc")))

# Export the plot
finalise_plot(plot_name = eppley.graph,
              source = "Visual by Alex Elfering | Source: Eppley Airfield",
              save_filepath = "Eppley_Passengers.png",
              width_pixels = 440,
              height_pixels = 550)