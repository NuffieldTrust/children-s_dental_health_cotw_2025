#This piece of code visualises regional data on children's oral health drawn from the
#National Dental Epidemiology Programme for England oral health survey 2024......

#Author: Stuti Bagri............................................................

#Set up project folder..........................................................
dir.create("data")
dir.create("output")
dir.create("r")
dir.create("r/packages")

#Install and load relevant packages.............................................
#Set library path
.libPaths("r/packages")

#Check libPath
.libPaths()

install.packages("tidyverse")
install.packages("sf")
install.packages("readODS")
install.packages("svglite")

library(tidyverse)
library(sf)
library(readODS)
library(svglite)

#Import the data................................................................
data <- read_ods("~/NDEP_for_England_OH_Survey_5yr_2024_Results_v2.0.ods", sheet= "NHSER", skip=3) %>% 
  rename_all(tolower) %>% 
  rename_with(~gsub(" ", "_", .x, fixed=TRUE)) %>% 
  rename(region = "nhs_region_name",
         prop_dd = "percentage_d3mft>0",
         prop_te = "percentage_mt>0",
         prop_obvdd = "percentage_d3t>0",
         pop = "5_year_old_population_estimate_(mid_2022)")

legend <- read_ods("~/NDEP_for_England_OH_Survey_5yr_2024_Results_v2.0.ods", sheet= "Glossary", skip=2)

shapefile <- st_read("~/NHSER_JAN_2024_EN_BFC.shp") %>% 
  rename(region = "NHSER24NM")

#What proportion of the population were surveyed for each region?.........................................
data_scope <- data %>% 
  select(region, pop, examined, prop_dd, prop_obvdd, prop_te) %>% 
  mutate(prop_examined = round(examined/pop*100, 2))

#Join the geographical and analysis data sets...............................................
map_data <- shapefile %>% 
  left_join(data %>% select(region, prop_obvdd, prop_dd, prop_te), by = "region")

#Understand the geographical coordinates system
st_crs(map_data)
st_bbox(map_data)

#Manually specify coordinates of label for London to prevent it from being covered by the label for South East England
map_data$nudge_x <- ifelse(map_data$region == "London", 95000, 0)
map_data$nudge_y <- ifelse(map_data$region == "London", 15000, 0)

#Create the map for dental decay

#Note: Colors have been assigned to regions according to the rank of values, i.e., the region with the highest percentage
#of dentinal decay has been assigned the darkest shade of red.

dd_static <- ggplot(map_data) +
  geom_sf(aes(fill = region),
          show.legend= FALSE) +
  scale_fill_manual(values = c("North West" = "#b10026",
                               "London" = "#e31a1c",
                               "Midlands" = "#fc4e2a",
                               "North East and Yorkshire" = "#fc4e2a",
                               "South West" = "#fd8d3c",
                               "South East" = "#feb24c",
                               "East of England" = "#fed976")) + 
  geom_sf_label(mapping = aes(label = paste0(region, ": ",prop_obvdd,"%")),
                fill = "#271544",
                family = "Arial",
                size = 2.5,
                color = "white",
                nudge_y = map_data$nudge_y,
                nudge_x = map_data$nudge_x,
                label.size = 0.15,
                label.padding = unit(0.15, "lines")) +
  theme_minimal() +
  labs(title = "Proportion of five year old children with any\nobvious untreated dentinal decay across England (2024)") +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(size=12, colour="black"))

#View the map
print(dd_static)

#Create the map for tooth extractions
te_static <- ggplot(map_data) +
  geom_sf(aes(fill = region),
          show.legend= FALSE) +
  scale_fill_manual(values = c("North East and Yorkshire" = "#b10026",
                               "London" = "#e31a1c",
                               "North West" = "#fc4e2a",
                               "South East" = "#fd8d3c",
                               "South West" = "#feb24c",
                               "Midlands" = "#fed976",
                               "East of England" = "#ffffb2")) + 
  geom_sf_label(mapping = aes(label = paste0(region, ": ",prop_te,"%")),
                fill = "#271544",
                family = "Arial",
                size = 2.5,
                color = "white",
                nudge_y = map_data$nudge_y,
                nudge_x = map_data$nudge_x,
                label.size = 0.15,
                label.padding = unit(0.15, "lines")) +
  theme_minimal() +
  labs(title = "Proportion of five year old children with one or more\nteeth extracted due to dental decay across England (2024)") +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(size=12, colour="black"))

#View the map
print(te_static)

#Save the images
ggsave("~/decayed_teethsvg.svg", plot=dd_static, 
       width = 6, height = 6, units = "in", 
       device="svg")

ggsave("~/decayed_teethpng.png",plot=dd_static,
       width = 6, height = 6, units = "in")

ggsave("~/extracted_teethsvg.svg", plot=te_static, 
       width = 6, height = 6, units = "in", 
       device="svg")

ggsave("~/extracted_teethpng.png",plot=te_static,
       width = 6, height = 6, units = "in")


