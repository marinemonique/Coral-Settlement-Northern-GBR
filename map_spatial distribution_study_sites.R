

# List of packages needed
library(tidyverse)
library(openxlsx)
library(rnaturalearth)
library(sp)
library(sf)
library(raster)
library(marmap)
library(cmocean)
library(ggnewscale)
library(ggtext)
library(ggrepel)
library(ggspatial)
library(ggmap)
library(MetBrewer)
library(lwgeom)
library(patchwork)
library(magick)
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)
library(ggpubr)

### 1. Import data and set up working variables --------------------------------------------------------------------------------------------------------------------------------------------------------

#setwd('/Users/jc312878/Documents/Hoey Lab/Teaching/HDR Students/Monique White MMB Research project')  # set working directory
data <- read.xlsx("~/Larval recruitment resources/Literature Review/Analysis/MWhite_Systematic_Review.xlsx", sheet = 1, startRow = 1, colNames = TRUE)                 # read Excel spreadsheet with the raw data

# I highly recommend you run this line at the beginning
# It will make the format of the column names consistent throughout your dataset (have a look at 'data' after you run it)
data <- janitor::clean_names(data)         # make all column names lowercase (specify package 'janitor' to avoid conflict with insight::clean_names())

data <- data |> filter(publication_year != "2025")

lat   = as.numeric(data$region_coord_lat)         # northing of each study site
long  = as.numeric(data$region_coord_long)        # easting of each study site


coords = as.data.frame(cbind(long, lat))   # create a df with the coordinates of each sampling location
coords <- coords %>% 
  na.omit()  
### 2. Geographic distribution of data sets compiled ---------------------------------------------------------------------------------------------------------------------------------------------------

# Calculate distance to nearest reef

# Import shapefile displaying reef areas worldwide
reef.area <- read_sf("WCMC008_CoralReef2021_Py_v4_1.shp") # read in shapefile with global coral reef area (file can be downloaded from: https://www.arcgis.com/home/item.html?id=364082c8b3be45f5b0bbf579295de653)
proj = st_crs(reef.area)

study.locations = SpatialPoints(coords)                                                                     # create spatial data frame for bird data (object used to be called 'c1')
crs(study.locations) = "+proj=longlat +datum=WGS84 +no_defs"                                                # set the coordinate reference system
# both are in geographic projection long/lat

# Convert to sf object
reef.area.sf = st_as_sf(reef.area)                                                                          # convert polygons of reef areas to sf object
study.locations.sf = st_as_sf(study.locations)                                                              # convert points of sampling locations to sf object

# Re-project for distance measurements
study.locations.robinson = lwgeom::st_transform_proj(study.locations.sf, crs = "+proj=robin")               # Robinson projection
reef.area.robinson = lwgeom::st_transform_proj(reef.area.sf, crs = "+proj=robin")                           # Robinson projection

# Measure distance of sampling locations to nearest reef
#reefs.near.me = st_nearest_feature(study.locations.robinson, reef.area.robinson)                            # returns row ID of nearest feature
#reef.dist = st_distance(study.locations.robinson, reef.area.robinson[reefs.near.me, 1], by_element = TRUE)  # units are in meters
#reef.dist.km = as.numeric(reef.dist/1000)                                                                   # transform to km
#reefs.near = reef.dist.km<25; print(reef.dist.km); summary(reef.dist.km)                                    # is study location "x" within 25 km of a reef? TRUE/FALSE list


#### 2.1. FIGURE Map of sampling locations ----------------------------------------------------------------------------------------------------------------------------------------------------------

##---- Map of the world ----##
world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> st_make_valid()      # get basemap with land masses
target_crs <- st_crs("+proj=longlat +datum=WGS84 + x_0=0 + y_0=0 +lat_0=0 +lon_0=155")            # center map on the Pacific, where most reef area occurs
offset <- 180 - 155                                                                               # centered in longitude 155 deg (East)

polygon <- st_polygon(x = list(rbind(c(-0.0001 - offset, 90),
                                     c(0 - offset, 90),
                                     c(0 - offset, -90),
                                     c(-0.0001 - offset, -90),
                                     c(-0.0001 - offset, 90)))) |>
  st_sfc() |>                                                                        # verifies sf list column's contents, and sets class
  st_set_crs(4326)                                                                   # sets the coordinate reference system (crs)

world <- world  |> 
  st_difference(polygon) |>                                                          # modify 'world' sf object to remove overlapping portions with world's polygons
  st_transform(crs = target_crs)                                                     # transform crs for map elements

# Combine all country polygons to display land masses without borders
world <- world |> dplyr::group_by(featurecla) |> 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") |>
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)))


##---- Bathymetry ----##
# Import bathymetric data from NOAA's ETOPO 2022 database ---> a csv file with bathy data will be downloaded to the working directory the first time this code is run
bathy.data <- marmap::getNOAA.bathy(-24.9, -25, -45, 45, res = 4, antimeridian = TRUE, keep = TRUE)

bathy.df <- marmap::fortify.bathy(bathy.data)      # convert bathymetry to data frame
bathy.df$x <- bathy.df$x-155                       # substract '-155' from each x value so that the bathymetry aligns with the Pacific-centered ggplot map

##---- Study sites ----##
# Create a df with the coordinates of each unique study site and the number of studies conducted at each site
study.sites <- data %>%
  filter(relevant == "Y", publication_year != "2025") %>%                     # only include relevant studies
  group_by(region_coord_lat, region_coord_long) %>%             # group by combinations of coords
  drop_na(region_coord_lat, region_coord_long) %>%              # remove rows with no coord info
  distinct(article_title) %>%                               # means that multiple treatments within a given site/study aren't included as a unique study, but the different sites within a study that are spatially distinct are included (e.g. The four regions in the GBR, or the study that has one site in French Polynesia, one in the Philippines, one in Taiwan)
  summarise(freq=n(), .groups = "drop") %>%                         # count studies per location
  mutate(across(everything(), as.numeric))        # ensure all columns are numeric
  
study.sites_1995 <- data %>%
  filter(relevant == "Y", publication_year != "2025",
         publication_year < 1995) %>%                     # only include relevant studies
  group_by(region_coord_lat, region_coord_long) %>%             # group by combinations of coords
  drop_na(region_coord_lat, region_coord_long) %>%              # remove rows with no coord info
  distinct(article_title) %>%                               # means that multiple treatments within a given site/study aren't included as a unique study, but the different sites within a study that are spatially distinct are included (e.g. The four regions in the GBR, or the study that has one site in French Polynesia, one in the Philippines, one in Taiwan)
  summarise(freq=n(), .groups = "drop") %>%                         # count studies per location
  mutate(across(everything(), as.numeric)) |> 
  mutate("Temporal" = "1984-1994")

study.sites_2005 <- data %>%
  filter(relevant == "Y", publication_year != "2025",
          publication_year<2005) %>%                     # only include relevant studies
  group_by(region_coord_lat, region_coord_long) %>%             # group by combinations of coords
  drop_na(region_coord_lat, region_coord_long) %>%              # remove rows with no coord info
  distinct(article_title) %>%                               # means that multiple treatments within a given site/study aren't included as a unique study, but the different sites within a study that are spatially distinct are included (e.g. The four regions in the GBR, or the study that has one site in French Polynesia, one in the Philippines, one in Taiwan)
  summarise(freq=n(), .groups = "drop") %>%                         # count studies per location
  mutate(across(everything(), as.numeric))|> 
  mutate("Temporal" = "1984-2004")

study.sites_2015 <- data %>%
  filter(relevant == "Y", publication_year != "2025",
          publication_year<2015) %>%                     # only include relevant studies
  group_by(region_coord_lat, region_coord_long) %>%             # group by combinations of coords
  drop_na(region_coord_lat, region_coord_long) %>%              # remove rows with no coord info
  distinct(article_title) %>%                               # means that multiple treatments within a given site/study aren't included as a unique study, but the different sites within a study that are spatially distinct are included (e.g. The four regions in the GBR, or the study that has one site in French Polynesia, one in the Philippines, one in Taiwan)
  summarise(freq=n(), .groups = "drop") %>%                         # count studies per location
  mutate(across(everything(), as.numeric))|> 
  mutate("Temporal" = "1984-2014")

study.sites_2025 <- data %>%
  filter(relevant == "Y", publication_year != "2025",
          publication_year<2025) %>%                     # only include relevant studies
  group_by(region_coord_lat, region_coord_long) %>%             # group by combinations of coords
  drop_na(region_coord_lat, region_coord_long) %>%              # remove rows with no coord info
  distinct(article_title) %>%                               # means that multiple treatments within a given site/study aren't included as a unique study, but the different sites within a study that are spatially distinct are included (e.g. The four regions in the GBR, or the study that has one site in French Polynesia, one in the Philippines, one in Taiwan)
  summarise(freq=n(), .groups = "drop") %>%                         # count studies per location
  mutate(across(everything(), as.numeric)) |> 
  mutate("Temporal" = "1984-2024")


# Recalculate longitudes of study sites to make them align with the Pacific-centered map created
study.sites$region_coord_long[study.sites$region_coord_long < 180]  <- study.sites$region_coord_long[study.sites$region_coord_long < 180] - 155       # subtract 155 from each study site where coord_long is <  180 
study.sites$region_coord_long[study.sites$region_coord_long < -180] <- study.sites$region_coord_long[study.sites$region_coord_long < -180] + 360      # add 360 to long of each study site where coord_long is < -180
study.sites <- study.sites[order(study.sites$freq, decreasing = TRUE),]                                                   # order locations (rows) based on # of studies at that location

study.list <- list(
  study.sites_1995,
  study.sites_2005,
  study.sites_2015,
  study.sites_2025
)

adjust_longitudes <- function(df) {
  df$region_coord_long[df$region_coord_long < 180]  <-
    df$region_coord_long[df$region_coord_long < 180] - 155
  
  df$region_coord_long[df$region_coord_long < -180] <-
    df$region_coord_long[df$region_coord_long < -180] + 360
  
  df <- df[order(df$freq, decreasing = TRUE), ]
  return(df)
}

study.list <- lapply(study.list, adjust_longitudes)

# extract back to original names if desired
study.sites_1995 <- study.list[[1]]
study.sites_2005 <- study.list[[2]]
study.sites_2015 <- study.list[[3]]
study.sites_2025 <- study.list[[4]]


# Try to make a single thing 
#study.sites_temporal <- bind_rows(study.sites_1995, study.sites_2005, study.sites_2015, study.sites_2025)

##---- Plot ----##
map <- ggplot() +
  geom_raster(data = bathy.df, aes(x = x, y = y, fill = z)) +                       # plot the bathymetry data as a raster layer
  scale_fill_cmocean(name = "ice", 
                     start = 0.15, 
                     end = 0.95,
                     alpha = 0.95, 
                     direction = 1,
                     limits = c(min(bathy.df$z),0),
                     guide = guide_colorbar(title = "Depth (m)", 
                                            title.position = "top",
                                            label.position = "bottom",
                                            label.theme = element_text(size = 8),
                                            frame.colour = "black",
                                            frame.linewidth = 0.5/.pt,
                                            barwidth = 12,
                                            order = 3)) +
  new_scale_fill() +                                                               # this command from package 'ggnewscale' allows the use of multiple fill scales
  geom_sf(data = world,                                                            # plot the base layer with the continents and islands
          aes(group = featurecla),
          fill  = "#464646") +
  geom_sf(data = reef.area,                                                        # plot reef areas
          aes(col = "#F0586F")) +                                                  # color of reef area  (alternative color teal -> #3393A4)
  scale_color_manual(values = "#F0586F",
                     labels = "Coral reef area",
                     guide = guide_legend(title = "Habitat",
                                          title.position = "top",
                                          label.position = "bottom", 
                                          order = 2,
                                          override.aes = list(fill = "#E9546B",
                                                              color = NA,
                                                              size = 1))) +
  geom_point(data = study.sites,                                                   # plot sampling locations
             aes(x = region_coord_long, y = region_coord_lat, size = as.factor(freq)),           # plot size of points based on 'freq' (number of studies using settlement tiles at each location) 
             fill = "#FDD353",                                                     # fill color yellow -> #FDD353
             alpha = 0.8,                                                          # set transparency (0 = transparent, 1 = opaque)
             shape = 21,                                                           # if shape = 21, it plots a point with a border
             color = "black") +                                                    # color of border
size_scale_common+
  coord_sf(ylim = c(-45, 45), expand = FALSE) +                                    # display only latitudes between 45 and -45 degrees
  xlab("Longitude") +                                                              # title of x axis
  ylab("Latitude") +                                                               # title of y axis
  labs(caption = "<p><strong><em>Source coral reef area:</em></strong> <em>UN Environment Programme World Conservation Monitoring Centre.</em></p>
                  <p><strong><em>Source bathymetry:</em></strong> <em>ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.</em></p>") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linewidth = 0.5, 
                                    linetype = "solid", 
                                    fill = NA, 
                                    colour = "black"),
        panel.grid = element_blank(),
        plot.caption = element_markdown(hjust = 1, 
                                        lineheight = 1, 
                                        size = 7.2),
        plot.caption.position = "panel",
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin = margin(r = 1, 
                               unit = 'cm'),
        legend.title = element_text(size = 9, 
                                    face = "bold"),
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.key.size = unit(0.4, 'cm'),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(r = 20), 
                                   face = "bold"),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10), 
                                   face = "bold"))
        #axis.title.x = element_text(size = 14, 
                                    #margin = margin(t = 10)),
        #axis.title.y = element_text(size = 14, 
                                    #margin = margin(r = 10)))map

##---- Save plot to PDF ----##
# ggsave(filename = "Spatial distribution of studies using settlement tiles.pdf",    # save plot in a PDF
#        width = 14,
#        height = 5.5,
#        units = "in",
#        dpi = 300)
# ## ---- Save Plot to PNG
# ggsave(filename = "Spatial distribution of studies using settlement tiles.png",    # save plot in a PDF
#        width = 14,
#        height = 5.5,
#        units = "in",
#        dpi = 300)


### 3. Session Info ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Run code below to print session info with packages loaded and their respective versions
sessionInfo()

# Run line of code below to see in what package each function is found
#NCmisc::list.functions.in.file('/Users/jc312878/Documents/Hoey Lab/Teaching/HDR Students/Monique White MMB Research project/map_spatial distribution_study_sites.R')
publication_plot <- readRDS("~/Larval recruitment resources/Literature Review/Analysis/publication_plot.RDS")
#publication_BR_plot <- readRDS("~/Larval recruitment resources/Literature Review/Analysis/publication_time_BR.RDS")
#fig1AB <- readRDS("~/Larval recruitment resources/Literature Review/Analysis/fig1AB.RDS")

#size common
size_scale_common <- scale_size_manual(
  values = c(
    "1" =2.5,
    "2"=3.25,
    "3"=4,
    "4"=4.75,
    "5"=5.5,
    "6"=6.25,
    "8"=7.5,
    "9"=8.25,
    "14"=10.25,
    "15"=11,
    "22"=12.75),
  guide = guide_legend(
    title = "Number of studies using settlement tiles at each location",
    title.position = "top",
    label.position = "bottom",
    order = 1,
    nrow = 1))



map84_95 <- ggplot() +
  geom_raster(data = bathy.df, aes(x = x, y = y, fill = z)) +                       # plot the bathymetry data as a raster layer
  scale_fill_cmocean(name = "ice",
                     start = 0.15,
                     end = 0.95,
                     alpha = 0.95,
                     direction = 1,
                     limits = c(min(bathy.df$z),0),
                     guide = guide_colorbar(title = "Depth (m)",
                                            title.position = "top",
                                            label.position = "bottom",
                                            label.theme = element_text(size = 8),
                                            frame.colour = "black",
                                            frame.linewidth = 0.5/.pt,
                                            barwidth = 12,
                                            order = 3)) +
  new_scale_fill() +                                                               # this command from package 'ggnewscale' allows the use of multiple fill scales
  geom_sf(data = world,                                                            # plot the base layer with the continents and islands
          aes(group = featurecla),
          fill  = "#464646") +
  geom_sf(data = reef.area,                                                        # plot reef areas
          aes(col = "#F0586F")) +                                                  # color of reef area  (alternative color teal -> #3393A4)
  scale_color_manual(values = "#F0586F",
                     labels = "Coral reef area",
                     guide = guide_legend(title = "Habitat",
                                          title.position = "top",
                                          label.position = "bottom", 
                                          order = 2,
                                          override.aes = list(fill = "#E9546B",
                                                              color = NA,
                                                              size = 1))) +
  geom_point(data = study.sites_1995,                                                   # plot sampling locations
             aes(x = region_coord_long, y = region_coord_lat, size = as.factor(freq)),           # plot size of points based on 'freq' (number of studies using settlement tiles at each location) 
             fill = "#FDD353",                                                     # fill color yellow -> #FDD353
             alpha = 0.8,                                                          # set transparency (0 = transparent, 1 = opaque)
             shape = 21,                                                           # if shape = 21, it plots a point with a border
             color = "black") +                                                    # color of border
 size_scale_common+
  coord_sf(ylim = c(-45, 45), expand = FALSE) +                                    # display only latitudes between 45 and -45 degrees
  xlab("") +                                                              # title of x axis
  ylab("Latitude") +
  labs(title = "1984-1994 (inclusive)")+# title of y axis
  #labs(caption = "<p><strong><em>Source coral reef area:</em></strong> <em>UN Environment Programme World Conservation Monitoring Centre.</em></p>
                 # <p><strong><em>Source bathymetry:</em></strong> <em>ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.</em></p>") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linewidth = 0.5, 
                                    linetype = "solid", 
                                    fill = NA, 
                                    colour = "black"),
        panel.grid = element_blank(),
        #plot.caption = element_markdown(hjust = 1, 
                                        #lineheight = 1, 
                                        #size = 7.2),
        #plot.caption.position = "panel",
        legend.position = "none",
        #legend.justification = "left",
        #legend.margin = margin(r = 1, 
                              # unit = 'cm'),
        #legend.title = element_text(size = 9, 
          #                          face = "bold"),
       # legend.text = element_text(size = 8),
      #  legend.spacing.x = unit(0.3, 'cm'),
       # legend.key.size = unit(0.4, 'cm'),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(r = 20), 
                                   face = "bold"),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10), 
                                   face = "bold"),
      title = element_text(size = 11))

map95_05 <- ggplot() +
  geom_raster(data = bathy.df, aes(x = x, y = y, fill = z)) +                       # plot the bathymetry data as a raster layer
  scale_fill_cmocean(name = "ice",
                     start = 0.15,
                     end = 0.95,
                     alpha = 0.95,
                     direction = 1,
                     limits = c(min(bathy.df$z),0),
                     guide = guide_colorbar(title = "Depth (m)",
                                            title.position = "top",
                                            label.position = "bottom",
                                            label.theme = element_text(size = 8),
                                            frame.colour = "black",
                                            frame.linewidth = 0.5/.pt,
                                            barwidth = 12,
                                            order = 3)) +
  new_scale_fill() +                                                               # this command from package 'ggnewscale' allows the use of multiple fill scales
  geom_sf(data = world,                                                            # plot the base layer with the continents and islands
          aes(group = featurecla),
          fill  = "#464646") +
  geom_sf(data = reef.area,                                                        # plot reef areas
          aes(col = "#F0586F")) +                                                  # color of reef area  (alternative color teal -> #3393A4)
  scale_color_manual(values = "#F0586F",
                     labels = "Coral reef area",
                     guide = guide_legend(title = "Habitat",
                                        title.position = "top",
                                          label.position = "bottom", 
                                          order = 2,
                                          override.aes = list(fill = "#E9546B",
                                                              color = NA,
                                                              size = 1))) +
  geom_point(data = study.sites_2005,                                                   # plot sampling locations
             aes(x = region_coord_long, y = region_coord_lat, size = as.factor(freq)),           # plot size of points based on 'freq' (number of studies using settlement tiles at each location) 
             fill = "#FDD353",                                                     # fill color yellow -> #FDD353
             alpha = 0.8,                                                          # set transparency (0 = transparent, 1 = opaque)
             shape = 21,                                                           # if shape = 21, it plots a point with a border
             color = "black") +                                                    # color of border
size_scale_common+
  guides(
    size = "none",      # remove bubble-size legend
    colour = "none"     # remove reef habitat legend
    # keep fill legend (bathymetry)
  ) +
  coord_sf(ylim = c(-45, 45), expand = FALSE) +                                    # display only latitudes between 45 and -45 degrees
  xlab("") +                                                              # title of x axis
  ylab("Latitude") + # title of y axis
  labs(title = "1984-2004 (inclusive)")+
  #labs(caption = "<p><strong><em>Source coral reef area:</em></strong> <em>UN Environment Programme World Conservation Monitoring Centre.</em></p>
                  #<p><strong><em>Source bathymetry:</em></strong> <em>ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.</em></p>") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linewidth = 0.5, 
                                    linetype = "solid", 
                                    fill = NA, 
                                    colour = "black"),
        panel.grid = element_blank(),
        plot.caption = element_markdown(hjust = 1, 
                                        lineheight = 1, 
                                        size = 7.2),
        #plot.caption.position = "panel",
        legend.position = "none",
        #legend.justification = "left",
        #legend.margin = margin(r = 1, 
                              # unit = 'cm'),
        #legend.title = element_text(size = 9, 
                                 #   face = "bold"),
        #legend.text = element_text(size = 8),
        #legend.spacing.x = unit(0.3, 'cm'),
        #legend.key.size = unit(0.4, 'cm'),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(r = 20), 
                                   face = "bold"),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10), 
                                   face = "bold"),
        title = element_text(size = 11))

map05_15 <- ggplot() +
  geom_raster(data = bathy.df, aes(x = x, y = y, fill = z)) +                       # plot the bathymetry data as a raster layer
  scale_fill_cmocean(name = "ice",
                     start = 0.15,
                     end = 0.95,
                     alpha = 0.95,
                     direction = 1,
                     limits = c(min(bathy.df$z),0),
                     guide = guide_colorbar(title = "Depth (m)",
                                            title.position = "top",
                                            label.position = "bottom",
                                            label.theme = element_text(size = 8),
                                            frame.colour = "black",
                                            frame.linewidth = 0.5/.pt,
                                            barwidth = 12,
                                            order = 3)) +
  new_scale_fill() +                                                               # this command from package 'ggnewscale' allows the use of multiple fill scales
  geom_sf(data = world,                                                            # plot the base layer with the continents and islands
          aes(group = featurecla),
          fill  = "#464646") +
  geom_sf(data = reef.area,                                                        # plot reef areas
          aes(col = "#F0586F")) +                                                  # color of reef area  (alternative color teal -> #3393A4)
  scale_color_manual(values = "#F0586F",
                     labels = "Coral reef area",
                     guide = guide_legend(title = "Habitat",
                                          title.position = "top",
                                          label.position = "bottom", 
                                          order = 2,
                                          override.aes = list(fill = "#E9546B",
                                                              color = NA,
                                                              size = 1))) +
  geom_point(data = study.sites_2015,                                                   # plot sampling locations
             aes(x = region_coord_long, y = region_coord_lat, size = as.factor(freq)),           # plot size of points based on 'freq' (number of studies using settlement tiles at each location) 
             fill = "#FDD353",                                                     # fill color yellow -> #FDD353
             alpha = 0.8,                                                          # set transparency (0 = transparent, 1 = opaque)
             shape = 21,                                                           # if shape = 21, it plots a point with a border
             color = "black") +                                                    # color of border
size_scale_common+
  coord_sf(ylim = c(-45, 45), expand = FALSE) +                                    # display only latitudes between 45 and -45 degrees
  xlab("") +                                                              # title of x axis
  ylab("Latitude") + # title of y axis
  labs(title = "1984-2014 (inclusive)")+
  #labs(caption = "<p><strong><em>Source coral reef area:</em></strong> <em>UN Environment Programme World Conservation Monitoring Centre.</em></p>
                 # <p><strong><em>Source bathymetry:</em></strong> <em>ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.</em></p>") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linewidth = 0.5, 
                                    linetype = "solid", 
                                    fill = NA, 
                                    colour = "black"),
        panel.grid = element_blank(),
        plot.caption = element_markdown(hjust = 1, 
                                        lineheight = 1, 
                                        size = 7.2),
        #plot.caption.position = "panel",
        legend.position = "none",
        axis.text.y = element_text(size = 11, 
                                   margin = margin(r = 20), 
                                   face = "bold"),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10), 
                                   face = "bold"),
        title = element_text(size = 11))


map15_25 <- ggplot() +
  geom_raster(data = bathy.df, aes(x = x, y = y, fill = z)) +                       # plot the bathymetry data as a raster layer
  scale_fill_cmocean(name = "ice",
                     start = 0.15,
                     end = 0.95,
                     alpha = 0.95,
                     direction = 1,
                     limits = c(min(bathy.df$z),0),
                     guide = guide_colorbar(title = "Depth (m)",
                                            title.position = "top",
                                            label.position = "bottom",
                                            label.theme = element_text(size = 8),
                                            frame.colour = "black",
                                            frame.linewidth = 0.5/.pt,
                                            barwidth = 12,
                                            order = 3)) +
  new_scale_fill() +                                                               # this command from package 'ggnewscale' allows the use of multiple fill scales
  geom_sf(data = world,                                                            # plot the base layer with the continents and islands
          aes(group = featurecla),
          fill  = "#464646") +
  geom_sf(data = reef.area,                                                        # plot reef areas
          aes(col = "#F0586F")) +                                                  # color of reef area  (alternative color teal -> #3393A4)
  scale_color_manual(values = "#F0586F",
                     labels = "Coral reef area",
                     guide = guide_legend(title = "Habitat",
                                          title.position = "top",
                                          label.position = "bottom", 
                                          order = 2,
                                          override.aes = list(fill = "#E9546B",
                                                              color = NA,
                                                              size = 1))) +
  geom_point(data = study.sites_2025,                                                   # plot sampling locations
             aes(x = region_coord_long, y = region_coord_lat, size = as.factor(freq)),           # plot size of points based on 'freq' (number of studies using settlement tiles at each location) 
             fill = "#FDD353",                                                     # fill color yellow -> #FDD353
             alpha = 0.8,                                                          # set transparency (0 = transparent, 1 = opaque)
             shape = 21,                                                           # if shape = 21, it plots a point with a border
             color = "black") +                                                    # color of border
size_scale_common+
  coord_sf(ylim = c(-45, 45), expand = FALSE) +                                    # display only latitudes between 45 and -45 degrees
  xlab("Longitude") +                                                              # title of x axis
  ylab("Latitude") +                                                               # title of y axis
  labs(title = "1984-2024 (inclusive)")+#labs(caption = "<p><strong><em>Source coral reef area:</em></strong> <em>UN Environment Programme World Conservation Monitoring Centre.</em></p>
                 # <p><strong><em>Source bathymetry:</em></strong> <em>ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.</em></p>") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linewidth = 0.5, 
                                    linetype = "solid", 
                                    fill = NA, 
                                    colour = "black"),
        panel.grid = element_blank(),
        plot.caption = element_markdown(hjust = 1, 
                                        lineheight = 1, 
                                        size = 7.2),
        plot.caption.position = "panel",
        legend.position = "none",
        legend.justification = "left",
        legend.margin = margin(r = 1, 
         unit = 'cm'),
        legend.title = element_text(size = 9, 
           face = "bold"),
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.key.size = unit(0.4, 'cm'),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(r = 20), 
                                   face = "bold"),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10), 
                                   face = "bold"),
        title = element_text(size = 11))

study.sites_test <- data.frame(
  region_coord_lat = c(
    -18.610912,
    -17.484355,
    18.306000,
    25.247989,
    7.288333,
    22.640164,
    29.501838,
    -20.567950,
    16.326667,
    24.497399,
    -21.924000,
    -8.553247,
    -5.282702,
    -5.033330,
    1.283300
  ),
  region_coord_long = c(
    -8.513563,
    55.148205,
    140.281000,
    -99.819441,
    -20.500000,
    -33.518919,
    -120.081904,
    -38.380131,
    -34.966389,
    123.270181,
    -41.072000,
    -35.422983,
    -115.885791,
    -35.683333,
    -51.400000
  ),
  freq = c(
    1,
    2,
    3,
    4,
    5,
    6,
    8,
    9,
    14,
    15,
    22,
    1,
    1,
    1,
    1
  )
)

maplegend <- ggplot() +
  geom_raster(data = bathy.df, aes(x = x, y = y, fill = z)) +                       # plot the bathymetry data as a raster layer
  scale_fill_cmocean(name = "ice",
                     start = 0.15,
                     end = 0.95,
                     alpha = 0.95,
                     direction = 1,
                     limits = c(min(bathy.df$z),0),
                     guide = guide_colorbar(title = "Depth (m)",
                                            title.position = "top",
                                            label.position = "bottom",
                                            label.theme = element_text(size = 8),
                                            frame.colour = "black",
                                            frame.linewidth = 0.5/.pt,
                                            barwidth = 12,
                                            order = 3)) +
  new_scale_fill() +                                                               # this command from package 'ggnewscale' allows the use of multiple fill scales
  geom_sf(data = world,                                                            # plot the base layer with the continents and islands
          aes(group = featurecla),
          fill  = "#464646") +
  geom_sf(data = reef.area,                                                        # plot reef areas
          aes(col = "#F0586F")) +                                                  # color of reef area  (alternative color teal -> #3393A4)
  scale_color_manual(values = "#F0586F",
                     labels = "Coral reef area",
                     guide = guide_legend(title = "Habitat",
                                          title.position = "top",
                                          label.position = "bottom", 
                                          order = 2,
                                          override.aes = list(fill = "#E9546B",
                                                              color = NA,
                                                              size = 1))) +
  geom_point(data = study.sites_test,                                                   # plot sampling locations
             aes(x = region_coord_long, y = region_coord_lat, size = as.factor(freq)),           # plot size of points based on 'freq' (number of studies using settlement tiles at each location) 
             fill = "#FDD353",                                                     # fill color yellow -> #FDD353
             alpha = 0.8,                                                          # set transparency (0 = transparent, 1 = opaque)
             shape = 21,                                                           # if shape = 21, it plots a point with a border
             color = "black") +                                                    # color of border
size_scale_common+
  coord_sf(ylim = c(-45, 45), expand = FALSE) +                                    # display only latitudes between 45 and -45 degrees
  xlab("Longitude") +                                                              # title of x axis
  ylab("Latitude") +                                                               # title of y axis
  #labs(caption = "<p><strong><em>Source coral reef area:</em></strong> <em>UN Environment Programme World Conservation Monitoring Centre.</em></p>
  # <p><strong><em>Source bathymetry:</em></strong> <em>ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.</em></p>") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linewidth = 0.5, 
                                    linetype = "solid", 
                                    fill = NA, 
                                    colour = "black"),
        panel.grid = element_blank(),
        plot.caption = element_markdown(hjust = 1, 
                                        lineheight = 1, 
                                        size = 7.2),
        plot.caption.position = "panel",
        legend.position = "bottom",
        legend.justification = "left",
        legend.margin = margin(r = 1, 
                               unit = 'cm'),
        legend.title = element_text(size = 9, 
                                    face = "bold"),
        legend.text = element_text(size = 8),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.key.size = unit(0.4, 'cm'),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(r = 20), 
                                   face = "bold"),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10), 
                                   face = "bold"))
#fig1 <- ggarrange(publication_plot, map_facet, labels = c("A", ""), nrow = 2, heights = c(0.4,4.5))
#fig1.b <- ggarrange(publication_plot, map, labels = c("A", "B"), nrow = 2, heights = c(0.5,1), widths = c(1, 1))
fig1.c <- ggarrange(publication_plot, map84_95, map95_05, map05_15, map15_25, labels = c("A", "B", "C", "D", "E"), nrow = 5, heights = c(0.7,1,1,1,1.12), widths = c(1, 1, 1, 1, 1))
fig1 <- ggarrange(publication_plot, map84_95, map95_05, map05_15, map15_25, maplegend, labels = c("A", "B", "C", "D", "E", ""), nrow = 6, heights = c(1, 1.12, 1.12, 1.12, 1.12, 1.45), widths = c(1, 1, 1, 1, 1, 1))

ggsave(filename = "legend.png",    # save plot in a PDF
       plot = maplegend,
       width = 9.1,
       height = 7.5,
       units = "in",
       dpi = 300)
# ggsave(filename = "Fig1_Attempt2.png",    # save plot in a PDF
#        plot = fig1.b,
#        width = 10,
#        height = 8,
#        units = "in",
#        dpi = 300)


ggsave(filename = "Fig1_attempt3.png",
       plot = fig1.c,
       width = 9.1,
       height = 12.6,
       units = "in",
       dpi = 100)

ggsave(filename = "Fig1.png",
       plot = fig1,
       width = 9.1,
       height = 12.8,
       units = "in",
       dpi = 300)

ggsave(filename = "Fig_legend.png",
       plot = maplegend,
       width = 9.1,
       height = 5,
       units = "in",
       dpi = 150)

# ggsave(filename = "Fig1_Maps_Only.png",
#        plot = fig1.c,
#        width = 9.1,
#        height = 10.5,
#        units = "in",
#        dpi = 150)

ggsave(filename = "Fig1.png",
       plot = fig1,
       width = 9.1,
       height = 18,
       units = "in",
       dpi = 300)
