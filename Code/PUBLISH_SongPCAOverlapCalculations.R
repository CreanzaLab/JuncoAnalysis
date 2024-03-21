### Song PCA overlap calculations

# Load packages
library(tidyverse)
library(scales)
library(rgeos)
library(sp)
library(rgdal)
library(ggplot2)
library(sf)
library(ggspatial)

# Song PCA
JuncoDataLog <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongData_Subspecies_and_ID.csv")
song.latlong <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongLatLongs.csv")
song_pca <- prcomp(JuncoDataLog[,2:11], scale = TRUE)
song_pc <- song_pca$x
song_pc_df <- data.frame(PC1 = song_pc[, 1], PC2 = song_pc[, 2], Subspecies = JuncoDataLog$subspecies, IDNum = JuncoDataLog$ID_Num)

# hyemalis x oreganus PCA area overlap calculation + plot

# Extract the PC points for each subspecies
hyemalis_pca <- song_pc_df %>%
  filter(Subspecies == "hyemalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "hyemalis")

oreganus_pca <- song_pc_df %>%
  filter(Subspecies == "oreganus") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "oreganus")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
hyemalis_spdf <- SpatialPointsDataFrame(hyemalis_pca[, c("PC1", "PC2")], hyemalis_pca)
oreganus_spdf <- SpatialPointsDataFrame(oreganus_pca[, c("PC1", "PC2")], oreganus_pca)

# Estimate the convex hull polygons for each subspecies
hyemalis_hull <- gConvexHull(hyemalis_spdf)
oreganus_hull <- gConvexHull(oreganus_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(hyemalis_hull, oreganus_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 93.44149

hyemalis_area <- gArea(hyemalis_hull)
hyemalis_area # 118.7583

oreganus_area <- gArea(oreganus_hull)
oreganus_area # 109.6614

pca_overlap_area_proportion <- intersection_area / (hyemalis_area + (oreganus_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.6922706

# Convert the polygons to sf objects
hyemalis_sf <- st_as_sf(hyemalis_hull)
oreganus_sf <- st_as_sf(oreganus_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = hyemalis_sf, Subspecies = "hyemalis"),
  data.frame(geometry = oreganus_sf, Subspecies = "oreganus"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# mearnsi x caniceps PCA area overlap calculation + plot

# Extract the PC points for each subspecies
mearnsi_pca <- song_pc_df %>%
  filter(Subspecies == "mearnsi") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "mearnsi")

caniceps_pca <- song_pc_df %>%
  filter(Subspecies == "caniceps") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "caniceps")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
mearnsi_spdf <- SpatialPointsDataFrame(mearnsi_pca[, c("PC1", "PC2")], mearnsi_pca)
caniceps_spdf <- SpatialPointsDataFrame(caniceps_pca[, c("PC1", "PC2")], caniceps_pca)

# Estimate the convex hull polygons for each subspecies
mearnsi_hull <- gConvexHull(mearnsi_spdf)
caniceps_hull <- gConvexHull(caniceps_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(mearnsi_hull, caniceps_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 27.49318

mearnsi_area <- gArea(mearnsi_hull)
mearnsi_area # 31.58175

caniceps_area <- gArea(caniceps_hull)
caniceps_area # 36.21917

pca_overlap_area_proportion <- intersection_area / (mearnsi_area + (caniceps_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.6820817

# Convert the polygons to sf objects
mearnsi_sf <- st_as_sf(mearnsi_hull)
caniceps_sf <- st_as_sf(caniceps_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = mearnsi_sf, Subspecies = "mearnsi"),
  data.frame(geometry = caniceps_sf, Subspecies = "caniceps"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# mearnsi x aikeni PCA area overlap calculation + plot

# Extract the PC points for each subspecies
mearnsi_pca <- song_pc_df %>%
  filter(Subspecies == "mearnsi") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "mearnsi")

aikeni_pca <- song_pc_df %>%
  filter(Subspecies == "aikeni") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "aikeni")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
mearnsi_spdf <- SpatialPointsDataFrame(mearnsi_pca[, c("PC1", "PC2")], mearnsi_pca)
aikeni_spdf <- SpatialPointsDataFrame(aikeni_pca[, c("PC1", "PC2")], aikeni_pca)

# Estimate the convex hull polygons for each subspecies
mearnsi_hull <- gConvexHull(mearnsi_spdf)
aikeni_hull <- gConvexHull(aikeni_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(mearnsi_hull, aikeni_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 18.00221

mearnsi_area <- gArea(mearnsi_hull)
mearnsi_area # 31.58175

aikeni_area <- gArea(aikeni_hull)
aikeni_area # 21.10826

pca_overlap_area_proportion <- intersection_area / (mearnsi_area + (aikeni_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.5189782

# Convert the polygons to sf objects
mearnsi_sf <- st_as_sf(mearnsi_hull)
aikeni_sf <- st_as_sf(aikeni_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = mearnsi_sf, Subspecies = "mearnsi"),
  data.frame(geometry = aikeni_sf, Subspecies = "aikeni"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# caniceps x dorsalis PCA area overlap calculation + plot

# Extract the PC points for each subspecies
dorsalis_pca <- song_pc_df %>%
  filter(Subspecies == "dorsalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "dorsalis")

caniceps_pca <- song_pc_df %>%
  filter(Subspecies == "caniceps") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "caniceps")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
dorsalis_spdf <- SpatialPointsDataFrame(dorsalis_pca[, c("PC1", "PC2")], dorsalis_pca)
caniceps_spdf <- SpatialPointsDataFrame(caniceps_pca[, c("PC1", "PC2")], caniceps_pca)

# Estimate the convex hull polygons for each subspecies
dorsalis_hull <- gConvexHull(dorsalis_spdf)
caniceps_hull <- gConvexHull(caniceps_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(dorsalis_hull, caniceps_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 21.8521

dorsalis_area <- gArea(dorsalis_hull)
dorsalis_area # 26.93136

caniceps_area <- gArea(caniceps_hull)
caniceps_area # 36.21917

pca_overlap_area_proportion <- intersection_area / (dorsalis_area + (caniceps_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.5291266

# Convert the polygons to sf objects
dorsalis_sf <- st_as_sf(dorsalis_hull)
caniceps_sf <- st_as_sf(caniceps_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = dorsalis_sf, Subspecies = "dorsalis"),
  data.frame(geometry = caniceps_sf, Subspecies = "caniceps"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# oreganus x caniceps PCA area overlap calculation + plot

# Extract the PC points for each subspecies
oreganus_pca <- song_pc_df %>%
  filter(Subspecies == "oreganus") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "oreganus")

caniceps_pca <- song_pc_df %>%
  filter(Subspecies == "caniceps") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "caniceps")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
oreganus_spdf <- SpatialPointsDataFrame(oreganus_pca[, c("PC1", "PC2")], oreganus_pca)
caniceps_spdf <- SpatialPointsDataFrame(caniceps_pca[, c("PC1", "PC2")], caniceps_pca)

# Estimate the convex hull polygons for each subspecies
oreganus_hull <- gConvexHull(oreganus_spdf)
caniceps_hull <- gConvexHull(caniceps_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(oreganus_hull, caniceps_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 33.25831

oreganus_area <- gArea(oreganus_hull)
oreganus_area # 109.6614

caniceps_area <- gArea(caniceps_hull)
caniceps_area # 36.21917

pca_overlap_area_proportion <- intersection_area / (oreganus_area + (caniceps_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.2953084

# Convert the polygons to sf objects
oreganus_sf <- st_as_sf(oreganus_hull)
caniceps_sf <- st_as_sf(caniceps_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = oreganus_sf, Subspecies = "oreganus"),
  data.frame(geometry = caniceps_sf, Subspecies = "caniceps"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# oreganus x mearnsi PCA area overlap calculation + plot

# Extract the PC points for each subspecies
oreganus_pca <- song_pc_df %>%
  filter(Subspecies == "oreganus") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "oreganus")

mearnsi_pca <- song_pc_df %>%
  filter(Subspecies == "mearnsi") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "mearnsi")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
oreganus_spdf <- SpatialPointsDataFrame(oreganus_pca[, c("PC1", "PC2")], oreganus_pca)
mearnsi_spdf <- SpatialPointsDataFrame(mearnsi_pca[, c("PC1", "PC2")], mearnsi_pca)

# Estimate the convex hull polygons for each subspecies
oreganus_hull <- gConvexHull(oreganus_spdf)
mearnsi_hull <- gConvexHull(mearnsi_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(oreganus_hull, mearnsi_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 29.61071

oreganus_area <- gArea(oreganus_hull)
oreganus_area # 109.6614

mearnsi_area <- gArea(mearnsi_hull)
mearnsi_area # 31.58175

pca_overlap_area_proportion <- intersection_area / (oreganus_area + (mearnsi_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.2652518

# Convert the polygons to sf objects
oreganus_sf <- st_as_sf(oreganus_hull)
mearnsi_sf <- st_as_sf(mearnsi_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = oreganus_sf, Subspecies = "oreganus"),
  data.frame(geometry = mearnsi_sf, Subspecies = "mearnsi"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# hyemalis x aikeni PCA area overlap calculation + plot

# Extract the PC points for each subspecies
hyemalis_pca <- song_pc_df %>%
  filter(Subspecies == "hyemalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "hyemalis")

aikeni_pca <- song_pc_df %>%
  filter(Subspecies == "aikeni") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "aikeni")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
hyemalis_spdf <- SpatialPointsDataFrame(hyemalis_pca[, c("PC1", "PC2")], hyemalis_pca)
aikeni_spdf <- SpatialPointsDataFrame(aikeni_pca[, c("PC1", "PC2")], aikeni_pca)

# Estimate the convex hull polygons for each subspecies
hyemalis_hull <- gConvexHull(hyemalis_spdf)
aikeni_hull <- gConvexHull(aikeni_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(hyemalis_hull, aikeni_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 20.16275

hyemalis_area <- gArea(hyemalis_hull)
hyemalis_area # 118.7583

aikeni_area <- gArea(aikeni_hull)
aikeni_area # 21.10826

pca_overlap_area_proportion <- intersection_area / (hyemalis_area + (aikeni_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.1684386

# Convert the polygons to sf objects
hyemalis_sf <- st_as_sf(hyemalis_hull)
aikeni_sf <- st_as_sf(aikeni_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = hyemalis_sf, Subspecies = "hyemalis"),
  data.frame(geometry = aikeni_sf, Subspecies = "aikeni"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# hyemalis x caniceps PCA area overlap calculation + plot

# Extract the PC points for each subspecies
hyemalis_pca <- song_pc_df %>%
  filter(Subspecies == "hyemalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "hyemalis")

caniceps_pca <- song_pc_df %>%
  filter(Subspecies == "caniceps") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "caniceps")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
hyemalis_spdf <- SpatialPointsDataFrame(hyemalis_pca[, c("PC1", "PC2")], hyemalis_pca)
caniceps_spdf <- SpatialPointsDataFrame(caniceps_pca[, c("PC1", "PC2")], caniceps_pca)

# Estimate the convex hull polygons for each subspecies
hyemalis_hull <- gConvexHull(hyemalis_spdf)
caniceps_hull <- gConvexHull(caniceps_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(hyemalis_hull, caniceps_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 36.1117

hyemalis_area <- gArea(hyemalis_hull)
hyemalis_area # 118.7583

caniceps_area <- gArea(caniceps_hull)
caniceps_area # 36.21917

pca_overlap_area_proportion <- intersection_area / (hyemalis_area + (caniceps_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.3038023

# Convert the polygons to sf objects
hyemalis_sf <- st_as_sf(hyemalis_hull)
caniceps_sf <- st_as_sf(caniceps_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = hyemalis_sf, Subspecies = "hyemalis"),
  data.frame(geometry = caniceps_sf, Subspecies = "caniceps"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# hyemalis x dorsalis PCA area overlap calculation + plot

# Extract the PC points for each subspecies
hyemalis_pca <- song_pc_df %>%
  filter(Subspecies == "hyemalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "hyemalis")

dorsalis_pca <- song_pc_df %>%
  filter(Subspecies == "dorsalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "dorsalis")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
hyemalis_spdf <- SpatialPointsDataFrame(hyemalis_pca[, c("PC1", "PC2")], hyemalis_pca)
dorsalis_spdf <- SpatialPointsDataFrame(dorsalis_pca[, c("PC1", "PC2")], dorsalis_pca)

# Estimate the convex hull polygons for each subspecies
hyemalis_hull <- gConvexHull(hyemalis_spdf)
dorsalis_hull <- gConvexHull(dorsalis_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(hyemalis_hull, dorsalis_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 26.93136

hyemalis_area <- gArea(hyemalis_hull)
hyemalis_area # 118.7583

dorsalis_area <- gArea(dorsalis_hull)
dorsalis_area # 26.93136

pca_overlap_area_proportion <- intersection_area / (hyemalis_area + (dorsalis_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.2267745

# Convert the polygons to sf objects
hyemalis_sf <- st_as_sf(hyemalis_hull)
dorsalis_sf <- st_as_sf(dorsalis_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = hyemalis_sf, Subspecies = "hyemalis"),
  data.frame(geometry = dorsalis_sf, Subspecies = "dorsalis"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# hyemalis x mearnsi PCA area overlap calculation + plot

# Extract the PC points for each subspecies
hyemalis_pca <- song_pc_df %>%
  filter(Subspecies == "hyemalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "hyemalis")

mearnsi_pca <- song_pc_df %>%
  filter(Subspecies == "mearnsi") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "mearnsi")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
hyemalis_spdf <- SpatialPointsDataFrame(hyemalis_pca[, c("PC1", "PC2")], hyemalis_pca)
mearnsi_spdf <- SpatialPointsDataFrame(mearnsi_pca[, c("PC1", "PC2")], mearnsi_pca)

# Estimate the convex hull polygons for each subspecies
hyemalis_hull <- gConvexHull(hyemalis_spdf)
mearnsi_hull <- gConvexHull(mearnsi_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(hyemalis_hull, mearnsi_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 31.58175

hyemalis_area <- gArea(hyemalis_hull)
hyemalis_area # 118.7583

mearnsi_area <- gArea(mearnsi_hull)
mearnsi_area # 31.58175

pca_overlap_area_proportion <- intersection_area / (hyemalis_area + (mearnsi_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.265933

# Convert the polygons to sf objects
hyemalis_sf <- st_as_sf(hyemalis_hull)
mearnsi_sf <- st_as_sf(mearnsi_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = hyemalis_sf, Subspecies = "hyemalis"),
  data.frame(geometry = mearnsi_sf, Subspecies = "mearnsi"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# oreganus x aikeni PCA area overlap calculation + plot

# Extract the PC points for each subspecies
oreganus_pca <- song_pc_df %>%
  filter(Subspecies == "oreganus") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "oreganus")

aikeni_pca <- song_pc_df %>%
  filter(Subspecies == "aikeni") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "aikeni")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
oreganus_spdf <- SpatialPointsDataFrame(oreganus_pca[, c("PC1", "PC2")], oreganus_pca)
aikeni_spdf <- SpatialPointsDataFrame(aikeni_pca[, c("PC1", "PC2")], aikeni_pca)

# Estimate the convex hull polygons for each subspecies
oreganus_hull <- gConvexHull(oreganus_spdf)
aikeni_hull <- gConvexHull(aikeni_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(oreganus_hull, aikeni_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 18.72336

oreganus_area <- gArea(oreganus_hull)
oreganus_area # 109.6614

aikeni_area <- gArea(aikeni_hull)
aikeni_area # 21.10826

pca_overlap_area_proportion <- intersection_area / (oreganus_area + (aikeni_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.1671037

# Convert the polygons to sf objects
oreganus_sf <- st_as_sf(oreganus_hull)
aikeni_sf <- st_as_sf(aikeni_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = oreganus_sf, Subspecies = "oreganus"),
  data.frame(geometry = aikeni_sf, Subspecies = "aikeni"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# oreganus x dorsalis PCA area overlap calculation + plot

# Extract the PC points for each subspecies
oreganus_pca <- song_pc_df %>%
  filter(Subspecies == "oreganus") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "oreganus")

dorsalis_pca <- song_pc_df %>%
  filter(Subspecies == "dorsalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "dorsalis")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
oreganus_spdf <- SpatialPointsDataFrame(oreganus_pca[, c("PC1", "PC2")], oreganus_pca)
dorsalis_spdf <- SpatialPointsDataFrame(dorsalis_pca[, c("PC1", "PC2")], dorsalis_pca)

# Estimate the convex hull polygons for each subspecies
oreganus_hull <- gConvexHull(oreganus_spdf)
dorsalis_hull <- gConvexHull(dorsalis_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(oreganus_hull, dorsalis_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 26.20836

oreganus_area <- gArea(oreganus_hull)
oreganus_area # 109.6614

dorsalis_area <- gArea(dorsalis_hull)
dorsalis_area # 26.93136

pca_overlap_area_proportion <- intersection_area / (oreganus_area + (dorsalis_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.237428

# Convert the polygons to sf objects
oreganus_sf <- st_as_sf(oreganus_hull)
dorsalis_sf <- st_as_sf(dorsalis_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = oreganus_sf, Subspecies = "oreganus"),
  data.frame(geometry = dorsalis_sf, Subspecies = "dorsalis"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# aikeni x caniceps PCA area overlap calculation + plot

# Extract the PC points for each subspecies
caniceps_pca <- song_pc_df %>%
  filter(Subspecies == "caniceps") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "caniceps")

aikeni_pca <- song_pc_df %>%
  filter(Subspecies == "aikeni") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "aikeni")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
caniceps_spdf <- SpatialPointsDataFrame(caniceps_pca[, c("PC1", "PC2")], caniceps_pca)
aikeni_spdf <- SpatialPointsDataFrame(aikeni_pca[, c("PC1", "PC2")], aikeni_pca)

# Estimate the convex hull polygons for each subspecies
caniceps_hull <- gConvexHull(caniceps_spdf)
aikeni_hull <- gConvexHull(aikeni_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(caniceps_hull, aikeni_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 19.68238

caniceps_area <- gArea(caniceps_hull)
caniceps_area # 36.21917

aikeni_area <- gArea(aikeni_hull)
aikeni_area # 21.10826

pca_overlap_area_proportion <- intersection_area / (caniceps_area + (aikeni_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.5228411

# Convert the polygons to sf objects
caniceps_sf <- st_as_sf(caniceps_hull)
aikeni_sf <- st_as_sf(aikeni_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = caniceps_sf, Subspecies = "caniceps"),
  data.frame(geometry = aikeni_sf, Subspecies = "aikeni"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# aikeni x dorsalis PCA area overlap calculation + plot

# Extract the PC points for each subspecies
dorsalis_pca <- song_pc_df %>%
  filter(Subspecies == "dorsalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "dorsalis")

aikeni_pca <- song_pc_df %>%
  filter(Subspecies == "aikeni") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "aikeni")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
dorsalis_spdf <- SpatialPointsDataFrame(dorsalis_pca[, c("PC1", "PC2")], dorsalis_pca)
aikeni_spdf <- SpatialPointsDataFrame(aikeni_pca[, c("PC1", "PC2")], aikeni_pca)

# Estimate the convex hull polygons for each subspecies
dorsalis_hull <- gConvexHull(dorsalis_spdf)
aikeni_hull <- gConvexHull(aikeni_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(dorsalis_hull, aikeni_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 13.09831

dorsalis_area <- gArea(dorsalis_hull)
dorsalis_area # 26.93136

aikeni_area <- gArea(aikeni_hull)
aikeni_area # 21.10826

pca_overlap_area_proportion <- intersection_area / (dorsalis_area + (aikeni_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.3748659

# Convert the polygons to sf objects
dorsalis_sf <- st_as_sf(dorsalis_hull)
aikeni_sf <- st_as_sf(aikeni_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = dorsalis_sf, Subspecies = "dorsalis"),
  data.frame(geometry = aikeni_sf, Subspecies = "aikeni"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()

# dorsalis x mearnsi PCA area overlap calculation + plot

# Extract the PC points for each subspecies
dorsalis_pca <- song_pc_df %>%
  filter(Subspecies == "dorsalis") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "dorsalis")

mearnsi_pca <- song_pc_df %>%
  filter(Subspecies == "mearnsi") %>%
  select(PC1, PC2) %>%
  mutate(Subspecies = "mearnsi")

# Create SpatialPointsDataFrame objects from the subspecies coordinates
dorsalis_spdf <- SpatialPointsDataFrame(dorsalis_pca[, c("PC1", "PC2")], dorsalis_pca)
mearnsi_spdf <- SpatialPointsDataFrame(mearnsi_pca[, c("PC1", "PC2")], mearnsi_pca)

# Estimate the convex hull polygons for each subspecies
dorsalis_hull <- gConvexHull(dorsalis_spdf)
mearnsi_hull <- gConvexHull(mearnsi_spdf)

# Calculate the intersection between the two polygons
intersection_hull <- gIntersection(dorsalis_hull, mearnsi_hull, byid = TRUE)

# Calculate the areas of intersection, of each parent subspecies, and PCA overlap proportion
intersection_area <- gArea(intersection_hull)
intersection_area # 16.15656

dorsalis_area <- gArea(dorsalis_hull)
dorsalis_area # 26.93136

mearnsi_area <- gArea(mearnsi_hull)
mearnsi_area # 31.58175

pca_overlap_area_proportion <- intersection_area / (dorsalis_area + (mearnsi_area - intersection_area)) # intersection area divided by the union of the two subspecies pca areas
pca_overlap_area_proportion # 0.3814418

# Convert the polygons to sf objects
dorsalis_sf <- st_as_sf(dorsalis_hull)
mearnsi_sf <- st_as_sf(mearnsi_hull)
intersection_sf <- st_as_sf(intersection_hull)

# Create a data frame for polygons and area of overlap
polygons_df <- rbind(
  data.frame(geometry = dorsalis_sf, Subspecies = "dorsalis"),
  data.frame(geometry = mearnsi_sf, Subspecies = "mearnsi"),
  data.frame(geometry = intersection_sf, Subspecies = "overlap")
)

# Plot the polygons and area of overlap
ggplot() +
  geom_sf(data = polygons_df, aes(fill = Subspecies, geometry = geometry), color = "black", alpha = 0.5) +
  theme_bw()