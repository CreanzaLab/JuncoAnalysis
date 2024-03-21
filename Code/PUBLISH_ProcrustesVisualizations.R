library(adegenet)
library(tidyr)
library(tidyverse)
library(ggplot2)

## Figure mapping genetic x geographic Procrustes to map
## PCA for Concatenated Friis genetic data
genetic_pca_fig <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ConcGeneticData_PCA.csv")

plotting_shape <- c("hyemalis" = 16, "oreganus" = 17, "aikeni" = 7, "caniceps" = 15, "dorsalis" = 18, "mearnsi" = 8)
plotting_colors <- c("hyemalis" = "palevioletred1", "oreganus" = "royalblue", "aikeni" = "green4", "caniceps" = "gold", "dorsalis" = "darkorchid4", "mearnsi" = "firebrick3")

genetic_pca_fig <- genetic_pca_fig %>%
  drop_na(spp) %>%
  ggplot(aes(x = PC1, y = PC2, colour = spp, shape = spp)) +
  geom_point(size=5, alpha=0.6) +
  ylim(NA, 0.05) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  theme_bw() +
  theme(legend.position = "none")
genetic_pca_fig

## Procrustes
library(readr)
library(ggplot2)
library(maps)
library(MCMCpack)

genetic.pca.dataset_withLatLong <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ConcGeneticData_PCA_withLatLong.csv")

genetic.pca.dataset_withLatLong <- genetic.pca.dataset_withLatLong %>%
  drop_na(spp)

PCA_junco_genetics_data <- subset(genetic.pca.dataset_withLatLong, select = c(Latitude, Longitude, spp))

PCA_junco_genetics_results_PC1andPC2 <- subset(genetic.pca.dataset_withLatLong, select = c(PC1, PC2))

genetic_coord_matrix<-as.matrix(cbind(PCA_junco_genetics_data$Longitude,PCA_junco_genetics_data$Latitude)) # Change data frame to matrix for Procrustes

genetic_PCA_matrix <- as.matrix(cbind(PCA_junco_genetics_results_PC1andPC2$PC1,PCA_junco_genetics_results_PC1andPC2$PC2)) # Change data frame to matrix for Procrustes

genetic_pro<-procrustes(genetic_PCA_matrix,genetic_coord_matrix,translation=TRUE,dilation=TRUE) # Procrustes with genetic PCA matrix and coordinate matrix, allowing translation and dilation

genetic_pro <- genetic_pro$X.new

genetic_pro <- data.frame(genetic_pro) # Switching back from matrix to dataframe

procrustes_genetics_final <- cbind(genetic_pro, PCA_junco_genetics_data$spp) # binding procrustes results to subspecies

colnames(procrustes_genetics_final) <- c('Longitude', 'Latitude', 'spp')

## Mapping
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")

genetic_procrustes_ggplot <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_point(data = procrustes_genetics_final, aes(x = Longitude, y = Latitude, colour = spp, shape = spp), size = 5, alpha = 0.6) +
  coord_sf(xlim = c(-130, -90), ylim = c(39, 75), expand = FALSE) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  theme_bw() +
  theme(legend.position = "none")
genetic_procrustes_ggplot

genetic_latlong_ggplot <- ggplot() +
  geom_sf(data = world, fill = "white") +
  geom_point(data = genetic.pca.dataset_withLatLong, aes(x = Longitude, y = Latitude, colour = spp, shape = spp), size = 5, alpha = 0.6) +
  coord_sf(xlim = c(-160, -70), ylim = c(25, 70), expand = FALSE) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  theme_bw() +
  theme(legend.position = "none")
genetic_latlong_ggplot

## Figure presentation
library(ggpubr)

genetic_figure <- ggarrange(genetic_pca_fig,
                    ggarrange(genetic_latlong_ggplot, genetic_procrustes_ggplot, nrow = 2, labels = c('B', 'C')),
                    ncol = 2,
                    labels = 'A',
                    common.legend = TRUE,
                    legend = "right"
                    )
genetic_figure

## Figure mapping song x geographic Procrustes to map
## PCA for song data
JuncoDataLog=read.csv(file="/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongData_Subspecies_and_ID.csv",sep=",")
song.latlong <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongLatLongs.csv")
song_pca <- prcomp(JuncoDataLog[,2:11], scale. = TRUE)
song_pc <- song_pca$x
song_pc_df <- data.frame(PC1 = song_pc[, 1], PC2 = song_pc[, 2], Subspecies = JuncoDataLog$subspecies, IDNum = JuncoDataLog$ID_Num, Latitude = song.latlong$latitude, Longitude = song.latlong$longitude)
song_pca_fig <- song_pc_df %>%
  drop_na(Subspecies) %>%
  ggplot(aes(x = PC1, y = PC2, color = Subspecies)) +
  geom_point() +
  labs(x = "PC1", y = "PC2") +
  scale_color_manual(values = c("hyemalis" = "palevioletred1",
                                "oreganus" = "royalblue",
                                "aikeni" = "green4",
                                "caniceps" = "gold",
                                "dorsalis" = "darkorchid4",
                                "mearnsi" = "firebrick3")) +
  xlim(NA, 7.5) +
  theme_bw() +
  theme(legend.position = "none")
song_pca_fig

## Procrustes
song.pca.dataset_withLatLong <- song_pc_df %>%
  drop_na('Subspecies')

PCA_junco_song_data <- subset(song.pca.dataset_withLatLong, select = c(Latitude, Longitude, Subspecies))

PCA_junco_song_results_PC1andPC2 <- subset(song.pca.dataset_withLatLong, select = c(PC1, PC2))

song_coord_matrix<-as.matrix(cbind(PCA_junco_song_data$Longitude,PCA_junco_song_data$Latitude)) # Change data frame to matrix for Procrustes

song_PCA_matrix <- as.matrix(cbind(PCA_junco_song_results_PC1andPC2$PC1,PCA_junco_song_results_PC1andPC2$PC2)) # Change data frame to matrix for Procrustes

song_pro<-MCMCpack::procrustes(song_PCA_matrix,song_coord_matrix,translation=TRUE,dilation=TRUE) # Procrustes with genetic PCA matrix and coordinate matrix, allowing translation and dilation

song_pro <- song_pro$X.new

song_pro <- data.frame(song_pro) # Switching back from matrix to dataframe

procrustes_song_final <- cbind(song_pro, PCA_junco_song_data$Subspecies) # binding procrustes results to subspecies

colnames(procrustes_song_final) <- c('Longitude', 'Latitude', 'Subspecies')

## Mapping
world <- ne_countries(scale = "medium", returnclass = "sf")

song_procrustes_ggplot <- ggplot() + #plot (:
  geom_sf(data = world, fill = "white") +
  geom_point(data = procrustes_song_final, aes(x = Longitude, y = Latitude, colour = Subspecies), size = 2, alpha = 0.8) +
  coord_sf(xlim = c(-115, -85), ylim = c(31, 55), expand = FALSE) +
  scale_color_manual(values = c("hyemalis" = "palevioletred1",
                                "oreganus" = "royalblue",
                                "aikeni" = "green4",
                                "caniceps" = "gold",
                                "dorsalis" = "darkorchid4",
                                "mearnsi" = "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")
song_procrustes_ggplot

song_latlong_ggplot <- ggplot() + #plot (:
  geom_sf(data = world, fill = "white") +
  geom_point(data = song.pca.dataset_withLatLong, aes(x = Longitude, y = Latitude, colour = Subspecies), size = 0.75, alpha = 0.8) +
  coord_sf(xlim = c(-165, -50), ylim = c(29, 69), expand = FALSE) +
  scale_color_manual(values = c("hyemalis" = "palevioletred1",
                                "oreganus" = "royalblue",
                                "aikeni" = "green4",
                                "caniceps" = "gold",
                                "dorsalis" = "darkorchid4",
                                "mearnsi" = "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")
song_latlong_ggplot

## Figure presentation
song_figure <- ggarrange(song_pca_fig,
                    ggarrange(song_latlong_ggplot, song_procrustes_ggplot, nrow = 2, labels = c('B', 'C')),
                    ncol = 2,
                    labels = 'A',
                    common.legend = TRUE
)
song_figure
