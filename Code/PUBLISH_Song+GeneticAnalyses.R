# Principal component analysis (PCA) for song data
library(scales)

JuncoDataLog <- read.csv(file="/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongData_Subspecies_and_ID.csv")
juncos.pca <- prcomp(JuncoDataLog[,c(2:11)], center = TRUE, scale = TRUE)
summary(juncos.pca)
plot(juncos.pca$x[,1],juncos.pca$x[,2], xlab = "PC1", ylab = "PC2", xlim = c(-6, 7), ylim = c(-5, 4.5), col = "black", bg=c("green4", "gold", "darkorchid4", "palevioletred1", "firebrick3", "royalblue")[as.factor(JuncoDataLog$subspecies)], pch=21, cex=1.5)

# Linear discriminant analysis (LDA) for song data
library("klaR")
junco_dat_all <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongData_Subspecies_and_ID.csv")
junco_dat <- subset(junco_dat_all, junco_dat_all$subspecies != "unknown")
junco_lda_results <- c(rep(0,3))

for (i in 1:10){
  feature_1 <- junco_dat[,i+1]
  feature_1_name <- colnames(junco_dat)[i+1]
  for (j in 1:10){
    feature_2 <- junco_dat[,j+1]
    feature_2_name <- colnames(junco_dat)[j+1]
    test <- stepclass(cbind(feature_1, feature_2), grouping = junco_dat$subspecies, method = "lda",direction = "forward",criterion="AS",improvement=0.01,fold=13)
    junco_lda_results <-  rbind(junco_lda_results, c(feature_1_name, feature_2_name,test[["result.pm"]][["crossval.rate"]]))
  }
}

# "Best" features for separation are avg silence duration and bout duration. Ability to separate: 0.34879
plot(junco_dat$bout_duration.ms., junco_dat$avg_silence_duration.ms., pch = 21, col = "black", bg = c("green4", "gold", "darkorchid4", "palevioletred1", "firebrick3", "royalblue")[as.factor(junco_dat$subspecies)], xlab = "Avg. syllable duration", ylab = "Avg. silence duration", las = 1, cex = 1.5)

# Principal component analysis (PCA) for COI genetic data
library(adegenet)
library(ggplot2)
library(tidyverse)

COImetadata <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_COImetadata.csv")
COIsnp <- fasta2genlight("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_COI_AlignedSequences.fasta", snpOnly=TRUE)

COIpca <- glPca(COIsnp, nf=10)
COIpca.dataset = as.data.frame(COIpca$scores)
COIpca.dataset$isolates = row.names(COIpca.dataset)
COIpca.dataset$spp = as.factor(COImetadata[match(COIsnp$ind.names, COImetadata$Locus),]$Subspecies_Friis)

plotting_shape <- c("hyemalis" = 16, "oreganus" = 17, "aikeni" = 7, "caniceps" = 15, "dorsalis" = 18, "mearnsi" = 8)
plotting_colors <- c("hyemalis" = "palevioletred1", "oreganus" = "royalblue", "aikeni" = "green4", "caniceps" = "gold", "dorsalis" = "darkorchid4", "mearnsi" = "firebrick3")

ggplot(COIpca.dataset, aes(PC1, PC2, color=spp, shape=spp)) + 
  geom_point(size=12, alpha = 0.7) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

# Principal component analysis (PCA) for ATP genetic data
ATPmetadata <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ATPmetadata.csv")
ATPsnp <- fasta2genlight("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ATP_AlignedSequences.fasta", snpOnly=TRUE)

ATPpca <- glPca(ATPsnp, nf=10)
ATPpca.dataset = as.data.frame(ATPpca$scores)
ATPpca.dataset$isolates = row.names(ATPpca.dataset)
ATPpca.dataset$spp = as.factor(ATPmetadata[match(ATPsnp$ind.names, ATPmetadata$Locus),]$Subspecies_Friis)

ggplot(ATPpca.dataset, aes(PC1, PC2, color=spp, shape=spp)) + 
  geom_point(size=12, alpha = 0.7) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

# Principal component analysis (PCA) for FGB genetic data
FGBmetadata <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_FGBmetadata.csv")
FGBsnp <- fasta2genlight("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_FGB_AlignedSequences.fasta", snpOnly=TRUE)

FGBpca <- glPca(FGBsnp, nf=10)
FGBpca.dataset = as.data.frame(FGBpca$scores)
FGBpca.dataset$isolates = row.names(FGBpca.dataset)
FGBpca.dataset$spp = as.factor(FGBmetadata[match(FGBsnp$ind.names, FGBmetadata$Locus),]$Subspecies_Friis)

ggplot(FGBpca.dataset, aes(PC1, PC2, color=spp, shape=spp)) + 
  geom_point(size=12, alpha = 0.7) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

# Principal component analysis (PCA) for ND2 genetic data
ND2metadata <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ND2metadata.csv")
ND2snp <- fasta2genlight("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ND2_AlignedSequences.fasta", snpOnly=TRUE)

ND2pca <- glPca(ND2snp, nf=10)
ND2pca.dataset = as.data.frame(ND2pca$scores)
ND2pca.dataset$isolates = row.names(ND2pca.dataset)
ND2pca.dataset$spp = as.factor(ND2metadata[match(ND2snp$ind.names, ND2metadata$Locus),]$Subspecies_Friis)

ggplot(ND2pca.dataset, aes(PC1, PC2, color=spp, shape=spp)) + 
  geom_point(size=12, alpha = 0.7) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

# Principal component analysis (PCA) for CR genetic data
CRmetadata <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_CRmetadata.csv")
CRsnp <- fasta2genlight("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_CR_AlignedSequences.fasta", snpOnly=TRUE)

CRpca <- glPca(CRsnp, nf=10)
CRpca.dataset = as.data.frame(CRpca$scores)
CRpca.dataset$isolates = row.names(CRpca.dataset)
CRpca.dataset$spp = as.factor(CRmetadata[match(CRsnp$ind.names, CRmetadata$Locus),]$Subspecies_Friis)

ggplot(CRpca.dataset, aes(PC1, PC2, color=spp, shape=spp)) + 
  geom_point(size=12, alpha = 0.7) +
  scale_color_manual(values = plotting_colors) +
  scale_shape_manual(values = plotting_shape) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16))

# Principal component analysis (PCA) for concatenated Friis data
ConcFriisMetadata <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ConcGeneticMetadata.csv")
ConcFriissnp <- fasta2genlight("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ConcGenetic_AlignedSequences.fasta", snpOnly=TRUE)

ConcFriispca <- glPca(ConcFriissnp, nf=10)
ConcFriispca.dataset = as.data.frame(ConcFriispca$scores)
ConcFriispca.dataset$isolates = row.names(ConcFriispca.dataset)
ConcFriispca.dataset$spp = as.factor(ConcFriisMetadata[match(ConcFriissnp$ind.names, ConcFriisMetadata$Isolate),]$Subspecies_Friis)

ConcFriispca.dataset %>%
  drop_na(spp) %>%
  ggplot(aes(PC1, PC2, color=spp)) + 
  geom_point(shape=19, size=10, alpha=0.9) +
  scale_color_manual(values = c("hyemalis" = "palevioletred1",
                                "oreganus" = "royalblue",
                                "aikeni" = "green4",
                                "caniceps" = "gold",
                                "dorsalis" = "darkorchid4",
                                "mearnsi" = "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")

# Procrustes 
library(vegan)

# Procrustes with COI genetic data and lat/long w/ procrustes resampling using 'protest' command
COIlatlong <- data.frame(COImetadata$Longitude, COImetadata$Latitude)
COIpc1pc2 <- data.frame(COIpca.dataset$PC1, COIpca.dataset$PC2)
COIprocrustes <- procrustes(COIpc1pc2, COIlatlong)
plot(COIprocrustes)
protest(COIpc1pc2, COIlatlong, permutations = 10000)

# Procrustes with ATP genetic data and lat/long w/ procrustes resampling using 'protest' command
ATPlatlong <- data.frame(ATPmetadata$Longitude, ATPmetadata$Latitude)
ATPpc1pc2 <- data.frame(ATPpca.dataset$PC1, ATPpca.dataset$PC2)
ATPprocrustes <- procrustes(ATPpc1pc2, ATPlatlong)
plot(ATPprocrustes)
protest(ATPpc1pc2, ATPlatlong, permutations = 10000)

# Procrustes with FGB genetic data and lat/long w/ procrustes resampling using 'protest' command
FGBlatlong <- data.frame(FGBmetadata$Longitude, FGBmetadata$Latitude)
FGBpc1pc2 <- data.frame(FGBpca.dataset$PC1, FGBpca.dataset$PC2)
FGBprocrustes <- procrustes(FGBpc1pc2, FGBlatlong)
plot(FGBprocrustes)
protest(FGBpc1pc2, FGBlatlong, permutations = 10000)

# Procrustes with ND2 genetic data and lat/long w/ procrustes resampling using 'protest' command
ND2latlong <- data.frame(ND2metadata$Longitude, ND2metadata$Latitude)
ND2pc1pc2 <- data.frame(ND2pca.dataset$PC1, ND2pca.dataset$PC2)
ND2procrustes <- procrustes(ND2pc1pc2, ND2latlong)
plot(ND2procrustes)
protest(ND2pc1pc2, ND2latlong, permutations = 10000)

# Procrustes with CR genetic data and lat/long w/ procrustes resampling using 'protest' command
CRlatlong <- data.frame(CRmetadata$Longitude, CRmetadata$Latitude)
CRpc1pc2 <- data.frame(CRpca.dataset$PC1, CRpca.dataset$PC2)
CRprocrustes <- procrustes(CRpc1pc2, CRlatlong)
plot(CRprocrustes)
protest(CRpc1pc2, CRlatlong, permutations = 10000)

# Procrustes with concatenated genetic data and lat/long w/ procrustes resampling using 'protest' command
ConcFriislatlong <- data.frame(ConcFriisMetadata$Longitude, ConcFriisMetadata$Latitude)
ConcFriispc1pc2 <- data.frame(ConcFriispca.dataset$PC1, ConcFriispca.dataset$PC2)
ConcFriisprocrustes <- procrustes(ConcFriispc1pc2, ConcFriislatlong)
plot(ConcFriisprocrustes)
protest(ConcFriispc1pc2, ConcFriislatlong, permutations = 10000)

# Procrustes with song PCs and lat/long w/ procrustes resampling using 'protest' command
JuncoSongLatLongs <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_JuncoSongLatLongs.csv")
SongLatLongs <- data.frame(JuncoSongLatLongs$longitude, JuncoSongLatLongs$latitude)
SongPC1PC2 <- data.frame(juncos.pca$x[,1], juncos.pca$x[,2])
SongxLatLongProcrustes <- procrustes(SongLatLongs, SongPC1PC2)
plot(SongxLatLongProcrustes)
protest(SongPC1PC2, SongLatLongs, permutations = 10000)

# Procrustes with song PCs and genetic PCs w/ procrustes resampling using 'protest' command
# For song PC1 and PC2, find the median values for each subspecies, which were entered into 'songPCmedians.csv'
median(juncos.pca$x[,1][which(JuncoSongLatLongs$subspecies=="hyemalis")])
median(juncos.pca$x[,1][which(JuncoSongLatLongs$subspecies=="oreganus")])
median(juncos.pca$x[,1][which(JuncoSongLatLongs$subspecies=="aikeni")])
median(juncos.pca$x[,1][which(JuncoSongLatLongs$subspecies=="caniceps")])
median(juncos.pca$x[,1][which(JuncoSongLatLongs$subspecies=="dorsalis")])
median(juncos.pca$x[,1][which(JuncoSongLatLongs$subspecies=="mearnsi")])

median(juncos.pca$x[,2][which(JuncoSongLatLongs$subspecies=="hyemalis")])
median(juncos.pca$x[,2][which(JuncoSongLatLongs$subspecies=="oreganus")])
median(juncos.pca$x[,2][which(JuncoSongLatLongs$subspecies=="aikeni")])
median(juncos.pca$x[,2][which(JuncoSongLatLongs$subspecies=="caniceps")])
median(juncos.pca$x[,2][which(JuncoSongLatLongs$subspecies=="dorsalis")])
median(juncos.pca$x[,2][which(JuncoSongLatLongs$subspecies=="mearnsi")])

# For concatenated genetic PC1 and PC2, find the median values for each subspecies, which were entered into 'ConcFriisPCmedians.csv
median(ConcFriispca.dataset$PC1[which(ConcFriisMetadata$Subspecies_Friis=="hyemalis")])
median(ConcFriispca.dataset$PC1[which(ConcFriisMetadata$Subspecies_Friis=="oreganus")])
median(ConcFriispca.dataset$PC1[which(ConcFriisMetadata$Subspecies_Friis=="aikeni")])
median(ConcFriispca.dataset$PC1[which(ConcFriisMetadata$Subspecies_Friis=="caniceps")])
median(ConcFriispca.dataset$PC1[which(ConcFriisMetadata$Subspecies_Friis=="dorsalis")])
median(ConcFriispca.dataset$PC1[which(ConcFriisMetadata$Subspecies_Friis=="mearnsi")])

median(ConcFriispca.dataset$PC2[which(ConcFriisMetadata$Subspecies_Friis=="hyemalis")])
median(ConcFriispca.dataset$PC2[which(ConcFriisMetadata$Subspecies_Friis=="oreganus")])
median(ConcFriispca.dataset$PC2[which(ConcFriisMetadata$Subspecies_Friis=="aikeni")])
median(ConcFriispca.dataset$PC2[which(ConcFriisMetadata$Subspecies_Friis=="caniceps")])
median(ConcFriispca.dataset$PC2[which(ConcFriisMetadata$Subspecies_Friis=="dorsalis")])
median(ConcFriispca.dataset$PC2[which(ConcFriisMetadata$Subspecies_Friis=="mearnsi")])
                        
songPCmedians <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_SongPC_Medians.csv")
songmedianPC1PC2 <- data.frame(songPCmedians$PC1medians, songPCmedians$PC2medians)
ConcFriisPCmedians <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ConcGenetic_PC_Medians.csv")
ConcFriismedianPC1PC2 <- data.frame(ConcFriisPCmedians$PC1medians, ConcFriisPCmedians$PC2medians)
SongxGeneticProcrustes <- procrustes(songmedianPC1PC2, ConcFriismedianPC1PC2)           
plot(SongxGeneticProcrustes)
protest(songmedianPC1PC2, ConcFriismedianPC1PC2, permutations = 10000)

# For concatenated genetic data, does pairwise Fst significantly vary between subspecies pairs that hybridize and don't hybridize?
ConcFriisPairwiseFst <- read.csv("/Users/sarahhourihan/Library/CloudStorage/Box-Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_ConcDataPairwiseFst.csv")

require(tidyverse)
boxplot_colors <- c("Non-adjacent" = "#bebada", "Hybridizing" = "#8dd3c7", "Adjacent non-hybridizing" = "#ffffb3")
ggplot(ConcFriisPairwiseFst, aes(x = Group, y = Pairwise.Fst, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot_colors) +
  labs(y = "Pairwise Fst",
       fill = "Hybrid Status") +
  theme_bw()
