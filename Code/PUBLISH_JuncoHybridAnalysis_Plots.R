### Plots to test the relationship between hybrid ratios, areas of overlap, and song difference metrics
library(tidyverse)
hybrid.and.song.data <- read.csv("/Users/sarahhourihan/Box/Creanza Lab/Sarah_Nicole/files_for_manuscript/PUBLISH_Data_for_JuncoHybridAnalysis.csv")

plotting_colors <- c("hyemalis/oreganus" = "green4", "mearnsi/caniceps" = "green4", "mearnsi/aikeni" = "green4", "caniceps/dorsalis" = "green4", "oreganus/caniceps" = "green4", "oreganus/mearnsi" = "green4", "hyemalis/aikeni" = "slategray", "hyemalis/caniceps" = "slategray", "hyemalis/dorsalis" = "slategray", "hyemalis/mearnsi" = "slategray", "oreganus/aikeni" = "slategray", "oreganus/dorsalis" = "slategray", "aikeni/caniceps" = "slategray", "aikeni/dorsalis" = "slategray", "dorsalis/mearnsi" = "slategray")

# Area of overlap vs. ML classifier accuracy
ggplot(hybrid.and.song.data, aes(Area.of.overlap..sq.km., Avg.classifier.accuracy, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Area of overlap (km^2)", 
       y = "Avg. ML classifier accuracy") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Area of overlap vs. number of significantly different song features
ggplot(hybrid.and.song.data, aes(Area.of.overlap..sq.km., Num.sig.song.features, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Area of overlap (km^2)", 
       y = "Number of significant song features") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Area of overlap vs. song PCA overlap proportion
ggplot(hybrid.and.song.data, aes(Area.of.overlap..sq.km., Proportion.PCA.overlap, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Area of overlap (km^2)", 
       y = "Song PCA overlap proportion") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Hybrid:parent ratio in hybrid zone vs. ML classifier accuracy
ggplot(hybrid.and.song.data, aes(Num.hybrids.num.parents.in.hybrid.region, Avg.classifier.accuracy, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Hybrid:parent ratio in hybrid zone", 
       y = "Avg. ML classifier accuracy") +
  geom_smooth(method = "lm", se = F) +
  xlim(NA, 0.006) + # one hybrid:parent ratio was way higher than the rest
  theme_bw()

# Hybrid:parent ratio in hybrid zone vs. number of significantly different song features
ggplot(hybrid.and.song.data, aes(Num.hybrids.num.parents.in.hybrid.region, Num.sig.song.features, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Hybrid:parent ratio in hybrid zone", 
       y = "Number of significant song features") +
  geom_smooth(method = "lm", se = F) +
  xlim(NA, 0.006) + # one hybrid:parent ratio was way higher than the rest
  theme_bw()

# Hybrid:parent ratio in hybrid zone vs. song PCA overlap proportion
ggplot(hybrid.and.song.data, aes(Num.hybrids.num.parents.in.hybrid.region, Proportion.PCA.overlap, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Hybrid:parent ratio in hybrid zone", 
       y = "Song PCA overlap proportion") +
  geom_smooth(method = "lm", se = F) +
  xlim(NA, 0.006) + # one hybrid:parent ratio was way higher than the rest
  theme_bw()

# Area of overlap vs. hybrid:parent ratio in hybrid zone
ggplot(hybrid.and.song.data, aes(Area.of.overlap..sq.km., Num.hybrids.num.parents.in.hybrid.region, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Area of overlap (km^2)", 
       y = "Hybrid:parent ratio in hybrid zone") +
  geom_smooth(method = "lm", se = F) +
  ylim(NA, 0.006) + # one hybrid:parent ratio was way higher than the rest
  theme_bw()

# Area of overlap vs. hybrid:parent ratio in overlap area
ggplot(hybrid.and.song.data, aes(Area.of.overlap..sq.km., Num.hybrids.num.parents.sightings.in.area.of.overlap, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Area of overlap (km^2)", 
       y = "Hybrid:parent ratio in area of overlap") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Hybrid:parent ratio in overlap area vs. ML classifier accuracy
ggplot(hybrid.and.song.data, aes(Num.hybrids.num.parents.sightings.in.area.of.overlap, Avg.classifier.accuracy, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Hybrid:parent ratio in area of overlap", 
       y = "Avg. ML classifier accuracy") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Hybrid:parent ratio in overlap area vs. number of significantly different song features
ggplot(hybrid.and.song.data, aes(Num.hybrids.num.parents.sightings.in.area.of.overlap, Num.sig.song.features, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Hybrid:parent ratio in area of overlap", 
       y = "Number of significant song features") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Hybrid:parent ratio in overlap area vs. song PCA overlap proportion
ggplot(hybrid.and.song.data, aes(Num.hybrids.num.parents.sightings.in.area.of.overlap, Proportion.PCA.overlap, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Hybrid:parent ratio in area of overlap",
       y = "Song PCA overlap proportion") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Realized potential overlap area vs. ML classifier accuracy
ggplot(hybrid.and.song.data, aes(Realized.potential.overlap.area, Avg.classifier.accuracy, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Realized potential overlap area", 
       y = "Avg. ML classifier accuracy") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Realized potential overlap area vs. number of significantly different song features
ggplot(hybrid.and.song.data, aes(Realized.potential.overlap.area, Num.sig.song.features, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Realized potential overlap area", 
       y = "Number of significant song features") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Realized potential overlap area vs. song PCA overlap proportion
ggplot(hybrid.and.song.data, aes(Realized.potential.overlap.area, Proportion.PCA.overlap, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Realized potential overlap area", 
       y = "Song PCA overlap proportion") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Realized potential overlap area vs. hybrid:parent ratio in hybrid zone
ggplot(hybrid.and.song.data, aes(Realized.potential.overlap.area, Num.hybrids.num.parents.in.hybrid.region, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Realized potential overlap area", 
       y = "Hybrid:parent ratio in hybrid zone") +
  geom_smooth(method = "lm", se = F) +
  ylim(NA, 0.006) + # one hybrid:parent ratio was way higher than the rest
  theme_bw() 

# Realized potential overlap area vs. hybrid:parent ratio in overlap area
ggplot(hybrid.and.song.data, aes(Realized.potential.overlap.area, Num.hybrids.num.parents.sightings.in.area.of.overlap, color = Subspecies.pair)) + 
  geom_point(shape=19, size=8) +
  scale_color_manual(values = plotting_colors) +
  labs(x = "Realized potential overlap area", 
       y = "Hybrid:parent ratio in area of overlap") +
  geom_smooth(method = "lm", se = F) +
  theme_bw()