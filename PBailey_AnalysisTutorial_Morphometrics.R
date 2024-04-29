# R script for GPA comparison

# load libraries
library(geomorph)

data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = F)    # GPA-alignment

par(mfrow=c(1,2)) 
plotAllSpecimens(plethodon$land, links=plethodon$links)  # Raw data
mtext("Raw Data")
plotAllSpecimens(Y.gpa$coords, links=plethodon$links)    # GPA-aligned data
mtext("GPA-Aligned Specimens")

# R script for general morphometric analysis

# load libraries
library(tidyverse)
library(abind)
library(geomorph)

# set working directory
setwd("~/Baylor/Spring 2024/Research Methods in Biology II (Bio 5202)/AnalysisTutorial") #replace with extension that leads to directory containing morphometric output data and classifiers

# import data
df <- read.csv("OutputData.csv")
classifier <- read.csv("Classifiers.csv")

# tidy data - want one row per sample x landmark, 1 column per coordinate
long_df = df %>% pivot_longer(cols =4:ncol(df), 
                              names_to =c('landmark','axis'), 
                              names_sep = '_', 
                              values_to = 'value') %>%
  pivot_wider(names_from = 'axis', values_from = 'value')

# abind package: combine multi-dimensional arrays based on sample name
df_3d = abind(split(long_df[,c(5,6,7)],long_df$Sample_name), along = 3)

# check that classifiers remain aligned
names(df_3d[1,1,])

# Reorder 'classifier' based on the order of sample names in 'df_3d'
sorted_classifier <- classifier[order(classifier$Sample_name),]
names(classifier)

# View the updated data frame
View(sorted_classifier)

# Check the current class of Group and Species
class(sorted_classifier$Group)
class(sorted_classifier$Species)
class(sorted_classifier$centeroid)

# Convert Group and Species to factors
Sorted_Group <- as.factor(sorted_classifier$Group)
Sorted_Species <- as.factor(sorted_classifier$Species)
Sorted_centeroid <- as.factor(sorted_classifier$centeroid)
# Check if they are now factors
is.factor(Sorted_Group)
is.factor(Sorted_Species)
is.factor(Sorted_centeroid)

#perform gpa 
skull_data <- gpagen(df_3d)

plot(skull_data)
plot(df_3d)

# Perform PCA on the superimposed data
pca_result <- gm.prcomp(skull_data$coords)

#View the plot
PCA2<-plot(pca_result, col=Sorted_Species)

# Check the levels of the Species factor
levels(Sorted_Species)#try to reorder these levels in the legend so that they match the appropriate colors in the graph
# Reassign or reorder levels if necessary
legend_Species <- factor(Sorted_Species, levels = c("Coyote (W)", "Hybrid (NE)", "Hybrid (GCC)", "Coyote (SE)", "Red Wolf"))  # Replace with your actual levels

# Plot the PCA results with custom colors
PCA2 <- plot(pca_result, col = legend_Species, pch=16, cex= 2)

# Define levels and colors for the legend based on the reassigned levels
legend_levels <- levels(legend_Species)
legend_colors <- unique(legend_Species)

# Create a custom legend
legend("topright", legend = legend_levels, col = legend_colors, pch = 16, cex=1, title = "Canis Group")


shapeHulls(PCA2, groups = Sorted_Group, group.cols = c("black","red","green", "blue", "lightblue")) #Need to make the colors for the hulls and for the circles match
#END PCA































