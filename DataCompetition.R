library(ggplot2)
library(hexbin)
library(corrplot)
library(dplyr)

thads <- read.csv(file="/home/william/Downloads/thads2011.txt")
thads_cleaned <- read.csv(file="/home/william/Downloads/thads2011.txt", na.strings=c("-6","'-6'", ".", "-9"))

# scatter plot with colors
ggplot(data=thads_cleaned, aes(x=ZSMHC, y=COSTMedRELAMIPCT, color=STRUCTURETYPE)) + geom_point(size=1, alpha=.01)

# density plot
ggplot(data=thads_cleaned, aes(x=ZSMHC, y=COSTMedRELAMIPCT)) + stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  labs(x = "Monthly Housing Cost", y="Median Mortgage Rate Housing Costs Relative to Median Income")

#head(thads)
#View(head(thads, 50))

# correlation matrix of all the numeric variables
numeric <- select_if(thads_cleaned, is.numeric)
cor_matrix <- cor(numeric, use="pairwise.complete.obs")
corrplot(cor_matrix)

# correleation matrix column for just COSTMedRELAMIPCT
corrplot(cor_matrix[,62, drop=FALSE], cl.pos='n')