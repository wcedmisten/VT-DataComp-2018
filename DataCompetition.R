library(ggplot2)
library(hexbin)
library(corrplot)
library(dplyr)

#thads <- read.csv(file="C:/Users/Daniel/OneDrive/Documents/Virginia Tech/Senior Year/Fall 2018/CMDA 3654/Competition/thads2011.txt")
#thads_cleaned <- read.csv(file="C:/Users/Daniel/OneDrive/Documents/Virginia Tech/Senior Year/Fall 2018/CMDA 3654/Competition/thads2011.txt", na.strings=c("-6","'-6'", ".", "-9"))

thads <- read.csv(file="/home/william/Downloads/thads2011.txt")
thads_cleaned <- read.csv(file="/home/william/Downloads/thads2011.txt", na.strings=c("-6","'-6'", ".", "-9"))

# scatter plot with colors
ggplot(data=thads_cleaned, aes(x=ZSMHC, y=COSTMedRELAMIPCT, color=STRUCTURETYPE)) + geom_point(size=1)

# density plot
ggplot(data=thads_cleaned, aes(x=ZSMHC, y=COSTMedRELAMIPCT)) + stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  labs(x = "Monthly Housing Cost", y="Median Mortgage Rate Housing Costs Relative to Median Income")

# FMR Density plot?
ggplot(data = thads_cleaned, aes(x = FMR, y = COSTMedRELAMIPCT)) + stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
    labs(x = "Fair Market Rent", y="Median Mortgage Rate Housing Costs Relative to Median Income")

# Scatter plot Household Income (ZINC2)
# Change x axis to log?
ggplot(data = thads_cleaned) +
    geom_point(aes(x = ZINC2, y = COSTMedRELAMIPCT, col = ZINC2)) +
    scale_x_log10() +
    labs(title = "Household Income vs. Affordability\n", x = "Household Income", y = "Median Mortgage Rate Housing Costs Relative to Median Income")

# Barplot for PER
PER <- sort(unique(thads_cleaned$PER))
PER_MEAN <- vector()
PER_MEAN <- c(PER_MEAN, 1:16)
i <- 1
for (p in PER) {
    PER_MEAN[i] <- mean((thads_cleaned$COSTMedRELAMIPCT[thads_cleaned["PER"] == p]), na.rm=TRUE)
    i <- i + 1
}
ggplot() +
    geom_bar(mapping = aes(x = PER, y = PER_MEAN, fill = as.factor(PER)), stat = "identity") +
    labs(title = "Affordability by People Per Home\n", x = "People Per Home", y = "Average Median Mortgage Rate Housing Costs Relative to Median Income") +
    scale_fill_discrete(name = "People in Household")

# regressions lol
#fit1 <- lm( COSTMedRELAMIPCT ~ ZSMHC, data = thads_cleaned )
#fit2 <- lm( COSTMedRELAMIPCT ~ poly(ZSMHC, 2, raw = TRUE), data = thads_cleaned )
#fit3 <- lm( COSTMedRELAMIPCT ~ poly(ZSMHC, 3, raw = TRUE), data = thads_cleaned )

#head(thads)
#View(head(thads, 50))

# correlation matrix of all the numeric variables
thads_numeric <- select_if(thads_cleaned, is.numeric)
cor_matrix <- cor(thads_numeric, use="pairwise.complete.obs")
corrplot(cor_matrix)

# correleation matrix column for just COSTMedRELAMIPCT
corrplot(cor_matrix[,62, drop=FALSE], cl.pos='n')

# find regression model

library(MASS)

fit3 <- lm(COSTMedRELAMIPCT ~ AGE1 + LMED + L30 + L50 + L80 + IPOV + PER +
             ZINC2 + ZSMHC + WEIGHT + BEDRMS + BUILT + VALUE + NUNITS + ROOMS + 
             UTILITY + OTHERCOST,
             data = thads_numeric)

summary(fit3)