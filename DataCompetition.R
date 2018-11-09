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


# density plot
ggplot(data=thads_cleaned, aes(x=BEDRMS, y=COSTMedRELAMIPCT)) + stat_density_2d(aes(fill = stat(level)), geom = "polygon") + geom_smooth(method='lm') +
  labs(x = "Monthly Housing Cost", y="Median Mortgage Rate Housing Costs Relative to Median Income", title="Affordability vs. Bedrooms") + theme(text = element_text(size=20))


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

# correlation matrix of all the numeric variables
thads_numeric <- select_if(thads_cleaned, is.numeric)
# remove binary classifiation vars
thads_numeric <- subset(thads_numeric, select=-c(ASSISTED,VACANCY, STRUCTURETYPE))
thads_numeric <- thads_numeric[-grep("CAT$", colnames(thads_numeric)), -grep("CAT$", colnames(thads_numeric))]


names(thads_numeric)


cor_matrix <- cor(thads_numeric, use="pairwise.complete.obs")
corrplot(cor_matrix)


affordability_matrix <- cor_matrix[41:64,41:64, drop=FALSE]
# affordability metrics
corrplot(affordability_matrix[-grep("CAT$", colnames(affordability_matrix)), -grep("CAT$", colnames(affordability_matrix))], type="upper", tl.col="black")

other_matrix <- cor_matrix[c(60,1:40),c(61,1:40), drop=FALSE]
# other variables correlated with affordability
corrplot(other_matrix[-grep("CAT$", colnames(other_matrix)), -grep("CAT$", colnames(other_matrix))], type="upper", tl.col="black")
rect(.5, 38.5, 38.5, 37.5, border="red")

locator()

# find regression model
library(MASS)


fit0 <- lm(COSTMedRELAMIPCT ~ 1, data=thads_numeric)
fit3 <- lm(formula = COSTMedRELAMIPCT ~ AGE1 + LMED + L30 +
     L50 + L80 + IPOV + PER + ZINC2 + ZSMHC + WEIGHT +
     BEDRMS + BUILT + VALUE + NUNITS + ROOMS + UTILITY
   + OTHERCOST, data = thads_numeric)

fit3 <- lm(COSTMedRELAMIPCT ~ ., data=thads_numeric)

foo <- stepAIC(fit0, direction = "both",
               scope = list(upper = fit3, lower = fit0 ) )

summary(fit3)

names(thads_numeric)

# run PCA on the data

no_COST <- na.omit(thads_numeric)[, -grep("^COST.*REL", colnames(thads_numeric))]
names(no_COST)

pca <- prcomp(no_COST, scale=TRUE)

which(is.na(thads_numeric))

length()

summary(thads_numeric)

names(thads_numeric)

# scale = TRUE ensures features are scaled uniformly before PCA, this is not necessary but recommended
summary(pca)
library(ggbiplot)

#biplot(pca, alpha=0,labels.size=1, circle=TRUE)

ggbiplot(pca, obs.scale=2, var.scale=2, circle=F, varname.size=4, varname.adjust=15,var.axes=TRUE, alpha=0.008) + 
  geom_point(col="blue", alpha=.015) + theme_bw() + xlim(-150, 100) + ylim(-40, 90) + theme(text = element_text(size=20)) +
  labs(title="Biplot for Principal Component Analysis")
round(pca$rotation[,1],2)
