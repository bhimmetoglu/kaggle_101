# Burak Himmetoglu
# begin: 11-21-2016
#
# Housing Prices: Detailed Exploratory Data Analysis
#
# Libraries
require(plyr)
require(dplyr)
require(ggplot2)
require(readr)
require(e1071)
require(Matrix)

# Read train/test data for actvities
setwd("~/Works/Rworkspace/HousePrices")

# train set
train <- read.csv("./data/train.csv") 

# test set
test <- read.csv("./data/test.csv")

### --- Outliers in train --- ###

# LotFrontage
outlier <- quantile(train$LotFrontage, na.rm = TRUE, probs = 0.99)
rmOut <- train[complete.cases(train$LotFrontage),]$LotFrontage > outlier
train <- train[!rmOut, ]

# GrLivArea
outlier <- quantile(train$GrLivArea, na.rm = TRUE, probs = 0.99)
rmOut <- train[complete.cases(train$GrLivArea),]$GrLivArea > outlier
train <- train[!rmOut, ]

# LotArea
outlier <- quantile(train$LotArea, na.rm = TRUE, probs = 0.99)
rmOut <- train[complete.cases(train$LotArea),]$LotArea > outlier
train <- train[!rmOut, ]

### --- Combine with test --- ###
train <- train %>% mutate(is.train = 1) %>% mutate(SalePrice = log(SalePrice))
test <- test %>% mutate(is.train = 0)
outcomes <- train$SalePrice; train$SalePrice <- NULL
fullData <- rbind(train,test) 

# Some integers are actually factors
fullData$MSSubClass <- as.factor(fullData$MSSubClass)

# Get total number of NAs
tot_na <- function(x) { sum(is.na(x)) }
naTable <- fullData %>% summarise_all(funs(tot_na))

## Change YearRemodAdd
fullData <- fullData %>% mutate(Remodeled = as.factor(ifelse(YearRemodAdd - YearBuilt > 0,"Y","N")))
fullData <- fullData %>% mutate(ReModTime = YearRemodAdd - YearBuilt)
fullData$YearRemodAdd <- NULL
fullData[fullData$ReModTime <0 ,]$ReModTime <- 0

### --- Filling of NA's --- ###

## Fill NA's in MSZoning with most common value
mostCommon <- fullData %>% group_by(MSZoning) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$MSZoning)
fullData[l.NA, ]$MSZoning <- mostCommon$MSZoning

## Fill NA's in Utilities
# mostCommon <- fullData %>% group_by(Utilities) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
#   dplyr::slice(1) %>% as.data.frame()
# l.NA <- is.na(fullData$Utilities)
# fullData[l.NA, ]$Utilities <- mostCommon$Utilities
#
# Almost all Utilities are AllPub. Let's drop this feature
fullData$Utilities <- NULL

## Fill NA's in LotFrontage by median (by Neighborhood)
# Get a table of NA's in LotFrontage and their medians by Neighborhood
naLotFrontage <- fullData %>% filter(is.na(LotFrontage)) %>% select(c(Neighborhood, Id))
medByNeigh <- fullData %>% group_by(Neighborhood) %>% summarise(medLotFrontage = median(LotFrontage, na.rm = TRUE))
temp <- left_join(medByNeigh, naLotFrontage, by = "Neighborhood")

l.NA <- is.na(fullData$LotFrontage)
fullData <- left_join(fullData, temp, by = c("Id", "Neighborhood"))
fullData[l.NA, ] <- fullData[l.NA, ] %>% mutate(LotFrontage = medLotFrontage)
fullData$medLotFrontage <- NULL

# OLD...
# l.NA <- is.na(fullData$LotFrontage)
# fullData[l.NA, ]$LotFrontage <- median(fullData$LotFrontage, na.rm = TRUE)

## Fill NA's in Alley: NA --> no alley
l.NA <- is.na(fullData$Alley)
levels(fullData$Alley) <- c(levels(fullData$Alley), "None")
fullData[l.NA, ]$Alley <- "None"

## Fill NA's in Exterior1st
mostCommon <- fullData %>% group_by(Exterior1st) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$Exterior1st)
fullData[l.NA, ]$Exterior1st <- mostCommon$Exterior1st

## Fill NA's in Exterior2nd
mostCommon <- fullData %>% group_by(Exterior2nd) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$Exterior2nd)
fullData[l.NA, ]$Exterior2nd <- mostCommon$Exterior2nd

## Fill NA's in MasVnrType & MasVnrArea
# Most common level in MasVnrType
mostCommonType <- fullData %>% group_by(MasVnrType) %>% summarise(n = n()) %>% arrange(desc(n)) %>% 
  dplyr::slice(1) %>% as.data.frame()

# Median of MasVnrArea for mostCommonType
medVnrAreaNone <- fullData %>% filter(MasVnrType == mostCommonType$MasVnrType) %>% select(MasVnrArea) %>% 
  summarise(med = median(MasVnrArea))

## Fill NA's with most common -> None
l.NA <- is.na(fullData$MasVnrType)
fullData[l.NA, ]$MasVnrType <- mostCommonType$MasVnrType
l.NA <- is.na(fullData$MasVnrArea)
fullData[l.NA, ]$MasVnrArea <- medVnrAreaNone$med

## Fill NA's on BsmtQual & BsmtCond & BsmtFinType1,2 & BsmtExposure : NA's mean no basement
Bsmt.NA <- is.na(fullData$BsmtExposure) | is.na(fullData$BsmtCond) | is.na(fullData$BsmtQual) | 
  is.na(fullData$BsmtFinType1) | is.na(fullData$BsmtFinType2)
# Create "None" levels
levels(fullData$BsmtQual) <- c(levels(fullData$BsmtQual),"None")
levels(fullData$BsmtCond) <- c(levels(fullData$BsmtCond),"None")
levels(fullData$BsmtFinType1) <- c(levels(fullData$BsmtFinType1),"None")
levels(fullData$BsmtFinType2) <- c(levels(fullData$BsmtFinType2),"None")
levels(fullData$BsmtExposure) <- c(levels(fullData$BsmtExposure),"None")
# Replace NA's with None
fullData[Bsmt.NA, ]$BsmtQual <- "None"
fullData[Bsmt.NA, ]$BsmtCond <- "None"
fullData[Bsmt.NA, ]$BsmtFinType1 <- "None"
fullData[Bsmt.NA, ]$BsmtFinType2 <- "None"
fullData[Bsmt.NA, ]$BsmtExposure <- "None"

## Fill NA's in BsmtFinSF1 & BsmtFinSF2 & TotalBsmtSF & BsmtUnfSF
# There is only one of these, which corressponds to "None" (i.e. no basement) so 0 value
fullData[is.na(fullData$BsmtFinSF1),]$BsmtFinSF1 <- 0
fullData[is.na(fullData$BsmtFinSF2),]$BsmtFinSF2 <- 0
fullData[is.na(fullData$TotalBsmtSF),]$TotalBsmtSF <- 0
fullData[is.na(fullData$BsmtUnfSF),]$BsmtUnfSF <- 0

## Fill NA's in Electrical (most common level)
mostCommon <- fullData %>% group_by(Electrical) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$Electrical)
fullData[l.NA, ]$Electrical <- mostCommon$Electrical

## Fill NA's in KitchenQual (most common level)
mostCommon <- fullData %>% group_by(KitchenQual) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$KitchenQual)
fullData[l.NA, ]$KitchenQual <- mostCommon$KitchenQual

## Fill NA's in Functional (most common level)
mostCommon <- fullData %>% group_by(Functional) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$Functional)
fullData[l.NA, ]$Functional <- mostCommon$Functional

## Fill NA's in BsmtFullBath and BsmtHalfBath (these have bo basements, so value is 0)
l.NA <- is.na(fullData$BsmtFullBath)
fullData[l.NA, ]$BsmtFullBath <- 0
fullData[l.NA, ]$BsmtHalfBath <- 0

## Fill NA's in FireplaceQu (0 fireplace, no quality...)
c(sum(fullData$Fireplaces == 0),sum(is.na(fullData$FireplaceQu)))
levels(fullData$FireplaceQu) <- c(levels(fullData$FireplaceQu), "None")
l.NA <- is.na(fullData$FireplaceQu)
fullData[l.NA, ]$FireplaceQu <- "None"

## NA's in GarageType (NA -- > no garage)
naGarage <- is.na(fullData$GarageType)
levels(fullData$GarageType) <- c(levels(fullData$GarageType), "None")
fullData[naGarage, ]$GarageType <- "None"
# Now fill the corresponding GarageYrBlt & GarageFinish & GarageQual & GarageCond
levels(fullData$GarageYrBlt) <- c(levels(fullData$GarageYrBlt), "None")
levels(fullData$GarageFinish) <- c(levels(fullData$GarageFinish), "None")
levels(fullData$GarageQual) <- c(levels(fullData$GarageQual), "None")
levels(fullData$GarageCond) <- c(levels(fullData$GarageCond), "None")
fullData[naGarage, ]$GarageYrBlt <- 0 # Add a weird year for non-existing garage
fullData[naGarage, ]$GarageFinish <- "None"
fullData[naGarage, ]$GarageQual <- "None"
fullData[naGarage, ]$GarageCond <- "None"

## Left over NA's in GarageYrBlt & GarageFinish & GarageQual & GarageCond. Fill with most common level
# GarageYrBlt
mostCommon <- fullData %>% group_by(GarageYrBlt) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$GarageYrBlt)
fullData[l.NA, ]$GarageYrBlt <- mostCommon$GarageYrBlt

# GarageFinish
mostCommon <- fullData %>% group_by(GarageFinish) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$GarageFinish)
fullData[l.NA, ]$GarageFinish <- mostCommon$GarageFinish

# GarageQual
mostCommon <- fullData %>% group_by(GarageQual) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$GarageQual)
fullData[l.NA, ]$GarageQual <- mostCommon$GarageQual

# GarageCond
mostCommon <- fullData %>% group_by(GarageCond) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$GarageCond)
fullData[l.NA, ]$GarageCond <- mostCommon$GarageCond

## Fill NA's in GarageCars & GarageArea  (these have no garage, so 0 area and 0 cars)
fullData[is.na(fullData$GarageCars), ]$GarageCars <- 0
fullData[is.na(fullData$GarageArea), ]$GarageArea<- 0

## Fill NA's in PoolQC
l.NA <- is.na(fullData$PoolQC)
levels(fullData$PoolQC) <- c(levels(fullData$PoolQC), "None")
fullData[l.NA, ]$PoolQC <- "None"

## Fill NA's in Fence
l.NA <- is.na(fullData$Fence)
levels(fullData$Fence) <- c(levels(fullData$Fence), "None")
fullData[l.NA, ]$Fence <- "None"

## Fill NA's in MiscFeature
l.NA <- is.na(fullData$MiscFeature)
levels(fullData$MiscFeature) <- c(levels(fullData$MiscFeature), "None")
fullData[l.NA, ]$MiscFeature <- "None"

## Fill NA's in SaleType
mostCommon <- fullData %>% group_by(SaleType) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
  dplyr::slice(1) %>% as.data.frame()
l.NA <- is.na(fullData$SaleType)
fullData[l.NA, ]$SaleType <- mostCommon$SaleType

# Recompute naTable
naTable <- fullData %>% summarise_all(funs(tot_na))
sum(naTable) # Check that all NA's are dealt with

### --- Look into all the features in detail --- ###

## Plot Saleprice by given col
plotSalePrice <- function(col){
  tr <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes)
  gg <- ggplot(tr, aes_string(col, "SalePrice")) + geom_boxplot()
  gg
}

## A dictionary of quality keys and values
qualities <- c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1", "NA"="0", "None"="0")

## MSZoning
# A        Agriculture
# C        Commercial
# FV       Floating Village Residential
# I        Industrial
# RH       Residential High Density
# RL       Residential Low Density
# RP       Residential Low Density Park
# RM       Residential Medium Density
summary(fullData$MSZoning)
plotSalePrice("MSZoning")
#fullData$MSZoning <- revalue(fullData$MSZoning, c("C (all)"="0", "RM"="1", "RH"="1", "RL"="1", "FV"="1"))

## Street
# 0 Grvl     Gravel
# 1 Pave     Paved
## Plot
summary(fullData$Street)
plotSalePrice("Street")
#
# NOTE: Good predictor to keep

## Alley
# 1 Grvl     Gravel
# 2 Pave     Paved
# 0 None     No Alley
## Plot
summary(fullData$Alley)
plotSalePrice("Alley")
#
# NOTE: Good predictor to keep

## LotShape (regular vs irregular)
# 1 Reg      Regular
# 0 IR1      Slightly irregular
# 0 IR2      Moderately Irregular
# 0 IR3      Irregular
## Plot
summary(fullData$LotShape)
plotSalePrice("LotShape") 
#
#fullData$LotShape <- revalue(fullData$LotShape, c("Reg"="1", "IR*"="0))
# NOTE: Not a great predictor, but can be kept. Regular=1 vs Irregular=0 encoding

## LandContour (Level vs non-Level)
# 1 Lvl      Near Flat/Level
# 0 Bnk      Banked - Quick and significant rise from street grade to building
# 0 HLS      Hillside - Significant slope from side to side
# 0 Low      Depression
## Plot
summary(fullData$LandContour)
plotSalePrice("LandContour")
#
# NOTE: Not a great predictor, but can be kept. Lvl=1, all others 0

## LotConfig
# 1 Inside   Inside lot
# 0 Corner   Corner lot
# 0 CulDSac  Cul-de-sac
# 0 FR2      Frontage on 2 sides of property
# 0 FR3      Frontage on 3 sides of property
summary(fullData$LotConfig)
plotSalePrice("LotConfig")

## LandSlope (Gentle vs non-Gentle)
# 1 Gtl      Gentle slope
# 0 Mod      Moderate Slope
# 0 Sev      Severe Slope
## Plot
summary(fullData$LandSlope)
plotSalePrice("LandSlope")
#
# NOTE: Not a great predictor, but keep it as 1/0.

## Neighborhood (From SalePrice Quantiles, determine NeighQual)
foo <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes)
foo <- foo %>% mutate(saleQ = cut(SalePrice, quantile(foo$SalePrice), include.lowest = T))
foo <- foo %>% group_by(Neighborhood, saleQ) %>% summarize(n = n()) %>% as.data.frame()
levels(foo$saleQ) <- c("1","2","3","4")
#
fullData$NeighQual <- "empty"
for (neigh in levels(fullData$Neighborhood)){
  baa <- foo %>% filter(Neighborhood == neigh) %>% arrange(desc(n)) %>% dplyr::slice(1) %>% select(saleQ)
  cat(neigh, as.character(baa$saleQ), "\n")
  fullData[fullData$Neighborhood == neigh, ]$NeighQual <- as.character(baa$saleQ)
}
fullData <- fullData %>% mutate(NeighQual = as.factor(NeighQual))

plotSalePrice("Neighborhood")
plotSalePrice("NeighQual") # Much better...

## Condition1
# Artery   Adjacent to arterial street
# Feedr    Adjacent to feeder street
# Norm     Normal
# RRNn     Within 200' of North-South Railroad
# RRAn     Adjacent to North-South Railroad
# PosN     Near positive off-site feature--park, greenbelt, etc.
# PosA     Adjacent to postive off-site feature
# RRNe     Within 200' of East-West Railroad
# RRAe     Adjacent to East-West Railroad
summary(fullData$Condition1)
plotSalePrice("Condition1")
# Note: OK feature. Relevel as: {norm, RR*, Street=(Feedr,Artery), Pos=(PosN,PosA)}
scores <- c("Norm"="Norm", "Artery"="Street", "Feedr"="Street", "RRNn"="RR", "RRAn"="RR", "PosN"="Pos", 
            "PosA"="Pos","RRNe"="RR","RRAe"="RR")
fullData$Condition1 <- revalue(fullData$Condition1, scores)
plotSalePrice("Condition1") # Much better

## Condition2
# Same with Condition1
summary(fullData$Condition2)
plotSalePrice("Condition2")
# Note: OK feature. Relevel as: {norm, RR*, Street=(Feedr,Artery), Pos=(PosN,PosA)}
scores <- c("Norm"="Norm", "Artery"="Street", "Feedr"="Street", "RRNn"="RR", "RRAn"="RR", "PosN"="Pos", 
            "PosA"="Pos","RRNe"="RR","RRAe"="RR")
fullData$Condition2 <- revalue(fullData$Condition2, scores, warn_missing = FALSE)
plotSalePrice("Condition2") # Much better

## BldgType
# 1Fam     Single-family Detached
# 2FmCon   Two-family Conversion; originally built as one-family dwelling
# Duplx    Duplex
# TwnhsE   Townhouse End Unit
# TwnhsI   Townhouse Inside Unit
summary(fullData$BldgType)
plotSalePrice("BldgType")
# NOTE: Merge 2fmCon with Duplex, and Twnhs with TwnhsE
scores <- c("2fmCon"="Duplex", "TwnhsE"="Twnhs", "TwnhsI"="Twnhs")
fullData$BldgType <- revalue(fullData$BldgType, scores, warn_missing = FALSE)

## HouseStyle
# 1 Story   One story
# 1.5 Fin   One and one-half story: 2nd level finished
# 1.5 Unf   One and one-half story: 2nd level unfinished
# 2 Story   Two story
# 2.5 Fin   Two and one-half story: 2nd level finished
# 2.5 Unf   Two and one-half story: 2nd level unfinished
# SFoyer   Split Foyer
# SLvl     Split Level
summary(fullData$HouseStyle)
plotSalePrice("HouseStyle")
scores <- c("1.5Unf"="1Story", "1.5Fin"="1Story", "2.5Fin"="2Story", "2.5Unf"="2Story", "SFoyer"="S", "SLvl"="S")
fullData$HouseStyle <- revalue(fullData$HouseStyle, scores)
# NOTE: OK I guess..

## OverallQual
summary(fullData$OverallQual)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(OverallQual, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## OverallCond
summary(fullData$OverallCond)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(OverallCond, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## YearBuilt
summary(fullData$YearBuilt)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(YearBuilt, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## RoofStyle
# Flat     Flat
# Gable    Gable
# Gambrel  Gabrel (Barn)
# Hip      Hip
# Mansard  Mansard
# Shed     Shed
summary(fullData$RoofStyle)
plotSalePrice("RoofStyle")
scores <- c("Flat"="Other", "Gambrel"="Other", "Mansard"="Other", "Shed"="Other")
fullData$RoofStyle <- revalue(fullData$RoofStyle, scores)
# NOTE: Seems OK. 

## RoofMatl
# 0 ClyTile  Clay or Tile
# 1 CompShg  Standard (Composite) Shingle
# 0 Membran  Membrane
# 0 Metal    Metal
# 0 Roll     Roll
# 0 Tar&Grv  Gravel & Tar
# 0 WdShake  Wood Shakes
# 0 WdShngl  Wood Shingles
summary(fullData$RoofMatl)
plotSalePrice("RoofMatl")
# NOTE: OK feature. CompShg=1, rest =0

## Exterior1st
# AsbShng  Asbestos Shingles
# AsphShn  Asphalt Shingles
# BrkComm  Brick Common
# BrkFace  Brick Face
# CBlock   Cinder Block
# CemntBd  Cement Board
# HdBoard  Hard Board
# ImStucc  Imitation Stucco
# MetalSd  Metal Siding
# Other    Other
# Plywood  Plywood
# PreCast  PreCast
# Stone    Stone
# Stucco   Stucco
# VinylSd  Vinyl Siding
# Wd Sdng  Wood Siding
# WdShing  Wood Shingles
#
# revalue = VinylSd, MetalSd, HdBoard, Wd Sdng, PlyWood, CemntBd, Other
summary(fullData$Exterior1st)
plotSalePrice("Exterior1st")
scores <- c("AsbShng"="Other", "AsphShn"="Other", "BrkComm"="Other", "BrkFace"="Other", "CBlock"="Other", "ImStucc"="Other",
            "PreCast"="Other", "Stone"="Other", "Stucco"="Other", "WdShing"="Other")
fullData$Exterior1st <- revalue(fullData$Exterior1st, scores, warn_missing = FALSE)
plotSalePrice("Exterior1st") # Much better...

## Exterior2nd
# Same as Exterior1st
#
summary(fullData$Exterior2nd)
plotSalePrice("Exterior2nd")
scores <- c("AsbShng"="Other", "AsphShn"="Other", "Brk Cmn"="Other", "BrkFace"="Other", "CBlock"="Other", "ImStucc"="Other",
            "PreCast"="Other", "Stone"="Other", "Stucco"="Other", "Wd Shng"="Other")
fullData$Exterior2nd <- revalue(fullData$Exterior2nd, scores, warn_missing = FALSE)
plotSalePrice("Exterior2nd") # Much better...

## MSVnrType
summary(fullData$MasVnrType)
plotSalePrice("MasVnrType")
scores <- c("BrkCmn"="Brk", "BrkFace"="Brk")
fullData$MasVnrType <- revalue(fullData$MasVnrType, scores)
# NOTE: Looks OK to keep. 

## MSVnrArea
summary(fullData$MasVnrArea)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(YearBuilt, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## ExterQual
summary(fullData$ExterQual)
plotSalePrice("ExterQual")
# Use the qualities dictionary (Ex=1, Gd=1, Fa=0, TA=0, Po=0)

## Extercond: same as ExterQual

## Foundation
# BrkTil   Brick & Tile
# CBlock   Cinder Block
# PConc    Poured Contrete
# Slab     Slab
# Stone    Stone
# Wood     Wood
# 
# revalue = Cblock, PCond, BrkTil, Others
summary(fullData$Foundation)
plotSalePrice("Foundation")
scores <- c("Slab"="Other", "Stone"="Other", "Wood"="Other")
fullData$Foundation <- revalue(fullData$Foundation, scores, warn_missing = FALSE)
summary(fullData$Foundation)

## BsmtQual, BsmtCond: use qualities dictionary
## BsmtQual
summary(fullData$BsmtQual)
plotSalePrice("BsmtQual")

## BsmtCond
summary(fullData$BsmtCond)
plotSalePrice("BsmtCond")

## BsmtExposure
# 1 Gd       Good Exposure
# 1 Av       Average Exposure 
# 0 Mn       Mimimum Exposure
# 0 No       No Exposure
# 0 NA       No Basement
## Plot
summary(fullData$BsmtExposure)
plotSalePrice("BsmtExposure")
#
# NOTE: Looks like a good predictor. Divide as indicated above

## BsmtFinType1
# 6 GLQ      Good Living Quarters
# 5 ALQ      Average Living Quarters
# 4 BLQ      Below Average Living Quarters
# 3 Rec      Average Rec Room
# 2 LwQ      Low Quality
# 1 Unf      Unfinshed
# 0 NA       No Basement
## Plot
summary(fullData$BsmtFinType1)
plotSalePrice("BsmtFinType1")
#
# NOTE: Looks like a good feature.

## BsmtFinType2
summary(fullData$BsmtFinType2)
plotSalePrice("BsmtFinType2")
# NOTE: Not that great, but keep it same as BsmtFinType2

## BsmtFinSF1
summary(fullData$BsmtFinSF1)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(BsmtFinSF1, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## BsmtFinSF2
summary(fullData$BsmtFinSF2)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(BsmtFinSF2, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## BsmtUnfSF
summary(fullData$BsmtUnfSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(BsmtUnfSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## TotalBsmtSF
summary(fullData$TotalBsmtSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(TotalBsmtSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## Heating
# Floor    Floor Furnace
# GasA     Gas forced warm air furnace
# GasW     Gas hot water or steam heat
# Grav     Gravity furnace
# OthW     Hot water or steam heat other than gas
# Wall     Wall furnace
#
# revalue: Gas, others
summary(fullData$Heating)
plotSalePrice("Heating")
scores <- c("Floor"="Other", "Grav"="Other", "OthW"="Other", "Wall"="Other", "GasA"="Gas", "GasW"="Gas")
fullData$Heating <- revalue(fullData$Heating, scores)
plotSalePrice("Heating") # Much better

## HeatingQC
summary(fullData$HeatingQC)
plotSalePrice("HeatingQC")
## OK Feature. Use qualities dict

## CentralAir
summary(fullData$CentralAir) # Keep as is

## Electrical
# SBrkr    Standard Circuit Breakers & Romex
# FuseA    Fuse Box over 60 AMP and all Romex wiring (Average)
# FuseF    60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP    60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix      Mixed
#
# revalue: SBrkr, Fuse, Mix
summary(fullData$Electrical)
plotSalePrice("Electrical")
scores <- c("FuseA"="Fuse", "FuseP"="Fuse", "FuseF"="Fuse")
fullData$Electrical <- revalue(fullData$Electrical, scores)

## X1stFlrSF
summary(fullData$X1stFlrSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(X1stFlrSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## X2ndFltSF
summary(fullData$X2ndFlrSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(X2ndFlrSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## LowQualFinSF
summary(fullData$LowQualFinSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(LowQualFinSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## GrLivArea (outliers alreay gone)
summary(fullData$GrLivArea)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(GrLivArea, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## BsmtFullBath
summary(fullData$BsmtFullBath)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(BsmtFullBath, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## BsmtHalfBath
summary(fullData$BsmtHalfBath)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(BsmtHalfBath, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## FullfBath
summary(fullData$FullBath)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(FullBath, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## HalffBath
summary(fullData$HalfBath)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(HalfBath, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## BedroomAbvGr
summary(fullData$BedroomAbvGr)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(BedroomAbvGr, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## KitchenAbvGr
summary(fullData$KitchenAbvGr)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(KitchenAbvGr, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## KitchenQual
summary(fullData$KitchenQual)
plotSalePrice("KitchenQual")
## OK Feature. Use qualities dictionary

## TotRmsAbvGrd
summary(fullData$TotRmsAbvGrd)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(TotRmsAbvGrd, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## Functional
# 3 Typ      Typical Functionality
# 2 Min1     Minor Deductions 1
# 2 Min2     Minor Deductions 2
# 1 Mod      Moderate Deductions
# 1 Maj1     Major Deductions 1
# 1 Maj2     Major Deductions 2
# 1 Sev      Severely Damaged
# 0 Sal      Salvage only
## Plot
summary(fullData$Functional)
plotSalePrice("Functional")
scores <- c("Typ"="3", "Min1"="2", "Min2"="2", "Mod"="1", "Maj1"="1", "Maj2"="1", "Sev"="1", "Sal"="0")
fullData$Functional <- revalue(fullData$Functional,scores, warn_missing = FALSE)
#
# NOTE: OK Feature.

## Fireplaces
summary(fullData$Fireplaces)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(Fireplaces, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## FireplaceQu
summary(fullData$FireplaceQu)
plotSalePrice("FireplaceQu")
## OK Feature. Use qualities dictionary.

## GarageType
# 2Types   More than one type of garage
# Attchd   Attached to home
# Basment  Basement Garage
# BuiltIn  Built-In (Garage part of house - typically has room above garage)
# CarPort  Car Port
# Detchd   Detached from home
# NA       No Garage
#
# Revalue Attchd, Detchd, BuiltIn, None, Other 
summary(fullData$GarageType)
plotSalePrice("GarageType")
scores <- c("2Types"="Other", "Basment"="Other", "CarPort"="Other")
fullData$GarageType <- revalue(fullData$GarageType, scores)

## GarageYrBlt
summary(fullData$GarageYrBlt)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(GarageYrBlt, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## GarageFinish
# 3 Fin      Finished
# 2 RFn      Rough Finished
# 1 Unf      Unfinished
# 0 NA       No Garagera
## Plot
summary(fullData$GarageFinish)
plotSalePrice("GarageFinish")
#
# NOTE: Looks like a good feature to keep.

## GarageCars
summary(fullData$GarageCars)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(GarageCars, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## GarageArea
summary(fullData$GarageArea)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(GarageArea, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## GarageQual
summary(fullData$GarageQual)
plotSalePrice("GarageQual")
## OK Feature. Use qualities dictionary

## GarageCond
summary(fullData$GarageCond)
plotSalePrice("GarageCond")
## OK Feature. Use qualities dictionary

## PavedDrive (Paved vs Non-Paved)
# 1 Y        Paved
# 0 P        Partial Pavement
# 0 N        Dirt/Gravel
## Plot
summary(fullData$PavedDrive)
plotSalePrice("PavedDrive")
#
# NOTE: Good feature to keep.

## WoodDeckSF
summary(fullData$WoodDeckSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(WoodDeckSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## OpenPorchSF
summary(fullData$OpenPorchSF)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(OpenPorchSF, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## EnclosedPorch
summary(fullData$EnclosedPorch)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(EnclosedPorch, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## X3SsnPorch
summary(fullData$X3SsnPorch)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(X3SsnPorch, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## ScreenPorch
summary(fullData$ScreenPorch)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(ScreenPorch, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## PoolArea
summary(fullData$PoolArea)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(PoolArea, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## PoolQC
summary(fullData$GarageCond)
plotSalePrice("GarageCond")
## OK Feature. Use qualities dictionary

## Fence
# 1 GdPrv    Good Privacy
# 1 MnPrv    Minimum Privacy
# 1 GdWo     Good Wood
# 1 MnWw     Minimum Wood/Wire
# 0 NA       No Fence
## Plot
summary(fullData$Fence)
plotSalePrice("Fence")
#
# NOTE: OK feature. Divide as binary

## MiscFeature
# 1 Elev     Elevator
# 1 Gar2     2nd Garage (if not described in garage section)
# 1 Othr     Other
# 1 Shed     Shed (over 100 SF)
# 1 TenC     Tennis Court
# 0 NA       None
summary(fullData$MiscFeature)
plotSalePrice("MiscFeature")
# Divide binary

## MiscVal
summary(fullData$MiscVal)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(MiscVal, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor

## MoSold
summary(fullData$MoSold)
fullData <- fullData %>% mutate(MoSold = as.factor(MoSold))
plotSalePrice("MoSold")
# Look at sales per month
foo <- fullData %>% group_by(MoSold) %>% summarise(nSale = n()) # High season is 5,6,7
#scores <- c("1"="0", "2"="0", "3"="0", "4"="0", "5"="1", "6"="1", "7"="1", "8"="0", "9"="0", "10"="0", "11"="0", "12"="0")
#fullData$MoSold <- revalue(fullData$MoSold, scores)

## YrSold
fullData <- fullData %>% mutate(YrSold = as.factor(YrSold))
summary(fullData$YrSold)
plotSalePrice("YrSold")
# Look at sales per year
foo <- fullData %>% group_by(YrSold, MoSold) %>% summarise(nSale = n()) # 2010 is incomplete.. After 7nth month, these is no sales
## Keep as is for now

## SaleCondition
# 1 Normal   Normal Sale
# 0 Abnorml  Abnormal Sale -  trade, foreclosure, short sale
# 0 AdjLand  Adjoining Land Purchase
# 0 Alloca   Allocation - two linked properties with separate deeds, typically condo with a garage unit
# 0 Family   Sale between family members
# 1 Partial  Home was not completed when last assessed (associated with New Homes)
summary(fullData$SaleCondition)
plotSalePrice("SaleCondition")
# NOTE: Looks OK to keep. Change into normal=1, rest = 0

## Remodeled
## Plot
summary(fullData$Remodeled)
plotSalePrice("Remodeled")
#
# NOTE: Looks like a good predictor to keep.

## RemodTime
summary(fullData$ReModTime)
gg <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes) %>% ggplot(aes(ReModTime, SalePrice)) +
  geom_point(); gg
# Keep as is. Numeric factor
