# Burak Himmetoglu
# Date: 10-05-2016
#
# Predicting Housing Prices
# Clean and preprocess the training and testing sets
# Detailed analysis: feature by feature
# 
# Libraries
require(plyr)
require(dplyr)
require(ggplot2)
require(readr)
require(e1071)
require(Matrix)

#slice <- dplyr::slice
# Read train/test data for actvities
setwd("~/Works/Rworkspace/HousePrices")

# train set
train <- read.csv("./data/train.csv") 

# test set
test <- read.csv("./data/test.csv")

### --- Outliers in train --- ###

# GrLivArea (most important)
outlier <- 4000 # By eye
rmOutTr <- train[complete.cases(train$GrLivArea),]$GrLivArea > outlier; sum(rmOutTr) # 4 in train set
rmOutTs <- test[complete.cases(test$GrLivArea),]$GrLivArea > outlier; sum(rmOutTs) # 1 in test set
train <- train[!rmOutTr, ]

# # TotalBsmtSF
# outlier <- 3000 # By eye
# rmOutTr <- train[complete.cases(train$TotalBsmtSF),]$TotalBsmtSF > outlier; sum(rmOutTr) # 3 in train
# rmOutTs <- test[complete.cases(test$TotalBsmtSF),]$TotalBsmtSF > outlier; sum(rmOutTs) # 1 in test set
# train <- train[!rmOutTr, ]
# 
# # LotArea
# outlier <- 100000 # By eye
# rmOutTr <- train[complete.cases(train$LotArea),]$LotArea > outlier; sum(rmOutTr) # 4 in train
# rmOutTs <- test[complete.cases(test$LotArea),]$LotArea > outlier; sum(rmOutTs) # 0 in test set
# train <- train[!rmOutTr, ] 
# 
# # LotFrontage
# outlier <- 200 # By eye
# rmOutTr <- train[complete.cases(train$LotFrontage),]$LotFrontage > outlier; sum(rmOutTr) # 1 in train set
# rmOutTs <- test[complete.cases(test$LotFrontage), ]$LotFrontage > outlier; sum(rmOutTs) # 0 in test set
# train <- train[!rmOutTr, ]

## Id's of training observations that fail to fit
out.Id <- c(633, 1325, 463, 971, 689); l.Id <- train$Id %in% out.Id
train <- train[!l.Id, ]

### --- Combine with test --- ###
train <- train %>% mutate(is.train = 1) %>% mutate(SalePrice = log(SalePrice))
test <- test %>% mutate(is.train = 0)
outcomes <- train$SalePrice; train$SalePrice <- NULL
fullData <- rbind(train,test) 

# Some integers are actually factors
fullData$MSSubClass <- as.factor(fullData$MSSubClass)
fullData$MoSold <- as.factor(fullData$MoSold)
fullData$YrSold <- as.factor(fullData$YrSold)

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

## Fill NA's in BsmtFullBath and BsmtHalfBath (these have no basements, so value is 0)
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

### --- Conversion of Some Factors into integers/numeric --- ###
##
## If there are less than 100 values per level, I merged it with a closer level
##

## A dictionary of quality keys and values (Fa=0)
qualities <- c("Ex"="5", "Gd"="4", "TA"="3", "Fa"="2", "Po"="1", "NA"="0", "None"="0")

## MSZoning (see plots in the exploratory analysis)
# A        Agriculture
# C        Commercial
# FV       Floating Village Residential
# I        Industrial
# RH       Residential High Density
# RL       Residential Low Density
# RP       Residential Low Density Park
# RM       Residential Medium Density
fullData$MSZoning <- revalue(fullData$MSZoning, c("C (all)"="0", "RM"="1", "RH"="1", "RL"="1", "FV"="1"), warn_missing = FALSE)

# Street
# 0 Grvl     Gravel
# 1 Pave     Paved
#
fullData$Street <- revalue(fullData$Street, c("Grvl"="0", "Pave"="1"))

# Alley
## Alley
# 1 Grvl     Gravel
# 2 Pave     Paved
# 0 None     No Alley
fullData$Alley <- revalue(fullData$Alley, c("Grvl"="1", "Pave"="2", "None"="0"))
fullData$Alley <- as.numeric(levels(fullData$Alley))[fullData$Alley]

## LotShape (regular vs irregular)
# 1 Reg      Regular
# 0 IR1      Slightly irregular
# 0 IR2      Moderately Irregular
# 0 IR3      Irregular
# Remove
fullData$LotShape <- revalue(fullData$LotShape, c("Reg"="1", "IR1"="0", "IR2"="0", "IR3"="0"))

## LandContour (Level vs non-Level) !!!!!!!!!
# 1 Lvl      Near Flat/Level
# 0 Bnk      Banked - Quick and significant rise from street grade to building
# 0 HLS      Hillside - Significant slope from side to side
# 0 Low      Depression
# Remove
#fullData$LandContour <- revalue(fullData$LandContour, c("Lvl"="1","Bnk"="0","HLS"="0","Low"="0"))
#fullData$LandContour <- as.numeric(levels(fullData$LandContour))[fullData$LandContour]

## LotConfig
# 1 Inside   Inside lot
# 0 Corner   Corner lot
# 0 CulDSac  Cul-de-sac
# 0 FR2      Frontage on 2 sides of property
# 0 FR3      Frontage on 3 sides of property
fullData$LotConfig <- revalue(fullData$LotConfig, c("Inside"="3","Corner"="2","CulDSac"="1","FR2"="0", "FR3"="0"))

## LandSlope (Gentle vs non-Gentle)
# 1 Gtl      Gentle slope
# 0 Mod      Moderate Slope
# 0 Sev      Severe Slope
# Remove
fullData$LandSlope <- revalue(fullData$LandSlope, c("Gtl"="1","Mod"="0","Sev"="0"))

## Neighborhood (From SalePrice Quantiles, determine NeighQual)
foo <- fullData %>% filter(is.train == 1) %>% mutate(SalePrice = outcomes)
foo <- foo %>% mutate(saleQ = cut(SalePrice, quantile(foo$SalePrice), include.lowest = T))
foo <- foo %>% group_by(Neighborhood, saleQ) %>% summarize(n = n()) %>% as.data.frame()
levels(foo$saleQ) <- c("1","2","3","4")
#
fullData$NeighQual <- "empty"
for (neigh in levels(fullData$Neighborhood)){
  baa <- foo %>% filter(Neighborhood == neigh) %>% arrange(desc(n)) %>% dplyr::slice(1) %>% select(saleQ)
  fullData[fullData$Neighborhood == neigh, ]$NeighQual <- as.character(baa$saleQ)
}
fullData <- fullData %>% mutate(NeighQual = as.factor(NeighQual))
fullData$Neighborhood <- NULL # Remove afterwards
# Change neighbor quality to numeric
fullData$NeighQual <- as.numeric(levels(fullData$NeighQual))[fullData$NeighQual]

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
scores <- c("Norm"="Norm", "Artery"="Street", "Feedr"="Street", "RRNn"="RR", "RRAn"="RR", "PosN"="Pos", 
            "PosA"="Pos","RRNe"="RR","RRAe"="RR")
fullData$Condition1 <- revalue(fullData$Condition1, scores)

## Condition2 (Same with Condition1)
fullData$Condition2 <- revalue(fullData$Condition2, scores, warn_missing = FALSE)

## BldgType
# 1Fam     Single-family Detached
# 2FmCon   Two-family Conversion; originally built as one-family dwelling
# Duplex    Duplex
# TwnhsE   Townhouse End Unit
# TwnhsI   Townhouse Inside Unit
scores <- c("2fmCon"="Duplex", "TwnhsE"="Twnhs", "TwnhsI"="Twnhs")
fullData$BldgType <- revalue(fullData$BldgType, scores, warn_missing = FALSE)

## HouseStyle
# 1Story   One story
# 1.5Fin   One and one-half story: 2nd level finished
# 1.5Unf   One and one-half story: 2nd level unfinished
# 2Story   Two story
# 2.5Fin   Two and one-half story: 2nd level finished
# 2.5Unf   Two and one-half story: 2nd level unfinished
# SFoyer   Split Foyer
# SLvl     Split Level
scores <- c("1.5Unf"="1Story", "1.5Fin"="1Story", "2.5Fin"="2Story", "2.5Unf"="2Story", "SFoyer"="S", "SLvl"="S")
fullData$HouseStyle <- revalue(fullData$HouseStyle, scores)

## RoofStyle
# 0 Flat     Flat
# 1 Gable    Gable
# 0 Gambrel  Gabrel (Barn)
# 0 Hip      Hip
# 0 Mansard  Mansard
# 0 Shed     Shed
scores <- c("Flat"="Other", "Gambrel"="Other", "Mansard"="Other", "Shed"="Other")
fullData$RoofStyle <- revalue(fullData$RoofStyle, scores)

## RoofMatl
# 0 ClyTile  Clay or Tile
# 1 CompShg  Standard (Composite) Shingle
# 0 Membran  Membrane
# 0 Metal    Metal
# 0 Roll     Roll
# 0 Tar&Grv  Gravel & Tar
# 0 WdShake  Wood Shakes
# 0 WdShngl  Wood Shingles
scores <- c("ClyTile"="0", "CompShg"="1", "Membran"="0", "Metal"="0", "Roll"="0", "Tar&Grv"="0", "WdShake"="0", "WdShngl"="0")
fullData$RoofMatl <- revalue(fullData$RoofMatl, scores)

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
#
scores <- c("AsbShng"="Other", "AsphShn"="Other", "BrkComm"="Other", "BrkFace"="Other", "CBlock"="Other", "ImStucc"="Other",
            "PreCast"="Other", "Stone"="Other", "Stucco"="Other", "WdShing"="Other")
fullData$Exterior1st <- revalue(fullData$Exterior1st, scores, warn_missing = FALSE)

## Exterior2nd
# Same as above
scores <- c("AsbShng"="Other", "AsphShn"="Other", "Brk Cmn"="Other", "BrkFace"="Other", "CBlock"="Other", "ImStucc"="Other",
            "PreCast"="Other", "Stone"="Other", "Stucco"="Other", "Wd Shng"="Other")
fullData$Exterior2nd <- revalue(fullData$Exterior2nd, scores, warn_missing = FALSE)

## MasVnrType
scores <- c("BrkCmn"="Brk", "BrkFace"="Brk")
fullData$MasVnrType <- revalue(fullData$MasVnrType, scores)

## ExterQual
fullData$ExterQual <- revalue(fullData$ExterQual, qualities, warn_missing = FALSE)
fullData$ExterQual <- as.numeric(levels(fullData$ExterQual))[fullData$ExterQual]

## ExterCond
fullData$ExterCond <- revalue(fullData$ExterCond, qualities, warn_missing = FALSE)
fullData$ExterCond <- as.numeric(levels(fullData$ExterCond))[fullData$ExterCond]

## Foundation
# BrkTil   Brick & Tile
# CBlock   Cinder Block
# PConc    Poured Contrete
# Slab     Slab
# Stone    Stone
# Wood     Wood
scores <- c("Slab"="Other", "Stone"="Other", "Wood"="Other")
fullData$Foundation <- revalue(fullData$Foundation, scores, warn_missing = FALSE)

## BsmtQual
fullData$BsmtQual <- revalue(fullData$BsmtQual, qualities, warn_missing = FALSE)
fullData$BsmtQual <- as.numeric(levels(fullData$BsmtQual))[fullData$BsmtQual]

## BsmtCond
fullData$BsmtCond <- revalue(fullData$BsmtCond, qualities, warn_missing = FALSE)
fullData$BsmtCond <- as.numeric(levels(fullData$BsmtCond))[fullData$BsmtCond]

## BsmtExposure
# 4 Gd       Good Exposure
# 3 Av       Average Exposure *
# 2 Mn       Mimimum Exposure
# 1 No       No Exposure
# 0 NA       No Basement
#
scores <- c("Gd"="4", "Av"="3", "Mn"="2", "No"="1", "NA"="0", "None"="0")
fullData$BsmtExposure <- revalue(fullData$BsmtExposure, scores, warn_missing = FALSE)
fullData$BsmtExposure <- as.numeric(levels(fullData$BsmtExposure))[fullData$BsmtExposure]

## BsmtFinType1
# 6 GLQ      Good Living Quarters *
# 5 ALQ      Average Living Quarters
# 4 BLQ      Below Average Living Quarters
# 3 Rec      Average Rec Room
# 2 LwQ      Low Quality
# 1 Unf      Unfinshed *
# 0 NA       No Basement
#
scores <- c("GLQ"="6", "ALQ"="5", "BLQ"="4", "Rec"="3", "LwQ"="2", "Unf"="1", "NA"="0", "None"="0")
fullData$BsmtFinType1 <- revalue(fullData$BsmtFinType1, scores, warn_missing = FALSE)
fullData$BsmtFinType1 <- as.numeric(levels(fullData$BsmtFinType1))[fullData$BsmtFinType1]

## BsmtFinType2
# Same as BsmtFinType1
fullData$BsmtFinType2 <- revalue(fullData$BsmtFinType2, scores, warn_missing = FALSE)
fullData$BsmtFinType2 <- as.numeric(levels(fullData$BsmtFinType2))[fullData$BsmtFinType2]

## Heating
# Floor    Floor Furnace
# GasA     Gas forced warm air furnace
# GasW     Gas hot water or steam heat
# Grav     Gravity furnace
# OthW     Hot water or steam heat other than gas
# Wall     Wall furnace
#
# revalue: Gas, others
scores <- c("Floor"="Other", "Grav"="Other", "OthW"="Other", "Wall"="Other", "GasA"="Gas", "GasW"="Gas")
fullData$Heating <- revalue(fullData$Heating, scores)

## HeatingQC
fullData$HeatingQC <- revalue(fullData$HeatingQC, qualities, warn_missing = FALSE)
fullData$HeatingQC <- as.numeric(levels(fullData$HeatingQC))[fullData$HeatingQC]

## CentralAir, keep as is (already binary)

## Electrical
# SBrkr    Standard Circuit Breakers & Romex
# FuseA    Fuse Box over 60 AMP and all Romex wiring (Average)
# FuseF    60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP    60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix      Mixed
#
scores <- c("FuseA"="Fuse", "FuseP"="Fuse", "FuseF"="Fuse")
fullData$Electrical <- revalue(fullData$Electrical, scores)

## KitchenQual
fullData$KitchenQual <- revalue(fullData$KitchenQual, qualities, warn_missing = FALSE)
fullData$KitchenQual <- as.numeric(levels(fullData$KitchenQual))[fullData$KitchenQual]

## Functional
# 2 Typ      Typical Functionality 
# 1 Min1     Minor Deductions 1
# 1 Min2     Minor Deductions 2
# 1 Mod      Moderate Deductions 
# 1 Maj1     Major Deductions 1 
# 0 Maj2     Major Deductions 2
# 0 Sev      Severely Damaged
# 0 Sal      Salvage only
# scores <- c("Typ" = "2", "Min1" = "1", "Min2" = "1", "Mod" = "1", "Maj1" = "1", "Maj2"="0", "Sev"="0", "Sal"="0")
# fullData$Functional <- revalue(fullData$Functional, scores, warn_missing = FALSE)
# fullData$Functional <- as.numeric(levels(fullData$Functional))[fullData$Functional]
scores <- c("Typ"="3", "Min1"="2", "Min2"="2", "Mod"="1", "Maj1"="1", "Maj2"="1", "Sev"="1", "Sal"="0")
fullData$Functional <- revalue(fullData$Functional,scores, warn_missing = FALSE)
fullData$Functional <- as.numeric(levels(fullData$Functional))[fullData$Functional]

## FireplaceQu
fullData$FireplaceQu <- revalue(fullData$FireplaceQu, qualities, warn_missing = FALSE)
fullData$FireplaceQu <- as.numeric(levels(fullData$FireplaceQu))[fullData$FireplaceQu]

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
scores <- c("2Types"="Other", "Basment"="Other", "CarPort"="Other")
fullData$GarageType <- revalue(fullData$GarageType, scores)

## GarageFinish
# 3 Fin      Finished
# 2 RFn      Rough Finished
# 1 Unf      Unfinished
# 0 NA       No Garagera
scores <- c("Fin"="3", "RFn"="2", "Unf"="1", "NA"="0", "None"="0")
fullData$GarageFinish <- revalue(fullData$GarageFinish, scores, warn_missing = FALSE)
fullData$GarageFinish <- as.numeric(levels(fullData$GarageFinish))[fullData$GarageFinish]

## GarageQual
fullData$GarageQual <- revalue(fullData$GarageQual, qualities, warn_missing = FALSE)
fullData$GarageQual <- as.numeric(levels(fullData$GarageQual))[fullData$GarageQual]

## GarageCond
fullData$GarageCond <- revalue(fullData$GarageCond, qualities, warn_missing = FALSE)
fullData$GarageCond <- as.numeric(levels(fullData$GarageCond))[fullData$GarageCond]

## PavedDrive (Paved vs Non-Paved)
# 1 Y        Paved
# 0 P        Partial Pavement
# 0 N        Dirt/Gravel
scores <- c("Y"="1", "P"="0", "N"="0")
fullData$PavedDrive <- revalue(fullData$PavedDrive, scores)

## PoolQC
fullData$PoolQC <- revalue(fullData$PoolQC, qualities, warn_missing = FALSE)
fullData$PoolQC <- as.numeric(levels(fullData$PoolQC))[fullData$PoolQC]

## Fence
# 1 GdPrv    Good Privacy *
# 1 MnPrv    Minimum Privacy
# 1 GdWo     Good Wood
# 1 MnWw     Minimum Wood/Wire
# 0 NA       No Fence *
scores <- c("GdPrv"="1", "MnPrv"="1", "GdWo"="1", "MnWw" = "1", "NA"="0", "None"="0")
fullData$Fence <- revalue(fullData$Fence, scores, warn_missing = FALSE)

## MiscFeature
# 1 Elev     Elevator
# 1 Gar2     2nd Garage (if not described in garage section)
# 1 Othr     Other
# 1 Shed     Shed (over 100 SF)
# 1 TenC     Tennis Court
# 0 NA       None
scores <- c("Elev"="1", "Gar2"="1", "Othr"="1", "Shed" = "1", "TenC"="0", "None"="0")
fullData$MiscFeature <- revalue(fullData$MiscFeature, scores, warn_missing = FALSE)

## MoSold
# High season=1, low season=0
scores <- c("1"="0", "2"="0", "3"="0", "4"="0", "5"="1", "6"="1", "7"="1", "8"="0", "9"="0", "10"="0", "11"="0", "12"="0")
fullData$MoSold <- revalue(fullData$MoSold, scores)

## YrSold (keep as is for now)

## SaleCondition
# 1 Normal   Normal Sale
# 0 Abnorml  Abnormal Sale -  trade, foreclosure, short sale
# 0 AdjLand  Adjoining Land Purchase
# 0 Alloca   Allocation - two linked properties with separate deeds, typically condo with a garage unit
# 0 Family   Sale between family members
# 1 Partial  Home was not completed when last assessed (associated with New Homes)
fullData$SaleCondition <- revalue(fullData$SaleCondition, 
                                  c("Normal"="1", "Abnorml" = "0", "AdjLand"="0", "Alloca"="0", "Family"="0", "Partial"="1"))

## Remodeled
fullData$Remodeled <- revalue(fullData$Remodeled, c("Y"="1", "N"="0"))

### --- Transform some of the columns since they are skewed --- ###

# Dskew the columns
deskew <- function(x){
  if (abs(skewness(x)) > 0.75){
    x <- log(1+x)
  }
  x
}

# Rescale columns
rescale <- function(x) { (x-mean(x))/sd(x) }

# Save locations of numeric columns
colClass <- sapply(fullData, class) # ColClass changes
numericCols <- setdiff(names(colClass[colClass == "integer" | colClass == "numeric"]),c("Id","is.train"))

# Deskewand standardize numeric columns
skewTable <- fullData %>% summarize_at(.cols = numericCols, funs(skewness))
fullData <- fullData %>% mutate_at(.cols=numericCols, funs(deskew)) %>% mutate_at(.cols=numericCols, funs(rescale))

### --- Split back to train and test --- ###
train <- filter(fullData, is.train == 1) %>% select(-is.train)
test <- filter(fullData, is.train == 0) %>% select(-is.train)
Id.test <- test$Id # Id numbers for test set
Id.train <- train$Id

# Predictor names (is.train  and Id is not a predictor) 
predictorNames <- setdiff(names(fullData),c("Id","is.train"))

# Use Sparse Model Matrices
trainSparse <- sparse.model.matrix(~., data = train[,predictorNames])[,-1]
testSparse <- sparse.model.matrix(~., data = test[,predictorNames])[,-1]

# Check unique elements in columns of training Matrix
lenUniq <- function(x) {length(unique(x))}
numUniq <- apply(trainSparse, 2, lenUniq)
rmCol <- which(numUniq == 1) # Remove this column
trainSparse <- trainSparse[,-rmCol]; testSparse <- testSparse[,-rmCol]

# Remove unnecessary variables
rm(fullData,colClass,l.NA,deskew, medVnrAreaNone,mostCommon,mostCommonType, naTable,Bsmt.NA, naGarage,
   foo, baa, medByNeigh, naLotFrontage, temp); gc()
