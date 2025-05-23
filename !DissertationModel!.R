#Install Necessary Packages

install.packages("tidyverse")
install.packages("sf")
install.packages("broom") #Extracts coefficients and confidence intervals for plotting
install.packages("interactions") #For Marginal Effects Plots
install.packages("zoo") #For Interpolation
install.packages("ggplot2") #For Plotting / Mapping
install.packages("lmtest") #For Homoskedasticity checks
install.packages("plm") #Necessary for econometric panel regression
install.packages("sandwich") #Necessary for panel regression
install.packages("pcse") #For autocorrelation adjustments

#Load Packages

library(tidyverse)
library(sf)
library(rnaturalearth)
library(broom)
library(interactions)
library(zoo)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)
library(pcse)

#------------------------------------------------------------------------------------------------------------
#Data Cleaning
#------------------------------------------------------------------------------------------------------------

#Interpolate Missing Data
#-----

ExPanD() #For EDA and seeing what data is missing

Original_China_Unemp <- read.csv("ChinaUnemp.csv") #Read in countries with missing data for a variable
Original_Argentina_Unemp <- read.csv("ArgentinaUnemp.csv") 

#Spline Interpolation
China_Unemp_Interpolated <- na.spline(Original_China_Unemp$Unemp)
Argentina_Unemp_Interpolated <- na.spline(Original_Argentina_Unemp$Unemp)

#View Interpolated Data
view(China_Unemp_Interpolated)
view(Argentina_Unemp_Interpolated)

#Read in Panel Data-set (includes Interpolated data and data for all variables)
DebtPanelData <- read.csv("~/DISS/FinalDataFiles/DebtPanelData.csv")

#------------------------------------------------------------------------------------------------------------
#COLLINEARITY AND HOMOSCEDASTICITY CHECKS
#------------------------------------------------------------------------------------------------------------

#Create a Correlation matrix
CollinearityCheck <- cor(DebtPanelData[, c("GenDebt", "GDPGrowth", "GenSpend", "Inflation", "GovRevenue", "PopGrowth", "Unemp", "AgeDepRatio", "HealthSpend", "MiliSpend", "EduSpend", "Corruption")])
view(CollinearityCheck)

#Correlation matrix with pairwise deletion to combat missing data
PairwiseCollinearity <- cor(DebtPanelData[, c("GenDebt", "GDPGrowth", "GenSpend", "Inflation", "GovRevenue", "PopGrowth", "Unemp", "AgeDepRatio", "HealthSpend", "MiliSpend", "EduSpend", "Corruption")], use = "pairwise.complete.obs")
view(PairwiseCollinearity)

write.csv(PairwiseCollinearity, "~/DISS/Outputs/CollinearityOutputs.csv") #Export for use in the write up

#RESIDUALS PLOT
#-----

#Data Estimation for residuals extraction
DataEst <- plm(GenDebt ~ GDPGrowth + GenSpend + Inflation + GovRevenue + UrbanPopGrowth + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend + Corruption, data = DebtPanelData, model = "within")

#Extract Residuals and fitted values
residuals <- resid(DataEst)
fitted <- fitted(DataEst)

#Combine data for plotting
FitRes <- data.frame(x = fitted, y = residuals)

#Plot residuals against fitted values
ggplot(FitRes, aes(x = fitted, y = residuals)) +
  geom_point(color = "seagreen3", size = 3, shape = 18, alpha = 0.7) + #for green diamonds
  geom_hline(yintercept = 0, color = "slateblue", linetype = "twodash") + #for baseline
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") + 
  theme_minimal()

#White test checking for homoscedasticity
#-----

WhiteTest <- bptest(residuals ~ fitted + I(fitted^2)) 
view(WhiteTest)

#------------------------------------------------------------------------------------------------------------
#PANEL DATA REGRESSION 
#------------------------------------------------------------------------------------------------------------

#Create a panel data object (helps R recognize the structure of the data)
PanelData <- pdata.frame(DebtPanelData, index = c("region", "year"))

#Core Panel Regression Model
#-----
#Breusch-Pagan Test
#---

#Estimate Random Effects Model
CorePanelRE <- plm(GenDebt ~ GDPGrowth + GenSpend + Inflation + PopGrowth + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend + Corruption, 
                   data = PanelData,
                   model = "random")

BPTest <- bptest(CorePanelRE) #Breusch-Pagan Test 
BPTest #If P-Value is less than 0.5 we should not used pooled effects
#P-Value = < 2.2e-16, so not pooled effects

#Hausman Test
#---

#Estimate Fixed Effects Model
CorePanelFE <- plm(GenDebt ~ GDPGrowth + GenSpend + Inflation + PopGrowth + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend + Corruption, 
                   data = PanelData,
                   model = "within")

HausmanTest <- phtest(CorePanelFE, CorePanelRE)
HausmanTest #If p-value is less than 0.5 we should use Fixed Effects
#4.854e-05, so fixed effects

#Final Core Model with Fixed Effects

CorePanel <- plm(GenDebt ~ GDPGrowth + GenSpend + Inflation + PopGrowth + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend + Corruption, 
                   data = PanelData,
                   model = "within")
summary(CorePanel)

#Export results for use in the write up

CoreResults <- summary(CorePanel)$coefficients
write.csv(CoreResults, "~/DISS/Outputs/OverallResults.csv")

#Given the presence of heteroskedasticity (as per White Test) this adjusts standard errors

RobustModel <- vcov(CorePanel, type = "HC3") 
coeftest(CorePanel, vcov = RobustModel)

#Wooldridge Test for serial correlation


pwartest(CorePanel) #Significant p-value

#Check for time-based autocorrelation

pcdtest(CorePanel, test = "cd") #No significant p-value

#Cluster Standard errors by country to account for within-country autocorrelation

ClusteredResults <- coeftest(CorePanel, vcov = vcovHC(CorePanel, type = "sss", cluster = "group"))
ClusteredResults


write.csv(ClusteredResults, "~/DISS/Outputs/ClusteredResults.csv")

#Interaction Modelling
#-----

#Centering variables to be interacted

MeanInflation <- mean(PanelData$Inflation, na.rm = TRUE) #Mean values must be calculated before manual centering
MeanCorruption <- mean(PanelData$Corruption, na.rm = TRUE)
MeanSpend <- mean(PanelData$GenSpend, na.rm = TRUE)
MeanPopGrowth <- mean(PanelData$PopGrowth, na.rm = TRUE)
MeanAgeDep <- mean(PanelData$PopGrowth, na.rm = TRUE)
MeanHealthSpend <- mean(PanelData$HealthSpend, na.rm = TRUE)
MeanEduSpend <- mean(PanelData$EduSpend, na.rm = TRUE)
MeanUnemp <- mean(PanelData$Unemp, na.rm = TRUE)

PanelData$CentredInflation <- PanelData$Inflation - MeanInflation #Centering variables
PanelData$CentredCorruption <- PanelData$Corruption - MeanCorruption
PanelData$CentredSpend <- PanelData$GenSpend - MeanSpend
PanelData$CentredPopGrowth <- PanelData$PopGrowth - MeanPopGrowth
PanelData$CentredAgeDep <- PanelData$AgeDepRatio - MeanAgeDep
PanelData$CentredHealthSpend <- PanelData$HealthSpend - MeanHealthSpend
PanelData$CentredEduSpend <- PanelData$EduSpend - MeanEduSpend
PanelData$CentredUnemp <- PanelData$Unemp - MeanUnemp

#Regression with Growth and Corruption Interaction
#-----

CorruptInflationModel <- plm(GenDebt ~ CentredInflation*CentredCorruption + GDPGrowth + GenSpend + PopGrowth + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend,
                          data = PanelData,
                          model = "within")
summary(CorruptInflationModel)

#Checks for autocorrelation

pwartest(CorruptInflationModel) #For serial autocorrelation

pcdtest(CorruptInflationModel, test = "cd") #For cross-sectional dependence

#Run Clustered Adjustments following pwartest significance

CorInfClusteredResults <- coeftest(CorruptInflationModel, vcov = vcovHC(CorruptInflationModel, type = "sss", cluster = "group"))
CorInfClusteredResults
write.csv(CorInfClusteredResults, "~/DISS/Outputs/CorInfClusteredResults.csv")


#Regression with General Expenditure and Corruption Interaction
#-----

CorruptSpendingModel <- plm(GenDebt ~ CentredCorruption*CentredSpend + GDPGrowth + Inflation + PopGrowth + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend,
                        data = PanelData,
                        model = "within")
summary(CorruptSpendingModel)

CorruptSpendingResults <- summary(CorruptSpendingModel)$coefficients
write.csv(CorruptSpendingResults, "~/DISS/Outputs/CorruptSpendingResults.csv")

#Regression with Population Growth and General Expenditure Interaction
#-----

PopGrowthSpendingModel <- plm(GenDebt ~ CentredSpend*CentredPopGrowth + GDPGrowth + Inflation + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend + Corruption,
                        data = PanelData,
                        model = "within")
summary(PopGrowthSpendingModel)

PopGrowthSpendingResults <- summary(PopGrowthSpendingModel)$coefficients
write.csv(PopGrowthSpendingResults, "~/DISS/Outputs/PopGrowthSpendingResults.csv")

#Regression with Age Dependency Ratio and General Expenditure Interaction
#-----

AgeSpendingModel <- plm(GenDebt ~ CentredAgeDep*CentredSpend + GDPGrowth + Inflation + Unemp + AgeDepRatio + HealthSpend + EduSpend + MiliSpend + Corruption,
                        data = PanelData,
                        model = "within")
summary(AgeSpendingModel)

AgeSpendingResults <- summary(AgeSpendingModel)$coefficients
write.csv(AgeSpendingResults, "~/DISS/Outputs/AgeSpendingResults.csv")

#Regression with Age Dependency Ratio and General Expenditure Interaction
#-----

AgeHealthSpendingModel <- plm(GenDebt ~ CentredAgeDep*CentredHealthSpend + GenSpend + GDPGrowth + Inflation + Unemp + AgeDepRatio + EduSpend + MiliSpend + Corruption,
                              data = PanelData,
                              model = "within")
summary(AgeHealthSpendingModel)

AgeHealthSpendingResults <- summary(AgeHealthSpendingModel)$coefficients
write.csv(AgeHealthSpendingResults, "~/DISS/Outputs/AgeHealthSpendingResults.csv")

#Regression with Unemployment and Education Expenditure Interaction
#-----

UnempEduModel <- plm(GenDebt ~ CentredEduSpend*CentredUnemp + AgeDepRatio + HealthSpend + GenSpend + GDPGrowth + Inflation + AgeDepRatio + MiliSpend + Corruption,
                     data = PanelData,
                     model = "within")
summary(UnempEduModel) 

UnempEduResults <- summary(UnempEduModel)$coefficients
write.csv(UnempEduResults, "~/DISS/Outputs/UnempEduResults.csv")

#-----------------------------------------------------------------------------------------------------------
#Introduction Maps
#-----------------------------------------------------------------------------------------------------------
install.packages("viridis") #For Map Aesthetics
install.packages("rnaturalearth") #Provides Map Data
install.packages("maps") #Provides Functions for Drawing Maps

library(viridis)
library(rnaturalearth)
library(maps)

mapdata <- map_data("world") #This creates a data frame for all countries of the world and their geography, necessary for mapping
GenDebt2020 <- read.csv("GenDebt2020.csv") #Single variable, non-temporal files for mapping
GenDebt2012 <- read.csv("GenDebt2012.csv")

GenDebtMapData2020 <- left_join(mapdata, GenDebt2020, by = "region") #Adds coordinates to our data
View(GenDebtMapData2020)

GenDebtMapData2012 <- left_join(mapdata, GenDebt2012, by = "region")
View(GenDebtMapData2012)

#Create The Map

DebtMap2020 <- ggplot(GenDebtMapData2020, aes(x = long, y = lat, group = group, fill = GenDebt)) + 
  geom_polygon(color = "black", linewidth = 0.1) +
  scale_fill_distiller(palette = "BuGn", direction = 1, na.value = "gray", limits = c(5, 260)) +
  theme_minimal() + 
  labs(title = "Global Map of General Govt. Gross Debt Ratio (2020)", fill = "General Gross Debt Ratio (%GDP)") +
  theme(legend.position = "bottom")
DebtMap2020

#Repeat for 2012

DebtMap2012 <- ggplot(GenDebtMapData2012, aes(x = long, y = lat, group = group, fill = GenDebt)) + 
  geom_polygon(color = "black", linewidth = 0.1) +
  scale_fill_distiller(palette = "BuGn", direction = 1, na.value = "gray", limits = c(5, 260)) +
  theme_minimal() + 
  labs(title = "Global Map of General Govt. Gross Debt Ratio (2012)", fill = "General Gross Debt Ratio(%GDP)") +
  theme(legend.position = "bottom")
DebtMap2012

#Zoom in on Europe to more easily view trends

Europe2020 <- DebtMap2020 +
  coord_sf(xlim = c(-25, 50), ylim = c(35, 72)) + 
  labs(title = "European Map of General Govt. Gross Debt Ratio (2020)") + 
  theme(legend.position = "bottom")
Europe2020

Europe2012 <- DebtMap2012 +
  coord_sf(xlim = c(-25, 50), ylim = c(35, 72)) + 
  labs(title = "European Map of General Govt. Gross Debt Ratio (2012)") + 
  theme(legend.position = "bottom")
Europe2012
