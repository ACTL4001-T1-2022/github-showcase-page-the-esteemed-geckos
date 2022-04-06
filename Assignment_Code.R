# AUTHORS: Aaron Vu, Aimon Mostofi, James Ngo, Liam La, Nathan Truong
# 
# Table of Contents
# 
#   1. Libraries
#   2. Directories
#   3. Data
#   4. Functions
#   5. Goalkeeper Selection
#   6. Defenders Selection
#   7. Midfielder Selection
#   8. Forwards Selection
#   9. Probability
#   10. Economic Forecasts
#   11. Salary
# 
# 
# 
# 
# 
#  
  

### Libraries ### -------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(randomForest)
library(gbm)
library(future)
library(caret)
library(fpp3)
library(future.apply)
library(forecast)
library(class)
library(visdat)

### END ### -------------------------------------------------------------


### Directories ### -----------------------------------------------------

ecoDirectory <- paste(dirname(rstudioapi::getSourceEditorContext()$path),'/2022-student-research-case-study-economic-data.xlsx',sep="")

footballDirectory <- paste(dirname(rstudioapi::getSourceEditorContext()$path),'/2022-student-research-case-study-football-soccer-data.xlsx',sep="") 

playerdataDirectory <- paste(dirname(rstudioapi::getSourceEditorContext()$path),'/2022-student-research-case-study-player-data.xlsx',sep="") 

### END ### -------------------------------------------------------------


### Data ### ------------------------------------------------------------

# Naming convention: directory + sheetname 

# Economic Data

ecoRarSpot <- read_excel(ecoDirectory, sheet = 'Rarita Spot Rates', range = 'B12:P72')

ecoRarInflation <- read_excel(ecoDirectory, sheet = 'Rarita Inflation Rates', range = 'B11:C41')

ecoRarGDP <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'B12:F22')

ecoRarGNI <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'H12:L22')

ecoRarPopulation <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'B26:F36')

ecoRarPopDensity <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'H26:L36')

ecoRarHealthSpend <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'B40:F40')

ecoRarHouseholdSave <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'H40:L40')

ecoRarCurrency <- read_excel(ecoDirectory, sheet = 'Rarita Economic', range = 'B54:D59')

ecoGDP <- read_excel(ecoDirectory, sheet = 'Other Countries GDP', range = 'B12:G32')


# Football Data

footballRevenueRaw <- read_excel(footballDirectory, sheet = 'Revenue', range = 'B13:V34')

footballTotalRevenue <- footballRevenueRaw[,c(1,2,6,10,14,18)]
colnames(footballTotalRevenue) <- c('Nation', '2020', '2019', '2018', '2017', '2016')

footballMatchday <- footballRevenueRaw[,c(1,3,7,11,15,19)]
colnames(footballMatchday) <- c('Nation', '2020', '2019', '2018', '2017', '2016')

footballBroadcast <- footballRevenueRaw[,c(1,4,8,12,16,20)]
colnames(footballBroadcast) <- c('Nation', '2020', '2019', '2018', '2017', '2016')

footballCommericial <- footballRevenueRaw[,c(1,5,9,13,17,21)]
colnames(footballCommericial) <- c('Nation', '2020', '2019', '2018', '2017', '2016')


footballExpenseRaw <- read_excel(footballDirectory, sheet = 'Expense', range = 'B13:Q34')

footballTotalExpense <- footballExpenseRaw[,c(1,2,5,8,11,14)]
colnames(footballTotalExpense) <- c('Nation', '2020', '2019', '2018', '2017', '2016')
footballTotalExpense <- footballTotalExpense %>% filter(Nation != 'Eastern Sleboube')

footballStaffCosts <- footballExpenseRaw[,c(1,3,6,9,12,15)]
colnames(footballStaffCosts) <- c('Nation', '2020', '2019', '2018', '2017', '2016')
footballStaffCosts <- footballStaffCosts %>% filter(Nation != 'Eastern Sleboube')

footballOtherExpenses <- footballExpenseRaw[,c(1,4,7,10,13,16)]
colnames(footballOtherExpenses) <- c('Nation', '2020', '2019', '2018', '2017', '2016')
footballOtherExpenses <- footballOtherExpenses %>% filter(Nation != 'Eastern Sleboube')


footballAttend <- read_excel(footballDirectory, sheet = 'Attendance', range = 'B13:C34')

footballSocial <- read_excel(footballDirectory, sheet = 'Social Media', range = 'B13:G34')
footballSocial <- footballSocial %>% mutate(Tiktok=as.double(Tiktok)) %>% replace(is.na(.),0)

# Player Data 

playLeagueShoot <- read_excel(playerdataDirectory, sheet = 'League Shooting', range = 'B12:AA5566')

playLeaguePass <- read_excel(playerdataDirectory, sheet = 'League Passing', range = 'B12:AF5566')

playLeagueDefend<- read_excel(playerdataDirectory, sheet = 'League Defense', range = 'B12:AG5566')

playLeagueGoalkeep <-read_excel(playerdataDirectory, sheet = 'League Goalkeeping', range = 'B12:AB425')

playTournResult2020 <-read_excel(playerdataDirectory, sheet = 'Tournament Results', range = 'B11:C27')

playTournResult2021 <-read_excel(playerdataDirectory, sheet = 'Tournament Results', range = 'E11:F35')

playTournShoot<-read_excel(playerdataDirectory, sheet = 'Tournament Shooting', range = 'B12:Z2027', guess_max = 2000)

playTournPass <-read_excel(playerdataDirectory, sheet = 'Tournament Passing', range = 'B12:AE500')

playTournDefend <-read_excel(playerdataDirectory, sheet = 'Tournament Defense', range = 'B12:AF500')

playTournGoalkeep <-read_excel(playerdataDirectory, sheet = 'Tournament Goalkeeping', range = 'B12:AA141')

play2020Salary <-read_excel(playerdataDirectory, sheet = '2020 Salaries', range = 'B12:G2744')

play2021Salary <-read_excel(playerdataDirectory, sheet = '2021 Salaries', range = 'B12:G2834')


# Adjusted Datasets

#Tournament Rankings
#2020 Tournament Results
ranks2020 <- playTournResult2020 %>%
  mutate(Year = 2020) %>%
  rename('Rank' = `2020 Tournament Place`)

#2021 Tournament Results
ranks2021 <- playTournResult2021 %>%
  mutate(Year = 2021) %>%
  rename('Rank' = `2021 Tournament Place`)

# Combined tournament rankings
ranks <- rbind(ranks2020, ranks2021)


#Salary
#2020 Goalkeeper salary
salary2020 <- play2020Salary %>%
  mutate(Year = 2020) %>%
  filter(Position == 'GK')

#2021 Goalkeeper salary
salary2021 <- play2021Salary %>%
  mutate(Year = 2021) %>%
  filter(Position == 'GK')

#Combined Goalkeeper salary
salaryCombined <- rbind(salary2020, salary2021)
### END ### -------------------------------------------------------------


### Functions ### -----------------------------------------------------

#Function that standardises the values to between 0 and 1
standardise <- function (column) {
  result <- (column - min(column))/(max(column) - min(column))
  return(result)
}

RSquare <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

playerStat <- function(Born,Stat,Year) {
  ifelse((Year - Born) <= 24,Stat*(1 + ifelse(runif(1) <= 0.9, runif(1)*0.1, -runif(1)*0.1)),
         ifelse((Year - Born) <= 27,Stat*(1 + ifelse(runif(1) <= 0.75, runif(1)*0.1, -runif(1)*0.1)),
                ifelse((Year - Born) <= 32, Stat,
                       ifelse((Year - Born) <= 35, Stat*(1 - ifelse(runif(1) <= 0.75, runif(1)*0.1, -runif(1)*0.1)),
                              Stat*(1 - ifelse(runif(1) <= 0.9, runif(1)*0.1, -runif(1)*0.1))))))
}

### END ### -------------------------------------------------------------


### Goalkeeper Selection ### -----------------------------------------------------

#CORRELATION ANALYSIS TO PICK THE VARIABLES
#Attach the ranks
goalkeeperCorr <- na.omit(merge(playTournGoalkeep, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE)) %>%
  select(-c(Nation, Born, Player, Pos, League, Year)) %>%
  mutate_all(standardise)


# Correlation
corr <- round(cor(goalkeeperCorr),2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(corr)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#DATA SELECTION

#Transform playLeagueGoalkeep to combine with playTournGoalKeep
leagueGoalkeep <- playLeagueGoalkeep %>%
  select(-League) %>%
  rename('League' = `Squad`)

#Combine to create training dataset
goalkeeperTotal <- na.omit(rbind(leagueGoalkeep, playTournGoalkeep)) %>%
  select(-c(Born, Player, Pos, League, `Playing Time Starts`, 
            `Playing Time Min`, `Playing Time 90s`, `Performance GA`, 
            `Performance PKatt`, `D`, `Performance CS`,
            `Performance PKatt`, `Penalty Kicks PKA`, `Penalty Kicks PKm`, `Performance Saves`)) %>%
  mutate_if(is.numeric,standardise) %>%
  rename_with(~gsub(" ", "_", .)) %>%
  rename('Performance_Saves' = `Performance_Save%`) %>%
  rename('Performance_CS' = `Performance_CS%`) %>%
  rename('Penalty_Kick_Save' = `Penalty_Kicks_Save%`)


#Training and test dataset.
goalkeeperTraining <- goalkeeperTotal %>%
  filter(Nation != 'Rarita') %>%
  select(-c(Nation, Year))

set.seed(0)
GKtrain <- sample(nrow(goalkeeperTraining), round(nrow(goalkeeperTraining)*0.75))

goalkeeperTest <- goalkeeperTraining[-GKtrain,]


#Rarita dataset
goalkeeperRarita <- goalkeeperTotal %>%
  filter(Nation == 'Rarita' & Year == 1) %>%
  select(-c(Nation,Year))

goalkeeperRaritaName <- na.omit(rbind(leagueGoalkeep, playTournGoalkeep)) %>%
  filter(Nation == 'Rarita' & Year == 2021)


#MODELLING
#Linear Regression
linearGK <- glm(Performance_Saves ~ ., data = goalkeeperTraining, subset = GKtrain, family = 'gaussian')
summary(linearGK)
yhat_lr <- predict(linearGK, newdata = goalkeeperTraining[-GKtrain,])
gk_LR_MSE <- mean((yhat_lr - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_LR_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_lr)


#KNN
i=1
k.optm=1
for (i in seq(from=1, to=20, by=1)){
  knn.mod <-  knnreg(goalkeeperTraining[GKtrain,-4], goalkeeperTraining$Performance_Saves[GKtrain], k=i)
  yhat <- predict(knn.mod,goalkeeperTraining[-GKtrain,-4])
  k.optm[i] <- mean((yhat - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
}
plot(k.optm)


knnGK <- knnreg(goalkeeperTraining[GKtrain,-4], goalkeeperTraining$Performance_Saves[GKtrain], k=2)
yhat_knn<- predict(knnGK,goalkeeperTraining[-GKtrain,-4])
gk_KNN_MSE <- mean((yhat_knn - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_KNN_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_knn)

#Bagging
set.seed(1)
bagGK <- randomForest(Performance_Saves ~ ., data = goalkeeperTraining, subset = GKtrain, mtry = 9, ntree = 5000, importance = TRUE)
yhat_bg <- predict(bagGK, newdata = goalkeeperTraining[-GKtrain,], n.trees = 5000)
gk_bag_MSE <- mean((yhat_bg - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_bag_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_bg)

#Random Forest
set.seed(1)
tuneRF(goalkeeperTraining[GKtrain,-4], goalkeeperTraining$Performance_Saves[GKtrain], stepFactor = 0.9, ntree=5000)
rfGK <- randomForest(Performance_Saves ~ ., data = goalkeeperTraining, subset = GKtrain, mtry = 8, ntree = 5000, importance = TRUE)
yhat_rf <- predict(rfGK, newdata = goalkeeperTraining[-GKtrain,], n.trees = 5000)
gk_RF_MSE <- mean((yhat_rf - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_RF_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_rf)
# varImpPlot(rfgk)

#Boosting
boostGK <- gbm(Performance_Saves ~ ., data = goalkeeperTraining[GKtrain,], distribution = "gaussian", n.trees = 1000, interaction.depth = 5, cv.folds = 10)
best.iter <- gbm.perf(boostGK, method="cv")
yhat_boost <- predict(boostGK, newdata = goalkeeperTraining[-GKtrain,],n.trees = 1000)
gk_boost_MSE <- mean((yhat_boost - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_boost_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_boost)

#Model MSE
Model <- c("Linear Regression","KNN","Bagging","Random Forest","Boosting")
MSE <- c(gk_LR_MSE,gk_KNN_MSE,gk_bag_MSE,gk_RF_MSE,gk_boost_MSE)
`R^2` <- c(gk_LR_R,gk_KNN_R,gk_bag_R,gk_RF_R,gk_boost_R) 
gkMSE <- data.frame(Model,MSE,`R^2`)



#New prediction

gkPred <- predict(knnGK, newdata = goalkeeperRarita[,-5])

goalkeeperBest <- cbind(goalkeeperRaritaName,gkPred) %>%
  arrange(desc(gkPred))




### END ### -------------------------------------------------------------


### Defenders Selection ### -----------------------------------------------------

# Data for correlation analysis
defendCorrDef <- na.omit(merge(playTournDefend, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE)) %>%
  filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF')) %>%
  select(-c(Born, Pos, League, Year))

defendCorrPass <- na.omit(playTournPass) %>%
  filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF')) %>%
  select(-c(Age, `90s`, Born, Pos, League, Year))

defendCorr <- na.omit(merge(defendCorrDef, defendCorrPass, by.x = c('Player','Nation'), 
                            by.y = c('Player','Nation'), all.x = TRUE)) %>%
  select(-c(Player, Nation)) %>%
  mutate_all(standardise)


corr <- round(cor(defendCorr),2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(corr)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#DATA SELECTION

#Transform playLeagueDefend to combine with playTournDefend
leagueDefend <- playLeagueDefend %>%
  select(-League) %>%
  rename('League' = `Squad`) %>%
  filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF'))

#Combine to create defence dataset
defenceTotal <- rbind(leagueDefend, playTournDefend) %>%
  select(c(Player, Year, Nation, League, Age, `Tackles Tkl`, `Vs Dribbles Past`, `Pressures %`, `Pressures Def 3rd`,
           `Pressures Press`, `Blocks Blocks`, `Tkl+Int`))


#Transform playLeaguePass to combine with playTournPass
leaguePass <- playLeaguePass %>%
  select(-League) %>%
  rename('League' = `Squad`) %>%
  filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF'))

#Combine to create pass dataset
passTotal <- rbind(leaguePass, playTournPass) %>%
  select(c(Player, Nation, League, Age, `Total Cmp%`, `1/3`, `Prog`))


#Combine to create Training dataset
defendTotal <- na.omit(merge(defenceTotal, passTotal, by.x = c('Player', 'Nation', 'Age', 'League'), by.y = c('Player', 'Nation', 'Age', 'League'))) %>%
  select(-c(Player, League)) %>%
  mutate_if(is.numeric,standardise) %>%
  rename_with(~gsub(" ", "_", .)) %>%
  rename('Pressures' = `Pressures_%`) %>%
  rename('TklInt' = `Tkl+Int`) %>%
  rename('Total_Cmp' = `Total_Cmp%`) %>%
  rename('ThirdComplete' = `1/3`)


#Training and test dataset.
defendTraining <- defendTotal %>%
  filter(Nation != 'Rarita') %>%
  select(-c(Nation, Year))

set.seed(0)
DFtrain <- sample(nrow(defendTraining), round(nrow(defendTraining)*0.75))

defendTest <- defendTraining[-DFtrain,]


#Rarita dataset
defendRarita <- defendTotal %>%
  filter(Nation == 'Rarita' & Year == 1) %>%
  select(-c(Nation, Year))

defendRaritaName <- na.omit(merge(defenceTotal, passTotal, by.x = c('Player', 'Nation', 'Age', 'League'), by.y = c('Player', 'Nation', 'Age', 'League'))) %>%
  filter(Nation == 'Rarita' & Year == 2021) 


#MODELLING
#Linear Regression
linearDF <- glm(Pressures ~ ., data = defendTraining, subset = DFtrain, family = 'gaussian')
summary(linearDF)
yhat_lr <- predict(linearDF, newdata = defendTraining[-DFtrain,])
df_LR_MSE <- mean((yhat_lr - defendTraining$Pressures[-DFtrain])^2)
df_LR_R <- RSquare(defendTraining$Pressures[-DFtrain],yhat_lr)

#KNN
i=1
k.optm=1
for (i in seq(from=1, to=30, by=1)){
  knn.mod <-  knnreg(defendTraining[DFtrain,-4], defendTraining$Pressures[DFtrain], k=i)
  yhat <- predict(knn.mod,defendTraining[-DFtrain,-4])
  k.optm[i] <- mean((yhat - defendTraining$Pressures[-DFtrain])^2)
}
plot(k.optm)


knnDF <- knnreg(defendTraining[DFtrain,-4], defendTraining$Pressures[DFtrain], k=25)
yhat_knn<- predict(knnDF,defendTraining[-DFtrain,-4])
df_KNN_MSE <- mean((yhat_knn - defendTraining$Pressures[-DFtrain])^2)
df_KNN_R <- RSquare(defendTraining$Pressures[-DFtrain],yhat_knn)

#Bagging
set.seed(1)
bagDF <- randomForest(Pressures ~ ., data = defendTraining, subset = DFtrain, mtry = 9, ntree = 5000, importance = TRUE)
yhat_bg <- predict(bagDF, newdata = defendTraining[-DFtrain,], n.trees = 5000)
df_bag_MSE <- mean((yhat_bg - defendTraining$Pressures[-DFtrain])^2)
df_bag_R <- RSquare(defendTraining$Pressures[-DFtrain],yhat_bg)


#Random Forest
set.seed(1)
tuneRF(defendTraining[DFtrain,-4], defendTraining$Pressures[DFtrain], stepFactor = 0.9, ntree=5000)
rfDF <- randomForest(Pressures ~ ., data = defendTraining, subset = DFtrain, mtry = 2, ntree = 5000, importance = TRUE)
yhat_rf <- predict(rfDF, newdata = defendTraining[-DFtrain,], n.trees = 5000)
df_RF_MSE <- mean((yhat_rf - defendTraining$Pressures[-DFtrain])^2)
df_RF_R <- RSquare(defendTraining$Pressures[-DFtrain],yhat_rf)
# varImpPlot(rfDF)

#Boosting
boostDF <- gbm(Pressures ~ ., data = defendTraining[DFtrain,], distribution = "gaussian", n.trees = 1000, interaction.depth = 5, cv.folds = 10)
best.iter <- gbm.perf(boostDF, method="cv")
yhat_boost <- predict(boostDF, newdata = defendTraining[-DFtrain,],n.trees = 1000)
df_boost_MSE <- mean((yhat_boost - defendTraining$Pressures[-DFtrain])^2)
df_boost_R <- RSquare(defendTraining$Pressures[-DFtrain],yhat_boost)

#Model MSE
Model <- c("Linear Regression","KNN","Bagging","Random Forest","Boosting")
MSE <- c(df_LR_MSE,df_KNN_MSE,df_bag_MSE,df_RF_MSE,df_boost_MSE)
`R^2` <- c(df_LR_R,df_KNN_R,df_bag_R,df_RF_R,df_boost_R)
dfMSE <- data.frame(Model,MSE,`R^2`)


#New prediction
dfPred <- predict(linearDF, newdata = defendRarita[,-4])

defendBest <- cbind(defendRaritaName,dfPred) %>%
  arrange(desc(dfPred))


# REGRESSION ON TWO VARIABLES
defendTotalMulti <- defendTotal %>%
  mutate('Pressures+Total_Cmp' = 0.5 * Pressures + 0.5 * Total_Cmp) %>%
  select(-c(Pressures, Total_Cmp))

#Training and test dataset.
defendTraining <- defendTotalMulti %>%
  filter(Nation != 'Rarita') %>%
  select(-c(Nation, Year))

set.seed(0)
DFtrain <- sample(nrow(defendTraining), round(nrow(defendTraining)*0.75))

defendTest <- defendTraining[-DFtrain,]


#Rarita dataset
defendRarita <- defendTotalMulti %>%
  filter(Nation == 'Rarita' & Year == 1) %>%
  select(-c(Nation, Year))

defendRaritaName <- na.omit(merge(defenceTotal, passTotal, by.x = c('Player', 'Nation', 'Age', 'League'), by.y = c('Player', 'Nation', 'Age', 'League'))) %>%
  filter(Nation == 'Rarita' & Year == 2021) 


#MODELLING
#Linear Regression
linearDF <- glm(`Pressures+Total_Cmp` ~ ., data = defendTraining, subset = DFtrain, family = 'gaussian')
summary(linearDF)
yhat_lr <- predict(linearDF, newdata = defendTraining[-DFtrain,])
df_LR_MSE <- mean((yhat_lr - defendTraining$`Pressures+Total_Cmp`[-DFtrain])^2)
df_LR_R <- RSquare(defendTraining$`Pressures+Total_Cmp`[-DFtrain],yhat_lr)

#KNN
i=1
k.optm=1
for (i in seq(from=1, to=30, by=1)){
  knn.mod <-  knnreg(defendTraining[DFtrain,-10], defendTraining$`Pressures+Total_Cmp`[DFtrain], k=i)
  yhat <- predict(knn.mod,defendTraining[-DFtrain,-10])
  k.optm[i] <- mean((yhat - defendTraining$`Pressures+Total_Cmp`[-DFtrain])^2)
}
plot(k.optm)


knnDF <- knnreg(defendTraining[DFtrain,-10], defendTraining$`Pressures+Total_Cmp`[DFtrain], k=12)
yhat_knn<- predict(knnDF,defendTraining[-DFtrain,-10])
df_KNN_MSE <- mean((yhat_knn - defendTraining$`Pressures+Total_Cmp`[-DFtrain])^2)
df_KNN_R <- RSquare(defendTraining$`Pressures+Total_Cmp`[-DFtrain],yhat_knn)


#Bagging
set.seed(1)
bagDF <- randomForest(`Pressures+Total_Cmp` ~ ., data = defendTraining, subset = DFtrain, mtry = 9, ntree = 5000, importance = TRUE)
yhat_bg <- predict(bagDF, newdata = defendTraining[-DFtrain,], n.trees = 5000)
df_bag_MSE <- mean((yhat_bg - defendTraining$`Pressures+Total_Cmp`[-DFtrain])^2)
df_bag_R <- RSquare(defendTraining$`Pressures+Total_Cmp`[-DFtrain],yhat_bg)


#Random Forest
set.seed(1)
tuneRF(defendTraining[DFtrain,-10], defendTraining$`Pressures+Total_Cmp`[DFtrain], stepFactor = 0.9, ntree=5000)
rfDF <- randomForest(`Pressures+Total_Cmp` ~ ., data = defendTraining, subset = DFtrain, mtry = 2, ntree = 5000, importance = TRUE)
yhat_rf <- predict(rfDF, newdata = defendTraining[-DFtrain,], n.trees = 5000)
df_RF_MSE <- mean((yhat_rf - defendTraining$`Pressures+Total_Cmp`[-DFtrain])^2)
df_RF_R <- RSquare(defendTraining$`Pressures+Total_Cmp`[-DFtrain],yhat_rf)
# varImpPlot(rfDF)

#Boosting
boostDF <- gbm(`Pressures+Total_Cmp` ~ ., data = defendTraining[DFtrain,], distribution = "gaussian", n.trees = 1000, interaction.depth = 5, cv.folds = 10)
best.iter <- gbm.perf(boostDF, method="cv")
yhat_boost <- predict(boostDF, newdata = defendTraining[-DFtrain,],n.trees = 1000)
df_boost_MSE <- mean((yhat_boost - defendTraining$`Pressures+Total_Cmp`[-DFtrain])^2)
df_boost_R <- RSquare(defendTraining$`Pressures+Total_Cmp`[-DFtrain],yhat_boost)

#Model MSE
Model <- c("Linear Regression","KNN","Bagging","Random Forest","Boosting")
MSE <- c(df_LR_MSE,df_KNN_MSE,df_bag_MSE,df_RF_MSE,df_boost_MSE)
`R^2` <- c(df_LR_R,df_KNN_R,df_bag_R,df_RF_R,df_boost_R)
dfMSE <- data.frame(Model,MSE,`R^2`)

#New prediction
dfPred <- predict(linearDF, newdata = defendRarita[,-10])

plot(yhat_knn,defendTraining$Pressures[-DFtrain])
abline(0,1)

defendBest <- cbind(defendRaritaName,dfPred) %>%
  arrange(desc(dfPred))


View(cbind(yhat_knn,defendTraining$Pressures[-DFtrain]))

### END ### -------------------------------------------------------------


### Midfielders Selection ### -----------------------------------------------------
# combine shooting passing and defending tournament
# use tournament 2021 data only for variable selection
playTournDefend21 <- playTournDefend %>% filter(Year == "2021")
# use shooting 90s 
playTournDefend21 <- playTournDefend21 %>% select(-c('90s','League'))

# data set with shooting passing and defending tournament 2021
TSPD <- merge(TSP,playTournDefend21, by = c("Player","Nation", "Year", "Pos","Age","Born"),all = TRUE)

#only the MF
TSPDMF <- TSPD %>% filter(Pos == "MF" | Pos == "FWMF" | Pos == "MFDF" | Pos == "MFFW" | Pos == "DFMF")


# league data for training
playLeagueDefendfix <- playLeagueDefend %>% select(-c("League","90s"))
LSPD <- merge(LSP,playLeagueDefendfix, by = c("Player", "Squad","Year","Pos","Age","Born","Nation"),all = TRUE)
#only MF
LSPDMF <- LSPD %>% filter(Pos == "MF" | Pos == "FWMF" | Pos == "MFDF" | Pos == "MFFW" | Pos == "DFMF") %>%
  select(-Squad)

# correleation for variable selection
MFcorr <- merge(TSPDMF, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(is.na(Rank) == TRUE,25,Rank)) %>%
  select(-c(Nation, Born, Player,League, Pos, Year, 'Standard SoT%','Total Cmp%','Short Cmp%','Medium Cmp%','Long Cmp%')) 

MFcorr2 <- round(cor(MFcorr, use = "complete.obs"),2)
MFupper_tri <- get_upper_tri(MFcorr2)
MFmelted_cormat <- melt(MFupper_tri, na.rm = TRUE)
ggplot(data = MFmelted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# data to train MF and use Total Cmp as the dependent variable
MFtotal <- rbind(LSPDMF,TSPDMF) %>% filter(Nation != "Rarita") %>%
  select(-c(Born, Player, Pos, League, Year,Nation)) %>%
  select(c(Age, '90s', 'Total Cmp', 'Medium Att', '1/3', 'Total TotDist', 'Tackles Mid 3rd', 'Prog', 'Total PrgDist', 'Blocks Pass')) %>%
  mutate_all(standardise)

# rarita MF

MFRarita <- rbind(LSPDMF,TSPDMF) %>% filter(Nation == "Rarita") %>%
  select(-c(Born, Player, Pos, League, Year,Nation)) %>%
  select(c(Age, '90s', 'Total Cmp', 'Medium Att', '1/3', 'Total TotDist', 'Tackles Mid 3rd', 'Prog', 'Total PrgDist', 'Blocks Pass')) %>%
  mutate_all(standardise)

MFName <- rbind(LSPDMF,TSPDMF) %>% filter(Nation == "Rarita") %>% select(Player)

#splitting the data
set.seed(0)
MFtrain <- sample(nrow(MFtotal), round(nrow(MFtotal)*0.75))

MFTest <- MFtotal[-MFtrain,]

#GLM
MFlinearFit <- glm(MFtotal$`Total Cmp` ~ ., data = MFtotal, subset = MFtrain, family = 'gaussian')
summary(MFlinearFit)
MFyhat_lr <- predict(MFlinearFit, newdata = MFtotal[-MFtrain,])
MF_LR_MSE <- mean((MFyhat_lr - MFtotal$'Total Cmp'[-MFtrain])^2)
MF_LR_R <- RSquare(MFtotal$'Total Cmp'[-MFtrain],MFyhat_lr)

#KNN
i=1
MF.k.optm=1
for (i in seq(from=1, to=20, by=1)){
  MF.knn.mod <-  knnreg(MFtotal[MFtrain,], MFtotal$'Total Cmp'[MFtrain], k=i)
  MF.yhat <- predict(MF.knn.mod,MFtotal[-MFtrain,])
  MF.k.optm[i] <- mean((MF.yhat - MFtotal$'Total Cmp'[-MFtrain])^2)
}
plot(MF.k.optm)


MFknn <- knnreg(MFtotal[MFtrain,], MFtotal$'Total Cmp'[MFtrain], k=2)
MFyhat_knn<- predict(MFknn,MFtotal[-MFtrain,])
MF_KNN_MSE <- mean((MFyhat_knn - MFtotal$'Total Cmp'[-MFtrain])^2)
MF_KNN_R <- RSquare(MFtotal$'Total Cmp'[-MFtrain],MFyhat_knn)

#Bagging
set.seed(1)
colnames(MFtotal) <- c("Age","min","TotalCmp","MediumAtt","third","TotalTotDist","TacklesMid3rd","Prog","TotalPrgDist","BlocksPass")
MFbag <- randomForest(MFtotal$TotalCmp ~ ., data = MFtotal, subset = MFtrain, mtry = 9, ntree = 5000, importance = TRUE)
MFyhat_bg <- predict(MFbag, newdata = MFtotal[-MFtrain,], n.trees = 5000)
MF_bag_MSE <- mean((MFyhat_bg - MFtotal$TotalCmp[-MFtrain])^2)
MF_bag_R <- RSquare(MFtotal$TotalCmp[-MFtrain],MFyhat_bg) 

#Random Forest
set.seed(1)
tuneRF(MFtotal[MFtrain,-4], MFtotal[MFtrain], stepFactor = 0.9, ntree=5000)
MFGK <- randomForest(TotalCmp ~ ., data = MFtotal, subset = MFtrain, mtry = 8, ntree = 5000, importance = TRUE)
MFyhat_rf <- predict(MFGK, newdata = MFtotal[-MFtrain,], n.trees = 5000)
MF_RF_MSE <- mean((MFyhat_rf - MFtotal$TotalCmp[-MFtrain])^2)
MF_RF_R <- RSquare(MFtotal$TotalCmp[-MFtrain],MFyhat_rf) 
# varImpPlot(rfgk)

#Boosting
MFboost <- gbm(TotalCmp ~ ., data = MFtotal[MFtrain,], distribution = "gaussian", n.trees = 1000, interaction.depth = 5, cv.folds = 10)
MFbest.iter <- gbm.perf(MFboost, method="cv")
MFyhat_boost <- predict(MFboost, newdata = MFtotal[-MFtrain,],n.trees = 1000)
MF_boost_MSE <- mean((MFyhat_boost - MFtotal$TotalCmp[-MFtrain])^2)
MF_boost_R <- RSquare(MFtotal$TotalCmp[-MFtrain],MFyhat_boost)

#Model MSE
Model <- c("Linear Regression","KNN","Bagging","Random Forest","Boosting")
MSE <- c(MF_LR_MSE,MF_KNN_MSE,MF_bag_MSE,MF_RF_MSE,MF_boost_MSE)
`R^2` <- c(MF_LR_R,MF_KNN_R,MF_bag_R,MF_RF_R,MF_boost_R) 
MFMSE <- data.frame(Model,MSE,`R^2`)

#New prediction

MFPred <- predict(MFlinearFit, newdata = MFRarita)
MFBest <- cbind(MFName,MFPred) %>%
  arrange(desc(MFPred))

MFRarita2 <- rbind(LSPDMF,TSPDMF) %>% filter(Nation == "Rarita")
MFData <- merge(MFBest,MFRarita2, by = "Player")

### END ### -------------------------------------------------------------


### Forwards Selection ### --------------------------------------------------------
# combine league shooting and passing for training
# taking only league and 90s from shoot data
playLeaguePassfix <- playLeaguePass %>% select(-c("League","90s"))
LSP <- merge(playLeagueShoot,playLeaguePassfix, by = c("Player", "Squad","Year","Pos","Age","Born","Nation"),all = TRUE)
#only FW
LSPFW <- LSP %>% filter(Pos == "FW" | Pos == "FWMF" | Pos == "FWDF" | Pos == "MFFW" | Pos == "DFFW") %>%
  select(-Squad)

# combine tournament shooting and passing
# use tournament 2021 data only for variable selection
playTournShoot21 <- playTournShoot %>% filter(Year == "2021")
PlayTourpassfix <- playTourPass %>% select(-c('90s','League')) # 90s, league are different for shot and pass

TSP <- merge(playTournShoot21,PlayTourpassfix, by = c("Player","Nation", "Year", "Pos","Age","Born"),all = TRUE)
# passing no 2020 data 

# get only fw
TSPFW <- TSP %>% filter(Pos == "FW" | Pos == "FWMF" | Pos == "FWDF" | Pos == "MFFW" | Pos == "DFFW")

# correlation and merge the ranks
FWcorr <- merge(TSPFW, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(is.na(Rank) == TRUE,25,Rank)) %>%
  select(-c(Nation, Born, Player, Pos, League, Year, 'Standard SoT%','Total Cmp%','Short Cmp%','Medium Cmp%','Long Cmp%')) 

# inspected and visualising na
colSums(is.na(FWcorr))
vis_miss(FWcorr)

# Correlation for FW to choose variables 
FWcorr2 <- round(cor(FWcorr, use = "complete.obs"),2)
FWupper_tri <- get_upper_tri(FWcorr2)
FWmelted_cormat <- melt(FWupper_tri, na.rm = TRUE)
ggplot(data = FWmelted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# From choose Age, 90s,  Standard sh, standard Fk, Total cmp, KP, Total Att, xA, Expected xG (Gls)

# not choosen as high correlation with chosen

# use this to standardise
mutate_all(standardise)

# data to train FW and use gls as the dependent variable and need to remove rarita nation
FWtotal <- rbind(LSPFW,TSPFW) %>% filter(Nation != "Rarita") %>%
  select(-c(Born, Player, Pos, League, Year,Nation)) %>%
  select(c(Age, '90s',  'Standard Sh', 'Standard FK', 'Total Cmp', 'KP', 'Total Att', 'xA', 'Expected xG', 'Gls')) %>%
  mutate_all(standardise)

# rarita FW

FWRarita <- rbind(LSPFW,TSPFW) %>% filter(Nation == "Rarita") %>%
  select(-c(Born, Player, Pos, League, Year,Nation)) %>%
  select(c(Age, '90s',  'Standard Sh', 'Standard FK', 'Total Cmp', 'KP', 'Total Att', 'xA', 'Expected xG', 'Gls')) %>%
  mutate_all(standardise)

FWName <- rbind(LSPFW,TSPFW) %>% filter(Nation == "Rarita") %>% select(Player)

#splitting the data
set.seed(0)
FWtrain <- sample(nrow(FWtotal), round(nrow(FWtotal)*0.75))

FWTest <- FWtotal[-FWtrain,]

#GLM
FWlinearFit <- glm(Gls ~ ., data = FWtotal, subset = FWtrain, family = 'gaussian')
summary(FWlinearFit)
FWyhat_lr <- predict(FWlinearFit, newdata = FWtotal[-FWtrain,])
FW_LR_MSE <- mean((FWyhat_lr - FWtotal$Gls[-FWtrain])^2)
FW_LR_R <- RSquare(FWtotal$Gls[-FWtrain],FWyhat_lr)

#KNN
i=1
FW.k.optm=1
for (i in seq(from=1, to=20, by=1)){
  FW.knn.mod <-  knnreg(FWtotal[FWtrain,-10], FWtotal$Gls[FWtrain], k=i)
  FW.yhat <- predict(FW.knn.mod,FWtotal[-FWtrain,-10])
  FW.k.optm[i] <- mean((FW.yhat - FWtotal$Gls[-FWtrain])^2)
}
plot(FW.k.optm)


FWknn <- knnreg(FWtotal[FWtrain,-10], FWtotal$Gls[FWtrain], k=2)
FWyhat_knn<- predict(FWknn,FWtotal[-FWtrain,-10])
FW_KNN_MSE <- mean((FWyhat_knn - FWtotal$Gls[-FWtrain])^2)
FW_KNN_R <- RSquare(FWtotal$Gls[-FWtrain],FWyhat_knn)

#Bagging
set.seed(1)
colnames(FWtotal) <- c("Age","min","StandardSh","StandardFK","TotalCmp","KP","TotalAtt","xA","ExpectedxG","Gls" )
FWbag <- randomForest(Gls ~ ., data = FWtotal, subset = FWtrain, mtry = 9, ntree = 5000, importance = TRUE)
FWyhat_bg <- predict(FWbag, newdata = FWtotal[-FWtrain,], n.trees = 5000)
FW_bag_MSE <- mean((FWyhat_bg - FWtotal$Gls[-FWtrain])^2)
FW_bag_R <- RSquare(FWtotal$Gls[-FWtrain],FWyhat_bg) 

#Random Forest
set.seed(1)
tuneRF(FWtotal[FWtrain,-4], FWtotal[FWtrain], stepFactor = 0.9, ntree=5000)
FWGK <- randomForest(Gls ~ ., data = FWtotal, subset = FWtrain, mtry = 8, ntree = 5000, importance = TRUE)
FWyhat_rf <- predict(FWGK, newdata = FWtotal[-FWtrain,], n.trees = 5000)
FW_RF_MSE <- mean((FWyhat_rf - FWtotal$Gls[-FWtrain])^2)
FW_RF_R <- RSquare(FWtotal$Gls[-FWtrain],FWyhat_rf) 
# varImpPlot(rfgk)

#Boosting
FWboost <- gbm(Gls ~ ., data = FWtotal[FWtrain,], distribution = "gaussian", n.trees = 1000, interaction.depth = 5, cv.folds = 10)
FWbest.iter <- gbm.perf(FWboost, method="cv")
FWyhat_boost <- predict(FWboost, newdata = FWtotal[-FWtrain,],n.trees = 1000)
FW_boost_MSE <- mean((FWyhat_boost - FWtotal$Gls[-FWtrain])^2)
FW_boost_R <- RSquare(FWtotal$Gls[-FWtrain],FWyhat_boost)

#Model MSE
Model <- c("Linear Regression","KNN","Bagging","Random Forest","Boosting")
MSE <- c(FW_LR_MSE,FW_KNN_MSE,FW_bag_MSE,FW_RF_MSE,FW_boost_MSE)
`R^2` <- c(FW_LR_R,FW_KNN_R,FW_bag_R,FW_RF_R,FW_boost_R) 
FWMSE <- data.frame(Model,MSE,`R^2`)

#New prediction

FWPred <- predict(FWknn, newdata = FWRarita[,-10])
FWBest <- cbind(FWName,FWPred) %>%
  arrange(desc(FWPred))

FWRarita2 <- rbind(LSPFW,TSPFW) %>% filter(Nation == "Rarita")

FWData <- merge(FWBest,FWRarita2, by = "Player")

### END ### -------------------------------------------------------------


### Probability ### -----------------------------------------------------
#Players
gkRar <- c('F. Akumu', 'F. Ithungu')
dfRar <- c('N. Terzi?', 'H. Zare', 'F. Yunusa', 'Q. bin Ismail', 'C. Tukamushaba')
mfRar <- c('O. Wanjala', 'X. Leroy', 'F. Chin', 'S. Barman')
fwRar <- c('H. Makumbi', 'X. Thomas', 'E. Mudzingwa', 'F. Ajio')


#Data Selection
gkRarita <- playLeagueGoalkeep %>%
  filter(Player %in% gkRar & Year == 2021) %>%
  select(-Squad)

dfRarita <- playLeagueDefend %>%
  filter(Player %in% dfRar & Year == 2021) %>%
  select(-Squad)

mfRarita <- playLeaguePass %>%
  filter(Player %in% mfRar & Year == 2021) %>%
  select(-Squad)

fwRarita <- playLeagueShoot %>%
  filter(Player %in% fwRar & Year == 2021) %>%
  select(-Squad)

#Top 10 Data
gkTeamDataRaritaTT <- rbind(playTournGoalkeep, gkRarita) %>%
  select(c('Player','Born','Nation','Performance Save%','Year')) %>%
  drop_na() %>%
  filter(Year == 2021)%>%
  mutate('PerfSavePer' = standardise(`Performance Save%`)) %>%
  filter(Nation != 'Rarita')

gkTeamDataTT <- merge(gkTeamDataRaritaTT, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(is.na(Rank) == TRUE,25,Rank)) %>%
  mutate(Rank = ifelse(Rank>10,0,1)) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank', 'PerfSavePer'),mean)

dfTeamDataRaritaTT <- rbind(playTournDefend, dfRarita) %>%
  filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF')) %>%
  select(c('Player','Born','Nation','Pressures %','Year','Pos')) %>%
  drop_na()%>%
  filter(Year == 2021)%>%
  mutate('PressurePer' = standardise(`Pressures %`)) %>%
  filter(Nation != 'Rarita')

dfTeamDataTT <- merge(dfTeamDataRaritaTT, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(Rank>10,0,1)) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank', 'PressurePer'),mean)

mfTeamDataRaritaTT <- rbind(playTournPass, mfRarita) %>%
  filter(Pos %in% c('MF','FWMF','MFDF','MFFW','DFMF')) %>%
  select(c('Player','Year','Born','Nation','Total Cmp')) %>%
  drop_na() %>%
  filter(Year == 2021)%>%
  mutate('TotalCmp' = standardise(`Total Cmp`)) %>%
  filter(Nation != 'Rarita')

mfTeamDataTT <- merge(mfTeamDataRaritaTT, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(Rank>10,0,1)) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank', 'TotalCmp'),mean)

fwTeamDataRaritaTT <- rbind(playTournShoot, fwRarita) %>%
  filter(Pos %in% c('FW','FWMF','FWDF','MFFW','DFFW')) %>%
  select(c('Player','Born','Nation','Gls','Year')) %>%
  drop_na() %>%
  filter(Year == 2021)%>%
  mutate('Gls' = standardise(`Gls`)) %>%
  filter(Nation != 'Rarita')

fwTeamDataTT <- merge(fwTeamDataRaritaTT, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(is.na(Rank) == TRUE,25,Rank)) %>%
  mutate(Rank = ifelse(Rank>10,0,1)) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank', 'Gls'),mean)

modelDataTT <- merge(gkTeamDataTT,merge(merge(dfTeamDataTT,mfTeamDataTT),fwTeamDataTT))[,-1]


#Models
lrprobTT <- glm(as.factor(Rank)~.,data=modelDataTT,family="binomial")

bagprobTT <- randomForest(factor(Rank)~.,data=modelDataTT,mtry=4,ntree=5000,importance=TRUE)

rfprobTT <-randomForest(factor(Rank)~.,data=modelDataTT,mtry=3,ntree=5000,importance=TRUE)



#Actual Rank Data

gkTeamDataRarita <- rbind(playTournGoalkeep, gkRarita) %>%
  select(c('Player','Born','Nation','Performance Save%','Year')) %>%
  drop_na() %>%
  filter(Year == 2021)%>%
  mutate('PerfSavePer' = standardise(`Performance Save%`)) %>%
  filter(Nation != 'Rarita')

gkTeamData <- merge(gkTeamDataRarita, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  mutate(Rank = ifelse(is.na(Rank) == TRUE,25,Rank)) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank', 'PerfSavePer'),mean)

dfTeamDataRarita <- rbind(playTournDefend, dfRarita) %>%
  filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF')) %>%
  select(c('Player','Born','Nation','Pressures %','Year','Pos')) %>%
  drop_na()%>%
  filter(Year == 2021)%>%
  mutate('PressurePer' = standardise(`Pressures %`)) %>%
  filter(Nation != 'Rarita')

dfTeamData <- merge(dfTeamDataRarita, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  group_by(Nation) %>%
  summarise_at(c('Rank', 'PressurePer'),mean)

mfTeamDataRarita <- rbind(playTournPass, mfRarita) %>%
  filter(Pos %in% c('MF','FWMF','MFDF','MFFW','DFMF')) %>%
  select(c('Player','Year','Born','Nation','Total Cmp')) %>%
  drop_na() %>%
  filter(Year == 2021)%>%
  mutate('TotalCmp' = standardise(`Total Cmp`)) %>%
  filter(Nation != 'Rarita')

mfTeamData <- merge(mfTeamDataRarita, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank','TotalCmp'),mean)

fwTeamDataRarita <- rbind(playTournShoot, fwRarita) %>%
  filter(Pos %in% c('FW','FWMF','FWDF','MFFW','DFFW')) %>%
  select(c('Player','Born','Nation','Gls','Year')) %>%
  drop_na() %>%
  filter(Year == 2021)%>%
  mutate('Gls' = standardise(`Gls`)) %>%
  filter(Nation != 'Rarita')

fwTeamData <- merge(fwTeamDataRarita, ranks, by.x = c('Year', 'Nation'), by.y = c('Year','Country'), all.x = TRUE) %>%
  group_by(Nation)%>%
  summarise_at(c('Rank', 'Gls'),mean)

modelData <- merge(gkTeamData,merge(merge(dfTeamData,mfTeamData),fwTeamData))[,-1]

#Linear Regression
lrprob <- glm(Rank~.,data=modelData,family="gaussian")

bagprob <- randomForest((Rank)~.,data=modelData,mtry=4,ntree=5000,importance=TRUE)

rfprob <-randomForest((Rank)~.,data=modelData,mtry=3,ntree=5000,importance=TRUE)





simulationTT <- function() {
  #GOALKEEPERS
  gkTeamData2 <- rbind(playTournGoalkeep, gkRarita) %>%
    select(c('Player','Born','Nation','Performance Save%','Year')) %>%
    drop_na() %>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Performance Save%`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    gkTeamData2 <- gkTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  gkTeamData2 <- gkTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'PerfSavePer')
  
  #DEFENDERS
  dfTeamData2 <- rbind(playTournDefend, dfRarita) %>%
    filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF')) %>%
    select(c('Player','Born','Nation','Pressures %','Year','Pos')) %>%
    drop_na()%>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Pressures %`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    dfTeamData2 <- dfTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  dfTeamData2 <- dfTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'PressurePer')
  
  #MIDFIELDERS
  mfTeamData2 <- rbind(playTournPass, mfRarita) %>%
    filter(Pos %in% c('MF','FWMF','MFDF','MFFW','DFMF')) %>%
    select(c('Player','Year','Born','Nation','Total Cmp')) %>%
    drop_na() %>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Total Cmp`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    mfTeamData2 <- mfTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  mfTeamData2 <- mfTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'TotalCmp')
  
  #FORWARDS
  fwTeamData2 <- rbind(playTournShoot, fwRarita) %>%
    filter(Pos %in% c('FW','FWMF','FWDF','MFFW','DFFW')) %>%
    select(c('Player','Born','Nation','Gls','Year')) %>%
    drop_na() %>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Gls`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    fwTeamData2 <- fwTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  fwTeamData2 <- fwTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'Gls')
  
  
  predictData <- merge(merge(gkTeamData2 ,dfTeamData2, by.x = c('Nation', 'variable')), 
                       merge(mfTeamData2 ,fwTeamData2, by.x = c('Nation', 'variable')), by.x = c('Nation', 'variable'))
  
  
  predictRarita <- predictData %>% 
    filter(Nation == 'Rarita')  %>% 
    select(-c('Nation',variable))
  
  resultsRarita <- predict(bagprobTT, newdata = predictRarita, type = 'prob')[,2]
  
  return(resultsRarita)
  
}
set.seed(1)
probabilityTT <- c(2022:2031)

for (i in 1:5000) {
  probabilityTT <- rbind(probabilityTT,simulationTT())
}
probabilityTT <- as.data.frame(probabilityTT)[-1,]
summary(probabilityTT)
library(resample)
colVars(as.matrix(probabilityTT[sapply(probabilityTT, is.numeric)]))*1.96*sqrt(5000)



simulationRank <- function() {
  #GOALKEEPERS
  gkTeamData2 <- rbind(playTournGoalkeep, gkRarita) %>%
    select(c('Player','Born','Nation','Performance Save%','Year')) %>%
    drop_na() %>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Performance Save%`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    gkTeamData2 <- gkTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  gkTeamData2 <- gkTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'PerfSavePer')
  
  #DEFENDERS
  dfTeamData2 <- rbind(playTournDefend, dfRarita) %>%
    filter(Pos %in% c('DF','DFFW','DFMF','FWDF','MFDF')) %>%
    select(c('Player','Born','Nation','Pressures %','Year','Pos')) %>%
    drop_na()%>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Pressures %`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    dfTeamData2 <- dfTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  dfTeamData2 <- dfTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'PressurePer')
  
  #MIDFIELDERS
  mfTeamData2 <- rbind(playTournPass, mfRarita) %>%
    filter(Pos %in% c('MF','FWMF','MFDF','MFFW','DFMF')) %>%
    select(c('Player','Year','Born','Nation','Total Cmp')) %>%
    drop_na() %>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Total Cmp`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    mfTeamData2 <- mfTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  mfTeamData2 <- mfTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'TotalCmp')
  
  #FORWARDS
  fwTeamData2 <- rbind(playTournShoot, fwRarita) %>%
    filter(Pos %in% c('FW','FWMF','FWDF','MFFW','DFFW')) %>%
    select(c('Player','Born','Nation','Gls','Year')) %>%
    drop_na() %>%
    filter(Year == 2021)%>%
    mutate('Stat' = standardise(`Gls`)) %>%
    mutate("2022" = playerStat(Born,Stat,2022))
  
  for (i in 2023:2031) {
    fwTeamData2 <- fwTeamData2 %>%
      mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
  }
  
  fwTeamData2 <- fwTeamData2 %>%
    group_by(Nation)%>%
    summarise_at(c('2022', '2023', '2024', '2025', '2026', '2027', '2028', '2029', '2030', '2031'),mean) %>%
    melt(id = c('Nation'), value.name = 'Gls')
  
  
  predictData <- merge(merge(gkTeamData2 ,dfTeamData2, by.x = c('Nation', 'variable')), 
                       merge(mfTeamData2 ,fwTeamData2, by.x = c('Nation', 'variable')), by.x = c('Nation', 'variable'))
  
  predictRarita <- predictData %>% 
    filter(Nation == 'Rarita')  %>% 
    select(-c('Nation',variable))
  
  resultsRarita <- predict(rfprobTT, newdata = predictRarita, type = 'prob')
  
  predictRank <- predictData %>% 
    select(-c(Nation,variable))
  
  predictRankname <- predictData %>% 
    select(c(Nation,variable))  
  
  resultsRank <- predict(rfprob, newdata = predictRank)
  
  totalRanks <- cbind(predictRankname,resultsRank) %>%
    group_by(variable) %>%
    mutate(rank = dense_rank(resultsRank))
  
  
  successiteration<-totalRanks %>%
    mutate(variable = as.numeric(variable) + 2021) %>%
    as_tsibble(index = variable, key = Nation) %>%
    filter(Nation=="Rarita")%>%
    mutate(`rank`=ifelse(`rank`==1,1,0))
  successiteration<-successiteration[,4]
  return(successiteration)
}
set.seed(1)
simulationRanks <- as_tibble(2022:2031)
for (i in 1:5000) {simulationRanks<-cbind(simulationRanks,simulationRank())
}

colnames(simulationRanks)<-c("rank",1:5000)

prob1st<-simulationRanks%>%
  mutate(Prob=rowMeans(select(.,-`rank`)))%>%
  select(Prob)
prob1st



#AUTOPLOT
predictRank <- predictData %>% 
  select(-c(Nation,variable))

predictRankname <- predictData %>% 
  select(c(Nation,variable))  

resultsRank <- predict(rfprob, newdata = predictRank)

totalRanks <- cbind(predictRankname,resultsRank) %>%
  group_by(variable) %>%
  mutate(rank = dense_rank(resultsRank)) %>%
  select(-resultsRank)


totalRanks %>%
  mutate(variable = as.numeric(variable) + 2021) %>%
  as_tsibble(index = variable, key = Nation) %>%
  autoplot(rank)



### END ### -------------------------------------------------------------


### Economy ### ---------------------------------------------------------

#Population Forecast
tsPop <- ts(ecoRarPopulation$Rarita, start=2011)
mPop <- auto.arima(tsPop) 
mPop %>% forecast(h=11) %>% autoplot()
fPop <- mPop %>% forecast(h=11)

#GDP Rarita Forecast
rarGDPTotal <- cbind(Year = ecoRarGDP$Year,as.data.frame(ecoRarGDP*ecoRarPopulation/10^9)[-1])
tsGDP <- ts(rarGDPTotal$Rarita,start=2011)
mGDP <- auto.arima(tsGDP) 
mGDP %>% forecast(h=11) %>% autoplot()
fGDP <- mGDP %>% forecast(h=11)

#GDP per capita for Rarita
GDPc <- as.numeric(fGDP$mean*10^9/fPop$mean)
rbind(ecoRarGDP%>%select(Year, Rarita),cbind(Year = c(2021:2030),Rarita = GDPc)) %>%
  as_tsibble(index = Year) %>%
  autoplot(Rarita)


#GNI Rarita Forecast
rarGNITotal <- cbind(Year = ecoRarGNI$Year,as.data.frame(ecoRarGNI*ecoRarPopulation/10^9)[-1])
tsGNI <- ts(rarGNITotal$Rarita, start = 2011)
mGNI <- auto.arima(tsGNI)
mGNI %>% forecast(h = 11) %>% autoplot()
fGNI <- mGNI %>% forecast(h = 11)

#GNI per capita for Rarita
GNIc <- as.numeric(fGNI$mean*10^9/fPop$mean)
rbind(ecoRarGDP%>%select(Year,Rarita),cbind(Year = c(2021:2030),Rarita = GNIc)) %>%
  as_tsibble(index = Year) %>%
  autoplot(Rarita)

tsInf <- ts(ecoRarInflation$`Annual Inflation Rate`, start=1991)
mInf <- auto.arima(tsInf)
mInf %>% forecast(h=10) %>% autoplot()
fInf <- mInf %>% forecast(h=10)


fInfets <- ets(tsInf,model = "AAN") %>% forecast(h=10)
fInfets %>% forecast(h=10) %>% autoplot()

# Forecast Matchday 
# Get Rarita match day revenue

R <- footballRevenueRaw %>% filter(Nation == "Rarita")
RM <- ts(as.numeric(c(R[1,3],R[1,7],R[1,11],R[1,15],R[1,19])), start = 2016)
RB <- ts(as.numeric(c(R[1,4],R[1,8],R[1,12],R[1,16],R[1,20])), start = 2016)
RC <- ts(as.numeric(c(R[1,5],R[1,9],R[1,13],R[1,17],R[1,21])), start = 2016)

TR <- ts(as.numeric(c(R[1,18],R[1,14],R[1,10],R[1,6],R[1,2])), start = 2016) # assume constant 
TR2 <- ts(as.numeric(c(R[1,18],R[1,14],R[1,10],R[1,6])), start = 2016) # assume constant 

NNTR <- ets(TR,model = "AAN") %>% forecast(h=11) # model for total revenue assuming no nation team

YNTR <- ets(TR,model = "AAN", alpha = 0.1) %>% forecast(h=11) # assuming national team

# add in competitive team

# get expenses

E <- footballExpenseRaw %>% filter(Nation == "Rarita")

ES <- ts(as.numeric(c(E[1,15],E[1,12],E[1,9],E[1,6],E[1,3])), start = 2016)
TE <- ts(as.numeric(c(E[1,14],E[1,11],E[1,8],E[1,5],E[1,2])), start = 2016)

NNTE <- ets(TE,model = "AAN") %>% forecast(h=11) #expenses w/o national team

YNTE <- ets(TE,model = "AAN", alpha = 0.75, beta = 0.9) %>% forecast(h=11) #expenses w/ national team

# assumption

# Additional revenues, expenses and net profit
addRev <- YNTR$mean - NNTR$mean # Additional revenue per capita
addRev <- addRev * fPop$mean

addExp <- YNTE$mean - NNTE$mean # Additional expenses per capita
addExp <- addExp * fPop$mean

addProf <- addRev[-1] - addExp[-1]
addNP <- addProf - colSums(playerSalary5[,-1])

totalProf <- (YNTR$mean[-1] - YNTE$mean[-1]) * fPop$mean[-1]

# Adding additional profit to GDP
tGDP <- fGDP$mean*10^9 + addRev - addExp
tGDPc <- as.numeric(tGDP/fPop$mean)

rbind(ecoRarGDP%>%select(Year,Rarita),cbind(Year = c(2021:2031),Rarita = tGDPc)) %>%
  as_tsibble(index = Year) %>%
  autoplot(Rarita) +
  xlab("Year") +
  ylab("GDP (Doubloons)") +
  ggtitle("Rarita GDP per capita (2011-2031)") +
  theme_bw()

# Adding additional profit to GNI
tGNI <- fGNI$mean*10^9 + addRev - addExp
tGNIc <- as.numeric(tGNI/fPop$mean)

rbind(ecoRarGDP%>%select(Year,Rarita),cbind(Year = c(2021:2031),Rarita = tGNIc)) %>%
  as_tsibble(index = Year) %>%
  autoplot(Rarita) +
  xlab("Year") +
  ylab("GNI (Doubloons)") +
  ggtitle("Rarita GNI per capita (2011-2031)") +
  theme_bw()

tsSpot <- ts(t(ecoRarSpot[2,])[-1],start=2008)
fSpot <- auto.arima(tsSpot) %>% forecast(h=10)

funding <- c(995000000,0,0,0,0,0,0,0,0,0,0)

for (x in 1:10) {
  funding[x + 1] = (funding[x] + addNP[x]) * (1 + fSpot$mean[x])
}



### END ### -------------------------------------------------------------


### Salary ### -----------------------------------------------------
playerNames <- c('F. Akumu', 'F. Ithungu','N. Terzi?', 'H. Zare', 'F. Yunusa', 'Q. bin Ismail', 'C. Tukamushaba','O. Wanjala', 'X. Leroy', 'F. Chin', 'S. Barman','H. Makumbi', 'X. Thomas', 'E. Mudzingwa', 'F. Ajio')

playTournCompiled<-rbind(playLeagueDefend[,c('Player','Born','Year')],playLeaguePass[,c('Player','Born','Year')],playLeagueGoalkeep[,c('Player','Born','Year')],playLeagueShoot[,c('Player','Born','Year')])

playerBorn<-playTournCompiled%>%
  filter(`Player`%in% playerNames & Year == 2021)%>%
  group_by(Player)%>%
  summarise(Born=mean(Born))

salaries2021<-play2021Salary%>%
  mutate(Year=2021)

playerSalaries<-salaries2021%>%
  filter(`Player Name`%in% playerNames)%>%
  mutate(Player=`Player Name`)%>%
  select(`Player`,`Annualized Salary`)

playerSalaryData<-merge(playerBorn,playerSalaries,by = "Player")

playerSalaryData<-playerSalaryData%>%
  mutate("2022"=`Annualized Salary`)%>%
  select(-`Annualized Salary`)

playerSalaryBonus<-playerSalaryData

set.seed(4)

for (i in 2023:2031) {
  playerSalaryBonus <- playerSalaryBonus %>%
    mutate_(.dots = setNames(list(paste0("playerStat(Born,`", i - 1,"`,", i,")")), i))
}

for (i in 2022:2030){
  playerSalaryBonus<-playerSalaryBonus%>%
    mutate_(.dots=setNames(list(paste0("ifelse(`",i+1,"`/","`",i,"`>1,1,0)")),i))
}

playerSalaryBonus<-playerSalaryBonus%>%
  mutate(PB3a=rowSums(select(.,`2022`,`2023`,`2023`)),
         PB3b=rowSums(select(.,`2024`,`2025`,`2026`)),
         PB3c=rowSums(select(.,`2027`,`2028`,`2029`)),
         PB4a=rowSums(select(.,`2022`,`2023`,`2023`,`2025`)),
         PB4b=rowSums(select(.,`2026`,`2027`,`2028`,`2029`)),
         PB5=rowSums(select(.,`2022`,`2023`,`2024`,`2025`,`2026`)),
         PB6=rowSums(select(.,`2022`,`2023`,`2024`,`2025`,`2026`,`2027`)),
  )%>%
  select(-c(3:10))

InfCumulated3a <-(1+fInfets$mean[1])*(1+fInfets$mean[2])*(1+fInfets$mean[3])
InfCumulated3b <-(1+fInfets$mean[4])*(1+fInfets$mean[5])*(1+fInfets$mean[6])
InfCumulated3c <-(1+fInfets$mean[7])*(1+fInfets$mean[8])*(1+fInfets$mean[9])
InfCumulated4a <-(1+fInfets$mean[1])*(1+fInfets$mean[2])*(1+fInfets$mean[3])*(1+fInfets$mean[4])
InfCumulated4b <-(1+fInfets$mean[5])*(1+fInfets$mean[6])*(1+fInfets$mean[7])*(1+fInfets$mean[8])
InfCumulated5 <-(1+fInfets$mean[1])*(1+fInfets$mean[2])*(1+fInfets$mean[3])*(1+fInfets$mean[4])*(1+fInfets$mean[5])
InfCumulated6 <-(1+fInfets$mean[1])*(1+fInfets$mean[2])*(1+fInfets$mean[3])*(1+fInfets$mean[4])*(1+fInfets$mean[5])*(1+fInfets$mean[6])

playerSalary5<-playerSalaryData%>%
  mutate("PB5"=playerSalaryBonus$PB5,"2023"=`2022`,"2024"=`2022`,"2025"=`2022`,"2026"=`2022`)%>%
  mutate("2027"=`2022`*(1+PB5*0.02)*InfCumulated5)%>%
  mutate("2028"=`2027`,"2029"=`2027`,"2030"=`2027`,"2031"=`2027`)%>%
  select(-Born,-PB5)
sum(playerSalary5[2:11])

# 3 year contract 
playerSalary3<-playerSalaryData%>%
  mutate("PB3a"=playerSalaryBonus$PB3a,"PB3b"=playerSalaryBonus$PB3b,"PB3c"=playerSalaryBonus$PB3c,"2023"=`2022`,"2024"=`2022`) %>%
  mutate("2025"=`2022`*(1+PB3a*0.02)*InfCumulated3a)%>% 
  mutate("2026"=`2025`,"2027"=`2025`) %>% 
  mutate("2028"=`2027`*(1+PB3b*0.02)*InfCumulated3b) %>% 
  mutate("2029"=`2028`,"2030"=`2028`)%>% 
  mutate("2031"=`2030`*(1+PB3c*0.02)*InfCumulated3c) %>%
  select(-Born,-PB3a,-PB3b,-PB3c)
sum(playerSalary3[2:11])
# 4 year contract

playerSalary4<-playerSalaryData%>%
  mutate("PB4a"=playerSalaryBonus$PB4a,"PB4b"=playerSalaryBonus$PB4b,"2023"=`2022`,"2024"=`2022`,"2025"=`2022`) %>%
  mutate("2026"=`2022`*(1+PB4a*0.02)*InfCumulated4a)%>% 
  mutate("2027"=`2026`,"2028"=`2026`,"2029"=`2026`) %>% 
  mutate("2030"=`2029`*(1+PB4b*0.02)*InfCumulated4b) %>% 
  mutate("2031"=`2030`) %>% 
  select(-Born,-PB4a,-PB4b)
sum(playerSalary4[2:11])

# 6 year contract
playerSalary6<-playerSalaryData%>%
  mutate("PB6"=playerSalaryBonus$PB6,"2023"=`2022`,"2024"=`2022`,"2025"=`2022`,"2026"=`2022`,"2027"=`2022`)%>%
  mutate("2028"=`2022`*(1+PB6*0.02)*InfCumulated6)%>%
  mutate("2029"=`2028`,"2030"=`2028`,"2031"=`2028`)%>%
  select(-Born,-PB6)
sum(playerSalary6[2:11])

### END ### -------------------------------------------------------------




