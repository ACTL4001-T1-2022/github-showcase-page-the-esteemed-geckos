# **SOA Case Study: Rarita Football Team Showcase**
---

## **The Esteemed Geckos :shipit:**
<p align="center">
 <img src="esteemedgecko.gif">
</p>

>Aaron Vu, Aimon Mostofi, James Ngo, Liam La and Nathan Truong

---

## **Objective Overview**

The objective of this study is to create a competitive national football team for Rarita to build a brand and
create positive economic opportunities. A team is defined as competitive by Commissioner Bayes if they:
 1. Rank within the top 10 members of the FSA for the season within the next 5 years with,
 2. High probability of achieving an FSA championship within the next 10 years. 

---

## **Libraries, Data and Data Cleaning**
The R Code for this project can be accessed [here](Assignment_Code.R). The packages that were utilised are shown below.
```{r}
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
```

The data used was provided by SOA. Three data sets were used:
- [Economy Data](2022-student-research-case-study-economic-data.xlsx)
- [Football Data](2022-student-research-case-study-football-soccer-data.xlsx)
- [Player Data](2022-student-research-case-study-player-data.xlsx)

Data entries that were missing or N/A were removed. All statistics were scaled to between 0 and 1. Below is the function that was applied to all statistics utilised.
```{r}
standardise <- function (column) {
  result <- (column - min(column))/(max(column) - min(column))
  return(result)
}
```
---

## **Team Selection**

Football teams have different formations for positioning themselves on the pitch. The most common and effective formation was determined to be the 1-4-3-3 formation consisting of 1 goalkeeper, 4 defenders, 3 midfielders and 3 forwards. The Raritan football team was selected based on this formation including a substitute for each role. The competitive football team comprises 15 of the top Raritan born players that have participated in a league in 2021. They were chosen based on specific attributes that are most important to their role. 

These attributes were chosen through exploring the correlation between each statistic and the rank from the teams that participated in the 2020 and 2021 FSA tournament. Statistics most correlated with rank were used as variables to build a model and the statistic that most embodied the definition of the role was chosen as the dependent variable. The dependent variables chosen were:
- Goalkeepers - Percentage of saves per shot on target (Saves%)
- Defenders - Successful pressure percentage (Pressure%)
- Midfielders - Pass completion percentage (Completion%)
- Forwards - Number of goals (Goals)

A combination of different statistics between shooting, passing, defense and goalkeeping were chosen as independent variables based on both correlation and importance for each role. They are shown below along with their correlation to rank.

#### Goalkeepers

| Predictors | Correlation With Rank |
| :---: | :---: |
| Age | 0.13 |
| Playing Time MP | -0.69 |
| Performance GA90 | 0.66 |
| Performance SoTA | 0.65 |
| W | -0.81 |
| L | 0.58 |
| Performance CS% | -0.56 |
| Penalty Kicks PKsv | 0.37 |
| Penalty Kicks Save% | 0.28 |

#### Defenders

| Predictors | Correlation With Rank |
| :---: | :---: |
| Age | -0.10 |
| Tackles Tkl | 0.21 |
| Vs Dribbles Past | 0.16 |
| Pressures Def 3rd | 0.36 |
| Pressures Press | 0.35 |
| Blocks Blocks | 0.15 |
| Tkl+Int | 0.17 |
| Total Cmp% | -0.34 |
| 1/3 | -0.29 |
| Prog | -0.18 |

#### Midfielders

| Predictors | Correlation With Rank |
| :---: | :---: |
| Age | -0.01 |
| 90s | -0.44 | 
| Total Cmp | -0.31 |
| Medium Att | -0.31 |
| 1/3 | -0.30 |
| Total TotDist | -0.29 |
| Tackles Mid 3rd | -0.27 |
| Prog | -0.26 |
| Total PrgDist | -0.20 |
| Blocks Pass | -0.13 |

#### Forwards

| Predictors | Correlation With Rank |
| :---: | :---: |
| Age  | 0.21 |
| 90s | -0.30 |
| Standard Sh | -0.16 |
| Standard FK | -0.22 |
| Total Cmp | -0.17 |
| KP | -0.16 |
| Total Att | -0.15 |
| xA | -0.14 |
| Expected xG | -0.13 |
| Gls | -0.10 |

### Modelling Player Selection

Multi-linear regression, K-nearest neighbours, bagging, random forest and boosting were the models chosen to model the players. A combination of the FSA tournament and league data was utilised to test and train the models. For each role, the model with the lowest mean squared error and highest R squared was used to predict the Raritan players' performance. The players were then ranked and the top players were chosen for the team. The players chosen were under age 30 in 2021 and their salaries were also considered to ensure economic viability.

Below is the process for selecting the model to choose the goalkeepers. 
The league goalkeeper and tournament goalkeeper data was firstly combined and transformed into a suitable format.

```{r}
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
```

The training and testing dataset was split 75% to 25% using `sample()`.

```{r}
#Training and test dataset.
goalkeeperTraining <- goalkeeperTotal %>%
  filter(Nation != 'Rarita') %>%
  select(-c(Nation, Year))

set.seed(0)
GKtrain <- sample(nrow(goalkeeperTraining), round(nrow(goalkeeperTraining)*0.75))
goalkeeperTest <- goalkeeperTraining[-GKtrain,]
```

The players from Rarita were also filtered out and not used in the modelling process.

```{r}
#Rarita dataset
goalkeeperRarita <- goalkeeperTotal %>%
  filter(Nation == 'Rarita' & Year == 1) %>%
  select(-c(Nation,Year))

goalkeeperRaritaName <- na.omit(rbind(leagueGoalkeep, playTournGoalkeep)) %>%
  filter(Nation == 'Rarita' & Year == 2021)
```
From these datasets we can build the different models. Below is the code and parameter selection for each of the models.


### Multi-Linear Regression
```{r}
linearGK <- glm(Performance_Saves ~ ., data = goalkeeperTraining, subset = GKtrain, family = 'gaussian')
summary(linearGK)
yhat_lr <- predict(linearGK, newdata = goalkeeperTraining[-GKtrain,])
gk_LR_MSE <- mean((yhat_lr - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_LR_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_lr)
```

### K-Nearest Neighbours

First, the optimal value of K must be chosen based on the lowest error.
```{r}
i=1
k.optm=1
for (i in seq(from=1, to=20, by=1)){
  knn.mod <-  knnreg(goalkeeperTraining[GKtrain,-4], goalkeeperTraining$Performance_Saves[GKtrain], k=i)
  yhat <- predict(knn.mod,goalkeeperTraining[-GKtrain,-4])
  k.optm[i] <- mean((yhat - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
}
```
We can then use the best value of k = 2 to build the KNN model.
```{r}
knnGK <- knnreg(goalkeeperTraining[GKtrain,-4], goalkeeperTraining$Performance_Saves[GKtrain], k=2)
yhat_knn<- predict(knnGK,goalkeeperTraining[-GKtrain,-4])
gk_KNN_MSE <- mean((yhat_knn - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_KNN_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_knn)
```

### Bagging
For the bagging model, 5000 trees were used and as there were 9 variables the number of splits was chosen as 9.
```{r}
set.seed(1)
bagGK <- randomForest(Performance_Saves ~ ., data = goalkeeperTraining, subset = GKtrain, mtry = 9, ntree = 5000, importance = TRUE)
yhat_bg <- predict(bagGK, newdata = goalkeeperTraining[-GKtrain,], n.trees = 5000)
gk_bag_MSE <- mean((yhat_bg - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_bag_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_bg)
```

### Random Forest
For the random forest model, 5000 trees were used and the number of splits chosen was based on the lowest out-of-bag error.
```{r}
set.seed(1)
tuneRF(goalkeeperTraining[GKtrain,-4], goalkeeperTraining$Performance_Saves[GKtrain], stepFactor = 0.9, ntree=5000)
```
The number of splits was chosen as 8 as this gave the lowest OOB.
```{r}
rfGK <- randomForest(Performance_Saves ~ ., data = goalkeeperTraining, subset = GKtrain, mtry = 8, ntree = 5000, importance = TRUE)
yhat_rf <- predict(rfGK, newdata = goalkeeperTraining[-GKtrain,], n.trees = 5000)
gk_RF_MSE <- mean((yhat_rf - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_RF_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_rf)
```

### Boosting
The number of trees utilised for the boosting model was choosen to give the smallest cross-validation error. This resulted in 1000 trees.
```{r}
boostGK <- gbm(Performance_Saves ~ ., data = goalkeeperTraining[GKtrain,], distribution = "gaussian", n.trees = 1000, interaction.depth = 5, cv.folds = 10)
best.iter <- gbm.perf(boostGK, method="cv")
yhat_boost <- predict(boostGK, newdata = goalkeeperTraining[-GKtrain,],n.trees = 1000)
gk_boost_MSE <- mean((yhat_boost - goalkeeperTraining$Performance_Saves[-GKtrain])^2)
gk_boost_R <- RSquare(goalkeeperTraining$Performance_Saves[-GKtrain],yhat_boost)
```

### Choosing the Model
From these models, we have also calculated the MSE and R^2. The R^2 was calculated as below,
```{r}
RSquare <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
```
For the goalkeeper models, the MSE and R^2 for each model was,

| **Model** | **MSE** | **R Square** |
| :---: | :---: | :---: |
| Multi-linear Regression | 0.007275711 | 0.5507444 |
| K-Nearest Neighbours | 0.004998123 | 0.7290712 | 
| Bagging | 0.007080357 | 0.5908597 |
| Random Forest | 0.007319475 | 0.5770460 |
| Boosting | 0.009113703 | 0.4429678 |

As the KNN model has the lowest MSE and highest R^2, the KNN model was chosen as the best model to predict the Rarita players performance. 
```{r}
gkPred <- predict(knnGK, newdata = goalkeeperRarita[,-5])
```
From the predicted values, the top 2 goalkeepers were chosen. This process was repeated for the defenders, midfielders and forwards. The MSE and R^2 of the models for each of the other positions are shown below.

#### Defenders
| **Model** | **MSE** | **R Square** |
| :---: | :---: | :---: |
| Multi-linear Regression | 0.005752477 | 0.11062766 |
| K-Nearest Neighbours | 0.006058334 | 0.07471167 |
| Bagging | 0.006335879 | 0.07147108 |
| Random Forest | 0.005986248 | 0.09114180 |
| Boosting | 0.006331765 | 0.11040315 |

> The multi-linear regression model had the lowest MSE and the highest R^2 and was chosen as the best model for defenders.

#### Midfielders
| **Model** | **MSE** | **R Square** |
| :---: | :---: | :---: |
| Multi-linear Regression | 0.00005803288 | 0.9914495 | 
| K-Nearest Neighbours | 0.00013630598 | 0.9795390 |
| Bagging | 0.00008030216 | 0.9879938 |
| Random Forest | 0.00008036564 | 0.9879725 |
| Boosting | 0.00013228586 | 0.9805513 |

> The multi-linear regression model had the lowest MSE and the highest R^2 and was chosen as the best model for midfielders.

#### Forwards
| **Model** | **MSE** | **R Square** |
| :---: | :---: | :---: |
| Multi-linear Regression | 0.0008249397 | 0.4811610 |
| K-Nearest Neighbours | 0.0014446711 | 0.2622476 |
| Bagging | 0.0009745583 | 0.4216689 |
| Random Forest | 0.0009426277 | 0.4284739 |
| Boosting | 0.0013975958 | 0.1560272 |

> The multi-linear regression model had the lowest MSE and the highest R^2 and was chosen as the best model for forwards.



---

## Rarita's National Team

| **Name** | **Year of Birth** | **Position** |
| :---: | :---: | :---: |
| C. Tukamushaba | 1995 | Defender | 
| E. Mudzingwa  | 1994 | Forward |
| F. Ajio | 1991 | Forward |
| F. Akumu | 2000 | Goalkeeper |
| F. Chin | 1997 | Midfield |
| F. Ithungu | 1992 | Goalkeeper |
| F. Yunusa | 1994 | Defender |
| H. Makumbi | 1993 | Forward |
| H. Zare | 1991 | Defender |
| N. Terzi | 1998 | Defender |
| O. Wanjala | 1996 | Midfield |
| Q. bin Ismail | 1996 | Defender |
| S. Barman | 1995 | Midfield |
| X. Leroy | 1994 | Midfield |
| X. Thomas | 1991 | Forward |

---


| Age | <= 24 | 24 - 27 | 27 - 32 | 32 - 35 | 35 <= |
| :---: | :---: | :---: | :---: | :---: | :---: |
| Chance to Increase/Decrease Value | 90% chance to increase by 0% - 10% |  75% chance to increase by 0% - 10% | 100% chance to stay the same | 75% chanceto decrease by 0% - 10% | 90% chance to decrease by 0% - 10% |

Below is the function that was used to change each players skill each year.
```{r}
playerStat <- function(Born,Stat,Year) {
  ifelse((Year - Born) <= 24,Stat*(1 + ifelse(runif(1) <= 0.9, runif(1)*0.1, -runif(1)*0.1)),
         ifelse((Year - Born) <= 27,Stat*(1 + ifelse(runif(1) <= 0.75, runif(1)*0.1, -runif(1)*0.1)),
                ifelse((Year - Born) <= 32, Stat,
                       ifelse((Year - Born) <= 35, Stat*(1 - ifelse(runif(1) <= 0.75, runif(1)*0.1, -runif(1)*0.1)),
                              Stat*(1 - ifelse(runif(1) <= 0.9, runif(1)*0.1, -runif(1)*0.1))))))
}
```
>`ifelse()` had to be used instead of `if` and `else` statements so that the function could be applied in a pipeline.





THIS IS A TEST





### Congrats on completing the [2022 SOA Research Challenge](https://www.soa.org/research/opportunities/2022-student-research-case-study-challenge/)!

>Now it's time to build your own website to showcase your work.  
>To create a website on GitHub Pages to showcase your work is very easy.

This is written in markdown language. 
>
* Click [4001 link](https://classroom.github.com/a/ggiq0YzO) to accept your group assignment.
* Click [5100 link](https://classroom.github.com/a/uVytCqDv) to accept your group assignment 

#### Follow the [guide doc](Doc1.pdf) to submit your work. 
---
>Be creative! Feel free to link to embed your [data](player_data_salaries_2020.csv), [code](sample-data-clean.ipynb), [image](ACC.png) here

More information on GitHub Pages can be found [here](https://pages.github.com/)
![](Actuarial.gif)

