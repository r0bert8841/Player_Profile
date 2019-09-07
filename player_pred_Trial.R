
library(RMariaDB)
library(DBI)
library(dplyr)


drv <- dbDriver("MariaDB")
con <- dbConnect(drv, username="remote", password="", dbname ="nhl_analytics", host="192.168.1.94")

player_season <- dbReadTable(conn = con, name = 'player_season')
df <- player_season %>% 
  filter(total_coaches == 1 & !is.na(season_change)) %>% 
  mutate(
    change = 
      case_when(
        season_change > 0 ~ 1,
        season_change <= 1 ~ 0
      )
    ) %>%
  select(age,prev_season_goals_per_game,max_season_goals_per_game,min_season_goals_per_game,avg_season_goals_per_game,change )


#########################################################################
# Divide the data into Test and Training
#########################################################################

attach(df)

set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = nrow(df)*.5)
# I am creating a training sample that is have the size of df

train <- df[train_ind, ]
test <- df[-train_ind, ]
#nrow(df) 
#4843
#nrow(train) 
#2421
#nrow(test) 
#2422
#nrow(train.X) 
#998
#nrow(test.X) 
#252

#########################################################################
# KNN
#########################################################################
library(class)

set.seed(1)
train.Y <- train %>% dplyr::select(change)
test.Y <- test %>% dplyr::select(change)
# 
# train.Y <- train %>% dplyr::select(as.factor(profit))
# test.Y <- test %>% dplyr::select(as.factor(profit))

class = train.Y[,1]
test.change=test.Y[,1]

train.X <- train %>% select(age,prev_season_goals_per_game,max_season_goals_per_game,min_season_goals_per_game,avg_season_goals_per_game )
test.X <- test %>% select(age,prev_season_goals_per_game,max_season_goals_per_game,min_season_goals_per_game,avg_season_goals_per_game )

knn.pred=knn(train.X,test.X,class,k=1)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7118084

knn.pred=knn(train.X,test.X,class,k=2)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7076796

knn.pred=knn(train.X,test.X,class,k=3)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7287366

knn.pred=knn(train.X,test.X,class,k=4)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7142857

knn.pred=knn(train.X,test.X,class,k=5)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7312139

knn.pred=knn(train.X,test.X,class,k=6)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7283237 

knn.pred=knn(train.X,test.X,class,k=7)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7398844


knn.pred=knn(train.X,test.X,class,k=8)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7184145

knn.pred=knn(train.X,test.X,class,k=9)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7365813

knn.pred=knn(train.X,test.X,class,k=10)
table(knn.pred,test.change)
mean(knn.pred==test.change)
#0.7283237


#########################################################################
# Logistic Regression
#########################################################################

glm.fit=glm(as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+min_season_goals_per_game+avg_season_goals_per_game,data=train,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,data=df,type="response")

glm.pred=rep(1,nrow(test))
glm.pred[glm.probs<.5]=0

table(glm.pred,test.change)
mean(glm.pred==test.change)
#0.5206441


glm.fit=glm(as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+avg_season_goals_per_game,data=train,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,data=df,type="response")

glm.pred=rep(1,nrow(test))
glm.pred[glm.probs<.5]=0

table(glm.pred,test.change)
mean(glm.pred==test.change)
#0.5156895

glm.fit=glm(as.factor(change) ~ age+prev_season_goals_per_game+avg_season_goals_per_game,data=train,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,data=df,type="response")

glm.pred=rep(1,nrow(test))
glm.pred[glm.probs<.5]=0

table(glm.pred,test.change)
mean(glm.pred==test.change)
#0.5206441


glm.fit=glm(as.factor(change) ~ age,data=train,family=binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,data=df,type="response")

glm.pred=rep(1,nrow(test))
glm.pred[glm.probs<.5]=0

table(glm.pred,test.change)
mean(glm.pred==test.change)
#0.5532618

#########################################################################
# LDA
#########################################################################
library(MASS)

lda.fit=lda(as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+min_season_goals_per_game+avg_season_goals_per_game,data=train)
lda.fit

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,test.change)
mean(lda.class==test.change)
#0.7386457


lda.fit=lda(as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+avg_season_goals_per_game,data=train)
lda.fit

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,test.change)
mean(lda.class==test.change)
#0.7365813

lda.fit=lda(as.factor(change) ~ age+prev_season_goals_per_game+avg_season_goals_per_game,data=train)
lda.fit

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,test.change)
mean(lda.class==test.change)
#0.7349298

lda.fit=lda(as.factor(change) ~ age,data=train)
lda.fit

lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,test.change)
mean(lda.class==test.change)
#0.5730801

#########################################################################
# QDA
#########################################################################
library(MASS)

qda.fit=qda(as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+min_season_goals_per_game+avg_season_goals_per_game,data=train)
qda.fit

qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
table(qda.class,test.change)
mean(qda.class==test.change)
#0.7444261

qda.fit=qda(as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+avg_season_goals_per_game,data=train)
qda.fit

qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
table(qda.class,test.change)
mean(qda.class==test.change)
#0.7229562

qda.fit=qda(as.factor(change) ~ age,data=train)
qda.fit

qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class
table(qda.class,test.change)
mean(qda.class==test.change)
#0.5730801


#########################################################################
# Support Vector Classifiers
#########################################################################
library(e1071)

set.seed (1) 
tune.out=tune(svm ,as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+min_season_goals_per_game+avg_season_goals_per_game,data=train,kernel ="linear", ranges =list(cost=c(0.0001,0.001 , 0.01, 0.1, 1,5,10,100) )) 
summary(tune.out) 

svm.pred <- predict(tune.out$best.model,test.X)
table(svm.pred,as.factor(test.change))
mean(svm.pred==as.factor(test.change))
#0.7563997


tune.out=tune(svm ,as.factor(change) ~ age+prev_season_goals_per_game+max_season_goals_per_game+min_season_goals_per_game+avg_season_goals_per_game,data=train,kernel ="linear", ranges =list(cost=c(0.0001,0.001 , 0.01, 0.1, 1,5,10,100) )) 
summary(tune.out) 

svm.pred <- predict(tune.out$best.model,test.X)
table(svm.pred,as.factor(test.change))
mean(svm.pred==as.factor(test.change))
#0.7563997