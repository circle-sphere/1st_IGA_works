
setwd(paste0("C:/Users/", Sys.info()[8], "/Desktop/kimhongdoh"))
# 압축된 폴더를 바탕화면에 놔두고 실행해주세요

# ---------------------- < 기본 패키지 > ----------------------

options(scipen = 999)

rm(list = ls())

library(tidyverse)
library(plyr)
library(magrittr)
library(data.table)
library(gridExtra)
library(stringr)

# ---------------------- < 추가 패키지 > ----------------------

library(tictoc)
library(catboost)

# ---------------------- < 준비 1 > ----------------------

#### 데이터 불러오기

data_train <- fread("1. preprocess/train.csv", drop = c("event_datetime", "bid_id"), stringsAsFactors = T)

####

data_train$marry[data_train$marry == ""] <- NA
data_train$gender[data_train$gender == ""] <- NA
data_train$age[data_train$age == ""] <- NA

#### 

# data_train %<>% select(-c(min60:min5))
# data_test %<>% select(-c(min60:min5))

# ---------------------- < 파라미터 > ----------------------

#### 

params <- list(
    
    random_seed = 1,
    
    loss_function = "Logloss",
    # custom_loss = "AUC", 
    logging_level = "Verbose",
    
    depth = 6,
    l2_leaf_reg = 3,
    rsm = 1,
    border_count = 254, 
    
    iterations = 500,
    learning_rate = 0.1,
    
    max_ctr_complexity = 4, # 1 일때는 너무 logloss 안 좋음, 3~4일 때는 시간이 오래 걸림... 그래도 일단 4!
    boosting_type = "Plain", # Plain이 압승
    bootstrap_type = "MVS", # No가 더 낮긴 했는데 일단 여지를 주자
    sampling_frequency = "PerTreeLevel", # 압승
    one_hot_max_size = 2,
    leaf_estimation_method = "Newton",
    leaf_estimation_backtracking = "AnyImprovement",
    score_function = "Cosine",
    model_shrink_rate = 0,
    feature_border_type = "GreedyLogSum",
    model_size_reg = 0.5,
    sparse_features_conflict_fraction = 0,
    counter_calc_method = "Full"
    
)

# ---------------------- < 모델링 > ----------------------

#### 

setwd(paste0("C:/Users/", Sys.info()[8], "/Desktop/kimhongdoh/2. model"))

####

click <- data_train$click

data_train %<>% select(-click) %>% catboost.load_pool(label = click)

rm(click)

####

time_start <- Sys.time(); tic()

model <- catboost.train(learn_pool = data_train, 
                        params = params)

time_finish <- Sys.time(); toc()

####

catboost.save_model(model, "model.rds")
