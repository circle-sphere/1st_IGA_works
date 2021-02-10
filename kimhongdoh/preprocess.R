
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

library(tm)
library(lubridate)
library(prophet)

# ---------------------- < AUD 파일 열기 > ----------------------

#### 파일 열기

data_aud <- fread("0. 기본파일/audience_profile.csv", fill = TRUE, sep = "#")

#### 전처리

colnames(data_aud) %<>% str_replace_all("!@", "")

data_aud$device_ifa %<>% str_sub(1, -3)
data_aud$age %<>% str_sub(1, -3)
data_aud$gender %<>% str_sub(1, -3)
data_aud$marry %<>% str_sub(1, -3)
data_aud$install_pack %<>% str_sub(1, -3)
data_aud$cate_code %<>% str_sub(1, -3)
data_aud$predicted_house_price %<>% str_sub(1, -3)

#### 필요없는 변수 처리

data_aud %<>% select(-asset_index)

#### 명목형 변수 처리

data_aud$age <- paste0("X", data_aud$age)

#### 세이브 포인트

dir.create("1. preprocess/1")

# fwrite(data_aud, "1. preprocess/1/audience_profile.csv")
data_aud <- fread("1. preprocess/1/audience_profile.csv")

#### TRAIN 기준으로 나누기

data_train <- fread("0. 기본파일/train.csv", select = "device_ifa") %>% unique

#### 나누기

data_aud_train_O <- inner_join(data_train, data_aud, by = c("device_ifa"))

# data_aud_train_X <- anti_join(data_aud, data_train, by = c("device_ifa"))

rm(data_train, data_aud)

#### 세이브 포인트

dir.create("1. preprocess/2")

# fwrite(data_aud_train_O, "1. preprocess/2/audience_profile_O.csv")
# data_aud_train_O <- fread("1. preprocess/2/audience_profile_O.csv")

# fwrite(data_aud_train_X, "1. preprocess/2/audience_profile_X.csv")
# data_aud_train_X <- fread("1. preprocess/2/audience_profile_X.csv")

# ---------------------- < data_aud_train_O > ----------------------

####

data_aud_train_O_1 <- data_aud_train_O %>% select(device_ifa:marry, predicted_house_price)

data_aud_train_O_2 <- data_aud_train_O %>% select(device_ifa, cate_code)

data_aud_train_O_3 <- data_aud_train_O %>% select(device_ifa, install_pack)

rm(data_aud_train_O)

####

dir.create("1. preprocess/3")

# fwrite(data_aud_train_O_1, "1. preprocess/3/audience_profile_O_1.csv")

rm(data_aud_train_O_1)

####

data_aud_train_O_2 <- cbind(data_aud_train_O_2 %>% select(device_ifa), data_aud_train_O_2$cate_code %>% str_split(",", simplify = T))

colnames(data_aud_train_O_2)[2:ncol(data_aud_train_O_2)] <- paste0("cate_code", 1:ncol(data_aud_train_O_2))

data_aud_train_O_2 %<>% mutate_all(na_if, "")

data_aud_train_O_2 %<>% 
    gather(cate_code_num, cate_code, cate_code1:ncol(data_aud_train_O_2), na.rm = T) %>%
    select(-cate_code_num)

data_aud_train_O_2 <- cbind(data_aud_train_O_2 %>% select(device_ifa), data_aud_train_O_2$cate_code %>% str_split(":", simplify = T))

colnames(data_aud_train_O_2)[-1] <- c("cate_code", "cate_score")

data_aud_train_O_2 %<>% spread(cate_code, cate_score)

for (i in 2:ncol(data_aud_train_O_2)) {
    
    data_aud_train_O_2[, i:i] <- data_aud_train_O_2[, i:i] %>% as.character() %>% as.numeric()
    
    data_aud_train_O_2[, i:i][is.na(data_aud_train_O_2[, i:i])] <- 0
    
    print(i)
    
}; rm(i)

####

data_aud_train_O_2$cat1 <- rowSums(data_aud_train_O_2[, -1] == 1)
data_aud_train_O_2$cat2 <- rowSums(data_aud_train_O_2[, -1] == 2)
data_aud_train_O_2$cat3 <- rowSums(data_aud_train_O_2[, -1] == 3)
data_aud_train_O_2$cat4 <- rowSums(data_aud_train_O_2[, -1] == 4)
data_aud_train_O_2$cat5 <- rowSums(data_aud_train_O_2[, -1] == 5)

####

# fwrite(data_aud_train_O_2, "1. preprocess/3/audience_profile_O_2.csv")

rm(data_aud_train_O_2)

####

data_aud_train_O_3 <- cbind(data_aud_train_O_3, data_aud_train_O_3$install_pack %>% str_split(",") %>% sapply(length))

colnames(data_aud_train_O_3)[ncol(data_aud_train_O_3)] <- "install_pack_num"

####

tb <- data_aud_train_O_3$install_pack %>% str_split(",") %>% unlist(use.names = FALSE) %>% table()

tb %<>% as.data.frame()

colnames(tb)[1] <- "app"

####

data_train <- fread("0. 기본파일/train.csv", select = c("click", "device_ifa"))

data_train <- inner_join(data_train, data_aud_train_O_3 %>% select(device_ifa, install_pack))

docs <- Corpus(VectorSource(data_train$install_pack))

docs <- tm_map(docs, content_transformer(function(x) gsub(x, pattern = ",", replacement = " ")))

dtm <- DocumentTermMatrix(docs)

rm(docs)

#### 

correlation <- NULL

for (i in 1:floor(ncol(dtm)/30)) {
    
    correlation <- c(correlation,
                     cor(cbind(data_train$click,
                               as.matrix(dtm[1:nrow(dtm), ((i-1)*30+1):(i*30)])),
                         use = "complete.obs")[-1, 1])
    
    print(i/floor(ncol(dtm)/30))
    
}; rm(i)

correlation <- c(correlation,
                 cor(cbind(data_train$click,
                           as.matrix(dtm[1:nrow(dtm), ((i)*30+1):(ncol(dtm))])),
                     use = "complete.obs")[-1, 1])

#### 

correlation <- cbind(correlation %>% names(), correlation) %>% as.data.frame()

colnames(correlation)[1] <- "app"

tb %<>% left_join(correlation)

rm(correlation, dtm, data_train)

#### 

# fwrite(tb, "1. preprocess/3/install_list.csv")
# tb <- fread("1. preprocess/3/install_list.csv")

#### 

tb %<>% filter(abs(correlation) >= sort(abs(tb$correlation), decreasing = T)[100])

####

docs <- data_aud_train_O_3$install_pack %>% VectorSource %>% Corpus

docs <- tm_map(docs, content_transformer(function(x) gsub(x, pattern = ",", replacement = " ")))

dtm <- DocumentTermMatrix(docs)

rm(docs)

####

result <- as.matrix(dtm[, tb$app])

data_aud_train_O_3 <- cbind(data_aud_train_O_3 %>% select(device_ifa, install_pack_num), result)

rm(dtm, result, tb)

####

fwrite(data_aud_train_O_3, "1. preprocess/3/audience_profile_O_3.csv")

rm(data_aud_train_O_3)

####

data_aud_train_O_1 <- fread("1. preprocess/3/audience_profile_O_1.csv")
data_aud_train_O_2 <- fread("1. preprocess/3/audience_profile_O_2.csv")
data_aud_train_O_3 <- fread("1. preprocess/3/audience_profile_O_3.csv")

data_aud_train_O <- left_join(data_aud_train_O_1, data_aud_train_O_2)
data_aud_train_O <- left_join(data_aud_train_O, data_aud_train_O_3)

rm(data_aud_train_O_1, data_aud_train_O_2, data_aud_train_O_3)

data_aud_train_O %<>% select(device_ifa:predicted_house_price, cat1:install_pack_num, "01001":p0025, p10022g:p9352g)

####

fwrite(data_aud_train_O, "1. preprocess/audience_profile_O.csv")

rm(data_aud_train_O)

# ---------------------- < data_aud_train > ----------------------

####

data_aud_train_X <- fread("1. preprocess/2/audience_profile_X.csv")

dir.create("1. preprocess/4")

####

dir.create("1. preprocess/4/audience_profile_X")

split <- seq(1, data_aud_train_X %>% nrow(), length.out = 100) %>% floor
j <- 0

for (i in split) {
    
    j <- j + 1
    
    if (i == max(split)) {
        
        fwrite(data_aud_train_X[split[j-1]:split[j], ], paste0("1. preprocess/4/audience_profile_X/audience_profile_X_", j-1,".csv"))
        
    } else {
        
        fwrite(data_aud_train_X[i:(split[j+1]-1), ], paste0("1. preprocess/4/audience_profile_X/audience_profile_X_", j,".csv"))
        
    }
    
}; rm(i, j, split, data_aud_train_X)

####

for (j in 1:99) {
    
    print(paste0(j, " : START"))
    
    ####
    
    data_aud_train_X <- fread(paste0("1. preprocess/4/audience_profile_X/audience_profile_X_", j, ".csv"))
    
    ####
    
    data_aud_train_X_1 <- data_aud_train_X %>% select(device_ifa:marry, predicted_house_price)
    
    data_aud_train_X_2 <- data_aud_train_X %>% select(device_ifa, cate_code)
    
    data_aud_train_X_3 <- data_aud_train_X %>% select(device_ifa, install_pack)
    
    rm(data_aud_train_X)
    
    ####
    
    dir.create("1. preprocess/4/audience_profile_X_1")
    
    fwrite(data_aud_train_X_1, paste0("1. preprocess/4/audience_profile_X_1/audience_profile_X_1_", j, ".csv"))
    
    rm(data_aud_train_X_1)
    
    ####
    
    data_aud_train_X_2 <- cbind(data_aud_train_X_2 %>% select(device_ifa), data_aud_train_X_2$cate_code %>% str_split(",", simplify = T))
    
    colnames(data_aud_train_X_2)[2:ncol(data_aud_train_X_2)] <- paste0("cate_code", 1:ncol(data_aud_train_X_2))
    
    data_aud_train_X_2 %<>% mutate_all(na_if, "")
    
    data_aud_train_X_2 %<>% 
        gather(cate_code_num, cate_code, cate_code1:ncol(data_aud_train_X_2), na.rm = T) %>%
        select(-cate_code_num)
    
    data_aud_train_X_2 <- cbind(data_aud_train_X_2 %>% select(device_ifa), data_aud_train_X_2$cate_code %>% str_split(":", simplify = T))
    
    colnames(data_aud_train_X_2)[-1] <- c("cate_code", "cate_score")
    
    data_aud_train_X_2 %<>% spread(cate_code, cate_score)
    
    for (i in 2:ncol(data_aud_train_X_2)) {
        
        data_aud_train_X_2[, i:i] <- data_aud_train_X_2[, i:i] %>% as.character() %>% as.numeric()
        
        data_aud_train_X_2[, i:i][is.na(data_aud_train_X_2[, i:i])] <- 0
        
        print(i)
        
    }; rm(i)
    
    ####
    
    data_aud_train_X_2$cat1 <- rowSums(data_aud_train_X_2[, -1] == 1)
    data_aud_train_X_2$cat2 <- rowSums(data_aud_train_X_2[, -1] == 2)
    data_aud_train_X_2$cat3 <- rowSums(data_aud_train_X_2[, -1] == 3)
    data_aud_train_X_2$cat4 <- rowSums(data_aud_train_X_2[, -1] == 4)
    data_aud_train_X_2$cat5 <- rowSums(data_aud_train_X_2[, -1] == 5)
    
    ####
    
    dir.create("1. preprocess/4/audience_profile_X_2")
    
    fwrite(data_aud_train_X_2, paste0("1. preprocess/4/audience_profile_X_2/audience_profile_X_2_", j,".csv"))
    
    rm(data_aud_train_X_2)
    
    #### 
    
    data_aud_train_X_3 <- cbind(data_aud_train_X_3, data_aud_train_X_3$install_pack %>% str_split(",") %>% sapply(length))
    
    colnames(data_aud_train_X_3)[ncol(data_aud_train_X_3)] <- "install_pack_num"
    
    #### 
    
    tb <- fread("1. preprocess/3/install_list.csv")
    
    #### 
    
    tb %<>% filter(abs(correlation) >= sort(abs(tb$correlation), decreasing = T)[100])
    
    ####
    
    docs <- data_aud_train_X_3$install_pack %>% VectorSource %>% Corpus
    
    docs <- tm_map(docs, content_transformer(function(x) gsub(x, pattern = ",", replacement = " ")))
    
    dtm <- DocumentTermMatrix(docs)
    
    rm(docs)
    
    ####
    
    result <- as.matrix(dtm[, tb$app])
    
    data_aud_train_X_3 <- cbind(data_aud_train_X_3 %>% select(device_ifa, install_pack_num), result)
    
    rm(dtm, result, tb)
    
    ####
    
    dir.create("1. preprocess/4/audience_profile_X_3")
    
    fwrite(data_aud_train_X_3, paste0("1. preprocess/4/audience_profile_X_3/audience_profile_X_3_", j,".csv"))
    
    rm(data_aud_train_X_3)
    
    ####
    
    print(paste0(j, " : FINISHED"))

}

rm(j)

####

dir.create("1. preprocess/4/audience_profile_X_final")

name1 <- fread("1. preprocess/3/audience_profile_O_1.csv", nrows = 1) %>% colnames()
name2 <- fread("1. preprocess/3/audience_profile_O_2.csv", nrows = 1) %>% colnames()
name3 <- fread("1. preprocess/3/audience_profile_O_3.csv", nrows = 1) %>% colnames()

data_aud_train_X_final <- NULL

for (j in 1:50) {
    
    data_aud_train_X_1 <- fread(paste0("1. preprocess/4/audience_profile_X_1/audience_profile_X_1_", j,".csv"), select = name1)
    data_aud_train_X_2 <- fread(paste0("1. preprocess/4/audience_profile_X_2/audience_profile_X_2_", j,".csv"), select = name2)
    data_aud_train_X_3 <- fread(paste0("1. preprocess/4/audience_profile_X_3/audience_profile_X_3_", j,".csv"), select = name3)
    
    data_aud_train_X <- left_join(data_aud_train_X_1, data_aud_train_X_2)
    data_aud_train_X <- left_join(data_aud_train_X, data_aud_train_X_3)
    
    rm(data_aud_train_X_1, data_aud_train_X_2, data_aud_train_X_3)
    
    data_aud_train_X_final <- rbind(data_aud_train_X_final, data_aud_train_X)
    
    rm(data_aud_train_X)
    
    print(j)
    
}; rm(j)

data_aud_train_X_final %<>% select(device_ifa:predicted_house_price, cat1:install_pack_num, "01001":p0025, p10022g:p9352g)

fwrite(data_aud_train_X_final, "1. preprocess/4/audience_profile_X_final/audience_profile_X_1.csv")

data_aud_train_X_final <- NULL

for (j in 51:99) {
    
    data_aud_train_X_1 <- fread(paste0("1. preprocess/4/audience_profile_X_1/audience_profile_X_1_", j,".csv"), select = name1)
    data_aud_train_X_2 <- fread(paste0("1. preprocess/4/audience_profile_X_2/audience_profile_X_2_", j,".csv"), select = name2)
    data_aud_train_X_3 <- fread(paste0("1. preprocess/4/audience_profile_X_3/audience_profile_X_3_", j,".csv"), select = name3)
    
    data_aud_train_X <- left_join(data_aud_train_X_1, data_aud_train_X_2)
    data_aud_train_X <- left_join(data_aud_train_X, data_aud_train_X_3)
    
    rm(data_aud_train_X_1, data_aud_train_X_2, data_aud_train_X_3)
    
    data_aud_train_X_final <- rbind(data_aud_train_X_final, data_aud_train_X)
    
    rm(data_aud_train_X)
    
    print(j)
    
}; rm(j, name1, name2, name3)

data_aud_train_X_final %<>% select(device_ifa:predicted_house_price, cat1:install_pack_num, "01001":p0025, p10022g:p9352g)

fwrite(data_aud_train_X_final, "1. preprocess/4/audience_profile_X_final/audience_profile_X_2.csv")

rm(data_aud_train_X_final)

####

data_aud_train_X_final_1 <- fread("1. preprocess/4/audience_profile_X_final/audience_profile_X_1.csv")
data_aud_train_X_final_2 <- fread("1. preprocess/4/audience_profile_X_final/audience_profile_X_2.csv")

data_aud_train_X_final <- rbind(data_aud_train_X_final_1, data_aud_train_X_final_2); rm(data_aud_train_X_final_1, data_aud_train_X_final_2)

fwrite(data_aud_train_X_final, "1. preprocess/4/audience_profile_X_final/audience_profile_X.csv")

####

data_aud_train_O_final <- fread("1. preprocess/audience_profile_O.csv")

data_aud_train_final <- rbind(data_aud_train_X_final, data_aud_train_O_final); rm(data_aud_train_X_final, data_aud_train_O_final)

fwrite(data_aud_train_final, "1. preprocess/audience_profile.csv")

rm(data_aud_train_final)

# ---------------------- < train 시게열 추가 > ----------------------

####

data_train <- fread("0. 기본파일/train.csv", select = c("click", "event_datetime", "bid_id"))

####

data_time_data_train1 <- data_train %>% mutate(day = str_sub(event_datetime, 9, 10) %>% as.character(),
                                               hour = str_sub(event_datetime, 12, 13) %>% as.character(),
                                               time = paste0(str_sub(event_datetime, 1, 13), ":00:00"))

#### 60

data_time_data_train <- 
    data_time_data_train1 %>% 
    group_by(time) %>% 
    dplyr::summarise(percent = mean(click))

data_time_data_train1 %<>% select(bid_id, time)

data_time_data_train1 %<>% left_join(data_time_data_train, by=c("time" = "time"))

final_data_train <- data_time_data_train1 %>% select(-time)

colnames(final_data_train)[2] <- "min60"

####

colnames(data_time_data_train) <- c("ds", "y")

data_time_data_train$floor <- 0

data_time_data_train$cap <- 0.09

model <- prophet(
    data_time_data_train, 
    n.changepoints = 30,
    growth="logistic"
)

future <- make_future_dataframe(model, periods = 25, freq = 3600) %>% mutate(floor = 0, cap = 0.09)

forecast <- predict(model, future)

forecast$ds <- as.character(forecast$ds)

pred60 <- forecast %>% select(ds, yhat) %>% filter(ds >= "2019-10-11 00:00:00")

colnames(pred60)[2] <- "min60"

rm(data_time_data_train1, data_time_data_train, model, future, forecast)

#### 30

data_time_data_train1 <- data_train %>% mutate(day = str_sub(event_datetime, 9, 10) %>% as.character(),
                                               hour = str_sub(event_datetime, 12, 13) %>% as.character(),
                                               min1 = str_sub(event_datetime, 15, 15) %>% as.character())

data_time_data_train1$min1 %<>% as.numeric()

data_time_data_train1$min1[data_time_data_train1$min1 >= 3] <- 3
data_time_data_train1$min1[data_time_data_train1$min1 < 3] <- 0

data_time_data_train1 %<>% mutate(time = paste0(str_sub(event_datetime, 1, 14),
                                                min1,
                                                "0:00"))

data_time_data_train <- 
    data_time_data_train1 %>% 
    group_by(time) %>% 
    dplyr::summarise(percent = mean(click))

data_time_data_train1 %<>% select(bid_id, time)

data_time_data_train1 %<>% left_join(data_time_data_train, by = c("time" = "time"))

data_time_data_train1 %<>% select(-time)

final_data_train %<>% left_join(data_time_data_train1, by = c("bid_id" = "bid_id"))

colnames(final_data_train)[3] <- "min30"

### 

colnames(data_time_data_train) <- c("ds", "y")

data_time_data_train$floor <- 0.02

data_time_data_train$cap <- 0.09

model <- prophet(data_time_data_train, 
                 n.changepoints = 15,
                 growth="logistic"
)

future <- make_future_dataframe(model, periods = 49, freq=1800) %>% mutate(floor = 0.02, cap = 0.09)

forecast <- predict(model, future)

forecast$ds <- as.character(forecast$ds)

pred30 <- forecast %>% select(ds, yhat) %>% filter(ds >= "2019-10-11 00:00:00")

colnames(pred30)[2] <- "min30"

rm(data_time_data_train1, data_time_data_train, model, future, forecast)

#### 20

data_time_data_train1 <- data_train %>% mutate(day = str_sub(event_datetime, 9, 10) %>% as.character(),
                                               hour = str_sub(event_datetime, 12, 13) %>% as.character(),
                                               min1 = str_sub(event_datetime, 15, 15) %>% as.character())

data_time_data_train1$min1 %<>% as.numeric()

data_time_data_train1$min1[data_time_data_train1$min1 >= 4 & data_time_data_train1$min1 < 6] <- 4
data_time_data_train1$min1[data_time_data_train1$min1 >= 2 & data_time_data_train1$min1 < 4] <- 2
data_time_data_train1$min1[data_time_data_train1$min1 < 2] <- 0

data_time_data_train1 %<>% mutate(time = paste0(str_sub(event_datetime, 1, 14),
                                                min1,
                                                "0:00"))

data_time_data_train <- 
    data_time_data_train1 %>% 
    group_by(time) %>% 
    dplyr::summarise(percent = mean(click))

data_time_data_train1 %<>% select(bid_id, time)

data_time_data_train1 %<>% left_join(data_time_data_train, by = c("time" = "time"))

data_time_data_train1 %<>% select(-time)

final_data_train %<>% left_join(data_time_data_train1, by = c("bid_id" = "bid_id"))

colnames(final_data_train)[4] <- "min20"

### 

colnames(data_time_data_train) <- c("ds", "y")

data_time_data_train$floor <- 0.05

data_time_data_train$cap <- 0.2

model <- prophet(data_time_data_train, 
                 n.changepoints = 50,
                 growth = "logistic"
)

future <- make_future_dataframe(model, periods = 73, freq=1200) %>% mutate(floor = 0.05, cap = 0.2)

forecast <- predict(model, future)

forecast$ds <- as.character(forecast$ds)

pred20 <- forecast %>% select(ds, yhat) %>% filter(ds >= "2019-10-11 00:00:00")

colnames(pred20)[2] <- "min20"

rm(data_time_data_train1, data_time_data_train, model, future, forecast)

#### 10

data_time_data_train1 <- data_train %>% mutate(day = str_sub(event_datetime, 9, 10) %>% as.character(),
                                               hour = str_sub(event_datetime, 12, 13) %>% as.character(),
                                               min1 = str_sub(event_datetime, 15, 15) %>% as.character())

data_time_data_train1 %<>% mutate(time = paste0(str_sub(event_datetime, 1, 14),
                                                min1,
                                                "0:00"))

data_time_data_train <- 
    data_time_data_train1 %>% 
    group_by(time) %>% 
    dplyr::summarise(percent = mean(click))

data_time_data_train1 %<>% select(bid_id, time)

data_time_data_train1 %<>% left_join(data_time_data_train, by = c("time" = "time"))

data_time_data_train1 %<>% select(-time)

final_data_train %<>% left_join(data_time_data_train1, by = c("bid_id" = "bid_id"))

colnames(final_data_train)[5] <- "min10"

### 

colnames(data_time_data_train) <- c("ds", "y")

data_time_data_train$floor <- 0.03

data_time_data_train$cap <- 0.11

model <- prophet(data_time_data_train, 
                 n.changepoints = 10,
                 growth="logistic"
)

future <- make_future_dataframe(model, periods = 145, freq = 600) %>% mutate(floor = 0.03, cap = 0.11)

forecast <- predict(model, future)

forecast$ds <- as.character(forecast$ds)

pred10 <- forecast %>% select(ds, yhat) %>% filter(ds >= "2019-10-11 00:00:00")

colnames(pred10)[2] <- "min10"

rm(data_time_data_train1, data_time_data_train, model, future, forecast)

#### 5 

data_time_data_train1 <- data_train %>% mutate(day = str_sub(event_datetime, 9, 10) %>% as.character(),
                                               hour = str_sub(event_datetime, 12, 13) %>% as.character(),
                                               min1 = str_sub(event_datetime, 15, 15) %>% as.character(),
                                               min2 = str_sub(event_datetime, 16, 16) %>% as.character())

data_time_data_train1$min2 %<>% as.numeric()

data_time_data_train1$min2[data_time_data_train1$min2 >= 5 & data_time_data_train1$min2 < 10] <- 5
data_time_data_train1$min2[data_time_data_train1$min2 < 5] <- 0

data_time_data_train1 %<>% mutate(time = paste0(str_sub(event_datetime, 1, 14),
                                                min1, 
                                                min2,
                                                ":00"))

data_time_data_train <- 
    data_time_data_train1 %>% 
    group_by(time) %>% 
    dplyr::summarise(percent = mean(click))

data_time_data_train1 %<>% select(bid_id, time)

data_time_data_train1 %<>% left_join(data_time_data_train, by = c("time" = "time"))

data_time_data_train1 %<>% select(-time)

final_data_train %<>% left_join(data_time_data_train1, by = c("bid_id" = "bid_id"))

colnames(final_data_train)[6] <- "min5"

### 

colnames(data_time_data_train) <- c("ds", "y")

data_time_data_train$floor <- 0.01

data_time_data_train$cap <- 0.2

model <- prophet(data_time_data_train, 
                 n.changepoints = 15,
                 growth="logistic"
)

future <- make_future_dataframe(model, periods = 289, freq = 300) %>% mutate(floor = 0.01, cap = 0.2)

forecast <- predict(model, future)

forecast$ds <- as.character(forecast$ds)

pred5 <- forecast %>% select(ds, yhat) %>% filter(ds >= "2019-10-11 00:00:00")

colnames(pred5)[2] <- "min5"

rm(data_time_data_train1, data_time_data_train, model, future, forecast)

#### 1

data_time_data_train1 <- data_train %>% mutate(day = str_sub(event_datetime, 9, 10) %>% as.character(),
                                               hour = str_sub(event_datetime, 12, 13) %>% as.character(),
                                               min1 = str_sub(event_datetime, 15, 15) %>% as.character(),
                                               min2 = str_sub(event_datetime, 16, 16) %>% as.character())

data_time_data_train1 %<>% mutate(time = paste0(str_sub(event_datetime, 1, 14),
                                                min1, 
                                                min2,
                                                ":00"))

data_time_data_train <- 
    data_time_data_train1 %>% 
    group_by(time) %>% 
    dplyr::summarise(percent = mean(click))

data_time_data_train1 %<>% select(bid_id, time)

data_time_data_train1 %<>% left_join(data_time_data_train, by = c("time" = "time"))

data_time_data_train1 %<>% select(-time)

final_data_train %<>% left_join(data_time_data_train1, by = c("bid_id" = "bid_id"))

colnames(final_data_train)[7] <- "min1"

### 

colnames(data_time_data_train) <- c("ds", "y")

data_time_data_train$floor <- 0

data_time_data_train$cap <- 0.09

model <- prophet(data_time_data_train, 
                 n.changepoints = 20,
                 growth="logistic"
)

future <- make_future_dataframe(model, periods = 1441, freq = 60) %>% mutate(floor = 0, cap = 0.09)

forecast <- predict(model, future)

forecast$ds <- as.character(forecast$ds)

pred1 <- forecast %>% select(ds, yhat) %>% filter(ds >= "2019-10-11 00:00:00")

colnames(pred1)[2] <- "min1"

rm(data_time_data_train1, data_time_data_train, model, future, forecast)

####

fwrite(final_data_train, "1. preprocess/data_train_prophet.csv")

rm(final_data_train, data_train)

####

test_pred <- pred1 %>% left_join(pred5) %>% left_join(pred10) %>% left_join(pred20) %>% left_join(pred30) %>% left_join(pred60)

rm(pred1, pred5, pred10, pred20, pred30, pred60)

for (j in 3:ncol(test_pred)) {
    
    for (i in 2:nrow(test_pred)) {
        
        test_pred[i, j] <- ifelse(is.na(test_pred[i, j]), test_pred[i-1, j], test_pred[i, j])
        
    }
    
}

rm(i, j)

####

colnames(test_pred)[1] <- "time"

fwrite(test_pred, "1. preprocess/data_test_prophet.csv")

rm(test_pred)

# ---------------------- < train 전처리 & 병합 > ----------------------

####

data_train <- fread("0. 기본파일/train.csv")

####

data_train %<>% mutate(weekend = wday(str_sub(event_datetime, 1, 10) %>% ymd()) %>% as.character(),
                       hour = paste0("X", str_sub(event_datetime, 12, 13)))

data_train$weekend %<>% plyr::revalue(c("1" = "we", "2" = "wd", "3" = "wd", "4" = "wd", "5" = "wd", "6" = "wd", "7" = "we"))

####

data_train %<>% select(-c(device_country))

####

data_prophet <- fread("1. preprocess/data_train_prophet.csv")

data_train %<>% left_join(data_prophet)

rm(data_prophet)

####

data_aud_train_O <- fread("1. preprocess/audience_profile_O.csv")

data_train %<>% left_join(data_aud_train_O)

rm(data_aud_train_O)

####

fwrite(data_train, "1. preprocess/train.csv")
