
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
library(lubridate)
library(catboost)

# ---------------------- < 준비 1 > ----------------------

tic()

#### 데이터 불러오기

data_test <- fread("0. 기본파일/test.csv", drop = c("device_country"), stringsAsFactors = T)

#### 

data_test %<>% mutate(weekend = wday(str_sub(event_datetime, 1, 10) %>% 
                                         ymd()) %>% as.character() %>% plyr::revalue(c("6" = "wd", "7" = "we")),
                      hour = paste0("X", str_sub(event_datetime, 12, 13)))

#### 

bid_id <- data_test %>% select(bid_id)

data_test %<>%
    mutate(time = paste0(str_sub(event_datetime, 1, 16), ":00")) %>%
    left_join(fread("1. preprocess/data_test_prophet.csv")) %>%
    select(-c(time, event_datetime)) %>%
    left_join(fread("1. preprocess/audience_profile_O.csv")) %>%
    select(-bid_id) %>%
    mutate_if(is.character, as.factor) %>%
    catboost.load_pool()

bid_id %<>% cbind(catboost.predict(catboost.load_model("2. model/model.cbm"), data_test, prediction_type = "Probability"))

fwrite(bid_id, "3. predict/test_predict.csv", col.names = F)

#### 

toc()
