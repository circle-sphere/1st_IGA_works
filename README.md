# 1st_IGA_works


----------- 실행환경 ------------

OS : Windows 10 Home 64비트
CPU : i7-7700HQ 
GPU : NVIDIA Geforce GTX 1060
메모리 : 32768MB RAM
사용툴 : R (R studio)
R version : 3.6.2
RStudio Version : RStudio Desktop 1.2.5033

----------- R 필요패키지 ------------

tidyverse
plyr
magrittr
data.table
stringr
tm
rlist
prophet
lubridate
catboost
devtools
parallel
doSNOW
foreach

----------- 설명 ------------

- R version은 3.6.2를 기준으로 사용했습니다.

- [중요] 실행환경은 각 모듈마다의 R script(3개의 R파일)를 만들어 놓았습니다. 이를 [R x64 3.6.2] 를 실행시키고 차례대로(preprocess - model - predict) 전체를 복사 붙여넣기 하여 진행합니다.
 ㄴ 이때 모든 R파일과 기본에 주어진 모든 파일 (audience_profile.csv, test.csv, train.csv)을 주어진 모듈과 같은 디렉토리에 넣고 실행해주십시오
 ㄴ 위 스크립트들은 모두 UTF-8로 인코딩되었습니다.
 ㄴ 각 스크립트마다 붙여넣기를 하기 전에 상단 워킹 디렉토리를 설정하는 부분이 있습니다. 현재 스크립트들과 파일들이 있는 디렉토리로 수정해야 합니다.
  -> setwd("작업디렉토리")를 실행해주세요. ex) 바탕화면에서 작업하는 경우, setwd("C:/Users/wongu/Desktop")

- 주어진 패키지 설치는 preprocess.R을 실행하면 초반에 설치되어집니다.
- 실행 순서는 [preprocess.R] -> [model.R] -> [predict.R] 입니다.
- 각각 모듈에서 실행되어 나오는 파일들은 이름과 순서번호를 부여받은 폴더 안에 저장되어집니다.
