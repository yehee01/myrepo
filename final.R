# 네이버 증권 외국인 보유 데이터 가져오기
library(rvest)
library(tidyverse)

# 네이버 증권 외국인 보유 url
url <- "https://finance.naver.com/sise/sise_foreign_hold.naver?sosok=0"

# 데이터 가져오기
html <- read_html(url, encoding = 'euc-kr') %>% 
  html_table() %>% 
  .[[2]] 

# 데이터프레임으로 변환
foreign_own <- as.data.frame(html) %>% 
  .[,-(11:12)] %>% 
  na.omit(.)

# foreign_own에서 우선주 종목만 추출
foreign_own_prefer <- foreign_own[grep("우", foreign_own$종목명), ] %>% 
  .[,-1] 
rownames(foreign_own_prefer) <- NULL  
foreign_own_prefer <- foreign_own_prefer[-c(11,13),]


# 우선주 데이터프레임에 있는 같은 회사의 보통주 데이터 가져오기 
url <- "https://finance.naver.com/sise/sise_market_sum.naver?&page="

# 데이터 가져오기
get_table <- function(url) {
  table <- read_html(url, encoding = 'euc-kr') %>% 
    html_table() %>% 
    .[[2]] %>% 
    filter(!is.na(N)) %>% 
    select(-토론실)
  
  return(table)
  
}

# 네이버 증권 시가총액 마지막 페이지 추출하는 함수 만들기
get_last_page <- function(url) {
  html <- read_html(url, encoding="euc-kr") # 해당 url의 html을 가져옴
  
  # 페이지 이동 링크에 대한 정보를 가져옴
  page_item <- html %>%
    html_nodes(".pgRR") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_split("page=") 
  
  # 마지막 페이지 번호를 추출
  last_page <- page_item[[1]][2]
  return(last_page)
}

# 함수 정의: 네이버 증권 시가총액 항목에서 해당 페이지의 데이터 가져와서 데이터프레임으로 만들기
get_stock_info <- function(base_url) {
  last_page <- get_last_page(paste0(base_url, 1)) # 첫 페이지에서 마지막 페이지 번호를 가져옴
  
  df <- data.frame() # 빈 데이터 프레임을 생성
  
  # 1,2,3,9 페이지에서 종목 시세 정보를 가져와서 df에 추가
  for(page in 1:last_page) {
    if(page %% 10 == 0) print(paste(page, "/ ", last_page)) # 페이지 로딩 중간 과정 출력
    df <- rbind(df, get_table(paste0(base_url, page))) # 페이지에 있는 테이블을 가져와서 df에 추가
  }
  return(df)
}

stock <- get_stock_info(url)

# stock 데이터프레임에서 foreign_own_prefer에 있는 회사와 같은 보통주 뽑기
stock[grep("LG", stock$종목명), ]
foreign_same <- stock[c(1,41,5,8,56,168,21,16,121,72),]
foreign_same <- foreign_same[,-1]

# foreign_own_prefer 우선주의 외국인 비율이 같은 회사의 보통주 비율보다 높은지 확인
foreign_own_prefer$외국인비율
foreign_same$외국인비율    # 모두 우선주가 보통주 비율보다 높음

# 네이버 증권 시가총액의 보통주와 우선주의 대부분의 외국인 비율 비교
stock[grep("삼성물산", stock$종목명), ] # 삼성물산 18.2 > 삼성물산우 16.2
stock[grep("삼성SDI", stock$종목명), ] # 삼성SDI 49.6 > 삼성SDI우 20.0
stock[grep("SK이노베이션", stock$종목명), ] # SK이노베이션 24.8 > 우 14.2
stock[grep("삼성화재", stock$종목명), ] # 삼성화재 52.6 > 우 25.2
stock[grep("미래에셋증권", stock$종목명), ] # 미래에셋증권 13.5 > 2우B 10.4 > 우 0.46

stock_prefer <- stock[grep("우", stock$종목명), ]
stock_prefer <- stock_prefer[c(1,4,6,7,9,12:15,17:28,30,32:41,44,46,47,50:54,56:59,61:63,66:68, 
                               70:74, 76, 78:82, 84:87, 89, 91:98, 100:103, 105, 107, 109:111, 
                               114:117, 119,121:124, 126, 128,129, 131:138, 140:143, 147:156, 158,159),]

par(mfrow=c(1,2))
min(foreign_own_prefer$외국인비율)

# stock에서 우선주 제외
stock[stock$외국인비율 > 31.07,]
stock_noprefer <- stock[-grep("우", stock$종목명), ]
stock_noprefer <- stock[-c(1,41,5,8,56,168,21,16,121,72),]

# foreign_own_prefer 거래량
stock$상장주식수 <- as.numeric(gsub(",", "", stock$상장주식수))
foreign_own_prefer$상장주식수 <- as.numeric(gsub(",", "", foreign_own_prefer$상장주식수))
stock_noprefer$상장주식수 <- as.numeric(gsub(",", "", stock_noprefer$상장주식수))


# 삼성전자 우선주 분석
url <- "https://finance.naver.com/item/main.naver?code=005935"
samsung_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table() %>% 
  .[[5]] %>% 
  as.data.frame()

## 시가총액, 매출액, 영업이익, 조정영업이익, 당기순이익 동일업종보다 크다.

# LG생활건강우, 아모레퍼시픽우, 아모레G우 분석
url <- "https://finance.naver.com/item/main.naver?code=051905"
LGlife_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table()  %>% 
  .[[5]] %>% 
  as.data.frame()

## 보통주보다 우선주가 외국인비율이 높은 동일업종: 아모레퍼시픽, 아모레G
## LG생활건강이 동일업종보다 시가총액, 매출액, 영업이익, 조정영업이익, 주당순이익, ROE이 가장 크다.
## 우선주 외국인 비율은 LG생활건강우, 아모레퍼시픽우, 아모레G우 순이다.
## 시가총액, 당기순이익, ROE는 LG생활건강, 아모레퍼시픽, 아모레G 순으로 LG생활건강이 가장 크다.
## 매출액은 LG생활건강, 아모레G, 아모레퍼시픽 순이다.

# LG생활건강우, 아모레퍼시픽우, 아모레G우 동일업종 분석
url <- "https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=266"
beauty_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table() %>% 
  .[[3]] %>% 
  as.data.frame(.)

beauty_ind <- beauty_ind[-1, -c(10:12)]

## LG생활건강, 아모레퍼시픽, 아모레G는 모두 시가총액, 매출액, 영업이익, 당기순이익, 주당순이익이 동일업종에서 큰 쪽에 속한다.

# LG화학우 분석
url <- "https://finance.naver.com/item/main.naver?code=051915"
LGchemi_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table()  %>% 
  .[[5]] %>% 
  as.data.frame()

## 동일업종보다 시가총액, 매출액, 영업이익, 조정영업이익, 당기순이이익, 주당순이익이 크다.

# 현대차우, 현대차2우B 분석
url <- "https://finance.naver.com/item/main.naver?code=005387"
heundai_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table()  %>% 
  .[[5]] %>% 
  as.data.frame()

## 동일업종보다 시가총액, 매출액, 영업이익, 조정영업이익, 당기순이익, 주당순이익, PER이 크다.

# 남양유업우, CJ 제일제당 분석
url <- "https://finance.naver.com/item/main.naver?code=003925"
namyang_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table()  %>% 
  .[[5]] %>% 
  as.data.frame()

## CJ 제일제당은 매출액, 영업이익, 조정영업이익이 가장 높고,
## 남양유업은 영업이익증가율이 가장 높았다.

# LG전자우 분석
url <- "https://finance.naver.com/item/main.naver?code=066575"
LGformer_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table()  %>% 
  .[[5]] %>% 
  as.data.frame()

## 시가총액, 매출액, 영업이익, 영업조정이익, 당기순이익, 주당순이익, PBR이 동일업종보다 크다.

# LG우 분석 
url <- "https://finance.naver.com/item/main.naver?code=003555"
LG_ind <- read_html(url, encoding = 'euc-kr') %>% 
  html_table()  %>% 
  .[[5]] %>% 
  as.data.frame()

## 영업이익증가율, 당기순이익, 주당순이익, ROE, PBR이 동일업종보다 크다.


## 위의 분석을 통해 보통주보다 우선주의 외국인비율이 높은 기업의 경우는
## 대체로 시가총액, 매출액, 영업이익, 당기순이익, 주당순이익 등이 동일업종보다 큰 편에 속한다는 것을 알 수 있다.


# 보통주와 우선주 괴리율
url <- "https://koreadividend.kr/disparity/"
disparity_rate <- read_html(url) %>% 
  html_table() %>% 
  as.data.frame()

## 괴리율 상위 125개의 종목들을 살펴보았을 때 외국인비율 상위 100에 해당하는 우선주들은 상위 60안에 해당하였다. 
## 외국인 비율 상위 100위에 해당하는 삼성전자우, LG우를 제외한 나머지 우선주들은 괴리율이 50~60%에 해당한다.
## 독일의 평균 괴리율 3.5%, 미국의 평균 괴리율 10~20%에 비해 매우 높은 괴리율을 보이고 있다.
## 이는 우선주가 저평가되어있음을 말한다.
## 특히 삼성전자우의 경우 시가총액이 현대차와 LG전자와 같은 다른 기업의 보통주의 시가총액보다 큰 편이고 
## 변동성이 작아 비교적 다른 우선주들에 비해 안정적이어서 의결권이 없고 우선주의 시세변동이 크다는 단점을 보완하고 
## 배당수익률이 높다는 장점이 있어 우선주 중에서 외국인 비율이 삼성전자 보통주(52.52%)보다 훨씬 높은 것으로 보인다.
