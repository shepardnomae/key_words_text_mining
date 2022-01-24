install.packages("tidyverse")
install.packages("backports")
library(tidyverse)
install.packages("RMeCab", repos = "http://rmecab.jp/R") 
library('RMeCab')
install.packages('wordcloud2') 
library('wordcloud2')

setwd("/Users/tongkimu/Documents/RStudio/text_mining")
# ------------------ 実行するコード ------------------
hokkaido_keywords_master <- RMeCabFreq("20210401_20211031_hokkaido.txt")
hokkaido_keywords_master
hokkaido_keywords_master <- hokkaido_keywords_master %>% as_tibble
hokkaido_keywords_master
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Info1=="名詞", Info2!="固有名詞")
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Term!="一", Term!="二", 
        Term!="三", Term!="四", Term!="五", Term!="六", Term!="七", Term!="八", 
        Term!="九", Term!="十", Term!="百", Term!="千", Term!="万", Term!="さん") 
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Freq>=2) %>% mutate(noun=str_match((Term), '[^0-9０-９]+')) %>% na.omit()
hokkaido_keywords_resized <- hokkaido_keywords_master %>% select(5, 4) %>% arrange(desc(Freq))
hokkaido_keywords_top30 <- hokkaido_keywords_resized[5:34, ]
hokkaido_keywords_top30
max_row <- nrow(hokkaido_keywords_resized)
hokkaido_keywords_rest <- hokkaido_keywords_resized[35:max_row, ]
hokkaido_keywords_rest
hokkaido_keywords_rest %>% wordcloud2(fontFamily="HiraKakuProN-W3", color='black', minRotation=0, maxRotation=0)
hokkaido_keywords_top30 %>% wordcloud2(fontFamily="HiraKakuProN-W3", color='black', minRotation=0, maxRotation=0)
# ------------------ 実行するコード ------------------
####### HOKKAIDO #######
hokkaido_keywords_master <- read.csv("Keyword_Hokkaido.csv")
hokkaido_keywords_master <- RMeCabFreq("20210401_20211031_niseko.txt")
hokkaido_keywords_dup <-data.frame(hokkaido_keywords_master[rep(seq_len(dim(hokkaido_keywords_master)[1]), hokkaido_keywords_master[,2]), 1], row.names = NULL)
write.csv(hokkaido_keywords_dup, "Keyword_Hokkaido_dup.csv")
hokkaido_keywords_master <- RMeCabFreq("Keyword_Hokkaido_dup.csv")
hokkaido_keywords_master
hokkaido_keywords_master <- hokkaido_keywords_master %>% as_tibble
hokkaido_keywords_master
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Info1=="名詞", Info2!="固有名詞")
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Term!="一", Term!="二", 
                                                                Term!="三", Term!="四", Term!="五", Term!="六", Term!="七", Term!="八", 
                                                                Term!="九", Term!="十", Term!="百", Term!="千", Term!="万", Term!="さん") 
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Freq>=2) %>% mutate(noun=str_match((Term), '[^0-9０-９]+')) %>% na.omit()
hokkaido_keywords_resized <- hokkaido_keywords_master %>% select(5, 4) %>% arrange(desc(Freq))
hokkaido_keywords_top30 <- hokkaido_keywords_resized[5:34, ]
hokkaido_keywords_top30
max_row <- nrow(hokkaido_keywords_resized)
hokkaido_keywords_rest <- hokkaido_keywords_resized[35:max_row, ]
hokkaido_keywords_rest
hokkaido_keywords_rest %>% wordcloud2(fontFamily="HiraKakuProN-W3", color='black', minRotation=0, maxRotation=0)
hokkaido_keywords_top30 %>% wordcloud2(fontFamily="HiraKakuProN-W3", color='black', minRotation=0, maxRotation=0)

####### HOKKAIDO #######
hokkaido_keywords_master <- read.csv("Keyword_Niseko.csv")
hokkaido_keywords_dup <-data.frame(hokkaido_keywords_master[rep(seq_len(dim(hokkaido_keywords_master)[1]), hokkaido_keywords_master[,2]), 1], row.names = NULL)
write.csv(hokkaido_keywords_dup, "Keyword_Niseko_dup.csv")
hokkaido_keywords_master <- RMeCabFreq("Keyword_Niseko_dup.csv")
hokkaido_keywords_master
hokkaido_keywords_master <- hokkaido_keywords_master %>% as_tibble
hokkaido_keywords_master
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Info1=="名詞", Info2!="固有名詞")
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Term!="一", Term!="二", 
                                                                Term!="三", Term!="四", Term!="五", Term!="六", Term!="七", Term!="八", 
                                                                Term!="九", Term!="十", Term!="百", Term!="千", Term!="万", Term!="さん") 
hokkaido_keywords_master <- hokkaido_keywords_master %>% filter(Freq>=2) %>% mutate(noun=str_match((Term), '[^0-9０-９]+')) %>% na.omit()
hokkaido_keywords_resized <- hokkaido_keywords_master %>% select(5, 4) %>% arrange(desc(Freq))
hokkaido_keywords_export <- hokkaido_keywords_resized[-c(1,2,6,7,8,9,10,11,12,14,15,16,17,18,19,20,23,24,26,27,28,29,31,32,34,38,39,40,43,45,47),]
hokkaido_keywords_top10 <- hokkaido_keywords_export[1:10, ]
hokkaido_keywords_11_30 <- hokkaido_keywords_export[11:30, ]
max_row <- nrow(hokkaido_keywords_export)
hokkaido_keywords_rest <- hokkaido_keywords_export[31:max_row, ]
hokkaido_keywords_rest
hokkaido_keywords_top10 %>% wordcloud2(fontFamily="HiraKakuProN-W3", minRotation=0, maxRotation=0)
hokkaido_keywords_rest %>% wordcloud2(fontFamily="HiraKakuProN-W3", minRotation=0, maxRotation=0)
hokkaido_keywords_11_30 %>% wordcloud2(fontFamily="HiraKakuProN-W3", minRotation=0, maxRotation=0)
write.csv(hokkaido_keywords_export, "niseko_keywords_export.csv")
reps <- max(df[,2])
df2 <- df[rep(1:nrow(df), reps), 1:2]
df <- data.frame(df)
dupdf <- df[idx,]

hokkaido_keywords_export <- read.csv("niseko_keywords_export_english.csv")
hokkaido_keywords_top10 <- hokkaido_keywords_export[1:10, ]
hokkaido_keywords_11_30 <- hokkaido_keywords_export[11:30, ]
max_row <- nrow(hokkaido_keywords_export)
hokkaido_keywords_rest <- hokkaido_keywords_export[31:max_row, ]

colornames=c("violet","indigo","blue","green","yellow","orange","red")
wavelength=c(400,425,470,550,600,630,665)
df <- data.frame(colornames, wavelength)

        # How many replicates you want of each row
        duptimes <- c(0,1,2,1,1,4,1)

        # Create an index of the rows you want with duplications
        idx <- rep(1:nrow(df), duptimes)

        # Use that index to genderate your new data frame
        dupdf <- df[idx,]

dat[rep(seq_len(dim(dat)[1]), dat$count), 2]
