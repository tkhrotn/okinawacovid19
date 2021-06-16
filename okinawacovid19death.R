library(readr)
library(lubridate)
library(tidyverse)
library(stringi)


system("wget -nc -r -l 1 -A pdf https://www.pref.okinawa.lg.jp/site/hoken/kansen/soumu/press/20200416_covid19_pr.html")

dir <- "www.pref.okinawa.lg.jp/site/hoken/kansen/soumu/press/documents/"


pdfs <- list.files(dir)

cases <- tibble()
for (p in pdfs) {
  no <- system(paste0("pdftotext -raw ", dir, p, " - | tr -d '\\f' | grep 例目"), intern = T)
  if (length(no) != 0) {
    agesex <- system(paste0("pdftotext -raw ", dir, p, " - | tr -d '\\f' | grep 年代性別"), intern = T)
    death <- system(paste0("pdftotext -raw ", dir, p, " - | tr -d '\\f' | grep 死亡確認"), intern = T)
    positive <- system(paste0("pdftotext -raw ", dir, p, " - | tr -d '\\f' | grep -e 陽性確認 -e 陽性を確認 -e 陽性判明"), intern = T)
    report <- gsub(" ", "", stri_trans_nfkc(system(paste0("pdftotext ", dir, p, " - | head -n 1"), intern = T)))
    if (length(agesex) != 0) {
      n <- as.numeric(gsub("【死亡例", "", gsub("例目】", "", no)))
      a <- strsplit(gsub(" ", "", gsub("非公表 ", "非公表,", gsub("以上 ", "以上,", gsub("代 ", "代,", gsub("・", " ", gsub("年代性別：", "", agesex)))))), ",")
      d <- gsub("死亡確認", "", gsub("。", "", death))
      p <- positive
      
      for (i in 1:length(n)) {
        if (length(a) < i) {
          cases <- rbind(cases, c(n[i], NA, NA, NA, NA, report))
        } else {
          cases <- rbind(cases, c(n[i], a[[i]][1], a[[i]][2], d[i], p[i], report))
        }
      }
    } else {
      agesex <- system(paste0("pdftotext -raw ", dir, p, " - | tr -d '\\f' | grep 年代"), intern = T)
      if (length(grep("（１例目）", no)) == 1) {
        cases <- rbind(cases, c(1, "70代", NA, NA, NA, report))
      } else {
        n <- as.numeric(stri_trans_nfkc(gsub("【死亡例", "", gsub("例目】", "", no))))
        a <- strsplit(agesex, " ")
        d <- gsub("死亡確認", "", gsub("。", "", death))
        p <- positive
        for (i in 1:length(n)) {
          if (length(a) < i) {
            cases <- rbind(cases, c(n[i], NA, NA, NA, NA, report))
          } else {
            x <- strsplit(a[[i]], "：")
            cases <- rbind(cases, c(n[i], x[[1]][2], x[[2]][2], d[i], p[i], report))
          }
        }
      }
    }
  }
}
cases <- rbind(cases, c(11, NA, NA, "８月１３日", "8月2日", "令和2年8月14日(金)"))
colnames(cases) <- c("No", "Age", "Sex", "Death", "Confirmed", "Published")
cases$No <- as.numeric(cases$No)

cases$Death[cases$No == 61] <- "10月26日"
cases$Death[cases$No == 84] <- "1月2日"
cases$Death[cases$No == 136] <- "4月25日"

cases$Confirmed[cases$No == 45] <- "9月10日"

cases$Death[grep("非公表", cases$Death)] <- NA
cases$Death <- gsub("令和3年", "", gsub("月", "-", gsub(" ", "", stri_trans_nfkc(sapply(strsplit(cases$Death, "日"), function(x) {x[1]})))))
cases$Death[cases$No <= 81 & !is.na(cases$Death)] <- paste0("2020-", cases$Death[cases$No <= 81 & !is.na(cases$Death)])
cases$Death[cases$No > 81 & !is.na(cases$Death)] <- paste0("2021-", cases$Death[cases$No > 81 & !is.na(cases$Death)])

cases$Confirmed[grep("非公表", cases$Confirmed)] <- NA
cases$Confirmed <- gsub("令和3年", "", gsub("月", "-", gsub(" ", "", stri_trans_nfkc(sapply(strsplit(cases$Confirmed, "日"), function(x) {x[1]})))))
cases$Confirmed[cases$No <= 83 & !is.na(cases$Confirmed)] <- paste0("2020-", cases$Confirmed[cases$No <= 83 & !is.na(cases$Confirmed)])
cases$Confirmed[cases$No %in% c(85, 86, 88) & !is.na(cases$Confirmed)] <- paste0("2020-", cases$Confirmed[cases$No %in% c(85, 86, 88) & !is.na(cases$Confirmed)])
cases$Confirmed[cases$No > 89 & !is.na(cases$Confirmed)] <- paste0("2021-", cases$Confirmed[cases$No > 89 & !is.na(cases$Confirmed)])
cases$Confirmed[cases$No %in% c(84, 87, 89) & !is.na(cases$Confirmed)] <- paste0("2021-", cases$Confirmed[cases$No %in% c(84, 87, 89) & !is.na(cases$Confirmed)])

cases$Published <- gsub("令和2年", "2020-", gsub("令和3年", "2021-", gsub("月", "-", gsub(" ", "", stri_trans_nfkc(sapply(strsplit(cases$Published, "日"), function(x) {x[1]}))))))


cases$Age <- stri_trans_nfkc(cases$Age)
cases$Age[grep("非公表", cases$Age)] <- NA
cases$Age[cases$Age == "80歳以上"] <- "80代"
cases$Age[cases$Age == "70歳以上"] <- "70代"
cases$Age[cases$Age == "90代"] <- "90歳以上"
cases$Age[cases$Age == "90代歳以上"] <- "90歳以上"
cases <- cases[order(cases$No),]

cases$Death <- ymd(cases$Death)
cases$Confirmed <- ymd(cases$Confirmed)
cases$Published <- ymd(cases$Published)

colnames(cases) <- c("症例No", "年代", "性別", "死亡日", "陽性確認日", "公表日")
write_excel_csv(cases, "okinawacovid19death.csv")
