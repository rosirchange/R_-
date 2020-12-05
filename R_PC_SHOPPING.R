# P705.R: R program for Practical Tech. BDAcourse - 05
# Jia-Sheng Heh, 02/18/2018, revised from CRAWLtest01.R (hiuDStest01.R)

wkDir = "C:/Users/陳冠勳/Desktop/BIGDATA/電蝦版爬蟲/scv檔放置處";   setwd(wkDir);

########## 1st hour: 網路爬文 ##########
library(RCurl);   
library(XML)                        
library(stringr)  
library(rvest) #開啟rvest套件
j = "RAM"

##########===== (A).文件檔案到詞語 =====##########

###====(A1)(KDD1)擷取數據(RR...csv->RR)====###

for (nameuse in 1:3){

if(nameuse == 2) {
  j = "RAM" #爬RAM有關的文章
}
if(nameuse == 1){
  j = "SSD" #爬CPU有關的文章
}
if(nameuse == 3){
  j = "CPU" #爬SSD有關的文章
}
  
for (i in 1:20){

#i=1

loc=paste("https://www.ptt.cc/bbs/PC_Shopping/search?page=",i,"&q=",as.character(j),sep="")#1
des=paste("C:/Users/陳冠勳/Desktop/BIGDATA/電蝦版爬蟲/存取網頁/",j,i,".html",sep="")  #產生網頁的路徑和檔名。#2

download.file(loc,des)            #for每次會下載網頁

title2=read_html(loc) 

title2=html_nodes(title2,".title a")   
test=html_attr(title2,"href")
for(cnt in 1:length(title2)){
  locone=paste("https://www.ptt.cc",test[cnt],sep="")
  desone=paste("C:/Users/陳冠勳/Desktop/BIGDATA/電蝦版爬蟲/test/",i,"-",j,cnt,".html",sep="")  #產生網頁的路徑和檔名。#2
  
  download.file(locone,desone)            #for每次會下載網頁

  title=read_html(locone)  
  date=read_html(locone)  
  hot=read_html(locone)  
  author=read_html(locone)  
  #輸入你要爬取網頁的網址，爬取此網頁的html資訊 
  
  title=html_nodes(title,".push-content") #回文內容
  title=html_text(title)   # 只篩選出文字
  author=html_nodes(author,".push-userid")   #回文者
  author=html_text(author)   # 只篩選出文字
  hot=html_nodes(hot,".push-tag")   #回文者是推是噓還是再回一篇
  hot=html_text(hot)   # 只篩選出文字
  date=html_nodes(date,".push-ipdatetime")   #回文者回文時間
  date=html_text(date)   # 只篩選出文字
  
  
  author=iconv(author,"UTF-8")  #若是文字出現亂碼，將格式改成可以辨識的形式
  hot=iconv(hot,"UTF-8")  #若是文字出現亂碼，將格式改成可以辨識的形式
  date=iconv(date,"UTF-8")  #若是文字出現亂碼，將格式改成可以辨識的形式
  title=iconv(title,"UTF-8")  #若是文字出現亂碼，將格式改成可以辨識的形式
  
  XX2 = NULL
  for (k in 1:length(title)) {   #-- length(XX)
    XX2 = rbind( XX2, c(hot=hot[k], date=date[k], author=author[k], title=title[k]) )
                      # hot	        date	        author	          title
                      # 推 	       "04/30 16:57 "	svd237      	: 自己用AI的感覺順序cpu ram ssd gpu，cad可能差不了
                      # → 	       "04/30 16:57 "	svd237      	: 多少
                      # → 	       "04/30 16:59 "	ray90910    	: 2D繪圖？，是的話就是CPU
                      # → 	       "04/30 17:00 "	ray90910    	: 不然就Ram
  }
  
  fname = paste0("XXPC_",j,i,"-",cnt,".csv") #3
  XXX = as.data.frame(XX2);   names(XXX) = c('hot',"date","author","title")
  write.csv(XXX, fname,fileEncoding="Big5")    #--> 通常，爬到文後，趕快存檔
}

}
  
}

#(2)(KDD2)數據探索

editcpu = NULL #初始化
editram = NULL
editssd = NULL
j = "SSD"

for(num3 in 1:3){
  
  if(num3 == 2) {
    j = "RAM" #爬RAM有關的文章
  }
  if(num3 == 1){
    j = "SSD" #爬CPU有關的文章
  }
  if(num3 == 3){
    j = "CPU" #爬SSD有關的文章
  }
for(num in 1:20){
for(num2 in 1:20){
  Rlist = paste("C:/Users/陳冠勳/Desktop/BIGDATA/電蝦版爬蟲/scv檔放置處/XXPC_",j,num,"-",num2,".csv",sep="")
data<-read.csv(Rlist, header=T, sep=",",stringsAsFactors=FALSE)
if(num3 == 1){
  editssd<-rbind(editssd,data[5]) #只入title部分的評論資料
  # 14945 obs
}
if(num3 == 2){
  editram<-rbind(editram,data[5]) #只入title部分的評論資料
  # 15214 obs
}
if(num3 == 3){
  editcpu<-rbind(editcpu,data[5]) #只入title部分的評論資料
  # 20241 obs
}
#                               title
#1  : 自己用AI的感覺順序cpu ram ssd gpu，cad可能差不了
#2                                              : 多少
#3                           : 2D繪圖？，是的話就是CPU
#4                                         : 不然就Ram
#5                          : 先打開工作管理員看哪個滿

} # for(num2)
} # for(num)
} # for(num3)

#(3)(KDD3)數據轉換:將文本轉為詞語

library(tm)
library(tmcn)
library(jiebaR)
library(stringr)
library(tidytext)
library(wordcloud)

mixseg<-worker()
seg<-mixseg[as.character(editcpu$title)]

segB<-seg[nchar(seg)>1]#table

segB_top100<-sort(table(segB),decreasing = TRUE)[1:100]#table
segB_top100=as.data.frame(segB_top100)#table->data.frame

View(segB_top100)

wordcloud(
  words = segB_top100[,1], # 或seg_top50$Var1
  freq =  segB_top100$Freq, 
  scale = c(2,.1), # 給定文字尺寸的區間（向量）
  random.order = FALSE,# 關閉文字隨機顯示 按順序
  ordered.colors = FALSE,#關閉配色順序
  rot.per = FALSE,#關閉文字轉角度
  min.freq = 7,# 定義最小freq數字 
  colors = brewer.pal(8,"Dark2")
)

mixsegC<-worker()
seg<-mixsegC[as.character(editram$title)]

seg<-seg[nchar(seg)>1]#table

segC_top100<-sort(table(seg),decreasing = TRUE)[1:100]#table
segC_top100=as.data.frame(segC_top100)#table->data.frame

View(segC_top100)

wordcloud(
  words = segC_top100[,1], # 或seg_top50$Var1
  freq =  segC_top100$Freq, 
  scale = c(2,.1), # 給定文字尺寸的區間（向量）
  random.order = FALSE,# 關閉文字隨機顯示 按順序
  ordered.colors = FALSE,#關閉配色順序
  rot.per = FALSE,#關閉文字轉角度
  min.freq = 7,# 定義最小freq數字 
  colors = brewer.pal(8,"Dark2")
)

mixsegA<-worker()
seg<-mixsegA[as.character(editssd$title)]

seg<-seg[nchar(seg)>1]#table

segA_top100<-sort(table(seg),decreasing = TRUE)[1:100]#table
segA_top100=as.data.frame(segA_top100)#table->data.frame

View(segA_top100)

wordcloud(
  words = segA_top100[,1], # 或seg_top50$Var1
  freq =  segA_top100$Freq, 
  scale = c(2,.1), # 給定文字尺寸的區間（向量）
  random.order = FALSE,# 關閉文字隨機顯示 按順序
  ordered.colors = FALSE,#關閉配色順序
  rot.per = FALSE,#關閉文字轉角度
  min.freq = 7,# 定義最小freq數字 
  colors = brewer.pal(8,"Dark2")
)

#(4)(KDD4)詞類分析

#----------------------------------------------cpu----------------------------------------------
kwKindcpu = list()
kwKindcpu$"品牌" = c("INTEL","AMD","Intel","原廠","amd")
kwKindcpu$"cp值" = c("考慮","效能","便宜","散熱","價格","升級","建議","溫度")
kwKindcpu$"型號" = c("2600","2700","8700","8400","i5","i7")
kwKindcpu$"器材" = c("顯卡","風扇","記憶體","主機板")
kwKindcpu$"系統" = c("nm","超頻","更新")
kwKindcpu$"用途" = c("遊戲")
#----------------------------------------------ssd----------------------------------------------
kwKindssd = list()
kwKindssd$"品牌" = c("美光","intel","三星","Intel")
kwKindssd$"型號" = c("MX500","SATA","TLC","500","MLC","mx500","HDD","sata","545","760","QLC","TB")
kwKindssd$"cp值" = c("便宜","比較","保固","價格","速度","GB","寫入","溫度","512","120")
kwKindssd$"器材" = c("硬碟","顆粒","散熱片","記憶體")
kwKindssd$"系統" = c("系統","資料")
kwKindssd$"用途" = c("遊戲")
#----------------------------------------------ram----------------------------------------------
kwKindram = list()
kwKindram$"品牌" = c("美光","AMD","三星","金士頓")
kwKindram$"型號" = c("GB","DDR3","2400","3000","DDR4")
kwKindram$"cp值" = c("16","便宜","32","保固","漲價","效能")
kwKindram$"器材" = c("記憶體","CPU","RAM","ram","SSD","顯卡","板子","主機板","cpu")
kwKindram$"系統" = c("超頻","雙通道")

