# load the libraries
library(quanteda)
library(dplyr)
library(seededlda)

# read the data
dta <- read.csv('weibo.csv', header = TRUE)
head(dta)

# etract texts from the html file 
cleanFun <- function(text) {
return(gsub("#.*?#", "", text))}
dta$text = cleanFun(dta$text)
head(dta$text)

# remove unnecessary words
names <- c("蔡徐坤","cxk","坤坤","kun","ikun","成毅","迪丽热巴","gj","热巴","dlrb","范冰冰","范爷","冰冰","fbb","龚俊","华晨宇","cy","花花","hcy","胡一天","hyt","鞠婧祎","jjy","李沁","lq","刘诗诗","诗诗","lss","刘雨昕","lyx","谭松韵","tsy","王俊凯","wjk","凯凯","肖战","xz","daytoy","战哥","谢娜","xn","娜姐","娜娜","薛之谦","xzq","杨超越","超越","松韵","ycy","张子枫","子枫","zzf","周深","深深","朱一龙","一龙","zyl","全文","图片","视频")
for (name in names){
  dta$text = gsub(name,"",dta$text)
}
head(dta$text)

# create the corps
corp <- corpus(dta, text_field = 'text')
head(corp)

# generate tokens
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords("zh_cn", source = "marimo"), min_nchar = 2) %>% 
  tokens_remove(pattern = c("@*","#*#")) %>%
  tokens_keep(pattern = "^\\p{script=Hani}+$", valuetype = 'regex')
print(toks[2], max_ndoc = 1, max_ntok = -1)

# generate dfm
dfm <- dfm(toks)
dfm_2 <- dfm_trim(dfm,min_termfreq = 50)

# descriptive analysis
stat <- textstat_summary(corp)
quantile(stat$chars)
textstat_frequency(dfm,n =50)

# grid search of the parameter
k <- c(2,3,4,5,6,7,8,9,10)
for (i in k) {
  lda <- textmodel_lda(dfm_2,k = i)
  print(paste0("k=",i))
  print(terms(lda,20))
  print("----------------------")
}

# the output of final model
lda <- textmodel_lda(dfm_2, k = 4)
terms(lda, 50)
