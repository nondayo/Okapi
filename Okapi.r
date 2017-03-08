library(jiebaR)
library(text2vec)
library(xml2)
library(dplyr)
library(httr)

# 爬蟲，將Okapi網站文章爬下來
table = lapply(c(1:9000) ,function(num) { 

    df = data.frame(number=num)
    response <- paste0("http://okapi.books.com.tw/article/", num , sep="") %>%
              as.character() %>%
              GET()
    abc <- content(response)
    if (status_code(response) == 200) {
        df$unit <- abc %>%
          xml_find_all(xpath = "//*[@id='article']/h2/em") %>%
          xml_text()
        df$title <- abc %>%
          xml_find_all(xpath = "//*[@id='article']/h1") %>%
          xml_text()
        df$writer <- abc %>%
          xml_find_all(xpath = "//*[@id='article']/p[1]/a") %>%
          xml_text() %>%
          paste0(collapse = ', ')
        df$article <- abc %>%
          xml_find_all(xpath = "//*[@id='article']/article") %>%
          xml_text()
        df$date <- abc %>%
          xml_find_all(xpath = "//*[@id='article']/p[1]") %>%
          xml_text()
        Sys.sleep(5)
        df
    }
})

df_total <- Reduce(x = table, f = rbind)

StartName1 <-regexpr("/", df_total$date)
EndName1 <- regexpr("瀏覽次數", df_total$date)
df_date <- substr(df_total$date, start = StartName1+2, stop = EndName1-2)

StartName2 <-regexpr("\\(", df_total$date)
EndName2 <- regexpr("\\)", df_total$date)
df_seen <- substr(df_total$date, start = StartName2 + 1, stop = EndName2 - 1)

df_total <- cbind(df_total[,-6], df_date, df_seen, stringsAsFactors = F)

save(df_total, file = "df_total.RData")


# 用jiebaR斷詞
cutter <- worker(bylines = T)
article_words <- sapply(df_article, function(x)
    segment(x, cutter)
    )

save(article_words, file = "article_words.RData")

#建立詞庫
library(text2vec)
# an iterator to acess tokens in each article
article_words.token <- itoken(article_words)

# to create vocabulary base on the above tokens
article_words.vocabulary <- create_vocabulary(article_words.token, ngram = c(1, 1))
article_words.vocabulary2 <- create_vocabulary(article_words.token, ngram = c(1, 2))

# 詞(terms), 在所有文章出現的總次數(terms count), 在幾篇文章裡出現過(doc_counts)
#terms: (character) vector of unique terms
#terms_counts: (integer) vector of term counts across all documents
#doc_counts: (integer) vector of document counts that contain corresponding term
head(article_words.vocabulary)
head(article_words.vocabulary2)


# enforce the encoding of terms to be 'UTF-8'
Encoding(article_words.vocabulary$vocab$terms) = 'UTF-8'

# show message
cat("\n",paste0("The vocabulary size, |V| = ",length(article_words.vocabulary$vocab$terms)),"\n") 

# show
head(article_words.vocabulary$vocab[order(-article_words.vocabulary$vocab$terms_counts)][120:150],10)
nrow(article_words.vocabulary$vocab)

# vectorization of words
article_words.token <- itoken(article_words)
article_words.vectorizer <- vocab_vectorizer(article_words.vocabulary, grow_dtm = FALSE, skip_grams_window = 5)

# construct term co-occurrence matrix according to a.token and a.vectorizer
# create_tcm(輸入值, 怎麼運算)
article_words.tcm <- create_tcm(article_words.token, article_words.vectorizer)

# show dimenstion of tcm
article_words.tcm@Dim[1]
article_words.tcm@Dim[2]

# glove = GlobalVectors$new(word_vectors_size, vocabulary, x_max, learning_rate = 0.15,
#                    max_cost = 10, alpha = 0.75, lambda = 0, shuffle = FALSE, initial = NULL)
# glove$fit(x, n_iter, convergence_tol = -1)

# Construct a Global vectors model
# x_max 一篇文章中出現多少次以上的詞就濾掉
glove = GlobalVectors$new(word_vectors_size = 100, vocabulary = article_words.vocabulary,
                          x_max = 15, learning_rate = 0.2)

# fit Glove model to input matrix x
glove$fit(article_words.tcm, n_iter = 100, closure = T)

word_vectors <- glove$get_word_vectors()
head(word_vectors)
str(word_vectors)

# word vector application
# calculate the unit vector
word.vec.norm <- sqrt(rowSums(word_vectors ^ 2))
word_vectors = word_vectors / word.vec.norm
save(word_vectors, file = "word_vectors.RData")

### write word analogy funciton
get_analogy = function(a, b, c) {
                test <- word_vectors[a, , drop = FALSE] - 
                        word_vectors[b, , drop = FALSE] + 
                        word_vectors[c, , drop = FALSE]
                
                cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
                head(sort(cos_sim[,1], decreasing = TRUE), 10)
}

# try the following analogy task
get_analogy("日本","東京","台灣")
#get_analogy("法國","巴黎","臺灣")
#get_analogy("中國","北京","臺灣")
#get_analogy("泰國","曼谷","臺灣")

# word vectors to article vectors
aw <- article_words
wv <- word_vectors

new_listnames = paste('A', df_total$number, sep = '')
names(aw) = new_listnames
str(aw[1])

#把文章向量接起來
t_article_vectors = sapply(aw, function(words){
  colSums(wv[unique(words), ])    
})

article_vectors = t(t_article_vectors)
df_clus <- as.data.frame(article_vectors)
df_clus$writer <- df_total$writer
writer_150 <- names(table(df_clus$writer)[table(df_clus$writer)>150])
df_clus <- df_clus[df_clus$writer %in% writer_150,]
#寫超過150篇文章的共有10位作者，1815篇文章
#把篩選出來的1815篇文章整理成df_clus_2
df_clus$writer_factor = as.factor(df_clus$writer)
df_clus_2 = df_clus[,setdiff(names(df_clus), c('writer'))]

#隨機森林
set.seed(5566)
df_clus.rf <- randomForest(writer_factor ~ ., df_clus_2, proximity=TRUE,
                        keep.forest=TRUE)

save(df_clus.rf, file = "df_clus.rf.RData")

#confusion matrix
(table.rf=df_clus.rf$confusion)
cat("AVERAGE CORRECTION RATIO =", sum(diag(table.rf)/sum(table.rf))*100,"%\n")
df_res = data.frame(writer = df_clus$writer, predicted = df_clus.rf$predicted)
plot(df_clus.rf)

#MDSplot(df_clus.rf, df_clus_2$writer_factor)
#顏色代表群，數字代表作者（'DL' '但唐謨' '個人意見' '博客來OKAPI編輯室' '寶妹' '張妙如' '李屏瑤' '米果' '莽斯特' '陳琡分'）
## Using different symbols for the classes:
#MDSplot(df_clus.rf, df_clus_2$writer_factor, palette =  rainbow(10) , pch=as.character(as.numeric(df_clus.rf$predicted)))
res = MDSplot(df_clus.rf, df_clus_2$writer_factor, palette =  rainbow(10) , pch=as.character(as.numeric(df_clus.rf$predicted)), k=3)

#install.packages('plot3D' ,repos='http://cran.csie.ntu.edu.tw/')
library(plot3D)
tobedraw = as.data.frame(res$points)
names(tobedraw) = list('x', 'y', 'z')
tobedraw$writer = df_clus$writer_factor
tobedraw$predicted = df_clus.rf$predicted
head(tobedraw)
scatter3D(x=tobedraw$x, y=tobedraw$y, z=tobedraw$z, 
          colvar = as.numeric(tobedraw$writer), 
          pch = as.character(as.numeric(tobedraw$predicted)))
          
# 輸入文章，讓模型預測作者
migo_1 <- readChar("米果-甘蔗的大人味.txt", nchars = file.info("米果-甘蔗的大人味.txt")$size)
migo_2 <- readChar("米果-東京人教我的雪天生活對策.txt", nchars = file.info("米果-東京人教我的雪天生活對策.txt")$size)
migo_3 <- readChar("米果-時時刻刻謹慎的日本.txt", nchars = file.info("米果-時時刻刻謹慎的日本.txt")$size)
migo_4 <- readChar("米果-突然想去家庭餐廳吃漢堡排.txt", nchars = file.info("米果-突然想去家庭餐廳吃漢堡排.txt")$size)
dan_1 <- readChar("但唐謨-看電影請勿笑得像白癡.txt", nchars = file.info("但唐謨-看電影請勿笑得像白癡.txt")$size)
dan_2 <- readChar("但唐謨-動作電影不熱血不酷.txt", nchars = file.info("但唐謨-動作電影不熱血不酷.txt")$size)
dan_3 <- readChar("但唐謨-荒島上的屍控奇幻旅程.txt", nchars = file.info("但唐謨-荒島上的屍控奇幻旅程.txt")$size)
dan_4 <- readChar("但唐謨-變遷中的美國亞裔同志影像.txt", nchars = file.info("但唐謨-變遷中的美國亞裔同志影像.txt")$size)

GuessWriter <- function(x){
    writer_aw <- segment(x, worker(bylines = T))
    rnames <- rownames(word_vectors)
    writer_aw_matched_unique <- unique(intersect(rnames, unlist(writer_aw)))

    writer_av <- colSums(word_vectors[writer_aw_matched_unique,])
    writer_av <- as.data.frame(t(writer_av))
    newnames <- paste('V', c(1:100), sep = '')
    names(writer_av) <- newnames

    writer_pred <- predict(df_clus.rf, writer_av)
    writer_pred
}

GuessWriter(migo_1)
GuessWriter(migo_2)
GuessWriter(migo_3)
GuessWriter(migo_4)
GuessWriter(dan_1)
GuessWriter(dan_2)
GuessWriter(dan_3)
GuessWriter(dan_4)