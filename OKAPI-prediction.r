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