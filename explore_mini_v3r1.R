#=====================================================================
# explore_mini_v1r1.R
#=====================================================================
require(quanteda)
require(dplyr)

#----------------------------------------------------------------------
# The following were provided by Ng Kuang Chern Nathaniel in the forums
#----------------------------------------------------------------------
library(parallel)
library(stringi)
my.cleaning.function <- function(text) {
    # function to deal with accented chars/unicode/numbers/punctuation/...
    text <- stringi::stri_trans_general(text, "Latin-ASCII")
    text <- gsub("&", "and", text)
    tolower(text)
}

parallel.clean <- function(text) {
    cl <- makeCluster(3, output = "")
    clusterExport(cl, "my.cleaning.function")
    clusterEvalQ(cl, library(stringi))
    text.clean <-
        parSapply(cl, text, my.cleaning.function, USE.NAMES = FALSE)
    stopCluster(cl)
    text.clean
}
#----------------------------------------------------------------------

maxlines <- 3000L

src_file <- "Data/all_samp.txt"
cln_file <- "Data/all_clean.txt"

con <- file(src_file, open = "rb")
txt <- scan(
    con,
    what = "complex",
    nlines = maxlines,
    skip = maxlines * 0,
    fileEncoding = "UTF-16LE",
    encoding = "ASCII",
    blank.lines.skip = TRUE,
    na.strings = "",
    skipNul = TRUE
)
txt <- parallel.clean(txt)
close(con)
con <- file(cln_file, "w")
cat(txt, file = con, fill = FALSE)
close(con)

txt_tf <-
    textfile("Data/all_clean.txt", encodingFrom = "Windows-1252", encodingTo = "ASCII")
cat("Making corpus\n")
samp_corpus <- corpus(txt_tf)

cat("Making ngrams\n")
cat("...unigram / vocabulary")
ng_1_tok <- tokenize(
    samp_corpus,
    what = "word",
    verbose = TRUE,
    simplify = FALSE,
    removePunct = TRUE,
    removeNumbers = TRUE,
    concatenator = " ",
    skip = 0L,
    ngrams = 1L
)
ng_1_dfm <- dfm(ng_1_tok,
                language = "english",
                ignoredFeatures = stopwords("english"))
unigram_freq <- colSums(ng_1_dfm)
unigram_tot <- sum(unigram_freq)
unigram_names <- names(unigram_freq)
unigram_df <- data_frame(word = unigram_names, freq = unigram_freq)
unigram_df <- unigram_df %>% mutate(rel_freq = freq / unigram_tot)
id <- c(1:nrow(unigram_df))
unigram_df <- unigram_df %>% arrange(desc(rel_freq))
unigram_df <- cbind(id, unigram_df)
saveRDS(unigram_df,"Data/unigram_df.Rds")


cat("...bigrams")
ng_2_tok <- tokenize(
    samp_corpus,
    what = "word",
    verbose = TRUE,
    simplify = FALSE,
    removePunct = TRUE,
    removeNumbers = TRUE,
    concatenator = " ",
    skip = 0L,
    ngrams = 2L
)
bigram_dfm <- dfm(ng_2_tok,
                  language = "english",
                  ignoredFeatures = stopwords("english"))
bigram_freq <- colSums(bigram_dfm)
bigram_tot <- sum(bigram_freq)
bigram_names <- names(bigram_freq)
bigram_df <- data_frame(ng_value = bigram_names, freq = bigram_freq)
bigram_df <- bigram_df %>%
    mutate(rel_freq = freq / bigram_tot) %>%
    mutate(ng_id = ng_value)
id <- c(1:nrow(bigram_df))
bigram_df <- bigram_df %>% arrange(desc(rel_freq))
bigram_df <- cbind(bigram_df,id)
saveRDS(bigram_df,"Data/bigram_df.Rds")

cat("...trigrams")
ng_2_dfm <- dfm(
    samp_corpus,
    what = "word",
    language = "english",
    verbose = TRUE,
    simplify = FALSE,
    ignoredFeatures = stopwords("english"),
    concatenator = " ",
    skip = 0L,
    ngrams = 3L
)
trigram_freq <- colSums(ng_2_dfm)
trigram_tot <- sum(trigram_freq)
trigram_names <- names(trigram_freq)
trigram_df <-
    data_frame(ng_value = trigram_names, freq = trigram_freq)
trigram_df <- trigram_df %>%
    mutate(rel_freq = freq / trigram_tot) %>%
    mutate(ng_id = ng_value)
id <- c(1:nrow(trigram_df))
trigram_df <- trigram_df %>% arrange(desc(rel_freq))
trigram_df <- cbind(trigram_df,id)
saveRDS(trigram_df,"Data/trigram_df.Rds")

cat("...quadgrams")
ng_2_dfm <- dfm(
    samp_corpus,
    what = "word",
    language = "english",
    verbose = TRUE,
    simplify = FALSE,
    ignoredFeatures = stopwords("english"),
    concatenator = " ",
    skip = 0L,
    ngrams = 4L
)
quadgram_freq <- colSums(ng_2_dfm)
quadgram_tot <- sum(quadgram_freq)
quadgram_names <- names(quadgram_freq)
quadgram_df <-
    data_frame(ng_value = quadgram_names, freq = quadgram_freq)
quadgram_df <- quadgram_df %>%
    mutate(rel_freq = freq / quadgram_tot) %>%
    mutate(ng_id = ng_value)
id <- c(1:nrow(quadgram_df))
quadgram_df <- quadgram_df %>% arrange(desc(rel_freq))
quadgram_df <- cbind(quadgram_df,id)
saveRDS(quadgram_df,"Data/quadgram_df.Rds")


# rm(list = c("voc_mini_freq","voc_mini_tot","voc_mini_names","id","voc_mini_dfm"))
# rm(list = c("con","bigram_freq","bigram_tot","bigram_names","bigram_dfm"))
# rm(list = c("samp_corpus","samp_ngrams","samp_vocab","src_file","maxlines"))
