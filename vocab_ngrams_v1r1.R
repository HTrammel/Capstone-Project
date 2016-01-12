#---------------------------------------------------------------------
# vocab_ngrams_v1r1.R
# version 1, Release 1
#---------------------------------------------------------------------

require(quanteda)
require(dplyr)
require(stringi)
require(stringr)

if (!file.exists("Data")) {
    dir.create("Data")
}

bottom <- 5
training <- 10
CLEAN <- FALSE
DEBUG <- TRUE


source("main_functions.R")

# ####################################################
# REAL PROGRAM
# ####################################################
src_df <- setup_source()
load_text(CLEAN)

if (!file.exists("Data/en_all.Rds")) {
    blog1 <- readRDS("Data/1_en_blog.Rds")
    blog3 <- readRDS("Data/3_en_blog.Rds")
    blog5 <- readRDS("Data/5_en_blog.Rds")
    b <- c(blog1, blog3, blog5)
    rm(blog1, blog3, blog5)

    news1 <- readRDS("Data/1_en_news.Rds")
    news3 <- readRDS("Data/3_en_news.Rds")
    news5 <- readRDS("Data/5_en_news.Rds")
    n <- c(news1, news3, news5)
    rm(news1, news3, news5)

    twit1 <- readRDS("Data/1_en_twit.Rds")
    twit3 <- readRDS("Data/3_en_twit.Rds")
    twit5 <- readRDS("Data/5_en_twit.Rds")
    t <- c(twit1, twit3, twit5)
    rm(twit1, twit3, twit5)

    en_all <- as.character(c(b, n, t))
    rm(b, n, t)

    saveRDS(en_all, "Data/en_all.Rds")
} else if (!exists("en_all")) {
    en_all <- readRDS("Data/en_all.Rds")
}

gc()

if (!file.exists("Data/vocab_corp.Rds")) {
    if (DEBUG == TRUE)
        cat("...creating corpus\n")
    voc_corpus <- make_corpus(en_all)
    saveRDS(voc_corpus, "Data/vocab_corp.Rds")
} else if (!exists("voc_corpus")) {
    if (DEBUG == TRUE)
        cat("...reading corpus from file\n")
    voc_corpus <- readRDS("Data/vocab_corp.Rds")
    rm("en_all")
}

gc()

if (!file.exists("Data/vocab.Rds")) {
    if (DEBUG == TRUE)
        cat("...going to make vocabulary\n")
    vocab <- make_vocab(voc_corpus)
    saveRDS(vocab, "Data/vocab.Rds")
} else if (!exists("vocab")) {
    if (DEBUG == TRUE)
        cat("...reading vocabulary from file\n")
    vocab <- readRDS("Data/vocab.Rds")
}

if (!file.exists("Data/voc_ngrams.Rds")) {
    if (DEBUG == TRUE)
        cat("...going to make voc_ngrams\n")
    voc_ngrams <- make_ngrams(voc_corpus)
    saveRDS(voc_ngrams, "Data/voc_ngrams.Rds")
} else if (!exists("voc_ngrams")) {
    if (DEBUG == TRUE)
        cat("...reading voc_ngrams from file\n")
    voc_ngrams <- readRDS("Data/voc_ngrams.Rds")
}

gc()

############
if (!file.exists("Data/vocab_df.Rds")) {
    if (DEBUG == TRUE)
        cat("Making vocab DFM\n")
    vocab_dfm <- dfm(vocab, ignoredFeatures = stopwords("english"))

    if (DEBUG == TRUE)
        cat("Making vocab Frequency\n")
    vocab_freq <- colSums(vocab_dfm)
    vocab_tot <- sum(vocab_freq)

    if (DEBUG == TRUE)
        cat("Making vocab Names\n")
    vocab_term <- names(vocab_freq)

    if (DEBUG == TRUE)
        cat("Making vocab Data Frame\n")
    vocab_df <- data_frame(vocab_term, vocab_freq)
    vocab_df <-
        vocab_df %>% mutate(rel_freq = vocab_freq / vocab_tot)
    id <- c(1:nrow(vocab_df))
    vocab_df <- vocab_df %>% arrange(desc(rel_freq))
    vocab_df <- cbind(id, vocab_df)
    vocab_df$id[vocab_df$voc_freq <= 4] <- 0

    if (DEBUG == TRUE)
        cat("Saving vocab Data Frame\n")
    saveRDS(vocab_df, "Data/vocab_df.Rds")
    rm(list = c(
        "vocab_freq", "vocab_tot", "vocab_term", "id", "en_all","vocab_dfm"
    ))
    gc()

} else if (!exists("vocab_df")) {
    if (DEBUG == TRUE)
        cat("...reading vocab_df from file\n")
    vocab_df <- readRDS("Data/vocab_df.Rds")
}

gc()

############
if (!file.exists("Data/ngrams_df.Rds")) {
    if (DEBUG == TRUE)
        cat("Making ngrams dfm\n")
    ngrams_dfm <-
        dfm(voc_ngrams, ignoredFeatures = stopwords("english"))


    if (DEBUG == TRUE)
        cat("Making ngrams Frequency\n")
    ngram_freq <- colSums(ngrams_dfm)
    ngram_tot <- sum(ngram_freq)

    if (DEBUG == TRUE)
        cat("Making ngrams Names\n")
    ngram_name <- names(ngram_freq)

    if (DEBUG == TRUE)
        cat("Making ngrams Data Frame\n")
    ngrams_df <- data_frame(ngram_name, ngram_freq)
    ngrams_df <-
        ngrams_df %>% mutate(rel_freq = ngram_freq / ngram_tot) %>% mutate(ng_id = ngram_name)

    if (DEBUG == TRUE)
        cat("Saving ngrams Data Frame\n")
    saveRDS(ngrams_df, "Data/ngrams_df.Rds")
    rm(list = c("ngram_freq", "ngram_tot", "ngram_name","ngrams_dfm"))
    gc()

} else if (!exists("ngrams_df")) {
    if (DEBUG == TRUE)
        cat("...reading ngrams_df from file\n")
    ngrams_df <- readRDS("Data/ngrams_df.Rds")
}

gc()

# ===============================================
# map vocabulary and ngrams to ids
# ===============================================
if (DEBUG == TRUE) print ("Mapping ngrams")
z <- nrow(ngrams_df)
for (i in 1:z) {
    cat(i, " of ", z, "\n")
    t_word <- c(" ")

    ng_cnt <- str_count(ngrams_df[i,1], " ")
    if (DEBUG == TRUE) print(ng_cnt)

    if (ng_cnt > 0) {
        t_word <- get_NG_id(ngrams_df[i,1])
        if (DEBUG == TRUE) cat("....t_word:", t_word, "\n")
    } else {
        tmp_id <- getID(ngrams_df[i,1])
        if (DEBUG == TRUE) cat("....tmp_id", tmp_id, "\n")
        t_word <- tmp_id
        if (DEBUG == TRUE) cat("....t_word:", t_word, "\n")
    }
    ngrams_df[i, 4] <- t_word
    if (DEBUG == TRUE) cat(paste("....row ng_id:", ngrams_df[i,4], "\n","\n", sep=" "))
}
gc()

saveRDS(ngrams_df, "Data/ngram_id.Rds")


}
