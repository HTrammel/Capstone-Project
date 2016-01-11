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


# sources
data_dir <- "Data"
en_dir <- "Data/final/en_US"

#----------------------------------------------------------------------
# The following were provided by Ng Kuang Chern Nathaniel in the forums
#----------------------------------------------------------------------
library(parallel)
library(stringi)
my.cleaning.function <- function(text) {
    # function to deal with accented chars/unicode/numbers/punctuation/...
    text <- stringi::stri_trans_general(text, "latin-ascii")
    #text <- chartr(..., text)
    text <- gsub("&", "and", text)
    text <- gsub("[--]*", "", text)
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


#---------------------------------------------------
# build data frame to hold variables for processing
#---------------------------------------------------
setup_source <- function() {
    tmp_df <- NULL
    tmp_df <-
        data.frame(cbind(tmp_df, src_type = c("blog", "news", "twitter")), stringsAsFactors = F)
    tmp_df <-
        cbind(
            tmp_df, src_file = c(
                "Data/final/en_US/en_US.blogs.txt", "Data/final/en_US/en_US.news.txt",
                "Data/final/en_US/en_US.twitter.txt"
            ), stringsAsFactors = F
        )
    tmp_df <-
        cbind(
            tmp_df, src_name = c("en_blog", "en_news", "en_twit"), stringsAsFactors = F
        )
    tmp_df <- cbind(tmp_df, src_lines = c(899288L, 1010242L, 2360148L))
    tmp_df <-
        cbind(
            tmp_df, src_rds = c(
                "Data/USblogs.Rds", "Data/USnews.Rds", "Data/UStwitter.Rds"
            ),
            stringsAsFactors = F
        )
    return(tmp_df)
}

#---------------------------------------------------
# function to read data from source text files
#---------------------------------------------------
get_data <- function(df, stp) {
    # list from df row
    maxlines <- df$src_lines / training
    con <- file(df$src_file, open = "rb")
    tmp <-
        scan(
            con, what = "complex", nlines = maxlines, skip = maxlines * stp, fileEncoding = "UTF-16LE",
            encoding = "ASCII", blank.lines.skip = TRUE, na.strings = "", skipNul = TRUE
        )
    tmp <- parallel.clean(tmp)

    outfile <-
        paste(paste(stp, df$src_name, sep = "_"), "Rds", sep = ".")
    saveRDS(tmp, paste(data_dir, outfile, sep = "/"))
    close(con)
    rm(con)
}

#---------------------------------------------------
# function to loop through source data frame and
# call get_data incrementally
#---------------------------------------------------
load_text <- function(flag) {
    if (flag == TRUE) {
        if (DEBUG == TRUE)
            cat("Removing working files...\n")
        file.remove(
            list = c(
                "Data/voc_corpus.Rds", "Data/vocab.Rds", "Data/voc_ngrams.Rds",
                "Data/vocab_df.Rds", "Data/ngrams_df.Rds"
            )
        )

        if (DEBUG == TRUE)
            cat("Loading text files...\n")
        for (i in 1:3) {
            r <- src_df[i,]
            for (j in 1:10) {
                get_data(r, j)
            }
        }
    } else {
        print("...As is")
    }
}

#---------------------------------------------------
# function to map vocabulary from sources
#---------------------------------------------------
make_corpus <- function(lines) {
    lines <- toLower(lines)
    if (DEBUG == TRUE)
        cat("Making corpus\n")
    c <- corpus(lines)
}

#---------------------------------------------------
# function to map vocabulary from corpus
#---------------------------------------------------
make_vocab <- function(lines) {
    lines <- toLower(lines)
    if (DEBUG == TRUE)
        cat("Making vocabulary\n")
    v <-
        tokenize(
            lines, what = "word", verbose = TRUE, simplify = FALSE, removeSeparators = TRUE,
            removeNumbers = TRUE, removePunct = TRUE, removeHyphens = TRUE, removeTwitter = TRUE
        )
}

#---------------------------------------------------
# function to make ngrams from corpus
#---------------------------------------------------
make_ngrams <- function(lines) {
    lines <- toLower(lines)
    if (DEBUG == TRUE)
        cat("Making tokens\n")
    n <-
        tokenize(
            lines, what = "word", verbose = TRUE, simplify = FALSE, removePunct = TRUE,
            removeNumbers = TRUE, removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE,
            concatenator = " ", skip = 0L, ngrams = 1:4
        )
}

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

    gc()

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

    gc()


    if (DEBUG == TRUE)
        cat("Saving vocab Data Frame\n")
    saveRDS(vocab_df, "Data/vocab_df.Rds")
    rm(list = c(
        "vocab_freq", "vocab_tot", "vocab_term", "id", "en_all","vocab_dfm"
    ))
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

    gc()


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

    gc()


    if (DEBUG == TRUE)
        cat("Saving ngrams Data Frame\n")
    saveRDS(ngrams_df, "Data/ngrams_df.Rds")
    rm(list = c("ngram_freq", "ngram_tot", "ngram_name","ngrams_dfm"))
} else if (!exists("ngrams_df")) {
    if (DEBUG == TRUE)
        cat("...reading ngrams_df from file\n")
    ngrams_df <- readRDS("Data/ngrams_df.Rds")
}
gc()

# ===============================================
# map vocabulary and ngrams to ids
# ===============================================
getID <- function(in_word) {
    id <- vocab_df[which(vocab_df$voc_term == in_word), "id"]
}

getWord <- function(in_id) {
    word <- vocab_df[which(vocab_df$id == in_id), "voc_term"]
}

z <- nrow(ngrams_df)
for (i in 1:20) {
    cat(i, " of ", z, "\n")
    t_word <- c(" ")
    tw_list <- str_split(ngrams_df[i, 1], " ")
    if (DEBUG == TRUE)
        print(tw_list)

    for (j in 1:lengths(tw_list)) {
        tw <- sapply(tw_list, "[[", j)
        if (DEBUG == TRUE)
            cat(i, " word:", tw, "\n")
        tmp_id <- getID(tw)
        if (DEBUG == TRUE)
            cat("....tmp_id", tmp_id, "\n")
        t_word <- paste(t_word, tmp_id)
        if (DEBUG == TRUE)
            cat("....t_word:", t_word, "\n")
    }
    ngrams_df[i, 4] <- t_word
    if (DEBUG == TRUE)
        print(paste("....row ng_id:", ngrams_df[i, 4]))
}
gc()

saveRDS(ngrams_df, "Data/ngram_id.Rds")