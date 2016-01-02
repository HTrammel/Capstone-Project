#---------------------------------------------------------------------
#
# load_corpus.R
#
# Purpose: Explore text files provided for class
#
#---------------------------------------------------------------------
require(tm)
require(quanteda)
require(data.table)
require(dplyr)

if (!file.exists("Data")) { dir.create("Data") }

bottom <- 5
training <- 1
CLEAN <- TRUE

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
    tolower(text)
}

parallel.clean <- function(text) {
  cl <- makeCluster(3)
  clusterExport(cl, "my.cleaning.function")
  clusterEvalQ(cl, library(stringi))
  text.clean <- parSapply(cl, text, my.cleaning.function, USE.NAMES=FALSE)
  stopCluster(cl)
  text.clean
}
#----------------------------------------------------------------------


#---------------------------------------------------
# build data frame to hold variables for processing
#---------------------------------------------------
setup_source <- function() {
    tmp_df <- NULL
    tmp_df <- data.frame(
                cbind(tmp_df,
                      src_type = c("blog","news","twitter")),
                stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_file = c("Data/final/en_US/samp_blog.txt",
                                 "Data/final/en_US/samp_news.txt",
                                 "Data/final/en_US/samp_twit.txt"),
                    stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_name = c("en_blog","en_news","en_twit"),
                    stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_lines = c(1000L,1000L,1000L))
    tmp_df <- cbind(tmp_df,
                    src_rds = c("Data/samp_blogs.Rds",
                                "Data/samp_news.Rds",
                                "Data/samp_twit.Rds"),
                    stringsAsFactors = F)
    return(tmp_df)
}

#---------------------------------------------------
# function to read data from source text files
#---------------------------------------------------
get_data <- function(df, stp) {
    # list from df row
    maxlines <- df$src_lines / training
    con <- file(df$src_file, open="rb")
    tmp <- scan(con,
                what = "complex",
                nlines = maxlines,
                skip = maxlines * 0,
                encoding = "UTF-8",
                blank.lines.skip = TRUE,
                skipNul = TRUE)
    tmp <- my.cleaning.function(tmp)
    outfile <- paste(paste(stp, df$src_name, sep = "_"), "Rds", sep = ".")
    saveRDS(tmp, paste(data_dir, outfile, sep ="/"))
    close(con)
}

#---------------------------------------------------
# function to loop through source data frame and
# call get_data incrementally
#---------------------------------------------------
load_text <- function(flag) {
  if (flag == TRUE) {
    print("Loading text files")
    for (i in 1:3) {
        r <- src_df[i,]
        for (j in 1:training) {
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
map_vocabulary <- function(lines) {
    print("Making vocabulary")
    lines <- toLower(lines)
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE
                       )
    words <- do.call(c, chunks)
    #words <- as.factor(words)
}

#---------------------------------------------------
# function to create ngrams from source text files
#---------------------------------------------------
map_ngrams <- function(lines) {
    print("Making ngrams")
    lines <- toLower(lines)
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = " ",
                       ngrams = 1:5
                    )
    words <- do.call(c, chunks)
    #words <- as.factor(words)
}

#---------------------------------------------------
# function to reduce terms to smaller form
#---------------------------------------------------
reduce_map <- function(terms) {
    word_freq <- table(terms)
    my_dt <- data.table(word_freq)
    # setkey(my_dt, N)
    # my_dt <- my_dt[N > bottom]
    # my_dt[order(-N)]
    # my_dt[1] <- as.factor(my_dt[1])
    # my_dt
}


src_df <- setup_source()
load_text( CLEAN )

if (!file.exists("Data/en_all.Rds")){

    blog1 <- readRDS("Data/1_en_blog.Rds")
    #blog3 <- readRDS("Data/3_en_blog.Rds")
    #blog5 <- readRDS("Data/5_en_blog.Rds")
    b <- c(blog1) #, blog3, blog5)

    news1 <- readRDS("Data/1_en_news.Rds")
    #news3 <- readRDS("Data/3_en_news.Rds")
    #news5 <- readRDS("Data/5_en_news.Rds")
    n <- c(news1) #, news3, news5)

    twit1 <- readRDS("Data/1_en_twit.Rds")
    #twit3 <- readRDS("Data/3_en_twit.Rds")
    #twit5 <- readRDS("Data/5_en_twit.Rds")
    t <- c(twit1) #, twit3, twit5)

    en_all <- as.character(c(b,n,t))
    rm("b","n","t")

    saveRDS(en_all, "Data/en_all.Rds")
} else if (!exists("en_all")) {
    en_all <- readRDS("Data/en_all.Rds")
}


if (!file.exists("Data/vocab.Rds")) {
  vocab <- map_vocabulary(en_all)
  saveRDS(vocab, "Data/vocab.Rds")
} else if (!exists("vocab")) {
  vocab <- readRDS("Data/vocab.Rds")
}

if (!file.exists("Data/n_grams.Rds")) {
  n_grams <- map_ngrams(en_all)
  saveRDS(n_grams, "Data/n_grams.Rds")
} else if (!exists("n_grams")) {
  n_grams <- readRDS("Data/n_grams.Rds")
}

#r_vocab <- reduce_map(vocab)


voc_dfm <- dfm(vocab)
voc_colo <- collocations(vocab, method = "lr", size = c(2,3))

ng_dfm <- dfm(n_grams, ngram=1:4, removeSeparators = FALSE)

# voc_dtm <- convert(voc_dfm, to = "tm" )
# ng_dtm <- convert(ng_dfm, to = "tm" )