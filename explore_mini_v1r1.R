#=====================================================================
# explore_mini_v1r1.R
#=====================================================================
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
    tmp <- parallel.clean(tmp)
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
        cat("Removing working files...\n")
        rm(list = c("Data/vc_samp.Rds",
                    "Data/ng_samp.Rds",
                    "Data/vocab_samp.Rds"))

        cat("Loading text files...\n")
        for (i in 1:3) {
            r <- src_df[i,]
            for (j in 1:training) {
                get_data(r, j)
            }
        }
    } else {
        cat("...As is\n")
    }
}

#---------------------------------------------------
# function to map vocabulary from sources
#---------------------------------------------------
make_corpus <- function(lines) {
    lines <- toLower(lines)
    cat("Making corpus\n")
    chunks <- corpus(lines, source = "Data/final/en_US/en_sources.txt")
}

#---------------------------------------------------
# function to map vocabulary from sources
#---------------------------------------------------
map_vocabulary <- function(lines) {
    lines <- toLower(lines)
    cat("Making vocabulary\n")
    chunks <- tokenize(lines,
                       what = "word",
                       verbose = TRUE,
                       simplify = TRUE,
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       # removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = "_",
                       ngrams = 1:6
    )
}

#---------------------------------------------------
# function to create ngrams from source text files
#---------------------------------------------------
map_ngrams <- function(lines) {
    lines <- toLower(lines)
    cat("Making ngrams\n")
    chunks <- collocations(lines,
                   what = "word",
                   method = "lr",
                   spanPunct = FALSE,
                   size = 2:3
                )
}

src_df <- setup_source()
load_text( CLEAN )

if (!file.exists("Data/en_all.Rds")){
    cat("...Building en_all\n")
    blog1 <- readRDS("Data/1_en_blog.Rds")
    b <- c(blog1) #, blog3, blog5)
    rm("blog1")

    news1 <- readRDS("Data/1_en_news.Rds")
    n <- c(news1) #, news3, news5)
    rm("news1")

    twit1 <- readRDS("Data/1_en_twit.Rds")
    t <- c(twit1) #, twit3, twit5)
    rm("twit1")

    en_all <- as.character(c(b,n,t))
    rm("b","n","t")

    saveRDS(en_all, "Data/en_all.Rds")
} else if (!exists("en_all")) {
    en_all <- readRDS("Data/en_all.Rds")
}


if (!file.exists("Data/vocab_corp.Rds")) {
    voc_corpus <- make_corpus(en_all)
    saveRDS(voc_corpus, "Data/vocab_corp.Rds")
} else if (!exists("voc_corpus")) {
    voc_corpus <- readRDS("Data/vocab_corp.Rds")
}

if (!file.exists("Data/vocab_samp.Rds")) {
    vocab <- map_vocabulary(en_all)
    saveRDS(vocab, "Data/vocab_samp.Rds")
} else if (!exists("vocab")) {
    vocab <- readRDS("Data/vocab_samp.Rds")
}

if (!file.exists("Data/ng_samp.Rds")) {
  n_grams <- map_ngrams(vocab)
  saveRDS(n_grams, "Data/ng_samp.Rds")
} else if (!exists("n_grams")) {
  n_grams <- readRDS("Data/ng_samp.Rds")
}

voc_dfm <- dfm(vocab)
voc_colo <- collocations(vocab,
                         method = "lr",
                         spanPunct = FALSE,
                         size = c(2,3))
voc_colo <- voc_colo %>% filter( count >= 5 )
saveRDS(voc_colo, "Data/vc_samp.Rds")
