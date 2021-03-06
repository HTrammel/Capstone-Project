#---------------------------------------------------------------------
# draft_backoff.R
# version 1, Release 2
#---------------------------------------------------------------------

require(quanteda)
require(dplyr)

if (!file.exists("Data")) { dir.create("Data") }

bottom <- 5
training <- 10
CLEAN <- FALSE

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
  cl <- makeCluster(3, output="")
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
                    src_file = c("Data/final/en_US/en_US.blogs.txt",
                                 "Data/final/en_US/en_US.news.txt",
                                 "Data/final/en_US/en_US.twitter.txt"),
                    stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_name = c("en_blog","en_news","en_twit"),
                    stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_lines = c(899288L,1010242L,2360148L))
    tmp_df <- cbind(tmp_df,
                    src_rds = c("Data/USblogs.Rds",
                                "Data/USnews.Rds",
                                "Data/UStwitter.Rds"),
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
                skip = maxlines * stp,
                fileEncoding = "UTF-16LE",
                encoding = "ASCII",
                blank.lines.skip = TRUE,
                na.strings = "",
                skipNul = TRUE)
    tmp <- parallel.clean(tmp)

    outfile <- paste(paste(stp, df$src_name, sep = "_"), "Rds", sep = ".")
    saveRDS(tmp, paste(data_dir, outfile, sep ="/"))
    close(con)
    rm(con)
}

#---------------------------------------------------
# function to loop through source data frame and
# call get_data incrementally
#---------------------------------------------------
load_text <- function(flag) {
  if (flag == TRUE) {
    cat("Removing working files...\n")
    file.remove(list = c("Data/voc_corpus.Rds",
                "Data/vocab.Rds",
                "Data/voc_tokens.Rds",
                "Data/voc_df.Rds"))

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
    cat("Making corpus\n")
    chunks <- corpus(lines)
}

# #---------------------------------------------------
# # function to map vocabulary from sources
# #---------------------------------------------------
# make_vocab <- function(lines) {
#     lines <- toLower(lines)
#     cat("Making vocabulary\n")
#     chunks <- tokenize(lines,
#                        what = "word",
#                        verbose = TRUE,
#                        simplify = FALSE,
#                        removeSeparators = TRUE,
#                        removeNumbers = TRUE,
#                        removePunct = TRUE,
#                        removeTwitter = TRUE
#                     )
# }

#---------------------------------------------------
# function to map vocabulary from sources
#---------------------------------------------------
make_tokens <- function(lines) {
    lines <- toLower(lines)
    cat("Making tokens\n")
    chunks <- tokenize(lines,
                       what = "word",
                       verbose = TRUE,
                       simplify = FALSE,
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = " ",
                       skip = 0L,
                       ngrams = 1:4
                    )
}

src_df <- setup_source()
load_text( CLEAN )

if (!file.exists("Data/en_all.Rds")){

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

    en_all <- as.character(c(b,n,t))
    rm(b, n, t)

    saveRDS(en_all, "Data/en_all.Rds")
} else if (!exists("en_all")) {
    en_all <- readRDS("Data/en_all.Rds")
}


if (!file.exists("Data/vocab_corp.Rds")) {
    cat("...creating corpus\n")
    voc_corpus <- make_corpus(en_all)
    saveRDS(voc_corpus, "Data/vocab_corp.Rds")
} else if (!exists("voc_corpus")) {
    cat("...reading corpus from file\n")
    voc_corpus <- readRDS("Data/vocab_corp.Rds")
}

if (!file.exists("Data/voc_tokens.Rds")) {
  cat("...going to make voc_tokens\n")
  voc_tokens <- make_tokens(voc_corpus)
  saveRDS(voc_tokens, "Data/voc_tokens.Rds")
} else if (!exists("voc_tokens")) {
  cat("...reading voc_tokens from file\n")
  voc_tokens <- readRDS("Data/voc_tokens.Rds")
}

cat("Making DFM\n")
voc_dfm <- dfm(voc_tokens)

cat("Making Frequency\n")
voc_freq <- colSums(voc_dfm)
voc_tot <- sum(voc_freq)

cat("Making Names\n")
voc_names <- names(voc_freq)

cat("Making Data Frame\n")
voc_df <- data_frame(voc_names, voc_freq)
voc_df <- voc_df %>% mutate(rel_freq = voc_freq/voc_tot)
voc_df <- voc_df %>% mutate(word_count = stri_count_words(voc_names))

cat("Saving Data Frame\n")
saveRDS(voc_df,"Data/voc_df.Rds")

rm(list=c("voc_freq",
            "voc_names",
            "voc_corpus",
            "en_all"
        ))
