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
                    src_file = c("Data/final/en_US/samp_blog.txt",
                                 "Data/final/en_US/samp_news.txt",
                                 "Data/final/en_US/samp_twit.txt"),
                    stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_name = c("smp_blog", "smp_news", "smp_twit"),
                    stringsAsFactors = F)
    tmp_df <- cbind(tmp_df,
                    src_lines = c(1000L,1000L,1000L))
    tmp_df <- cbind(tmp_df,
                    src_rds = c("Data/1_smp_blog.Rds",
                                "Data/1_smp_news.Rds",
                                "Data/1_smp_twit.Rds"),
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
                fileEncoding = "UTF-16LE",
                encoding = "ASCII",
                blank.lines.skip = TRUE,
                na.strings = "",
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
        if (file.exists("Data/vc_samp.Rds")) {file.remove("Data/vc_samp.Rds")}
        if (file.exists("Data/ng_samp.Rds")) {file.remove("Data/ng_samp.Rds")}
        if (file.exists("Data/corp_samp.Rds"))  {file.remove("Data/corp_samp.Rds")}
        if (file.exists("Data/vocab_samp.Rds"))  {file.remove("Data/vocab_samp.Rds")}

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
    chunks <- corpus(lines)
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
                       simplify = FALSE,
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE
    )
}


#---------------------------------------------------
# function to map vocabulary from sources
#---------------------------------------------------
tok_corpus <- function(lines, toke) {
    lines <- toLower(lines)
    cat(paste("Making vocabulary", toke, "\n", sep=" "))
    chunks <- tokenize(lines,
                       what = "word",
                       verbose = TRUE,
                       simplify = FALSE,
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = " ",
                       ngrams = 1:i
    )
}

#---------------------------------------------------
# function to create ngrams from source text files
#---------------------------------------------------
make_ngrams <- function(lines) {
    lines <- toLower(lines)
    cat("Making ngrams\n")
    chunks <- ngrams(lines, n = c(1,4) )
}




src_df <- setup_source()
load_text( CLEAN )

if (!file.exists("Data/smp_all")){
    cat("...Building smp_all\n")
    blog1 <- readRDS("Data/1_smp_blog.Rds")
    b <- c(blog1) #, blog3, blog5)
    rm("blog1")

    news1 <- readRDS("Data/1_smp_news.Rds")
    n <- c(news1) #, news3, news5)
    rm("news1")

    # twit1 <- readRDS("Data/1_smp_twit.Rds")
    twit1 <- ""
    t <- c(twit1) #, twit3, twit5)
    rm("twit1")

    smp_all <- as.character(c(b,n,t))
    rm("b","n","t")

    saveRDS(smp_all, "Data/samp_all")
} else if (!exists("smp_all")) {
    smp_all <- readRDS("Data/samp_all")
}


if (!file.exists("Data/corp_samp.Rds")) {
    cat("...creating corpus\n")
    voc_corpus <- make_corpus(smp_all)
    saveRDS(voc_corpus, "Data/corp_samp.Rds")
} else if (!exists("voc_corpus")) {
    cat("...reading corpus from file\n")
    voc_corpus <- readRDS("Data/corp_samp.Rds")
}

# if (!file.exists("Data/vocab_samp.Rds")) {
#     cat("...going to make vocabulary\n")
#     vocab <- map_vocabulary(voc_corpus)
#     saveRDS(vocab, "Data/vocab_samp.Rds")
# } else if (!exists("vocab")) {
#     cat("...reading vocabulary from file\n")
#     vocab <- readRDS("Data/vocab_samp.Rds")
# }

# if (!file.exists("Data/ng_samp.Rds")) {
#   cat("...going to make n_grams\n")
#   n_grams <- make_ngrams(vocab)
#   saveRDS(n_grams, "Data/ng_samp.Rds")
# } else if (!exists("n_grams")) {
#   cat("... reading n_grams from file\n")
#   n_grams <- readRDS("Data/ng_samp.Rds")
# }

# voc_dfm <- dfm(vocab)
# voc_colo <- collocations(vocab,
#                          method = "lr",
#                          spanPunct = FALSE,
#                          size = c(2,3))
# voc_colo <- voc_colo %>% filter( count >= 5 )
# saveRDS(voc_colo, "Data/vc_samp.Rds")


cat("...going to make ngrams\n")
for (i in 1:4) {
    v <- tok_corpus(voc_corpus, i)
    outfile <- paste(paste(i, "gram_smpl", sep = "_"), "Rds", sep = ".")
    saveRDS(v, paste(data_dir, outfile, sep ="/"))
}

ng_1 <- readRDS("Data/1_gram_smpl.Rds")
ng_2 <- readRDS("Data/2_gram_smpl.Rds")
ng_3 <- readRDS("Data/3_gram_smpl.Rds")
ng_4 <- readRDS("Data/4_gram_smpl.Rds")
