#============================================
# main_functions.R
#============================================

require(quanteda)
require(dplyr)
require(stringi)
require(stringr)
require(parallel)


DEBUG <- TRUE

# sources
data_dir <- "Data"
en_dir <- "Data/final/en_US"


#----------------------------------------------------------------------
# The following were provided by Ng Kuang Chern Nathaniel in the forums
#----------------------------------------------------------------------
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

#-------------------------------------------------------------------------
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

# ---------------------------------------------------
# functions to lookup id or word
# ---------------------------------------------------
getID <- function(in_word) {
    if (is.na(in_word)) return(0)
    id <- ifelse(is.na(vocab_df[which(vocab_df$voc_term == in_word), "id"]),
                "0",
                vocab_df[which(vocab_df$voc_term == in_word), "id"])
    id <- ifelse(is.null(vocab_df[which(vocab_df$voc_term == in_word), "id"]),
                 "0",
                 vocab_df[which(vocab_df$voc_term == in_word), "id"])
    return(id)
}

getWord <- function(in_id) {
    if (ia.na(in_id)) return("unk")
	word <- ifelse(is.na(vocab_df[which(vocab_df$id == in_id), "voc_term"]),
                "unk",
                vocab_df[which(vocab_df$id == in_id), "voc_term"])
    word <- ifelse(is.na(vocab_df[which(vocab_df$id == in_id), "voc_term"]),
                "unk",
                vocab_df[which(vocab_df$id == in_id), "voc_term"])
    return(word)
}

get_NG_id <- function(ng) {
    n_word <- c(" ")
    ng_list <- str_split(ng, " ")
    if (DEBUG == TRUE) print(ng_list)

    for (j in 1:lengths(ng_list)) {
        nw <- sapply(ng_list, "[[" , j)
        if (DEBUG == TRUE) cat(i, " word:", nw, "\n")
        n_id <- getID(nw)
        if (DEBUG == TRUE) cat("....n_id", n_id, "\n")
        n_word <- paste(n_word, n_id)
        if (DEBUG == TRUE) cat("....n_word:", n_word, "\n")
    }
    return(n_word)
}

get_NG_word <- function(ng_id) {
    n_word <- c(" ")
    ng_list <- str_split(ng_id, " ")
    if (DEBUG == TRUE) print(ng_list)

    for (j in 1:lengths(ng_list)) {
        n_id <- sapply(ng_list, "[[" , j)
        if (DEBUG == TRUE) cat(i, " n_id:", n_id, "\n")
        n_wd <- getWord(n_id)
        if (DEBUG == TRUE) cat("....n_wd", n_wd, "\n")
        n_word <- paste(n_word, n_wd)
        if (DEBUG == TRUE) cat("....n_word:", n_word, "\n")
    }
    return(n_word)
}
