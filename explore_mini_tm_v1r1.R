#=====================================================================
# explore_mini_v1r1.R
#=====================================================================
require(tm)
#require(quanteda)
require(stringi)
require(stringr)
require(dplyr)
library(parallel)

#----------------------------------------------------------------------
# The following were provided by Ng Kuang Chern Nathaniel in the forums
#----------------------------------------------------------------------
my.cleaning.function <- function(text) {
    # function to deal with accented chars/unicode/numbers/punctuation/...
    text <- stringi::stri_trans_general(text, "latin-ascii")
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

maxlines <- 3000L
src_file <- "Data/all_samp.txt"

con <- file(src_file, open="rb")
txt <- SimpleSource(con,
          what = "complex",
          nlines = maxlines,
          skip = maxlines * 0,
          fileEncoding = "UTF-16LE",
          encoding = "ASCII",
          blank.lines.skip = TRUE,
          na.strings = "",
          skipNul = TRUE)
txt <- parallel.clean(txt)
close(con)

txt <- toLower(txt)

cat("Making corpus\n")
samp_corpus <- as.VCorpus(txt)
rm(txt)



# cat("Making vocabulary\n")
# samp_vocab <- tokenize(samp_corpus,
#                    what = "word",
#                    verbose = TRUE,
#                    simplify = FALSE,
#                    removeSeparators = TRUE,
#                    removeNumbers = TRUE,
#                    removePunct = TRUE,
#                    removeHyphens = FALSE,
#                    removeTwitter = TRUE
#               )
#
# cat("Making dfm\n")
# voc_mini_dfm <- dfm(samp_vocab, ignoredFeatures = stopwords("english"))
# voc_mini_freq <- colSums(voc_mini_dfm)
# voc_mini_tot <- sum(voc_mini_freq)
# voc_mini_names <- names(voc_mini_freq)
# voc_mini_df <- data_frame(voc_term = voc_mini_names, voc_freq = voc_mini_freq)
# voc_mini_df <- voc_mini_df %>% mutate(rel_freq = voc_freq/voc_mini_tot)
# id <- c(1:nrow(voc_mini_df))
# voc_mini_df <- voc_mini_df %>% arrange(desc(rel_freq))
# voc_mini_df <- cbind(id, voc_mini_df)
# #voc_mini_df$id[voc_mini_df$voc_freq <= 4] <- 0
# saveRDS(voc_mini_df,"Data/voc_mini_df.Rds")
#
# cat("Making ngrams\n")
# samp_ngrams <- tokenize(samp_corpus,
#                    what = "word",
#                    verbose = TRUE,
#                    simplify = FALSE,
#                    removePunct = TRUE,
#                    removeNumbers = TRUE,
#                    removeSeparators = TRUE,
#                    removeTwitter = TRUE,
#                    removeHyphens = FALSE,
#                    concatenator = " ",
#                    skip = 0L,
#                    ngrams = 2L
#                   )
# ng_dfm <- dfm(samp_ngrams, ignoredFeatures = stopwords("english"))
# ng_freq <- colSums(ng_dfm)
# ng_tot <- sum(ng_freq)
#
# ng_names <- names(ng_freq)
#
# ngram_df <- data_frame(ng_term = ng_names, ng_freq = ng_freq)
# ngram_df <- ngram_df %>%
#               mutate(rel_freq = ng_freq/ng_tot) %>%
#               mutate(ng_id = ng_term)
# id <- c(1:nrow(ngram_df))
# ngram_df <- ngram_df %>% arrange(desc(rel_freq))
# ngram_df <- cbind(ngram_df,id)
# saveRDS(ngram_df,"Data/ngram_df.Rds")
#
# getID <- function(in_word) {
#     if (is.na(in_word)) return(0)
#     id <- ifelse(is.na(voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"]),
#                 "0",
#                 voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"])
#     id <- ifelse(is.null(voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"]),
#                  "0",
#                  voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"])
#     return(id)
# }
#
# ng_df <- data.frame(id=numeric(0),
#                     ng_term=character(),
#                     ng_freq=numeric(0),
#                     rel_freq=numeric(0),
#                     ng_1_word=character(0),
#                     ng_2_word=character(0),
#                     ng_id=character(0),
#                     ng_1_id=character(0),
#                     ng_2_id=character(0),
#                     stringsAsFactors=FALSE
#                     )
# z <- nrow(ngram_df)
# #z <- 5
# for (i in 1:z) {
#     cat(i, " of ", z, "\n")
#     ng_list <- unlist(ngram_df[i,])
#     ng_df[i,1] <- as.numeric(ng_list[5])
#     ng_df[i,3] <- as.numeric(ng_list[2])
#     ng_df[i,4] <- as.numeric(ng_list[3])
#
#     ng_df[i,2] <- ng_list[1]
#     ng_split <- unlist(str_split( ng_list[1], " "))
#     ng_df[i,5] <- ng_split[1]
#     ng_df[i,6] <- ng_split[2]
#     ng_df[i,8] <- as.numeric(getID(ng_split[1]))
#     ng_df[i,9] <- as.numeric(getID(ng_split[2]))
#     ng_df[i,7] <- paste(ng_df[i,8], ng_df[i,9], sep=" ")
# }
# saveRDS(ng_df, "Data/ng_df.Rds")
