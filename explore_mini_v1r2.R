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
txt <- scan(con,
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
samp_corpus <- corpus(txt)
rm(txt)

cat("Making vocabulary\n")
samp_vocab <- tokenize(samp_corpus,
                   what = "word",
                   verbose = TRUE,
                   simplify = FALSE,
                   removeSeparators = TRUE,
                   removeNumbers = TRUE,
                   removePunct = TRUE,
                   removeHyphens = TRUE,
                   removeTwitter = TRUE
              )

cat("Making ngrams\n")
samp_ngrams <- tokenize(samp_corpus,
                   what = "word",
                   verbose = TRUE,
                   simplify = FALSE,
                   removePunct = TRUE,
                   removeNumbers = TRUE,
                   removeSeparators = TRUE,
                   removeTwitter = TRUE,
                   removeHyphens = TRUE,
                   concatenator = " ",
                   skip = 0L,
                   ngrams = 1:4
                  )

cat("Making dfm\n")
voc_mini_dfm <- dfm(samp_vocab, ignoredFeatures = stopwords("english"))
voc_mini_freq <- colSums(voc_mini_dfm)
voc_mini_tot <- sum(voc_mini_freq)
voc_mini_names <- names(voc_mini_freq)
voc_mini_df <- data_frame(voc_term = voc_mini_names, voc_freq = voc_mini_freq)
voc_mini_df <- voc_mini_df %>% mutate(rel_freq = voc_freq/voc_mini_tot)
id <- c(1:nrow(voc_mini_df))
voc_mini_df <- voc_mini_df %>% arrange(desc(rel_freq))
voc_mini_df <- cbind(id, voc_mini_df)
voc_mini_df$id[voc_mini_df$voc_freq <= 4] <- 0
saveRDS(voc_mini_df,"Data/voc_mini_df.Rds")

ng_mini_dfm <- dfm(samp_ngrams,
                 ignoredFeatures = stopwords("english"))
ng_mini_freq <- colSums(ng_mini_dfm)
ng_mini_tot <- sum(ng_mini_freq)
ng_mini_names <- names(ng_mini_freq)
ng_mini_df <- data_frame(ng_term = ng_mini_names, ng_freq = ng_mini_freq)
ng_mini_df <- ng_mini_df %>%
              mutate(rel_freq = ng_freq/ng_mini_tot) %>%
              mutate(ng_id = ng_term)
id <- c(1:nrow(ng_mini_df))
ng_mini_df <- ng_mini_df %>% arrange(desc(rel_freq))
ng_mini_df <- cbind(ng_mini_df,id)
saveRDS(ng_mini_df,"Data/ng_mini_df.Rds")

rm(list = c("voc_mini_freq","voc_mini_tot","voc_mini_names","id","voc_mini_dfm"))
rm(list = c("con","ng_mini_freq","ng_mini_tot","ng_mini_names","ng_mini_dfm"))
rm(list = c("samp_corpus","samp_ngrams","samp_vocab","src_file","maxlines"))


