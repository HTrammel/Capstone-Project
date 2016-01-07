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

maxlines <- 3000L
src_file <- "Data/all_samp.txt"

con <- file(src_file, open="rb")
txt <- scan(con,
          what = "complex",
          nlines = maxlines,
          skip = maxlines * 0,
          # fileEncoding = "ISO-8859-2",
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

# cat("Making vocabulary\n")
# samp_vocab <- tokenize(samp_corpus,
#                    what = "word",
#                    verbose = TRUE,
#                    simplify = FALSE,
#                    removeSeparators = TRUE,
#                    removeNumbers = TRUE,
#                    removePunct = TRUE,
#                    removeTwitter = TRUE
#               )

cat("Making tokens\n")
samp_tokens <- tokenize(samp_corpus,
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

cat("Making dfm\n")
samp_dfm <- dfm(samp_tokens)

samp_freq <- colSums(samp_dfm)
samp_tot <- sum(samp_freq)
samp_names <- names(samp_freq)
samp_df <- data_frame(samp_names, samp_freq)
samp_df <- samp_df %>% mutate(rel_freq = samp_freq/samp_tot)
samp_df <- samp_df %>% mutate(word_count = stri_count_words(samp_names))

saveRDS(samp_df,"Data/samp_df.Rds")

#rm(list=c("samp_freq","samp_names"))

