require(tm)
require(stringr)


# --- Setup ---
if (!file.exists("Data")) { dir.create("Data") }

data_dir <- "Data"
en_blg_c_rds <- paste(data_dir, "USblogs.Rds", sep="/")
en_nws_c_rds <- paste(data_dir, "USnews.Rds", sep="/")
en_twt_c_rds <- paste(data_dir, "UStwitter.Rds", sep="/")
us_corpus_rds <- paste(data_dir, "us_corpus.Rds", sep="/")

en_blogs <- "en_US.blogs.txt"
en_news <- "en_US.news.txt"
en_twit <- "en_US.twitter.txt"

if (!file.exists(us_corpus_rds)) {
    us_corpus <- Corpus(ZipSource("Data/Coursera-SwiftKey.zip", pattern="en.us", recursive = T), readerControl = list(language = "en"))
    saveRDS(us_corpus, us_corpus_rds)
} else {
    us_corpus <- readRDS(us_corpus_rds)
}


# news_tdm <- TermDocumentMatrix(news_c,
#                           control = list(removePunctuation = T,
#                                          stopwords = T))