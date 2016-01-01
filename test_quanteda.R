# -------------------------------------------------
# simple quanteda test
# -------------------------------------------------
require(quanteda)
require(lambda.tools)
require(dplyr)

if (!exists("all_corpus")) {
    source("load_corpus_mini.R")
}

txt <- c(text1 = "This is $10 in 999 different ways,\n up and down; left and right!",
         text2 = "@kenbenoit working: on #quanteda 2day\t4ever, http://textasdata.com?page=123.",
         text3 = "Now is the time for all good men to come to the aid of their country.",
         text4 = "The quick red fox jumped over the lazy dog.")


clean <- function(x) {
    # gsub('[,.;:"()]','',tolower(x))
   gsub('[,.;:\'"()]','',tolower(x))
}


word_count_simple <- function(lines) {
    chunks <- strsplit(clean(lines),'\\s')
    words <- do.call(c, chunks)
    table(words)
}

x <- word_count_simple(news_tok)


