# ========================================================
# guess_it_v1r1.R
# ========================================================

require(quanteda)
require(dplyr)
require(stringi)
require(stringr)

DEV <- TRUE

if (DEV == FALSE) {
    if (!exists("vocab_df")) {
        vocab_df <- readRDS("Data/vocab_df.Rds")
    }
    if (!exists("ngrams_id")) {
        ngrams_id <- readRDS("Data/ngram_id.Rds")
    }
} else {
    if (!exists("voc_mini_df")) {
        voc_mini_df <- readRDS("Data/voc_mini_df.Rds")
    }
    if (!exists("ng_mini_df")) {
        ng_mini_df <- readRDS("Data/ng_mini_df.Rds")
    }
}


guess_it <- function(inText) {
    i_cnt <- str_count(inText, " ")

    if (i_cnt > 3) {
        return (c("Error","Too many words entered"))
    }

    tout <- if (is.na(ng_mini_df[str_detect(ng_mini_df$ng_term, inText),])) { "0" }
            else if (is.null(ng_mini_df[str_detect(ng_mini_df$ng_term, inText),])) { "0" }
            else { ng_mini_df[str_detect(ng_mini_df$ng_term, inText),] }

    if (tout$word_count == 4) {
        words <- lapply(tout$ngram_name, function(x) str_split(x, " "))
    } else {
    	tout$word_count
    }
}

my_guess <- guess_it("hard driving tiger")

print(my_guess)



