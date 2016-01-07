# ========================================================
# guess_it_v1r1.R
# ========================================================


require(quanteda)
require(dplyr)
library(stringr)

if (!exists("ngrams_df")) {
    ngrams_df <- readRDS("Data/voc_df.Rds")
}

guess_it <- function(inText) {
    i_cnt <- str_count(inText, " ")

    if (i_cnt > 3) {
        return (c("Error","Too many words entered"))
    }

    tout <- ngrams_df[str_detect(ngrams_df$voc_names, inText),]

    if (tout$word_count == 4) {
        words <- lapply(tout$voc_names, function(x) str_split(x, " "))
    } else {
    	tout$word_count
    }
}

my_guess <- guess_it("the children and")

print(my_guess)



