#=================================================
# map_vocab_ngrams_v1r1.R
#=================================================

require(quanteda)
require(dplyr)
require(stringr)

if (!exists("voc_mini_df")) {
    voc_mini_df <- readRDS("Data/voc_mini_df.Rds")
}
if (!exists("ng_mini_df")) {
    ng_mini_df <- readRDS("Data/ng_mini_df.Rds")
}

getID <- function(in_word) {
    id <- voc_mini_df[which(voc_mini_df$voc_term == in_word), "id"]
}

getWord <- function(in_id) {
    word <- voc_mini_df[which(voc_mini_df$id == in_id), "voc_term"]
}

t <- ng_mini_df[388,1]

t_words <- str_split(t, " ")

print(t_words)

t_word <- NULL

t <- as.list(ng_mini_df[i,])
tw_list <- str_split(t$ng_term, " ")

for (j in 1:lengths(tw_list)) {
    tw <- sapply(tw_list, "[[" , j)
    cat("word:", tw, "\n")
    tmp_id <- getID(tw)
    t_word <- paste(t_word, tmp_id)
}