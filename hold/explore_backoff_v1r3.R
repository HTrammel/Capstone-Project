#---------------------------------------------------------------------
#
# Explore_Backoff.R
#
# Purpose: Implementation of "stupid backoff"
#
#---------------------------------------------------------------------
require(quanteda)
require(stringr)
require(dplyr)
require(data.table)

# set lower limit to keep
bottom <- 0

# sources

txt <- c(text1 = "In the years thereafter, most of the Oil fields and platforms were named after pagan “gods”.",
text2 = "We love you Mr. Brown.",
text3 = "Chad has been awesome with the kids and holding down the fort while I work later than usual! The kids have been busy together playing Skylander on the XBox together, after Kyan cashed in his $$$ from his piggy bank. He wanted that game so bad and used his gift card from his birthday he has been saving and the money to get it (he never taps into that thing either, that is how we know he wanted it so bad). We made him count all of his money to make sure that he had enough! It was very cute to watch his reaction when he realized he did! He also does a very good job of letting Lola feel like she is playing too, by letting her switch out the characters! She loves it almost as much as him.  so anyways,",
text4 = " i am going to share some home decor inspiration that i have been storing in my folder on the puter. i have all these amazing images stored away ready to come to life when we get our home.",
text5 = "With graduation season right around the corner, Nancy has whipped up a fun set to help you out with not only your graduation cards and gifts, but any occasion that brings on a change in one's life. I stamped the images in Memento Tuxedo Black and cut them out with circle Nestabilities. I embossed the kraft and red cardstock with TE's new Stars Impressions Plate, which is double sided and gives you 2 fantastic patterns. You can see how to use the Impressions Plates in this tutorial Taylor created. Just one pass through your die cut machine using the Embossing Pad Kit is all you need to do - super easy! If you have an alternative argument, let's hear it! :)",
text6 = "If I were a bear with the kids,",
text7 = "Other friends have similar stories, of how they were treated brusquely by Laurelwood staff, and as often as not, the same names keep coming up. About a half-dozen friends of mine refuse to step foot in there ever again because of it. How many others they’re telling - and keeping away - one can only guess.",
text8 = "Although our beloved Cantab can’t claim the international recognition afforded the Station Inn, otherwise these two joints feel like twins separated by nothing more than distance. They share a complete lack of pretense that can’t be imitated or approximated. Their very ordinariness makes them special.",
text9 = "Peter Schiff: Hard to tell. It will look pretty bad for most Americans when prices will go way up and they can’t afford to buy stuff. It could also get very bad as far as loss of individual liberty. A lot of people will blame it on capitalism, on freedom, and they will claim we need more government. It could be used as an impetus for more regulation, which would be a disaster, or it could be an impetus to get rid of all the regulation that was causing the problem. But whether we will do the right or the wrong thing here in America, there will be a lot of pain first. We got some serious problems we have to deal with, but we are not dealing with the problems, we only make the problems worse."
)


map_vocabulary <- function(lines) {
    print("Making vocabulary")
    lines <- toLower(lines)
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE
                       )
    words <- do.call(c, chunks)
    word_freq <- table(words)
    dt <- data.table(word_freq)
    setkey(dt, N)
    dt <- dt[N > bottom]
    dt$words <- as.factor(dt$words)
    dt
}

map_ngrams <- function(lines) {
    print("Making ngrams")
    lines <- toLower(lines)
    chunks <- tokenize(lines,
                       what = "word",
                       removeSeparators = TRUE,
                       removeNumbers = TRUE,
                       removePunct = TRUE,
                       removeTwitter = TRUE,
                       concatenator = " ",
                       ngrams = 1:5
                    )
    words <- do.call(c, chunks)
    word_freq <- table(words)
    dt <- data.table(word_freq)
    setkey(dt, N)
    dt <- dt[N > bottom]
    dt[order(-N)]
    dt$words <- as.factor(dt$words)
    dt
}

tx_vocab <- map_vocabulary(txt)
tx_ngrams <- map_ngrams(txt)

# reduce_ngram <- function(lines) {
#     print("Reducing ngrams")
#     ng <- lines$words
#     fq <- lines$N
#     chunks <- tokenize(ng, what="word")
#     words <- do.call(c, chunks)
#     ids <- sapply(words, function(x) chmatch(x, tx_vocab$words))
#     rng <- list(ng, fq, words, ids)
# }

# r_ngram <- reduce_ngram(tx_ngrams)