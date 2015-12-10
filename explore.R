#---------------------------------------------------------------------
#
# Explore.R
#
# Purpose: Explore text files provided for class
# NOTE: rJava requires 32-bit R because I have 32-bit Java
#
#---------------------------------------------------------------------

require(rJava)
require(tm)
require(RWeka)
require(openNLP)

if (!file.exists("Data")) { dir.create("Data") }

en_dir <- "Data/final/en_US"
ru_dir <- "Data/final/ru_RU"
fi_dir <- "Data/final/fi_FI"
de_dir <- "Data/final/de_DE"

en_blogs <- paste(en_dir, "en_US.blogs.txt", sep="/")
ru_blogs <- paste(ru_dir, "ru_RU.blogs.txt", sep="/")
fi_blogs <- paste(fi_dir, "fi_FI.blogs.txt", sep="/")
de_blogs <- paste(de_dir, "de_DE.blogs.txt", sep="/")

en_news <- paste(en_dir, "en_US.news.txt", sep="/")
ru_news <- paste(ru_dir, "ru_RU.news.txt", sep="/")
fi_news <- paste(fi_dir, "fi_FI.news.txt", sep="/")
de_news <- paste(de_dir, "de_DE.news.txt", sep="/")

en_twit <- paste(en_dir, "en_US.twitter.txt", sep="/")
ru_twit <- paste(ru_dir, "ru_RU.twitter.txt", sep="/")
fi_twit <- paste(fi_dir, "fi_FI.twitter.txt", sep="/")
de_twit <- paste(de_dir, "de_DE.twitter.txt", sep="/")