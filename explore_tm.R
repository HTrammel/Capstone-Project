require(tm)
require(stringr)

if (!file.exists("Data")) { dir.create("Data") }

library(filehashSQLite)
# this string becomes filename, must not contain dots.

s <- "Data/sqldb_pcorpus_en_US"

suppressMessages(library(filehashSQLite))

if(! file.exists(s)){
    pc = PCorpus(ZipSource("Data/Coursera-SwiftKey.zip", recursive = T), readerControl = list(language = "en"), dbControl = list(dbName = s, dbType = "SQLite"))
    dbCreate(s, "SQLite")
    db <- dbInit(s, "SQLite")
    set.seed(234)
} else {
    db <- dbInit(s, "SQLite")
    pc <- dbLoad(db)
}

show(pc)

# # remove it
# rm(db)
# rm(pc)
#
# #reload it
# db <- dbInit(s, "SQLite")
# pc <- dbLoad(db)
#
# show(pc)