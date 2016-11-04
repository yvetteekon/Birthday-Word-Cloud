# Initialize libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Load text
textFile <- "birthdayMessages.txt"
text <- readLines(textFile)

# Create corpus and inspect text
length(text)
# dim(text)
docs <- Corpus(VectorSource(text))
inspect(docs)
writeLines(as.character(docs[[51]]))

# Mine and manipulate text
# Clean text
# Convert text to lowercase
docs <- tm_map(docs, content_transformer(tolower))

# toProper <- content_transformer(function(x, str) gsub(str, "God", x))
# docs <- tm_map(docs, toProper, "god")

# Remove Punctuations
docs <- tm_map(docs, removePunctuation)

# Remove generic and custom stop words
my_stopwords <- c(stopwords("english"), "may", "will", "dont", "wide", "way", "want",
                  "upon", "youve", "youre", "can")
docs <- tm_map(docs, removeWords, my_stopwords)
inspect(docs)

# Replace gods with god
docs <- tm_map(docs, gsub, pattern = "gods", replacement = " ")
docs <- tm_map(docs, gsub, pattern = "god", replacement = " ")
docs <- tm_map(docs, gsub, pattern = "bless", replacement = "blessed")
docs <- tm_map(docs, gsub, pattern = "blesseded", replacement = "blessed")
docs <- tm_map(docs, gsub, pattern = "blessedings", replacement = "blessed")
docs <- tm_map(docs, gsub, pattern = "sis", replacement = "sister")
docs <- tm_map(docs, gsub, pattern = "sisterter", replacement = "sister")
docs <- tm_map(docs, gsub, pattern = "year", replacement = "years")
docs <- tm_map(docs, gsub, pattern = "yearss", replacement = "years")

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Copy of Corpus for dictionary
docsCopy <- docs

# Stem the text
docsStem <- tm_map(docs, stemDocument)

# Inspect first 5 lines
# for (i in 1:5){
#  cat(paste("[[", i, "]]", sep = ""))
#  writeLines(docs[[i]])
# }

# Stem completion
# docs <- tm_map(docsStem, content_transformer(stemCompletion),
#                  dictionary = docsCopy, lazy = TRUE)

# Clean corpus
docsClean <- tm_map(docs, PlainTextDocument)

# Build a term-document matrix
tdm <- TermDocumentMatrix(docsClean, control = list(minWordLength = 1))
tdm

# Remove sparse data
tdm <- removeSparseTerms(tdm, sparse = 0.95)

# Convert term-document to a matrix and data.frame
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(words = names(v), freq = v )
head(d, 10)
d

# Inspect most popular words
findFreqTerms(tdm, lowfreq = 10)

# Find collocation
findAssocs(tdm, "abena", 0.3)

# Generate the Word Cloud
set.seed(123)
wordcloud(d$words, d$freq, min.freq = 3, max.words = 300, random.order = FALSE,
          rot.per = 0.3, colors = brewer.pal(8, "Set2"))

# set.seed(123)
# wordcloud(d$words, d$freq, min.freq = 3, max.words = 300, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Dark2"))

# set.seed(123)
# wordcloud(d$words, d$freq, min.freq = 3, max.words = 200, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Set1"))

# set.seed(123)
# wordcloud(d$words, d$freq, min.freq = 3, max.words = 300, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Accent"))

# wordcloud(d$words, d$freq, min.freq = 3, max.words = 200, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Paired"))

# wordcloud(d$words, d$freq, min.freq = 3, max.words = 300, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Pastel1"))

# wordcloud(d$words, d$freq, min.freq = 3, max.words = 200, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Pastel2"))

# wordcloud(d$words, d$freq, min.freq = 3, max.words = 200, random.order = FALSE,
#          rot.per = 0.3, colors = brewer.pal(8, "Set3"))

