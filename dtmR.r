# set working directory to desktop
setwd(file.path("C:", "Users", "name", "Desktop", "folder", "projname", "projdetails"))

# clean up the environment before starting
rm(list = ls())
library(slam)
library(tm)
library(SnowballC)
library(proxy)


#Get file path to folder "test" where the documents are located
cname = file.path(".", "corpus")
cname
print(dir(cname))
docs = Corpus(DirSource((cname)))
print(summary(docs))

#Tokenisation
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))

#Filter words
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, content_transformer(tolower))

# Remove stop words and white space
docs <- tm_map(docs, stripWhitespace)

# Stem
docs <- tm_map(docs, stemDocument, language = "english")

#Create document term matrix
dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:15, 1:4])
freq <- colSums(as.matrix(dtm))
length(freq)
ord = order(freq)
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 10)
tail(table(freq), 10)

#Remove sparse term
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.6666)
inspect(dtms)

dtms = as.matrix(dtms)
write.csv(dtms, "a3.csv")

#identify clustering patterns:
groups

#cluster
distmatrix = proxy::dist(dtms, method = "cosine")
fit = hclust(distmatrix, method = "ward.D")
plot(fit, hang = -1, main = "Documents Cosine Distance")


# Calculate the accuracy of clustering 
topics = c("1","4","4","3","3","3","3","3","3","2","3","4","3","4","3")

# using cluster object "fit" create required number of
# clusters. This will be a vector of cluster numbers
groups = cutree(fit, k = 4)

# make a table of topic labels vs cluster numbers
table(GroupNames = topics, Clusters = groups)

