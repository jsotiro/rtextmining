library("XML")
library("RCurl")
library("tm")
library("SnowballC")
library("wordcloud")
library("boilerpipeR")
r

rssSummary<-function(url,stopwordsToIgnore=stopwords("english"),maxwords=100){
  rssXml<-getURL(url)

  blogs<-getTextFromHtmlPages(rss$links)
  corpus<-cleanUpCorpus(blogs,stopwordsToIgnore)
  quickWordCloud(corpus,maxwords)
  tdm<-TermDocumentMatrix(corpus)
  df<-termMatrixAsSortedDataframe()
  df[1:maxwords,]
  return(tdm)
}

quickWordCloud<-function(corpus, maxwords=100){
  wordcloud(corpus, scale=c(5,0.5), max.words=maxwords, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
}


 getTextFromHtmlPages<-function(links,keepHtml=FALSE){
 
  docsText<-sapply(links,getURL)
  if (keepHtml==FALSE)
    docsText<-sapply(docsText,DefaultExtractor)
  return(docsText)
}  

rssFeed<-function(sourceUrl,keepHtml=FALSE){
  rssXml=getURL(sourceUrl)
  rssXml<-gsub("\r\n"," ",rssXml)
  rssXml<-gsub("\n"," ",rssXml)
  feed=xmlTreeParse(rssXml)
  rootNode<-xmlRoot(feed)
  titles<-sapply(getNodeSet(rootNode,"/rss/channel/item/title"),xmlValue)
  descriptions<-sapply(getNodeSet(rootNode,"/rss/channel/item/description"),xmlValue)
  if (keepHtml==FALSE)
    descriptions<-gsub("<.*?>", " ", descriptions)
  
  pubDates<-sapply(getNodeSet(rootNode,"/rss/channel/item/pubDate"),xmlValue)
  links<-sapply(getNodeSet(rootNode,"/rss/channel/item/link"),xmlValue)  
  #rssFeed<-data.frame(as.character(titles),as.character(descriptions),as.character(pubDates),as.character(links))
  rssFeed<-data.frame(titles,descriptions,pubDates,links)
  
  colnames(rssFeed)=c("titles","descriptions","pubDates","links")
  
  return(rssFeed)
}


cleanUpCorpus<-function(vector, stopwordsToRemove=stopwords("english") ){
  myCorpus<-Corpus(VectorSource(vector))
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removeWords, stopwordsToRemove)
  # myCorpus <- tm_map(myCorpus, stemDocument)
 return(myCorpus)
}


termMatrixAsSortedDataframe<-function(myDtm){
  m <- as.matrix(myDtm)
  v <- sort(rowSums(m), decreasing=TRUE)
  myNames <- names(v)
  d <- data.frame(word=myNames, freq=v)
  return(d)
}