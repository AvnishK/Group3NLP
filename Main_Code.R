setwd("/Users/maniiyer/Desktop/twitter-sentiment-analysis-master/")
library(twitteR)
library(plyr)

consumerKey <-'GKLzODclkgAlPzwtIdUBdGjxZ'
consumerSecret <-'HeobbJClW8KuYvLxyshAWmZOjTvxVOyg5vGUniW2QLISCMlBcy'
accessToken <- '807825575200976896-Qn9DOv7QduMWVLusyipJi80m4PQdm6M'
accessSecret <- 'IETEON9kd6HyVDMNV49HxSyMHh1N30DmV6fxB0cL2NHJo'



score.sentiments = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentences)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    neg.matches = match(words, neg.words)
    pos.matches = match(words, pos.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}




options(httr_oauth_cache=TRUE) # skip question appearing on console
setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                    access_token = accessToken, access_secret = accessSecret)

tweets <- searchTwitter('#DonaldTrump', n=1000, lang='en')
length(tweets)

tweets.txt = laply(tweets,function(t)t$getText())

pos=scan('/Users/maniiyer/Desktop/twitter-sentiment-analysis-master/wordbanks/positive-words.txt',what = 'character',comment.char = ';')
neg=scan('/Users/maniiyer/Desktop/twitter-sentiment-analysis-master/wordbanks/negative-words.txt',what = 'character',comment.char = ';')

#source('/Users/maniiyer/Desktop/twitter-sentiment-analysis-master/R/sentiment_new.R')

analysis=score.sentiments(tweets.txt,pos,neg)
table(analysis$score)
