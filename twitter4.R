# Stuff
# look at remarks/tagline too

# Improvements
# rando select 3 comments to paste
# auto search for imei joins
# generate email
# ignore status =-1?
# look at remarks
# keep a list of previously looked at
#

library(stringi)
library(dplyr)
library(purrr)
library(twitteR)
library(ggplot2)
library(syuzhet)
library(tidytext)
library(RMySQL)

#load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

library(tidyr)

mydb <- list(
  user = "weread",
  password = "r0ckth3cl0ud",
  host = "prod-slave-1.csnjf4cqzowh.us-west-2.rds.amazonaws.com",
  drv = "MySQL"
)


datc <- fetchQuery("
                   select a1.*, a2.nickname, sex, a2.status as acc_status, ipaddr, remark, fans, follow, inviter, modifysex, card, city, region, country ,
                   case when lower(content) regexp '[0-9].*){7}' then 1 else 0 end number,
                   case when lower(content) regexp '(http)|(www)|(bit\\.ly)|(goo\\.gl)|(tinyurl)|(olaurl)|(infelseg)|(\\.com)|(\\.top)|(\\.li)|(\\.us)|(\\.org)|(\\.re)|(\\.xyz)|(\\.site)|(\\.ni)|(\\.it)|(104\\.168\\.141\\.190)|(\\.pw)|(\\.me)|(\\.io)|(\\.net)|(\\.ga)|(\\.ru)|(\\.gq)' then 1 else 0 end as link
                   from qubo_show.show_audience_detail_1702 a1
                   join qubo_user.user_account a2
                   on a1.userid = a2.userid
                   where 
                   1=1
                   and (mobile is null ) #or mobile <> 5000000000000001)
                   and a2.userid <> 10000
                   and type = 3
                   and lower(content) not in ('its lit', 'aye')
                   and ipaddr not in ('24.143.248.98', '70.90.168.211', '107.77.213.167')
                   #and lower(content) not like 'hi%'
                   and not lower(content) regexp '^([[:space:]])*(/IMG[0-9]{3})+([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(n|y)(o)+([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(s+e*x*y*)([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*((h+m*)*)([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*((a+w*)*)([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(b)*y(e)*(a+)*(h+)*(s+)*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*h(((e|a)+y*)|(i+)*)(l*o*)(ola)*([[:space:]])*(\\[.+\\])*([[:punct:]])*((yo)*u+)*(beautiful|sexy|boo)*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(w+)(h+)(a+)(t+)([[:space:]])*(s+)([[:space:]])*(u+)(p+)([[:space:]])*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(w+)*(a+)*(s+)*(u+)*(p+)([[:space:]])*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*((h*a*)*)([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*((l+(m)*(a)*o*l*)*|(w+o*w*)*([[:punct:]])*)*$'
                   and not lower(content) regexp '^([[:space:]])*(c+o*l*)([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(o*k*)*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(a*y*e*)*([[:punct:]])*$'
                   and not lower(content) regexp '^[[:space:]]*(th)(x)*(ank)*(s)*[[:space:]]*(y)*(o)*(u)*[[:space:]]*(\\[.+\\])*[[:punct:]]*$'
                   and not lower(content) regexp '^[[:space:]]*(how)*[[:space:]]*(old)*[[:space:]]*(a)*(r)*(e)*[[:space:]]*(y)*(o)*(u)*[[:space:]]*(\\[.+\\])*[[:punct:]]*$'
                   and not lower(content) regexp '^([[:space:]])*(o*h*i*e*)*[[:space:]]*([[:punct:]])*$'
                   and not lower(content) regexp '^[[:space:]]*(\\[.+\\])*[[:space:]]*[[:punct:]]*$'
                   and not lower(content) regexp '^[[:space:]]*(\\@)*(\\[.+\\])*[[:space:]]*[[:punct:]]*$'
                   #and lower(content) regexp '^bitch.*'
                   #and lower(content) like 'xnxx%'
                   #and lower(content) regexp '^([[:space:]])*((just)*[[:punct:]]*t+e*s*t*(ing)*)([[:punct:]])*)*$'
                   #and lower(content) like '%test%'
                   #and lower(content) REGEXP '(tits|ass|fuck|bitch|cock|dick|penis|pay|\\$|skype|scam|spam|cum|kill|beat |murder|charge|cam |bank|western|loan|head|(check out)|www\\.|http|\\.com|\\.net)'
                   #and lower(content) like '%phone%'
                   and a1.userid > 300000
                   and length(content) >= 3
                   and a1.modifytime > '2017-02-08'
                   ", mydb)

datc$content <- iconv(datc$content, "UTF-8", "UTF-8",sub='')


datc <- rbind(datc, fetchQuery("
                   select a1.*, a2.nickname, sex, a2.status as acc_status, ipaddr, remark, fans, follow, inviter, modifysex, card, city, region, country ,
                   case when lower(content) regexp '[0-9].*){7}' then 1 else 0 end number,
                    case when lower(content) regexp '(http)|(www)|(bit\\.ly)|(goo\\.gl)|(tinyurl)|(olaurl)|(infelseg)|(\\.com)|(\\.top)|(\\.li)|(\\.us)|(\\.org)|(\\.re)|(\\.xyz)|(\\.site)|(\\.ni)|(\\.it)|(104\\.168\\.141\\.190)|(\\.pw)|(\\.me)|(\\.io)|(\\.net)|(\\.ga)|(\\.ru)|(\\.gq)' then 1 else 0 end as link
                   from qubo_show.show_audience_detail_1701 a1
                   join qubo_user.user_account a2
                   on a1.userid = a2.userid
                   where 
                   1=1
                   and (mobile is null ) #or mobile <> 5000000000000001)
                   and a2.userid <> 10000
                   and type = 3
                   and lower(content) not in ('its lit', 'aye')
                   and ipaddr not in ('24.143.248.98', '70.90.168.211', '107.77.213.167')
                   #and lower(content) not like 'hi%'
                   and not lower(content) regexp '^([[:space:]])*(/IMG[0-9]{3})+([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(n|y)(o)+([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(b)*y(e)*(a+)*(h+)*(s+)*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*h(((e|a)+y*)|(i+)*)(l*o*)(ola)*([[:space:]])*(\\[.+\\])*([[:punct:]])*((yo)*u+)*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(w+)(h+)(a+)(t+)([[:space:]])*(s+)([[:space:]])*(u+)(p+)([[:space:]])*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(w+)*(a+)*(s+)*(u+)*(p+)([[:space:]])*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*((h*a*)*)([[:punct:]])*$'
                   #and not lower(content) regexp '^([[:space:]])*((l+o*l*)*|(w+o*w*)*([[:punct:]])*)*$'
                   and not lower(content) regexp '^([[:space:]])*(c+o*l*)([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(o*k*)*([[:punct:]])*$'
                   and not lower(content) regexp '^([[:space:]])*(a*y*e*)*([[:punct:]])*$'
                   #and not lower(content) regexp '^[[:space:]]*(th)(x)*(ank)*(s)*[[:space:]]*(y)*(o)*(u)*[[:space:]]*(\\[.+\\])*[[:punct:]]*$'
                   and not lower(content) regexp '^[[:space:]]*(how)*[[:space:]]*(old)*[[:space:]]*(a)*(r)*(e)*[[:space:]]*(y)*(o)*(u)*[[:space:]]*(\\[.+\\])*[[:punct:]]*$'
                   and not lower(content) regexp '^([[:space:]])*(o*h*i*e*)*[[:space:]]*([[:punct:]])*$'
                   and not lower(content) regexp '^[[:space:]]*(\\[.+\\])*[[:space:]]*[[:punct:]]*$'
                   and not lower(content) regexp '^[[:space:]]*(\\@)*(\\[.+\\])*[[:space:]]*[[:punct:]]*$'
                   #and lower(content) regexp '^bitch.*'
                   #and lower(content) like 'xnxx%'
                   #and lower(content) regexp '^([[:space:]])*((just)*[[:punct:]]*t+e*s*t*(ing)*)([[:punct:]])*)*$'
                   #and lower(content) like '%test%'
                   #and lower(content) REGEXP '(tits|ass|fuck|bitch|cock|dick|penis|pay|\\$|skype|scam|spam|cum|kill|beat |murder|charge|cam |bank|western|loan|head|(check out)|www\\.|http|\\.com|\\.net)'
                   #and lower(content) like '%phone%'
                   and a1.userid > 300000
                   and length(content) >= 3
                   and a1.modifytime >= '2016-12-16'
                   ", mydb))

### fill in array of emotes
datc <- cbind(datc, get_nrc_sentiment(iconv(datc$content, "UTF-8", "UTF-8",sub='')))

#nrc <- sentiments %>%
 # filter(lexicon == "nrc") %>%
#  dplyr::select(word, sentiment)


#fixed...add shit to comment table
comments <- 
  filter(datc) %>% left_join( 
    filter(datc)[, c("userid", "id", "content", "negative", "positive", "anger", "disgust")] %>% 
      unnest_tokens(word, content, drop = FALSE) %>% 
      left_join(get_sentiments("afinn")) %>%
      #   left_join(summarize(group_by(nrc, word), sentiment= min(sentiment)), by = "word") %>%
      group_by(userid,id) %>% 
      summarize(total_score = sum(score, na.rm=TRUE),
                avg_score = mean(score, na.rm=TRUE),
                angerCt = sum(anger),
                anger = mean(anger),
                disgustCt = sum(disgust),
                disgust = mean(disgust),
                avg_Positive = mean(positive, na.rm=TRUE),
                avg_Negative = mean(negative, na.rm=TRUE)
      ) %>%
      arrange(desc(total_score)))



user_scored <- comments %>%
  group_by(userid) %>%
  summarize(avgScore = sum(total_score, na.rm= TRUE)/n(),
            totalBlocked = sum(status==0, na.rm= TRUE),
            avgBlocked = sum(status==0, na.rm= TRUE)/n(),
            disgustCt = sum(disgust),
            angerCt = sum(anger),
            anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust),
            negative = mean(negative),
            positive = mean(positive),
            msgs = n())

user_scored <- data.frame(user_scored)
user_scored$mostSentiment = names(user_scored[1,5:12])[max.col(user_scored[,5:12],ties.method="random")]



comment_scored <- comments %>%
  group_by(userid, showid, id) %>%
  summarize(totalScore = mean(score, na.rm= TRUE),
            blocked = sum(status==0, na.rm= TRUE),
            angerCt = sum(anger),
            anger = mean(anger),
            disgustCt = sum(disgust)
            disgust = mean(disgust),
            totalPositive = mean(positive),
            totalNegative = mean(negative))

session_scored <- comment_scored %>%
  group_by(userid, showid) %>%
  summarise(
    avgScore = mean(totalScore, na.rm = TRUE),
    minScore = min(totalScore, na.rm = TRUE),
    negScore = sum(totalScore < 0, na.rm = TRUE),
    negExtScore = sum(totalScore < -3, na.rm = TRUE),
    avgAnger = mean(anger),
    avgDisgust = mean(disgust))

user_session <- session_scored %>%
  group_by(userid) %>%
  summarise(
    countNegContent = sum(negScore, na.rm=TRUE) ,
    countNegExtContent = sum(negExtScore, na.rm=TRUE) ,
    negSessions = sum(avgScore < 0 , na.rm=TRUE) ,
    negExtSessions = sum(avgScore < -3 , na.rm=TRUE)
  )


final_user <- data.frame(user_scored %>% left_join(user_session))
#dead: final_comments <- (comments[, c("id", "showid", "userid", "imei", "type", "content", "status", "audit", "createtime", "modifytime", "showuserid", "nickname", "sex", "acc_status", "")]) %>% left_join(comment_scored, by = c("userid", "id")) 

Look <- function(x, neg = 0){
  tmp <- arrange(filter(comments, userid == x), desc(modifytime))
  if (neg == 1){
    filter(tmp[1:min(100, nrow(tmp)),c("content", "modifytime", "negative", "showid")], negative >= 1)
  }else{
    tmp[1:min(100, nrow(tmp)),c("content", "modifytime", "negative", "showid")]
  }
}







### multiple same messages
a1 <- arrange(filter(data.frame(dplyr::summarize(group_by(filter(datc, showuserid != 9006217), content, userid), ct = n())), ct >= 3), desc(ct))

#dplyr::arrange(a1, desc())
#a1[1:20,]


kek <- a1[1:35,"userid"]



### links
a2 <- filter(datc, showuserid != 9006217, !grepl("aww", tolower(content)),
grepl("(http)|(www\\.)|(bit\\.ly)|(goo\\.gl)|(tinyurl)|(olaurl)|(infelseg)|(\\.com)|(\\.top)|(\\.li)|(\\.us)|(\\.org)|(\\.re)|(\\.xyz)|(\\.site)|(\\.ni)|(\\.it)|(104\\.168\\.141\\.190)|(\\.pw)|(\\.me)|(\\.io)|(\\.net)|(\\.ga)|(\\.ru)|(\\.gq)", tolower(content)
      ))

a3 <- arrange(data.frame(dplyr::summarize(group_by(filter(a2), content, userid), ct = n())), desc(ct))

#View(a3[1:125, ])

#a3[1:125, ]

kek <- c(kek, a3[1:60,"userid"])



### hotwords
a4 <- filter(datc, showuserid != 9006217, !grepl("(sexy)|(can i)|(can you)", tolower(content)),
grepl("(tits|( ass)|(asshole)|fuck|bitch|cock|c0k|c0ck|cawk|clit|cnts|dyke|fag|fuk|whore|jerk|jizz|jiss|kunt|lezz|masterbate|
      nigger|nigur|niiger|niigr|orgasm|orgasum|orgasim|oriface|peeenus|peenus|pen1s|pen1s|phuc|phuk|pr1c|pusse|queer|
      slut|turd|vagina|vag1na|vagiina|vaj1na|vulva|wh00r|whore|chink|dildo|l3itch|nazi|teets|testicle|testical|wank|whoar|
      butt|ejac|honkey|monkey|twat|wetback|scank|skank|schlong|sh1t|shyt|cntz|dick|penis|pay|\\$|nigger|skype|scam|spam|cum|kill|
      (beat )|murder|charge|(cam )|bank|western|loan|head|(check out)|\\.net|(die))", tolower(content)))


a5 <- arrange(data.frame(dplyr::summarize(group_by(filter(a4), content, userid), ct = n())), desc(ct))

#a5[1:125, ]
#View(a5[1:125, ])

kek <- c(kek, a5[1:80,"userid"])



### nums
a6 <- filter(datc, showuserid != 9006217,
             grepl("([0-9].+){7}", tolower(content)))

a7 <- arrange(data.frame(dplyr::summarize(group_by(filter(a6), content, userid), ct = n())), desc(ct))
a7$content <- paste("'", a7$content, sep =" ")
#View(a7[1:125, ])

kek <- c(kek, a5[1:45,"userid"])


datc[1:1,]


xx <- c(
  9011121,
  9013597,
  9021513,
  9023206,
  9027089)
arrange(filter(datc,userid == xx[5]), as.POSIXct(modifytime))$content


## general disgust pps
kek <- c(kek,arrange(filter(final_user), desc(disgust))[1:40, 1]) 

## most disgust pps
kek <- c(kek, arrange(filter(final_user), desc(disgustCt))[1:40, 1] )
           
## anger ct; not much             
kek <- c(kek, arrange(filter(final_user), desc(angerCt))[1:10, 1])              

## score
kek <- c(kek, arrange(filter(final_user, msgs >= 3), (avgScore))[1:45, 1]) 

## neg Ses; not much
kek <-  c(kek, arrange(filter(final_user, msgs >= 3), desc(negSessions))[1:10, 1]) 

## adding the previous stuff saved to bwuh like repped msgs
#kek <- c(kek, bwuh)

ignoreList <- c(9025035,
                )


kek <- unique(kek)

length(kek)


j=0

j=j+1
#tmp <- arrange(filter(comments, userid == kek[j]), desc(disgust))
#tmp[1:min(20, nrow(tmp)),c("content", "disgust", "modifytime", "userid")]
j
kek[j]

Look(kek[j])
kek[j]
Look(kek[j],1)



                        


arrange(filter(final_user, avgScore <0), (avgScore))[1:20,]







arrange(filter(final_user, avgScore <0), desc(negExtContent))
arrange(filter(final_user), desc(anger), desc(negative))[1:10,]

View(arrange(datc[datc$userid %in% arrange(filter(final_user), desc(anger), desc(negative))[1:10,1],], userid, showid))

View(arrange(datc[datc$userid %in% arrange(filter(final_user), (avgScore), desc(negative))[1:10,1],], userid, showid))

View(arrange(datc[datc$userid %in% arrange(filter(final_user), desc(countNegExtContent), desc(negative))[1:15,1],], userid, showid))

View(arrange(datc[datc$userid %in% arrange(filter(final_user), desc(countNegContent), desc(negative))[15,1],], userid, desc(negative)))

View(arrange(datc[datc$userid %in% arrange(filter(final_user, disgust==1), desc(negative))[1:15,1],], userid, desc(negative)))


filter(comments, id==148198)
group_by(user_id,)

  filter(comment_scored, totalScore <= -5)

filter(datc, id==161259)
filter(comments, id==161259)
filter(comments_scored, id==161259)


comments_scored %>%
group_by(user_id)

user_scored$bads <- 0
user_scored$ <- 1 

comment_scored[1:10,] %>%
  filter(, total_score <=0)

content_scored
comment_scored[1:10,]




comments[1:10,] %>%
#  group_by(userid) %>%
  summarize(names(which(table(sentiment) == max(table(sentiment)))[1]))

which(table(comments[1:10, "sentiment"]) == max(table(comments[1:10, "sentiment"])))
      
datc[1:20,] %>%
  inner_join(comments_scored, by = c("userid")) 

datc[1:2,]

filter(comments_scored, userid ==9001670)#50077)
9001662
filter(comments, userid == 9001670)

summarize(filter(comments, userid == 9001670), avgScore = mean(score, na.rm= TRUE))
  summarize(avgScore = mean(score, na.rm= TRUE),
            
            

comments2 <- arrange(
  comments
    %>% inner_join(get_sentiments("afinn")) 
      %>% inner_join(nrc, by = "word")
  , desc(score))

get_sentiments("afinn"))



by_source_sentiment <- tweet_words2 %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, id) %>%
  ungroup() %>%
  complete(sentiment, id, fill = list(n = 0)) %>%
  inner_join(sources) %>%
  group_by(source, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  ungroup()




arrange(summarize(group_by(fuckk, userid), avg = mean(score), max(word)), desc(avg))



nrow(datt)


datc <- 






tweets <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

tweets2 <- datt[, c("id", "showid", "userid", "imei", "nickname", "sex", "status", "content", "modifytime")]
colnames(tweets2) <- c("id", "userid", "source", "text", "created")


library(lubridate)
library(scales)



library(tidytext)
library(stringr)

filter(!str_detect(text, '^"')) %>%
  
  


library(lubridate)
library(scales)
library(tidytext)
library(tidyr)

tweet_words2[tweet_words2$source == 2, "source"] <- "F"
tweet_words2[tweet_words2$source == 1, "source"] <- "M"

# figure out this syntax
# android_iphone_ratios2 <- tweet_words2 %>%
#   count(word, source) %>%
#   filter(sum(n) >= 5) %>%
#   spread(source, n, fill = 0) %>%
#   ungroup() %>%
#   mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
#   mutate(logratio = log2(F / M)) %>%
#   arrange(desc(logratio))


nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

nrc





# first link all user_ids to get a real one
# id, showid, primary_userid, imei, content, status, audit,
# repeats, similar msgs
# urls, numbers
# high risk isps?






library(tm)
library(stringr)
library(wordcloud)



#############################################################################################################



9055476	9087935	b76e917b2de4ed57ccd23f4b7d3af3c33f3709da
9055476	9089960	b76e917b2de4ed57ccd23f4b7d3af3c33f3709da
9055476	9090031	b76e917b2de4ed57ccd23f4b7d3af3c33f3709da
9089925	9089960	b76e917b2de4ed57ccd23f4b7d3af3c33f3709da
9089925	9090031	b76e917b2de4ed57ccd23f4b7d3af3c33f3709da
9089925	9087935	b76e917b2de4ed57ccd23f4b7d3af3c33f3709da


arrange(filter(datc, userid %in% c(9087935,
                                   9089960,
                                   9090031,
                                   9089960,
                                   9090031,
                                   9087935,9055476,9089925),     )
        
kek <- c(9087935,
         9089960,
         9090031,
         9089960,
         9090031,
         9087935,9055476,9089925)
        
        



        j=0
        j=j+1
        #tmp <- arrange(filter(comments, userid == kek[j]), desc(disgust))
        #tmp[1:min(20, nrow(tmp)),c("content", "disgust", "modifytime", "userid")]
        j
        kek[j]
        
        Look(kek[j])
        
        
        
