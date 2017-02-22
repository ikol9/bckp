library(tagStats)
library(dplyr)
library(gbm)
library(rpart)
library(rattle)
source ("~/.Rprofile")
library(cluster)
library(AnomalyDetection)
library(cluster)
library(RCurl)
library(googlesheets)
library(RPostgreSQL)

file.sources = list.files(c("/Users/lho/Downloads/tagStats/R"), 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE)

sapply(file.sources,source,.GlobalEnv)

library(RPostgreSQL)
source("/Users/lho/Downloads/tagStats/R/makeCxn.R")



system(paste0("R CMD build ", '/Users/lho/Downloads/tagStats'))
install.packages(list.files('./', pattern = 'tagStats_1.1.tar.gz')[1], type = 'source')


install.packages(list.files('./', pattern = 'tagStats_1.1.tar.gz')[1], type = 'source')

config.redshift <- list(
  drv <- "PostgreSQL",
  host = "ramblas-poc.cxchz6z8xmcj.us-west-2.redshift.amazonaws.com",
  port="5439",
  dbname="ramblas", 
  user = "lho",
  password = "1LHo1234"
)


require("Rfacebook")

fb_oauth <- 'EAAESxULj15oBAIh3ZBzhOZBfY4ut1KHbrHk9thBmUPZATPotjDcNftvkuPt8An3HLkZBdC1tLnw1wuY4LrKfiLa7hGkqRkRrd3oZBgWC55lS4xsZCKthZBXsjk9EhGZCbbkwG3YZBTxF9GtspKhf98cyZCPGkaUrDZCEKRvOghRdkqYwAZDZD'
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

getUsers("me",token=fb_oauth)

"http://www.facebook.com/search/results.php?q=louis.h.ho@gmail.com"

con <- curl("https://httpbin.org/get")
readLines(con)

http://www.facebook.com/search/results.php?q=<email address here>
  
huh = curl("https://www.facebook.com/search/top/?q=ikolsregards%40hotmail.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=louis.h.ho%40gmail.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=arceomariana0%40gmail.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=hindboucague%40gmail.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=cnjames540%40gmail.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=echoboi%40yahoo.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=adfasdf%40botsfeld.com", open = "")
huh = curl("https://www.facebook.com/search/top/?q=condmen%40botsfeld.com", open = "")

https://www.facebook.com/search/str/louis.h.ho%2540gmail.com/keywords_top
bwuh <- readLines(huh)
grepl("See More Results", bwuh[2])
close(huh)

## detect..cut away variables, chisq test if signif


grepl("alt=\\\\", 'wtf is this \ hm')

library(curl)

## make wat -> actual entry pull auto
## capslock %
## Keep track of number of bulkmarks
## photo hash so I can ignore pets players spiking my shit
## XML tree? to make the filtering process go faster
## group a bunch with high activity.... might be worth looking at low hits if nfws or msg is high
## add badisp boolean
## look for businesses on tagline
## singleton <- did I mean class or something?

## double email where one acc is nigerian: 6045967578
## finish 61 series?


## sentiment analysis on wechill comments

## bad payment accounts/ids

#https://cleantalk.org/blacklists/AS4250 grab and search from here

## send calvin a list of accounts to refund

#automate notes for current regs

### MOTHERFUCKER ...  dood just run reg spike method on slices of monthly metricssssss yoooooooooooo

## make a personal monthly report


## the hell is stored as parquet
## jimdo.com in messages 
## lots of parens in msgs : 6041386157

## simulate how random or calculate what you can actually detect on 250 n sample

## orgy: drug and disease free
## just search romancescam.com yourself; scamsurvivors.com; anti-scam-forum.net; scamwarners.com;scamsurvivors.com;ripandscam.com;


### if we throw someone into spammerworld, can we remove msgs from nonspammers...stop them from reading/harvesting email?

## automate accuracy email

#"age is just a number/ distance doesn't matter"

#email has current year

#take ritas update button and roll it into your auto mthly

#backup your shit
  
 ## double parens in msgs like 6054073482 ua/ru spam


## email has this year as part of the name
## https://cleantalk.org/blacklists/charlesworth@gmail.com
## You have been selected to receive a whooping sum of £1million
## use my own responses for what I find as training
## FB accounts...minimum number of FB friends
## "I am not interested in games or drama"
## "having past events shape your life is one thing carrying the past as a burden that sits heavily upon your shoulders is not the way i view life."
## score an account and then send for manual review
## should we add in safety tips
## locate me button...good signal?
## ramen red hot cheetos spread
## 'unknown itel itel' somethin in ua: 6051550019
## lots of male female switch?

## chinese brands: 
#   meizu huawei) xiaomi Gionee  innjoo timmy Spreadtrum viwa oppo (Fortune Ship) (orange luno) xgody X551<-infinix  leimin Jlinksz 
# india: Micromax 
# uae: magnus
# africa: MOBICEL (tecno)|(infinix)|(itel)|(midcom)|(fero)|(mobicel)|(transsion)

  ## Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36 OPR/41.0.2353.69 (Edition Campaign 34)
## filter(dat, grepl('tecno|infinix|itel|midcom|fero|hisense', ua))$ua

nrow(filter(dat, grepl('tecno|infinix|itel', ua), spam==0, cc_iso %in% c('US', 'GB', 'FR', 'CA', 'AU', 'NL', 'NO', 'SE', 'CH', 'DE')))

## tecno, infinix is also a african only ua
## perdate chinese app, gaga, ana
## entropy value as part of glm

##straight up ips if one is bad

## god-fearing, baby


## single var super spikes like photohashes, names, displaynames

## create keyword cooccurence for caught spam: https://www.r-bloggers.com/turning-keywords-into-a-co-occurrence-network/

# Sites that signed up to the ACCC’s Best
# Practice Guidelines said they were initially
# concerned that sending scam warning
# messages might discourage customers.
# However feedback from those who adopted
# the recommended measures “indicates that
# customers have responded positively and felt
# secure in the knowledge that service
# providers were actively protecting their
# interests.” 
# sw needs profile viewers

# single compares like ...multiple instances of gisele taglines
# ISP us location is legit but loc is random as fuck
# 23Media GmbH bad isp possibly


## vice doc "internet scamming in ghana"
## '419 scammers'
#Vice documentary ‘Internet Scamming in Ghana’.

a man who knows how to treat a woman and who
understands her deep inside
my dream is to be in arms of right and wise man
trust me i am romantic tender honest intelligent and
active lady
forthright and honest i value integrity a sense of
fairness
i like to laugh and have been described as having a
good sense of humor


credit card checking reverse
1754/60

## TOr networks to disguise?

## parens in messages

## add extra "time till caught" var to data set

## auto add hrisps where theres kw with digitalocean etc

# “god fearing

## ask how chargeback rates are doing etc
## symantec has a spammer report? pindrop too
## double up on calidation date
## look into zipcodes
## look into any who have started to msg with some number
## Take out pets like menaj etc man
## after you sw or acknowledge something....take it out of the set for subsequent looks
## add device from reg logs
## large number of bulk actions on uid but still active .... need to canc

## you need to do text pattern analysis to figure out mmbots and what not

## instead of newtork rule....keep track of bad logins for yourself
## symanetc report, pindrop security report

# In the documentary “419: The Internet Romance Scam”, 
#“god fearing”.

## DO spam: rapid processing of mobile site browse sections, lots of filter changes, early VIP buy, payment signatures, ent of pay funnel cause always same linkid



#filter(dat, grepl('add me on facebook', tagline))
#6040694164

## msgs with just/mostly numbers
## am, byname
## message to age + country location + gender
## you could also check for spike changes of spam caught for groups....to see if something is no longer


## spoof rule...or just distance travelled as indicator
### photo snag like mmm?
## hiearchy of search?
## ignore accounts with keywords petsxx
## each search condition should have a threshold characteristic
##cc_iso %in% c('GB', 'CA', 'AU', 'NZ', 'NL', 'SG'),

## similar analysis for logins
## responsiveness as a feature; maybe with numbers or words too

## gender switching?

## H1 LLC DurableDNS Inc isp

## add that dating pref combo string

## do we not check for emails for bl on wall and photo comments?

##get msgs sent
##get age of recipients

## count logins n shit yo
## "god fearing"


## make a copy of dat and then just keep removing rows as you check
## you don't have to actually set ==i, mod the groupby?

## alert me when there is a spam spike too like with yandex 6/13 F



pa_sht <- gs_title('Payment alerts v1.2')
gs_browse(pa_sht)

xx0 <-gs_read(pa_sht, ws = 'VIP Subs')
colnames(xx0) <- c("Platform", "Provider", "Ct")
xx0[, "Ct"] <- as.numeric(xx2[, "Ct"])
xx1 <- xx0

xx2 <- merge(xx0,xx1,by = c("Platform", "Provider"), all.x= TRUE, sort= TRUE)
xx2 <- xx2[-9,]

xx2[,4] <- xx2[,4] *.5

###################### add

reg_sht <- function(x=NULL){
  if (is.null(x)){
    tmp_sht_name <- paste("tmp_",end_date <- Sys.Date(), sep = '')
    spam_sht <- gs_new(tmp_sht_name)
  } else {  
    spam_sht <- gs_title(x)
    gs_browse(spam_sht)
  }
}

refresh_sht <- function(x=NULL){
  if (is.null(x)){
    spam_sht <- gs_title(curr_sht)
  } else {
    spam_sht <- gs_title(x)
  }
}


curr_sht <- paste("tmp_",end_date <- Sys.Date(), sep = '')
spam_sht <- reg_sht()
spam_sht <- refresh_sht(curr_sht)
gs_browse(spam_sht)

QR <- data.frame(nick=character(),
                 query=character(),
                 stringsAsFactors=FALSE)

QR[1,] <- c('Quintlinks', "gender == 'F', spam !=1, DR >= '2016-01-01', grepl('geil.+[0-9]@freenet.de', email)")
QR[2,] <- c('UAE Pros', "gender == 'F', birthdate >= '1988-01-01', spam != 1, cc_iso == 'AE', grepl('([0-9]{2}.*){4}', display_name)")
QR[3,] <- c('Gisele', "gender == 'F', birthdate >= '1970-01-01', spam != 1,  grepl('new.+(Gisele|Giselle)', display_name)")
#QR[4,] <- c('mail.ru', "user_id %!in% c(6041603817, 6039420075,6048953674,6041399702), domain == 'mail.ru', gender == 'F', birthdate >= '1996-01-01', birthdate < '1997-01-01', spam != 1, reg_source %in% c('ind', 'rgn', 'fwn'), pets <=12, cc_iso %in% c('RU', 'UA')")
QR[4,] <- c('mail.ru', "domain == 'mail.ru', spam != 1, reg_source %in% c('dmi'), isp %in% c('OJSC Vimpelcom', 'CJSC TSI Service', 'Beeline Broadband', 'OJSC Rostelecom', 'JSC Informtehtrans', 'Dynamic Pools for Clients in the Samara Branch', 'HostBar Ltd.', 'PJSC MegaFon'), pets <=12, reg_iso == 'RU'")
QR[5,] <- c('HI5/Tagged Promo', "first_name %in% c('HI5 PROMOTION' ,'TAGGED PROMOTION', 'Taggedpromotion', 'Tagged promotion'), spam!=1")
QR[6,] <- c('Okoiji', "last_name == 'Okoiji' | first_name == 'Realove' | first_name == 'Reallove', gender == 'F', spam!=1")
#QR[7,] <- c('Daniela', "gender == 'F', spam!=1, grepl('auff([0-9]){3}', email), domain == 'mail.ru', is.na(date_cancelled)")
QR[7,] <- c('Belle', "gender == 'F', birthdate >= '1970-01-01', spam != 1,   grepl('-(s|S)kype id', display_name) | grepl('`(s|S)kype id: ', display_name),  !grepl('Gisel', display_name)")
QR[8,] <- c('IN-Hotmail/Outlook', "spam != 1, combo_iso == 'IN-IN', domain %in% c('outlook.com', 'hotmail.com'), isp == 'This Space is Statically Assigned.', reg_source == 'ind', gender =='F'")
QR[9,] <- c('Lottery', "is.na(date_cancelled),  cc_iso == 'GB', reg_source %in% c('iph', 'and'), 
              grepl('Carol M', display_name) | grepl('C Martin', display_name), birthdate >= '1959-01-01', birthdate <= '1965-01-01'")
QR[10,] <- c('MM bots', "spam !=1, domain == 'dropboxsecure.com', DR >= (as.Date(Sys.time())-1)")
QR[11,] <- c('bootycall', "spam!=1, is.na(date_cancelled), grepl('B(O|0)(O|0)TYCALLPLANET', gsub(' ', '',toupper(tagline)))")
QR[12,] <- c('RU 3s', "spam!=1, gender == 'F', (domain == 'mail.ee' & cc_iso != 'EE' & cc_iso %in% c('US', 'CA', 'GB') & msgs > 0 & pets <=50) | (first_name == 'Loren' & last_name == 'Loren')")
QR[13,] <- c('Xtra mailcom', "spam!=1, gender == 'F', birthdate == '1985-02-16', zipcode == 10011, reg_source == 'ind'")
##QR[14,] <- c('mail.ru alt', "gender == 'F', domain == 'mail.ru', spam != 1, reg_source %in% c('ind'), isp %in% c('OJSC Vimpelcom', 'CJSC TSI Service', Beeline Broadband', 'OJSC Rostelecom', 'JSC Informtehtrans', 'Dynamic Pools for Clients in the Samara Branch', 'HostBar Ltd.', 'PJSC MegaFon'), pets <=12, cc_iso %in% c('RU', 'UA'), reg_iso == 'RU'")
QR[14,] <- c('mail.ru2', "birthdate >= '1985-01-01', domain %in% c('bk.ru', 'mail.ru', 'yandex.ru', 'mail.ua'), gender == 'F',  spam != 1, (tagline %in% c('I LOVE VIRTUAL SEX.', 'i love sex') | (reg_source %in% c('ind', 'rgn') & isp %in% c('OJSC Vimpelcom', 'Beeline Broadband', 'OJSC Rostelecom', 'JSC Informtehtrans', 'Dynamic Pools for Clients in the Samara Branch', 'HostBar Ltd.', 'PJSC MegaFon', 'Iskratelecom CJSC'))), pets <=8, reg_iso == 'RU'")
QR[15,] <- c('IN rand', "spam!=1, gender == 'M', domain %in% c('protonmail.com'), combo_iso == 'GB-GB', pets <=8")
QR[16,] <- c('IN rand2', "spam!=1, gender == 'M', domain %in% c('gmail.com'), combo_iso == 'US-IN', pets <=8, reg_source == 'ind', locale == 'en_US', ua== 'Mozilla/5.0 (Windows NT 5.1; rv:49.0) Gecko/20100101 Firefox/49.0'")
QR[17,] <- c("afc lottery scam", "spam!=1, is.na(date_cancelled), reg_source == 'iph', cc_iso == 'GB', grepl('^dcm.+@outlook', email)")
QR[18,] <- c("DO link", "spam!=1, is.na(date_cancelled), reg_iso == 'DO', domain == 'haribu.com', gender == 'F'")
QR[19,] <- c("DO link2", "spam!=1, is.na(date_cancelled), pets <=10, reg_iso == 'DO', gender == 'F', birthdate >= '1970-01-01', locale %in% c('es_CO', 'es_ES'), reg_source %in% c('rgn', 'ind'), cc_iso != 'DO'")
QR[20,] <- c("NL-DE", "spam!=1, is.na(date_cancelled), combo_iso == 'NL-DE', domain == 'yandex.com'")
QR[21,] <- c("afc lottery scam2", "spam!=1, is.na(date_cancelled), domain %in% c('mail.com','outlook.com'), reg_source == 'iph', country_locale == 'GB-en_US', birthdate >= '1961-01-01', birthdate < '1967-01-01', reg_iso %in% c('CA', 'US', 'NL'), grepl('[0-9]', email), photo == 'y', msgs >= 1")
QR[22,] <- c("mishas", "spam!=1, is.na(date_cancelled), domain == 'gmx.com', reg_source == 'ind', gender == 'F', birthdate >= '1992-01-01', birthdate <= '1997-01-01'
              , cc_iso == 'US', locale == 'en_US',
             ua %in% c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.94 Safari/537.36',
                            'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Dragon/43.3.3.185 Chrome/43.0.2357.81 Safari/537.36',
                        'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Dragon/45.9.12.393 Chrome/45.0.2454.93 Safari/537.36')
             ")
QR[23,] <- c('ID/BD spam2', "birthdate >= '1990-01-01', gender == 'F',  spam != 1, reg_iso %in% c('ID', 'BD'), tagline %in% c('looking for a partner', 'looking for dating partner', 'looking for new friends', 'op zoek naar een partner', 'looking for a partner ', 'op zoek naar dating partner', 'enjoy the life', 'Looking for best partner')")
QR[24,] <- c('fbers', "gender == 'F', spam != 1, grepl('add me on facebook.+((\\\\()|(-->))', tagline)")
QR[25,] <- c('LV link spam', "gender == 'F', spam != 1, domain %in% c('mail.ru', 'bk.ru'), combo_iso == 'LV-LV', birthdate >= '1990-01-01', reg_source %in% c('ind', 'dmi') ")
QR[26,] <- c('loren2s', "gender == 'F', spam != 1, first_name == 'Loren', domain %in% c('mail.ru',   'mail.ua',   'bk.ru',     'yandex.ru', 'inbox.ru')")
QR[27,] <- c("DO link3", "spam!=1, is.na(date_cancelled), pets <=10, reg_iso == 'DO', gender == 'F', birthdate >= '1990-01-01', locale %in% c('es_CO', 'es_ES', 'en_US'), reg_source %in% c('rgn', 'ind', 'dmi'), ispdomain == 'codetel.net.do', cc_iso != 'DO'")
QR[28,] <- c("Nika0", "spam!=1, is.na(date_cancelled), gender == 'F', birthdate >= '1980-01-01', domain == 'mail.ru', first_name == 'Nika', substring(last_name,1,1) == 'O'" )
QR[29,] <- c("RuNuWave", "spam!=1, is.na(date_cancelled), gender == 'F', birthdate >= '1990-01-01', reg_source %in% c('ind', 'rgn'), locale %in% c('en_US', 'ru_RU'), grepl('19[0-9]{2}.+\\\\.ru$', email),  ua2 %in% c('Mozilla/.(WindowsNT.;WOW;rv:.)Gecko/Firefox/.','Mozilla/.(WindowsNT.)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.')")
QR[30,] <- c("RuNuWaveAU", "spam!=1, is.na(date_cancelled), gender == 'F', birthdate >= '1990-01-01', pets == 0 , grepl('\\\\.ru$', email), reg_source == 'ind',  locale == 'en_US', cc_iso == 'AU'")
QR[31,] <- c("AfCell", "spam!=1, is.na(date_cancelled),  grepl('(tecno)|(infinix)|(itel)|(midcom)|(fero)|(mobicel)|(transsion)|(x551)', ua), cc_iso %in% c('US', 'GB', 'AU', 'CA')")



# assemble known
#QR.res <- list()
#for (i in 1:nrow(QR)){
#  print('wtf')
#  QR.res <- append(QR.res, list(list(QR[i,1], eval(parse(text = paste("filter(dat,", QR[i,2], ")[,1]"))))))
#}
#if (length(booty)>0

#filter(dat, user_id %in% QR.res[[1]][[2]])

#lugalink.net

#tolower(aa)

#hmm <- filter(dat, first_name == 'Loren', domain %in% c('mail.ru',   'mail.ua',   'bk.ru',     'yandex.ru', 'inbox.ru'))



"mozilla/.(windowsnt.)applewebkit/.(khtml,likegecko)chrome/...safari/."                       
"mozilla/.(windowsnt.;win;x)applewebkit/.(khtml,likegecko)chrome/...safari/."                 
"mozilla/.(windowsnt.;wow)applewebkit/.(khtml,likegecko)chrome/...safari/.opr/...(editionexp)"
 
#summarize(group_by(filter(dat, ua2 == 'mozilla/.(windowsnt.;win;x)applewebkit/.(khtml,likegecko)chrome/...safari/.'), DR), ct = n())


#filter(dat)
  
#filter(dat, grepl('add me on facebook.+\\(', tagline))


#arrange(summarize(group_by(dat, display_name), ct = n()), desc(ct))
        
## UA-PA

#filter(dat, spam!=1, first_name %in% c('HI5 PROMOTION' ,'TAGGED PROMOTION', 'Taggedpromotion', 'Tagged promotion'))[,1]
#filter(dat, spam!=1, domain == 'mail.ru', reg_source %in% c('ind', 'rgn', 'dmi'), reg_iso == 'RU', cc_iso != 'RU')

#filter(dat, domain == 'mail.ru', gender == 'F',  spam != 1, 
#       reg_source %in% c('ind', 'rgn'), 
#       isp %in% c('OJSC Vimpelcom', 'Beeline Broadband', 'OJSC Rostelecom', 'JSC Informtehtrans', 'Dynamic Pools for Clients in the Samara Branch', 'HostBar Ltd.', 'PJSC MegaFon')
      # ,isp == 'CJSC TSI Service'
#       , pets <=8, reg_iso == 'RU')[,1]     

##              photohash %in% c('040a975c70c7c05e', 'd0d6925d04f0d694', '081712ff2e7d9996', '4992313c44507900', 'f0f228669588160e', '8b7a8ffcb9b789d1' )
      
#filter(dat, spam!=1, tagline %in% c('I LOVE VIRTUAL SEX.', 'i love sex'))[,1]

#arrange(filter(dat, grepl(first_name, tagline)),DR)

 booty <- fetchQuery("select distinct t.user_id from tagline_event t left join userdata_light u on t.user_id = u.user_id
             where lower(replace(tagline, ' ', '')) ~ lower('(B(O|0)(O|0)TYCALLPLANET)')
             and u.date_cancelled is null
             and (u.date_registered is null or u.date_registered::Date >= '1/1/2014')
             and t.dt::date +40 >= current_date", config.redshift)


 
# 
# 
# select distinct t.user_id from tagline_event t left join userdata_light u on t.user_id = u.user_id
# where tagline in ('I LOVE VIRTUAL SEX.', 'i love sex')
# and u.date_cancelled is null
# and (u.date_registered is null or u.date_registered::Date >= '1/1/2016')
# and t.dt::date +30 >= current_date
# and gender = 'F'


 
# assemble known
QR.res <- list()
for (i in 1:nrow(QR)){
  print('wtf')
  QR.res <- append(QR.res, list(list(QR[i,1], eval(parse(text = paste("filter(dat,", QR[i,2], ")[,1]"))))))
}
## append to bootycall
QR.res[[11]][[2]] <- c(QR.res[[11]][[2]], booty[,1])

  
  # add to worksheets
    for (i in 1:length(QR.res)){
      if (length(QR.res[[i]][[2]])>=1){
        if (QR.res[[i]][[1]]  %in%  gs_ws_ls(spam_sht)){
          gs_add_row(spam_sht, ws = QR.res[[i]][[1]], input = QR.res[[i]][[2]])
        } else {
          spam_sht <- gs_ws_new(spam_sht, QR.res[[i]][[1]], input = QR.res[[i]][[2]], trim = FALSE)
        }
      }
    }

spam_sht <- gs_ws_new(spam_sht, 'all')
spam_sht <- gs_edit_cells(spam_sht, ws ='all', input = c('Type','UID'), byrow=TRUE)

all.spam <- filter(data.frame(do.call(rbind, sapply(1:length(QR.res), function(x){ cbind(if(length(QR.res[[x]][[2]])==0){0}else{QR.res[[x]][[2]]}, QR.res[[x]][[1]]) }))), X1 !=0)
colnames(all.spam) <- c("UID","Nick")  
spam_sht <- gs_edit_cells(spam_sht, ws ='all', input = all.spam)


### that arg shit needs to be null
update_spam <- function(sht=spam_sht){
  xx0 <-gs_read(sht, ws = 'all')[[1]]
  dat[dat$user_id %in% xx0, "spam"] <<-1
  if (exists("xx")){
    dat[dat$user_id %in% xx, "spam"] <<-1
  }
}
update_spam()



##########################
slim= 0

fetchQuery("select min(date_registered::Date), max(date_registered::date) from lh_udetect limit 100;")
fetchQuery("select min(date_registered::Date), max(date_registered::date) from lh_udetect_cur limit 100;")

  
fetchQuery("delete from lh_udetect where date_registered::Date = (select min(date_registered::date) from lh_udetect_cur);

'talkblog.cf', 'iword.ml', 'mail.ru', 'mail.com'


update lh_uDetect 
set date_spammer_added = u.date_spammer_added,
date_cancelled = u.date_cancelled,
date_spammer_removed = u.date_spammer_removed
from lh_uDetect l join userdata_light u
on l.user_id = u.user_id
where u.date_registered::Date >=  current_date - 31;


insert into lh_udetect
select * from lh_udetect_cur", config.redshift)




a <- Sys.time()
fetchQuery("drop table if exists lh_uDetect;
            select u.*, 
CASE WHEN (cancel_reason_code IS NOT NULL AND cancel_reason_code IN (1, 3, 5, 6, 10, 17, 20, 21, 22, 31, 32, 33, 37))
                  OR (date_spammer_added IS NOT NULL AND date_spammer_removed IS NULL) 
           OR (date_spammer_added IS NOT NULL AND date_spammer_removed IS NOT NULL AND date_spammer_added > date_spammer_removed)
           THEN 1 ELSE 0 END as spam,

       (split_part(ip_address, '.', 1)::bigint << 24) +
       (split_part(ip_address, '.', 2)::bigint << 16) +
       (split_part(ip_address, '.', 3)::bigint << 8) +
       (split_part(ip_address, '.', 4)::bigint) as ipInt,
       0 as pets,
       cast(0 as numeric) as usd_amt,
       0 as vip,
       0 as pFunnel,
       0 as badtag,
       cast(null as varchar(256)) as isp,
       cast(null as varchar(256)) as ispdomain,
       cast(null as char(2)) as reg_iso,
       0 as badisp,
       cast(null as varchar(100)) as photohash,
       cast(null as varchar(256)) as tagline,
       cast(null as varchar(256)) as displayname,
       0 as msgs,
       cast(null as varchar(256)) as ua
            into public.lh_uDetect
            from userdata_light u
            where --email = 'lho@ifwe.co' 
          --user_id in (select user_id from public.lh_sandshrew)
           date_registered::date = current_date - 30", config.redshift)
b <- Sys.time()
##4 min
##51 secs

fetchQuery("GRANT all privileges on public.lh_udetect to public;")


if (slim<=4){
  a<-Sys.time()
  fetchQuery("update lh_uDetect
            set isp = ipisp, ispdomain =ipdomain, reg_iso = x.cc_iso
           from lh_uDetect s,
           (select s.user_id, ipisp, ipdomain, i.cc_iso from lh_uDetect s join ip_ref i 
           on ipInt between  ipfrom and ipto) x
           where s.user_id = x.user_id
           ",config.redshift)
  b <-Sys.time()
  b-a
}
## 18 min


if (slim <=1){
  a<-Sys.time()
  fetchQuery("update lh_uDetect
            set displayname = x.displayname
           from lh_uDetect s,
          (select user_id, displayname, ROW_NUMBER()  over (partition by user_id order by dt desc)  as rn
            from displayname_event where displayname <> '' and dt::date >= current_date - 31
          ) x
          where s.user_id = x.user_id
          and x.rn = 1
           ",config.redshift)
  b <-Sys.time()
  b-a
}
##40 secs


a<-Sys.time()
fetchQuery("
           update lh_uDetect
           set tagline = x.tagline
           from lh_udetect s join
           (select user_id, tagline, ROW_NUMBER()  over (partition by user_id order by dt desc)  as rn
           from tagline_event where tagline <> '' and dt::date >= current_date - 31
           ) x
           on s.user_id = x.user_id
           where x.rn = 1
",config.redshift)
           b <-Sys.time()
           b-a
           ## 1.5 min

if (slim <=3){
  a<-Sys.time()
  fetchQuery("update lh_uDetect 
  set pets = x.ct
  from (select buyer_id, max(pets_purchased) ct from pets_purchased_event m 
  where buyer_id in (select user_id from lh_uDetect) 
  and dt >= current_date - interval '30 days' 
  group by 1) x
  where user_id = x.buyer_id", config.redshift)
  b <-Sys.time()
  b-a
}
## 1.5 min

if (slim <=3){
  a<-Sys.time()
  fetchQuery("
update lh_uDetect 
set msgs = x.ct
from (select from_user_id, count(*) ct from message_event m 
where action_type = 'send'
and dt::date>= current_date -31
and from_user_id in (select user_id from lh_udetect) group by 1) x
where user_id = x.from_user_id;",config.redshift
  )
  b <-Sys.time()
  b-a
}
##13 min

           if (slim <=3){
             a<-Sys.time()
             fetchQuery("
                        update lh_uDetect 
                        set msgs = x.ct
                        from (select from_user_id, count(*) ct from message_event m 
                        where action_type = 'send'
                        and dt::date>= current_date -31
                        and from_user_id in (select user_id from lh_udetect) group by 1) x
                        where user_id = x.from_user_id;",config.redshift
             )
             b <-Sys.time()
             b-a
           }
           ##13 min
           
           
           
if (slim <=1){
  a<-Sys.time()
  fetchQuery("update lh_uDetect 
           set photohash= photo_hash
           from lh_uDetect u join photo_upload_event m 
           on u.user_id = m.user_id
           and u.primary_photo_id = m.photo_id
           where dt::date>= current_date -31
          ",config.redshift)
  b <-Sys.time()
  b-a
}
## 1.5 min


if (slim <=1){
  a<-Sys.time()
  fetchQuery("update lh_uDetect 
             set ua= meta__request_user_agent
             from lh_uDetect u join registration_event m 
             on u.user_id = m.user_id
             where dt::date>= current_date -32
             ",config.redshift)
  b <-Sys.time()
  b-a
}
## 1.5 min

           
           if (slim<=4){
             a<-Sys.time()
             fetchQuery("update lh_uDetect
              set isp = ipisp, ispdomain =ipdomain, reg_iso = x.cc_iso
             from lh_uDetect s,
             (select s.user_id, ipisp, ipdomain, i.cc_iso from lh_uDetect s join ip_ref i 
             on ipInt between  ipfrom and ipto) x
             where s.user_id = x.user_id
             ",config.redshift)
             b <-Sys.time()
             b-a
           }
           
##bad ua: Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36 OPR/42.0.2393.94

dat <- fetchQuery("select u.user_id, 
                  CASE WHEN (cancel_reason_code IS NOT NULL AND cancel_reason_code IN (1, 3, 5, 6, 10, 17, 20, 21, 22, 31, 32, 33, 37))
                  OR (date_spammer_added IS NOT NULL AND date_spammer_removed IS NULL) 
                  OR (date_spammer_added IS NOT NULL AND date_spammer_removed IS NOT NULL AND date_spammer_added > date_spammer_removed)
                  THEN 1 ELSE 0 END as spam, 
                  gender, birthdate, date_registered, 
                  email, SUBSTRING(email FROM POSITION('@' IN email)+1) as domain, cc_iso,
                  locale, last_login_date, date_validated, coalesce(apps_optout_settings_1,999) as apps_optout_settings_1, 
                  state, zipcode, reg_source,  first_name, last_name, coalesce(inferred_ethnicity,999) as inferred_ethnicity,
                  coalesce(sexual_preference,'Default') as sexual_preference,
                  type, case when primary_photo_id is null then 'n' else 'y' end as photo,
                  coalesce(dating,'Default') as dating, coalesce(friends,'Default') as friends, 
                  coalesce(serrelationship,'Default') as serrelationship, coalesce(networking,'Default') as networking,
                  coalesce(relationship_status,'Default') as relationship_status,
                  --hide_online_status,
                  pets, usd_amt, vip, pfunnel, badtag,
                  isp, ispdomain, reg_iso,
                  badisp,
                  photohash, tagline, display_name, displayname, msgs, date_cancelled, date_spammer_added, date_spammer_removed,
                  dating || '.' || friends ||'.' || serrelationship || '.' ||networking || '.' ||relationship_status || '.' ||sexual_preference as dcombo,
                  ua, mms
                  from lh_uDetect u", config.redshift)


fetchQuery("select max(date_registered) from lh_udetect", config.redshift)
## 5 min

dat$DR <- as.Date(as.POSIXct(strptime(dat$date_registered, "%Y-%m-%d")))

dat$full_name <- paste(dat$first_name, dat$last_name)
dat$combo_iso <- paste(dat$cc_iso, dat$reg_iso, sep = '-')
dat$country_locale <- paste(dat$cc_iso, dat$locale, sep = '-')

colnames(dat)
facN <- c("inferred_ethnicity","type","photo", "dating", "friends","serrelationship", 
          "networking", "gender", 'relationship_status', 'reg_source', 
          'zipcode', 'state', 'apps_optout_settings_1','locale', 'domain', 'cc_iso', 'reg_iso', 'isp', 'ispdomain',  'sexual_preference')

for (i in (facN[-1])){
  dat[, i] <- as.factor(dat[,i])  
}

dat[dat$gender == 'f', "gender"] = 'F'
dat[dat$gender == 'm', "gender"] = 'M'

dat$ua2 <- gsub(tolower("[[:space:]]|[0-9]"), "",dat$ua)

start_date <- Sys.Date()-30
end_date <- Sys.Date()-1

date_index <- data.frame(DR= as.Date(as.POSIXct(strptime(seq(start_date, end_date , "days"),"%Y-%m-%d"))))


safeList <- c(6045920924,
              6046900571,7325476720, 7327209527,
              6052160641, 6055190024, 6046166485, 6055355080,6046900571,6046166485, 6055190024,6045501950,
              6052917569, 6047764314, 6047711196, 6046120101,6055546955,6045854060,6045840484,
              6048862483, 6046533073, 6047871645, 6046392621,
              6056084427,
              6047871645,
              6052917569, 6046120101, 6045840484, 6056637704,6056770184,6046533073,6047871645,
              6044649638, 6048369912, 6047899375, 6048074776, 6048862483,6046392621,6046347437,
              6044649638)
dat[dat$user_id %in% safeList,"spam"] = 1

max(dat$DR)

getUniques <- function(data, x, threshold){
  bdown <- plyr::count(data, x)
  bdown[bdown[,2] >= threshold,]
}
getUniques(dat, "gender",100)

idx[[1]] <- 
  
  
aggregate(dat[,"gender"], by = list(dat[, "gender"]), )
Gender <- filter(summarise(group_by(dat, gender), ct = n()), ct >= 5)[,1][[1]]
domain <- filter(summarise(group_by(dat, domain), ct = n()), ct >= 10)[,1][[1]]

cl.defs <- list()
cl.defs[[1]] <- list("test4", list("gender", "==", list("M")), list("reg_source", "%in%", list("ind", "rgn")), list("cc_iso", "==", "IN"))
cl.defs[[2]] <- list("test5", list("gender", "==", list("F")), list("reg_source", "%in%", list("ind", "rgn")), list("cc_iso", "==", "US"), list("locale", "!=", "en_US"))



cl.defs[[1]] <- list("test4", list("gender", "==", list("M")), list("gender", "%in%",  list("ind", "rgn")), list("cc_iso", "==", "IN"))


cl.defs1 <- list("test4", list("gender", "=", list("M")), list("reg_source", "%in%", list("ind", "rgn")))

paste(cl.defs1[[i]][[1]], cl.defs1[[i]][[2]], cl.defs1[[i]][[3]])
paste(cl.defs1[[i]][[1]], cl.defs1[[i]][[2]], paste("c(", paste("'", unlist(cl.defs1[[i]][[3]]), "'", sep = "", collapse =","), ")", sep = ""))

idxs <- data.frame( group = c(), idx1 = c())
idxs <- list()
for (j in cl.defs){
  cl.defs1 <- j
  
  tmp = TRUE
  i = 1
  
  for (j in getUniques(dat, "gender" ,100)[,1]){
    dat[, "gender"] == j
  }
  idxs <- append(idxs, list(list(cl.defs1[[1]], tmp)))
}

x <-c("gender", "domain")
c("gender", "isp")

gender <- domain <- list()
gender[[1]] = dat$gender == "M"
gender[[2]] = dat$gender == "F"
domain[[1]] = dat$domain == "mail.ru"
domain[[2]] = dat$domain == "yahoo.com"

getUniques(dat, "gender", 100)

for (i in x){
  for(j in getUniques(dat, i, 100)[,1]){
    cl.defs[[1]] <- list(paste(x, sep = "_", collapse= "_"), 
                         list(i, "==", list(j)))
  }
}

cl.defs[[1]] <- list("test5", list("gender", "==", list("F")), list("reg_source", "%!in%", list("ind", "rgn")))



dat2 <- filter(dat, gender == 'F', spam != 1, reg_source %!in% c('ind', 'rgn', 'fwn') )#, reg_source %in% c('ind', 'rgn', 'fwn')) ## (reg_source %in% c('ung', 'ind', 'rgn', 'fwn','and')))

es <- filter(dplyr::summarise(group_by(dat, domain), ct = n()), ct >= 10)[,1][[1]]
es <- filter(dplyr::summarise(group_by(dat, isp), ct = n()), ct >= 15)[,1][[1]]

es <- filter(summarise(group_by(dat, full_name), ct = n()), ct >= 5)[,1][[1]]
es <- filter(summarise(group_by(dat, displayname), ct = n()), ct >= 10)[,1][[1]]

es <- filter(dplyr::summarise(group_by(dat, combo_iso), ct = n()), ct >= 10)[,1][[1]]
es <- filter(dplyr::summarise(group_by(dat, locale != 'en_US', country_locale), ct = n()), ct >= 10)[,1][[1]]

es <- filter(dplyr::summarise(group_by(dat, reg_source), ct = n()), ct >= 10)[,1][[1]]
es <- filter(dplyr::summarise(group_by(dat, cc_iso), ct = n()), ct >= 10)[,1][[1]]

es <- filter(dplyr::summarise(group_by(dat, cc_iso), ct = n()), ct >= 10)[,1][[1]]

es <- filter(dplyr::summarise(group_by(dat, zipcode), ct = n()), ct >= 10)[,1][[1]]


cat(filter(dat, gender == 'F', spam ==1,domain == 'yandex.com', DR == '2016-06-13', reg_source == 'and')[,1])


a <- Sys.time()  
for (i in 1:100){
  summarise(group_by(filter(dat,gender =='F', cc_iso == 'US', reg_iso != 'RU'), DR), ct=n())
}
b <- Sys.time()  
b-a

dplyr::summarise(group_by(filter(dat), gender, gender), ct=n())

var.splits <- c('gender', 'domain', 'combo_iso', 'reg_source', 'isp')
var.splits <- c('gender', 'domain', 'combo_iso', 'reg_source', 'isp', 'dcombo')
#'dating', 'friends', 'serrelationship', 'ispdomain'
#'networking', 'relationship_status', 
#'sexual_preference'

#var.combo <- as.data.frame(arrange(expand.grid(a=var.splits,b=var.splits
#                                               ,c = var.splits
#                                               ),a))
var.combo <- t(combn(var.splits,3))
var.combo <- rbind(var.combo, c("combo_iso", "combo_iso", "locale"))
var.combo <- rbind(var.combo, c("gender", "domain", "domain"))
var.combo <- rbind(var.combo, c("gender",  "combo_iso", "combo_iso"))
var.combo <- rbind(var.combo, c("gender",  "gender", "isp"))
var.combo <- rbind(var.combo, c("gender",  "combo_iso", "locale"))
var.combo <- rbind(var.combo, c("cc_iso",  "cc_iso", "domain"))
var.combo <- rbind(var.combo, c("cc_iso",  "domain", "reg_source"))
var.combo <- rbind(var.combo, c("cc_iso",  "cc_iso", "domain"))
var.combo <- rbind(var.combo, c("cc_iso",  "gender", "domain"))
var.combo <- rbind(var.combo, c("domain",  "domain", "domain"))
##var.combo <- rbind(var.combo, c("display_name",  "display_name", "display_name"))
##var.combo <- rbind(var.combo, c("full_name",  "full_name", "full_name"))
var.combo <- unique(var.combo)

for(i in 1:nrow(var.combo)){
  var.combo[i,] <- sort(var.combo[i,])  
}  
var.combo <- rbind(var.combo, c("domain",  "domain", "domain"))
var.combo <- unique(var.combo)
#var.combo <- rbind(var.combo, c("full_name",  "full_name", "full_name"))
#var.combo <- rbind(var.combo, c("displayname",  "displayname", "displayname"))

##var.combo <- rbind(var.combo, c("full_name, full_name, full_name"))
##var.combo <- rbind(var.combo, c("displayname,displayname,displayname"))
#var.combo <- rbind(var.combo, c("dating, friends, serrelationship, ispdomain"))

# var.combo[21,] <-  c("combo_iso", "combo_iso", "locale")
# var.combo <- var.combo[var.combo[,2] != var.combo[,3],]

#  a <- Sys.time()

  
###HEREEEEE  
dat2 <- summarise(group_by(filter(dat, spam!=1), #pets <=20, is.na(date_cancelled)), 
                                   ## domain == 'gmail.com', dcombo == '0.0.0.0.X.X', combo_iso == 'US-US', gender == 'F'), 
                                   DR,  gender, domain, locale, reg_source, combo_iso, 
                                   ispdomain, isp, cc_iso
                            # , dcombo
                             ), ct = n())
  m = 1
  mdat <- max(dat2$DR)
  a <- Sys.time()
  wat <-c()
  watplot <- list()
  
  for(g in 1:nrow(var.combo)){ #length(var.splits)){
    #
    ekk <- dplyr::summarise(group_by_(filter(dat2), #, spam!=1, pets <=30),
                                      .dots = c(as.character(unlist(var.combo[g,])), "DR")), ct=sum(ct))
    print(paste("G1: ", g))
    data.frame(ekk) -> ekk2
    
    idx.name.0 <- c()
    for (i in 1:(length(ekk2[1,])-2)){
      idx.name.0 <- cbind(idx.name.0, paste0(colnames(ekk2)[i],": ", as.character(unlist(ekk2[,i])), sep = ""))
    }
    
    print(paste("G2: ", g))
    idx.name<- c()
 #   for (i in 1:nrow(idx.name.0)){
  #    idx.name <- rbind(idx.name, paste0(idx.name.0[i,], collapse =", "))
   # }
  #  idx.name<- paste(ahh[,1], ahh[,2], ahh[,3])
    if(length(idx.name.0[1,]) == 3){
      idx.name <- paste(idx.name.0[,1], idx.name.0[,2], idx.name.0[,3])
    }else if(length(idx.name.0[1,]) == 2){
      idx.name <- paste(idx.name.0[,1], idx.name.0[,2])
    }else if(length(idx.name.0[1,]) == 1){
      idx.name <- paste(idx.name.0[,1])
    }   
    
      
    print(paste("G3: ", g))
    ekk2 <- cbind(idx.name, ekk2)
    
    if(c("locale") %in% colnames(ekk2)){
      ekk2 <- filter(ekk2, locale!= 'en_US')
    }
    
    if(c("dcombo") %in% colnames(ekk2)){
      ekk2 <- filter(ekk2, !is.na(dcombo))
    }
    
    print(paste("G4: ", g))
    #colnames(ekk) <- "boo"
    #a <- Sys.time()
    capA <- unique(filter(summarise(group_by(ekk2, idx.name,DR), mct = max(ct)), mct >= 10)[,1])
    
    for (i in capA[,1][[1]]){
      print(paste(g,i,m, mdat)) ##(reg_source %in% c('ung', 'ind', 'rgn')),reg_source %in% c('ind', 'rgn')),  cc_iso == "IN", reg_iso != 'US', !(reg_source %in% c('ung', 'ind', 'rgn', 'fwn'))
      ##hm <- dplyr::summarise(group_by(filter(dat2, combo_iso == i), DR), ct=n())
      hm <- dplyr::filter(ekk2,  idx.name == i)
      grpFilled <-merge(date_index, hm, by = c("DR"), all.x= TRUE, sort= TRUE)[,c("idx.name", "DR", "ct")]
      grpFilled[is.na(grpFilled[,3]),3] <-0
      #  res = AnomalyDetectionVec(grpFilled[,2], max_anoms=0.2, alpha = .1, period=7, direction='pos', only_last=FALSE, plot=TRUE, e_value =T, title = i, ylabel = "")
      # res$plot
      # cbind(1,hm)
      
      if(nrow(hm)){ 
        res = AnomalyDetectionVec(grpFilled[,3], max_anoms=0.2, alpha = .05, period=7, direction='pos', only_last=FALSE, plot=TRUE, e_value =T, title = i, ylabel = "")
        res$anoms$diff <- res$anoms$anoms- res$anoms$expected_value
        res$anoms$perdiff <- (round(100*(res$anoms$anoms- res$anoms$expected_value)/(res$anoms$expected_value),1))
        
        if(!is.null(res$plot)){
          res$anoms$sig = 0
          if( (max(res$anoms$diff, na.rm = TRUE) >= 8 && max(abs(res$anoms$perdiff), na.rm = TRUE) >= 800) || (max(res$anoms$diff, na.rm = TRUE) >= 100 && max(abs(res$anoms$perdiff), na.rm = TRUE)>= 10)){
            res$anoms$sig = 1
          }
          wat <- rbind(wat, cbind(i, m, res$anoms, grpFilled[res$anoms$index,2], max(grpFilled[res$anoms$index,2])))
          watplot <- append(watplot, list(res$plot))
          m=m+1
        } 
      }
    }
  }
  
colnames(wat)[10] <-"DR"
colnames(wat)[10] <-"mdate"

sOs <- '2017-02-19'
ceo <- filter(wat, diff>= 10, perdiff >= 10, mdate >= sOs )[,1:10]
ceo[ceo[,9] >= sOs & ceo[,5] <= 1 & ceo[,4] >= 5,][,1:9]


#7328503410
##!grepl('(mail.ru)|(Beeline)|(RU-RU)', i)
ceo <- filter(wat, diff>= 30, perdiff >= 100 , diff !=0 , mdate >= sOs)[,1:10]
arrange(ceo[ceo[,9] >= sOs & ceo[,5] >1,], m)
#arrange(ceo[ceo[,9] >= '2016-08-07',], desc(perdiff))[201:233,1:9]

ceo <- filter(wat, diff>= 200, perdiff >= 5 , diff !=0 , mdate >= sOs)[,1:10]
arrange(ceo[ceo[,9] >= sOs & ceo[,5] >1,], m)
#arrange(ceo[ceo[,9] >= '2016-08-07',], desc(perdiff))[201:233,1:9]


filter(dat, domain == 'mail.ru', spam!=1)


filter(dat, domain == 'mail.com', combo_iso == 'CO-CO', reg_source == 'ind', DR== '2016-08-31', spam!=1)[,1]

# locale == 'en_US',
arrange(filter(dat, spam!=1, domain == 'mail.ee', gender == 'F', cc_iso != 'EE', cc_iso %in% c('US', 'CA', 'GB'), msgs > 0), (DR))[,1]


#############################################
#############################################
#############################################


wtf<- 
filter(dat, 
spam==0, birthdate >= '1980-01-01',  pets <=10 | is.na(pets)
, domain %in% c('inbox.ru', 'mail.ru', 'yandex.ru', 'bk.ru', 'mail.ua')  #'inbox.ru', 'mail.ru', 'mail.ua', 'yandex.ru')
, gender == 'F' #, photo == 'y'
, DR >= '2017-02-02'  #& DR <=  '2016-09-13'
, ua2 %in% c('Mozilla/.(WindowsNT.)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.', 'Mozilla/.(WindowsNT.;WOW;rv:.)Gecko/Firefox/.', 'Mozilla/.(WindowsNT.;rv:.)Gecko/Firefox/.')
, reg_source %in% c('ind', 'rgn', 'dmi') 
 #,cc_iso %in% c('UA', 'RU', 'KZ', 'BY', 'CZ', 'TM')
)




wtf<- 
  filter(dat 
         #user_id==6048052188
,  spam==0, birthdate >= '1990-01-01', pets <= 10
, domain %in% c('inbox.ru', 'mail.ru', 'yandex.ru', 'bk.ru', 'mail.ua')  #'inbox.ru', 'mail.ru', 'mail.ua', 'yandex.ru')
, gender == 'F' #, photo == 'y'
, DR >= '2017-01-15'  #& DR <=  '2016-09-13'
, ua2 %!in% c('Mozilla/.(WindowsNT.)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.', 'Mozilla/.(WindowsNT.;WOW;rv:.)Gecko/Firefox/.', 'Mozilla/.(WindowsNT.;rv:.)Gecko/Firefox/.')
, reg_source %in% c('ind', 'rgn', 'dmi') 
, cc_iso %!in% c('UA', 'RU', 'KZ', 'BY', 'CZ', 'TM')
)

wtf <- filter(dat, spam==0, 
              domain %in% c('yandex.ru'), 
              cc_iso == 'UA',
              gender == 'F', #DR == '2017-01-22', 
              ua2== 'Mozilla/.(WindowsNT.;WOW)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.',
              locale== 'ru_RU')




cat(wtf[,1])


eh = 130
wat[wat$m==eh,]
watplot[[ eh ]]


cat(
  
  wtf<-  
      (
      arrange(filter(dat, spam==0   #is.na(date_cancelled)
                 # , pets <= 20
               #  , user_id ==6045566865
                  #, user_id %in% c(6051143947,6051981770)
                # , birthdate >= '1990-01-01'
              # , pets <= 10
            #   , isp == 'Republican Unitary Telecommunication Enterprise Beltelecom'
                # , grepl('(eamale)|(printemailtext)', domain)
              #  , domain %in% c('inbox.ru', 'mail.ru', 'yandex.ru', 'bk.ru', 'mail.ua')  #'inbox.ru', 'mail.ru', 'mail.ua', 'yandex.ru')
            #  , domain == 'mail.ru'
               , domain == 'mail.ru'
                , combo_iso == 'KZ-KZ'
               , gender == 'F' #, photo == 'y'
              # , grepl('19[0-9]{2}.+\\.((ru)|(ua))', email) #19[0-9]{2}
              #  , grepl('\\.((ru)|(ua))', email) #19[0-9]{2}
              # , cc_iso %in% c('KZ')
             #  , reg_iso == 'UA'
             # ,cc_iso == 'CN'
              #  , locale == 'ru_RU'
                , DR >= '2017-02-01'  #& DR <=  '2016-09-13'
              # , user_id %!in% dolly
               # , combo_iso %in% c( 'DE-PK')
               # , nchar(display_name) >= 16
             #   , ua ==  'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.157 Safari/537.36' #'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0'
              #  , ua2 %!in% c('Mozilla/.(WindowsNT.)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.', 'Mozilla/.(WindowsNT.;WOW;rv:.)Gecko/Firefox/.', 'Mozilla/.(WindowsNT.;rv:.)Gecko/Firefox/.')
          # , ua2 == 'Mozilla/.(WindowsNT.;Win;x)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.'
             #, ua2 == 'Mozilla/.(WindowsNT.;rv:.)Gecko/Firefox/.'
            #      , reg_source %!in% c('dmi')
              # ,# is.na(displayname)
             # ,country_locale == 'KR-zh_CN'
            # , photo =='y'
              # , cc_iso %in% c('RU', 'UA') #, 'DE', 'PL')
          #  , cc_iso %in% c('US', 'GB', 'HK', 'AE', 'CI', 'GR')
                 # , reg_iso %in% c('GB') ##'HK', 'JP', 'IE', 'NL','CH','CA','TH','TN','SI','AZ', 'SK','VN', 'CN','TW')
            #  , ispdomain =='midphase.com'
                  #, isp %in% c("JSC ER-Telecom Holding")
           #, cc_iso == 'US'
                  #, grepl('12345', email)
                  # , cc_iso %!in%  c('US','NL')
                 # , locale == 'ru_RU'fi9\
        # , isp == 'Republican Unitary Telecommunication Enterprise Beltelecom'
        #  , ispdomain == '163.com'
      #      , msgs >0 #| 
      # ,photo == 'y'
            #Statically Assigned.', 
         # , reg_source %in% c('ind') 
     # , gender =='F'
    #  , reg_source == 'ind'
            ), DR)) [,1]
)




filter(dat, spam==0,  isp %in% c('1&1 Internet SE', 'Contabo GmbH'))[,1]


arrange(filter(dat, ua == 'Tagged 6.6.1, Android 5.1'), desc(DR))
arrange(summarise(group_by(wtf, ua), ct = n()), desc(ct))

arrange(filter(dat, spam==1, !is.na(mms), DR == '2016-10-17'), desc(mms))[1:20,1]

arrange(filter(dat, DR >= '2016-09-01', spam!=1, gender == 'F', nchar(ua) <= 40,
               !grepl('Dorado', ua), desc(DR))) [,1]

cat((filter(dat, gender == 'F', domain == 'yandex.com', spam!=1, 
            DR >= '2016-06-09', nchar(first_name) <=4, pets <= 12, ispdomain != 'internap.com',
            !(first_name %in% c('Ben', 'Max', 'Ali', 'Tom', 'AIR', 'No', 'God')), 
            ispdomain %in% c('123host.vn', 'digitalocean.com', 'cyberghostvpn.com', 'esecuredata.com', 'wavecom.ee',
                             'egihosting.com', 'oneprovider.com',  'interserver.net', 'leaseweb.com', 'kaiaglobal.com', 'thegigabit.com'),
            
            reg_iso != 'PK', grepl( "bbartdin|pra", email)<=0, nchar(display_name) <= 12))[,1])




arrange(filter(dat, spam!=2, gender == 'F', domain == 'dropboxsecure.com'), desc(DR))  [,1]


hmm <- nrow(filter(dat, spam!=1, DR <= as.Date('2016-10-01'), as.character(reg_iso) != as.character(cc_iso), cc_iso %in% c('US') #, 'CA', 'GB', 'AU')#, 'DE', 'NL', 'NO', 'SE'),
              #, pets > 10 ##, domain == 'mail.ru'
              #, reg_iso != 'RU'
              #, locale == 'ru_RU'
              ,zipcode %in% c(10000:10011, 90000:90013, 12345, 11111)
              #, msgs >=1
       ))

sample_n(hmm,100)[,1]

filter(dat, spam!=1, reg_source %in% c('fan', 'and', 'gan'), gender == 'F')[1:10,]

summarize(group_by(filter(dat,spam==1), zipcode), ct =n()) ->mac
filter(arrange(mac, desc(ct))#, zipcode %in% 90000:90013
       )[20:30,]




arrange(filter(dat , spam!=1   #is.na(date_cancelled)
               # , pets <= 20
               #, user_id %in% c(6051143947,6051981770)
               #  , birthdate >= '1987-01-01'
               , domain %in% c( 'gmail.com') #--inbox.ru', 'yandex.ru')
               , gender == 'F' #, photo == 'y'
               , DR == '2016-11-17'  #& DR <=  '2016-09-13'
               , combo_iso %in% c( 'DO-DO')),DR)[1:30,]
               

huh <- filter(dat, gender == 'F', ## reg_source == 'gan', 
              # gender == 'F', domain %in% c('mail.ru', 'mail.ua', 'bk.ru', 'inbox.ru', 'yandex.ru'),
              # cc_iso %in% c('RU', 'UA', 'KZ'),
              # grepl('19[0-9]{2}@', email),
              reg_iso %in% c('ID'),
              pets <= 8,
              reg_source %in% c('ind', 'rgn'),
              ua2 %in% c('Mozilla/.(WindowsNT.)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.', 'Mozilla/.(WindowsNT.;Win;x)AppleWebKit/.(KHTML,likeGecko)Chrome/...Safari/.'),
              # combo_iso == 'AU-ID',
              cc_iso != 'ID',
              #   reg_source %!in% c('dmi'), #c('ind', 'rgn'), # reg_source %in% c('ind', 'rgn'),
              # DR == '2016-11-13'
              #  isp == 'Tricom',
              spam == 0, #pets <=40,
              #                                       ua2 == 'Tagged/..(samsungSM-GF;Android..;samsung/zeroltexx/zerolte:../MMBK/GFXXSDPJ:user/release-keys)'
              1==1)
  
  
#bucky <-filter(dat, combo_iso == "US-GB", gender == '', spam!=1, pets <=10), DR == '2016-06-14')
hm <- dplyr::summarize(group_by(filter(dat, gender == 'F', ## reg_source == 'gan', 
                                       gender == 'F', domain %in% c('mail.ru', 'mail.ua', 'bk.ru', 'inbox.ru', 'yandex.ru'),
                                      # cc_iso %in% c('RU', 'UA', 'KZ'),
                                      # grepl('19[0-9]{2}@', email),
                                       # reg_iso %in% c('ID'),
                                   # combo_iso == 'AU-ID',
                                      #  cc_iso != 'ID',
                                    #   reg_source %!in% c('dmi'), #c('ind', 'rgn'), # reg_source %in% c('ind', 'rgn'),
                                      # DR == '2016-11-13'
                                    #  isp == 'Tricom',
                                       spam == 0, #pets <=40,
#                                       ua2 == 'Tagged/..(samsungSM-GF;Android..;samsung/zeroltexx/zerolte:../MMBK/GFXXSDPJ:user/release-keys)'
                                       1==1), DR), ct= n())
#hm <- dplyr::summarise(group_by(filter(dat,  spam!=1, reg_iso %in% c('PH'), cc_iso == 'PH'), DR), ct=n())
grpFilled <-merge(date_index, hm, by = c("DR"), all.x= TRUE, sort= TRUE)
grpFilled[is.na(grpFilled[,2]),2] <-0
res = AnomalyDetectionVec(grpFilled[,2], max_anoms=0.2, alpha = .4, period=7, direction='both', only_last=FALSE, plot=TRUE, e_value =T, title = "bleh", ylabel = "")
res$plot
data.frame(hm)
res$anoms$diff <- res$anoms$anoms- res$anoms$expected_value
res$anoms$perdiff <- (round(100*(res$anoms$anoms- res$anoms$expected_value)/(res$anoms$expected_value),1))
cbind(res$anoms, grpFilled[res$anoms$index,1], max(grpFilled[res$anoms$index,1]))



data.frame(arrange(summarize(group_by(filter(dat, gender == 'M', ## reg_source == 'gan', gender == 'M', domain == 'gmail.com',
       reg_iso == 'PK',
       #reg_source %!in% c('ind', 'rgn'), # reg_source %in% c('ind', 'rgn'),
       # DR == '2016-11-14' 
       spam ==0,
       DR == '2016-11-14',
       1==1), reg_source, domain), ct = n()), desc(ct)))

xy <- data.frame(arrange(summarize(group_by(filter(dat, #reg_source == 'and', domain == 'gmail.com',
                                             gender == 'M', ## reg_source == 'gan', gender == 'M', domain == 'gmail.com',
       reg_iso == 'PK',
       #reg_source %!in% c('ind', 'rgn'), # reg_source %in% c('ind', 'rgn'),
       # DR == '2016-11-13'
       DR == '2016-11-06',
       1==1), yr = substring(birthdate,0,4)), ct = n()), desc(yr)))

hist(as.numeric(substring(filter(dat, reg_source == 'and', domain == 'gmail.com', gender == 'F', ## reg_source == 'gan', gender == 'M', domain == 'gmail.com',
      reg_iso == 'PH',
      #reg_source %!in% c('ind', 'rgn'), # reg_source %in% c('ind', 'rgn'),
      # DR == '2016-11-13'
      spam ==0,
      DR == '2016-11-06',
      1==1)$birthdate, 0,4)), breaks = 20)


res


filter(dat, reg_source == 'and', domain == 'gmail.com', gender == 'F', ## reg_source == 'gan', gender == 'M', domain == 'gmail.com',
       reg_iso == 'PH',
       #reg_source %!in% c('ind', 'rgn'), # reg_source %in% c('ind', 'rgn'),
       # DR == '2016-11-13'
       spam ==0,
       DR == '2016-11-06',
       birthdate >= '1/1/1998',
       1==1)
       
arrange(summarize(group_by(filter(dat, user_id %in% pat, reg_iso == 'US'),DR), ct = n()), DR)



filter(dat, spam!=1, gender== 'M', combo_iso == 'MX-MX',reg_source %in% c( 'fan', 'gan','and'), DR== '2016-10-04')[1:100,]




menace <- filter(dat, spam!=1,  cc_iso %in% c('PH'), reg_source %in% c('fan', 'gan', 'and'), gender== 'M', DR == '2016-10-02', isp %in% c('Globe WiMAX IP Pool', 'Philippine Long Distance Telephone Company', 'PLDT ALAHME60I01 DHCP'))
atwork <-   filter(dat, spam!=1,  cc_iso %in% c('PH'), reg_source %in% c('fan', 'gan', 'and'), gender== 'M', DR >= '2016-10-01')
arrange(summarize(group_by(atwork,isp), ct = n()), desc(ct))


### PUKE

gsub(tolower("[[:space:]]|[0-9]"), "",dat[1, 'ua'])

dat$ua2 <- gsub(tolower("[[:space:]]|[0-9]"), "",dat$ua)


es <- filter(dplyr::summarise(group_by(filter(dat),ua), ct = n()), ct >= 8)[,1][[1]]

a <- Sys.time()
#
ekk <- dplyr::summarise(group_by(filter(dat, #gender == 'M',
                                        spam!=1,  
                                        ua %in% es #pets <= 50 ##,  reg_source %in% c('ind', 'rgn', 'fwn')
), ua, DR), ct=n())
data.frame(ekk) -> ekk2
#colnames(ekk) <- "boo"
#a <- Sys.time()



wat2 <-c()
watplot2 <- list()
for (i in es){
  hm <- dplyr::filter(ekk2, ua == i)
  grpFilled <-merge(date_index, hm, by = c("DR"), all.x= TRUE, sort= TRUE)[,-2]
  grpFilled[is.na(grpFilled[,2]),2] <-0
  #  res = AnomalyDetectionVec(grpFilled[,2], max_anoms=0.2, alpha = .1, period=7, direction='pos', only_last=FALSE, plot=TRUE, e_value =T, title = i, ylabel = "")
  # res$plot
  # cbind(1,hm)
  print(i)
  if(nrow(hm)){ 
    res = AnomalyDetectionVec(grpFilled[,2], max_anoms=0.2, alpha = .05, period=7, direction='pos', only_last=FALSE, plot=TRUE, e_value =T, title = as.character(i), ylabel = "")
    res$anoms$diff <- res$anoms$anoms- res$anoms$expected_value
    res$anoms$perdiff <- (round(100*(res$anoms$anoms- res$anoms$expected_value)/(res$anoms$expected_value),1))
    
    if(!is.null(res$plot)){
      res$anoms$sig = 0
      if( (max(res$anoms$diff, na.rm = TRUE) >= 5 && max(abs(res$anoms$perdiff), na.rm = TRUE) >= 200) || (max(res$anoms$diff, na.rm = TRUE) >= 80 && max(abs(res$anoms$perdiff), na.rm = TRUE)>= 5)){
        res$anoms$sig = 1
      }
      wat2 <- rbind(wat2, cbind(i, which(i==es), res$anoms, grpFilled[res$anoms$index,1], max(grpFilled[res$anoms$index,1])))
    } else {
    }
    watplot2 <- append(watplot2, list(res$plot))
  }else{
    watplot2 <- append(watplot2, list(NA))
  }
}

colnames(wat2)[10] <-"mdate"
filter(wat2, diff >= 30, mdate >= '2016-09-01')[,c(2:10)][,1] -> bleh

3,4,5,15,16,17

bleh
j = 
  8
k = bleh[j]
filter(wat2, wat2[,2]== k)
watplot2[k]

cat(
filter(dat, ua== 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:45.0) Gecko/20100101 Firefox/45.0'
       , DR == '2016-10-17')[,1]
)


filter(wat, grepl("Philippine Long Distance Telephone Company",i), diff>=30)




####
5                                               gender: M/F, domain: bk.ru, combo_iso: CA-CA   95    30    57              0   57     Inf   1                    2016-07-18 2016-07-18
7                                            gender: M/F, domain: inbox.ru, combo_iso: CA-CA  195    30    47              0   47     Inf   1                    2016-07-18 2016-07-18
8                                             gender: M/F, domain: mail.ua, combo_iso: CA-CA  202    30    45              0   45     Inf   1                    2016-07-18 2016-07-18
18                                      gender: F, domain: bk.ru, isp: Privax Ltd IP Range  351    30    13              0   13     Inf   1                    2016-07-18 2016-07-18

45                                            gender: F, combo_iso: NL-NL, reg_source: dmi  994    30    21              0   21     Inf   1                    2016-07-18 2016-07-18
56                                   gender: F, combo_iso: NL-NL, isp: Privax Ltd IP Range 1396    30    20              0   20     Inf   1                    2016-07-18 2016-07-18


56 domain: yandex.com, reg_source: ind, isp: Reliance Jio Infocomm Limited 5191    29    30              0   30     Inf   1                    2016-07-20 2016-07-21
35                             gender: F, domain: yandex.com, isp: HostRoyale Technologies Pvt Ltd  580    30     8              0    8     Inf   1                    2016-07-21
83                                           gender: F, combo_iso: NL-NL, isp: Privax Ltd IP Range 1584    27    13              0   13     Inf   1                    2016-07-18


113                          gender: M, combo_iso: US-US, isp: Digital Energy Technologies Limited 1950    30    12              1   11    1100   1                    2016-07-21


138                                 gender: F, reg_source: ind, isp: Reliance Jio Infocomm Limited 2439    30     9              0    9     Inf   1                    2016-07-21

84                                           gender: F, combo_iso: NL-NL, isp: Privax Ltd IP Range 1584    28    10              0   10     Inf   1                    2016-07-19

filter(dat, spam!=1, gender== 'F', DR>= '2016-07-01', combo_iso == 'NL-NL', domain %in% c('bk.ru', 'inbox.ru', 'mail.ru', 'mail.ua') )##isp == 'Privax Ltd IP Range', reg_source == 'dmi')

cat(  filter(dat, spam!=1, DR>= '2016-07-18', combo_iso %in% c('NL-NL', 'CA-CA'), reg_source %in% c('dmi'), domain %in% c('bk.ru', 'inbox.ru', 'mail.ru', 'mail.ua') )[,1])
cat(  filter(dat, spam!=1, DR>= '2016-07-18', ispdomain == 'hidemyass.com', reg_source %!in% c('dmi'), domain %in% c('bk.ru', 'inbox.ru', 'mail.ru', 'mail.ua') )[,1])

##isp == 'Privax Ltd IP Range', reg_source == 'dmi')

filter(dat, spam!=1,  DR>= '2016-07-18', gender != '3', reg_source== 'ind', isp %in% c(
  'Reliance Jio Infocomm Limited', 'HostRoyale Technologies Pvt Ltd')
  , domain == 'yandex.com')




filter(dat, spam!=1, gender== 'M', domain== 'inbox.ru', reg_source== 'ind', DR== '2016-07-19')



filter(dat, spam!=1, domain == 'mail.com', DR>= '2016-07-08', birthdate == '1973-05-08',reg_source == 'ind')[1:100,1] ##reg_source == 'ind', gender =='F') ##

filter(dat, spam!=1, reg_source == 'ind', isp== 'Total Server Solutions L.L.C.', DR >= '2016-07-11', domain == 'gmail.com', gender == 'M')

filter(dat, gender == 'F', spam!=1, domain == 'yandex.com', DR>= '2016-06-20', isp == 'SecuredConnectivity.net', full_name !='No Buy', pets <=10)[,1]

## check  Packet One Networks (M) Sdn

cat(filter(dat, gender == 'F', spam!=1, isp == 'LeaseWeb USA Inc.',  reg_source %in% c('ind', 'rgn', 'fwn'), DR >= '2016-06-16', pets <= 12,
           birthdate >= '1900-01-01')[,1])

filter(dat, user_id ==6039911454)
##(cc_iso %in% c('RU', 'UA')), birthdate >= '1996-01-01', birthdate< '1997-01-01')[,1])

1                                                       gender: F, domain: mail.ru, combo_iso: BD-BD   65    28     9              1    8   800.0   1                    2016-07-15
3                                                         gender: M, domain: bk.ru, combo_iso: CA-CA, ind  101    30    17              0   17     Inf   1                    2016-07-17
4                                                      gender: M, domain: inbox.ru, combo_iso: CA-CA  201    30    16              0   16     Inf   1                    2016-07-17
5                                                       gender: M, domain: mail.ua, combo_iso: CA-CA  207    30    14              0   14     Inf   1                    2016-07-17
17                                         gender: M, domain: gmail.com, isp: FastWebs Main Location  605    24     8              0    8     Inf   1                    2016-07-11

118                                                                  combo_iso: SE-MY, locale: ms_MY 4358    15    10              1    9   900.0   1                    2016-07-02



filter(dat, spam!=1, gender== 'F', domain== 'yandex.com',DR>= '2016-07-15', pets <=20) ##, isp == 'FastWebs Main Location')

filter(dat, spam!=1, gender == 'F', domain== 'yandex.com', DR>= '2016-07-01', combo_iso== 'CA-CA')

filter(dat, spam==1, gender== 'M', domain %in% c('mail.ru', 'mail.ua', 'bk.ru', 'inbox.ru'), DR>= '2016-07-17', combo_iso== 'CA-CA', reg_source %in% c('rgn', 'ind'), ispdomain %in% c('bell.ca', 'shawcable.net','telus.com','kwdatacenter.com','teksavvy.com',' videotron.com','personainc.com','rogers.com'))

cat(filter(dat, gender == 'M', spam!=1, isp == 'Digital Ocean Inc.', domain %in% c('gmx.com', 'yandex.com'),  reg_source %in% c('ind', 'rgn', 'fwn'), DR >= '2016-06-18', pets <= 12,
           birthdate <= '1980-01-01')[,1])


cat((filter(dat, gender == 'F', domain == 'yandex.com', spam!=1, 
            DR >= '2016-06-09', nchar(first_name) <=4, pets <= 12, 
            !(first_name %in% c('Ben', 'Max', 'Ali', 'Tom', 'AIR', 'No', 'God')), 
            reg_iso != 'PK', grepl( "bbartdin|pra", email)<=0, nchar(display_name) <= 12))[,1])


url = "https://cleantalk.org/blacklists/esr-mail.ru"
webpage <- getURL(url)

#url <- "http://www.tagged.com/profile.html?uid=7315455348"

webpage <- getURL(url)
#webpage <- readLines(tc <- textConnection(webpage))

https://cleantalk.org/blacklists/esr-smartmailserver.com
link in newsfeed
hm <- (filter(summarise(group_by(wat, i), ct = sum(diff)), ct <= 5))



















#################### index evals new
library(lazyeval)
'%!in%' <- function(x,y)!('%in%'(x,y))

## run through all combinations of major fields
## run specificed combinations
## run an input of some kind of cluster definition


## create a specific index for each variable-value
## gender, domain, cc_iso, locale, reg_source, sexual_preference, dating, friends, serrelationship, networking, relationship_status, isp, ispdomain, reg_iso
gender M 1 0 1       
gender F 1 0 1

fields <- c("gender", "domain")

interp(~n_distinct(v), v=as.name(uniq.var))

idx.mat <- c()
for (i in fields[2]) {
  es <- filter(dplyr::summarise(group_by_(dat,i), ct = n()), ct >= 5)[,1][[1]]
  
  for (k in es){
    idx.mat <- rbind(idx.mat, c(i, k, k == dat[1:10, i]))
  }
}
idx.mat


cl.defs <- list()
cl.defs[[1]] <- list("test1", list("gender", "==", list("M")), list("domain", "==", "yahoo.com"))
cl.defs[[2]] <- list("test2", list("gender", "==", list("M")), list("domain", "==", "gmail.com"))
cl.defs[[3]] <- list("test3", list("gender", "==", list("M")), list("domain", "%in%", list("yahoo.com","gmail.com"))
                     cl.defs[[4]] <- list("test4", list("gender",), list("domain", "%in%", list("yahoo.com","gmail.com"))
                                          
                                          
                                          a<- Sys.time()
                                          idxs <- data.frame( group = c(), idx1 = c())
                                          idxs <- list()
                                          for (j in cl.defs){
                                            cl.defs1 <- j
                                            
                                            tmp = TRUE
                                            for (i in 2:length(cl.defs1)){
                                              if (length(cl.defs1[[i]][[3]])>1){
                                                cL <- c("c(", ")")
                                              } else {
                                                cL <- c("","")
                                              }
                                              print (paste(paste("dat$", cl.defs1[[i]][[1]], sep = ""), cl.defs1[[i]][[2]], paste(cL[1], paste("'", unlist(cl.defs1[[i]][[3]]), "'", sep = "", collapse =","), cL[2], sep = "")))
                                              tmp <- eval(parse(text = paste(paste("dat$", cl.defs1[[i]][[1]], sep = ""), cl.defs1[[i]][[2]], paste(cL[1], paste("'", unlist(cl.defs1[[i]][[3]]), "'", sep = "", collapse =","), cL[2], sep = ""))))
                                            }
                                            idxs <- append(idxs, list(list(cl.defs1[[1]], tmp)))
                                          }
                                          b<- Sys.time()
                                          
                                          b-a
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          warnings()
                                          
                                          
                                          
                                          
                                          es <- filter(dplyr::summarise(group_by(dat, domain), ct = n()), ct >= 10)[,1][[1]]
                                          
                                          M  1 0 1 0 1
                                          F  0 1 0 1 0
                                          US 1 1 1 0 0
                                          CA 0 0 0 1 0
                                          
                                          M F 
                                          1 0
                                          0 1
                                          1 0
                                          
                                          a<- Sys.time()
                                          hm <- as.data.frame((arrange(summarise(group_by(filter(dat), gender, domain, cc_iso, locale, reg_source, sexual_preference,
                                                                                          dating, friends, serrelationship, networking, relationship_status, isp, ispdomain, reg_iso, DR), ct= n()), DR)))
                                          b<- Sys.time()
                                          b-a
                                          nrow(hm)
                                          cbind(unique(hm[,1:2]),
                                                
                                                
                                                
                                                Gender = F, combo_Iso = 'US-RU', reg_source = 'IND'
                                                Gender = F, combo_Iso = 'US-RU', reg_source = 'RGN'
                                                
                                                while()
                                                  
                                                  ##gender
                                                  ##combo_iso
                                                  ##reg_source
                                                  
                                                  
                                                  cl.defs <- list()
                                                cl.defs[[1]] <- list("test4", list("gender", "==", list("M")), list("reg_source", "%in%", list("ind", "rgn")), list("cc_iso", "==", "IN"))
                                                cl.defs[[2]] <- list("test5", list("gender", "==", list("F")), list("reg_source", "%in%", list("ind", "rgn")), list("cc_iso", "==", "US"), list("locale", "!=", "en_US"))
                                                cl.defs[[3]] <- list("test5", list("gender", "==", list("F")), list("reg_source", "%in%", list("ind", "rgn")), list("cc_iso", "==", "US"), list("locale", "!=", "en_US"))
                                                cl.defs[[4]] <- list("test5", list("gender", "==", list("F")), list("reg_source", "%in%", list("ind", "rgn")), list("cc_iso", "==", "US"), list("locale", "!=", "en_US"))
                                                cl.defs[[1]] <- list("test5", list("gender", "==", list("F")), list("reg_source", "%!in%", list("ind", "rgn")))
                                                
                                                cl.defs[[5]] <- cl.defs[[2]]
                                                
                                                for(i in 6:100)
                                                {
                                                  cl.defs[[i]] <- cl.defs[[2]]
                                                }
                                                
                                                
                                                
                                                cl.defs1 <- list("test4", list("gender", "=", list("M")), list("reg_source", "%in%", list("ind", "rgn")))
                                                
                                                paste(cl.defs1[[i]][[1]], cl.defs1[[i]][[2]], cl.defs1[[i]][[3]])
                                                paste(cl.defs1[[i]][[1]], cl.defs1[[i]][[2]], paste("c(", paste("'", unlist(cl.defs1[[i]][[3]]), "'", sep = "", collapse =","), ")", sep = ""))
                                                
                                                a<- Sys.time()
                                                idxs <- data.frame( group = c(), idx1 = c())
                                                idxs <- list()
                                                for (j in cl.defs){
                                                  cl.defs1 <- j
                                                  
                                                  tmp = TRUE
                                                  for (i in 2:length(cl.defs1)){
                                                    if (length(cl.defs1[[i]][[3]])>1){
                                                      cL <- c("c(", ")")
                                                    } else {
                                                      cL <- c("","")
                                                    }
                                                    print (paste(paste("dat$", cl.defs1[[i]][[1]], sep = ""), cl.defs1[[i]][[2]], paste(cL[1], paste("'", unlist(cl.defs1[[i]][[3]]), "'", sep = "", collapse =","), cL[2], sep = "")))
                                                    tmp <- eval(parse(text = paste(paste("dat$", cl.defs1[[i]][[1]], sep = ""), cl.defs1[[i]][[2]], paste(cL[1], paste("'", unlist(cl.defs1[[i]][[3]]), "'", sep = "", collapse =","), cL[2], sep = ""))))
                                                  }
                                                  idxs <- append(idxs, list(list(cl.defs1[[1]], tmp)))
                                                }
                                                b<- Sys.time()
                                                
                                                b-a
                                                
                                                wat2 <-c()
                                                watplots2 <- list()
                                                for (i in idxs){ ##print(1)
                                                  grp <- summarise(group_by(filter(dat[i[[2]],], spam != 1), DR), ct=n())
                                                  
                                                  grpFilled <-merge(date_index, grp,by = c("DR"), all.x= TRUE, sort= TRUE)
                                                  grpFilled[is.na(grpFilled[,2]),2] <-0
                                                  #  res = shesd(grpFilled[,2])
                                                  
                                                  # watplots2 <- append(watplots2, list(list(i[[1]], res$plot)))
                                                  #  if(!is.null(res$plot))  wat2 <- rbind(wat2, cbind(i[[1]], res$anoms, grpFilled[res$anoms$index,1]))
                                                }b<- Sys.time()
                                                
                                                b-a
                                                
                                                
                                                wat2
                                                watplots2[[2]][[2]]
                                                
                                                
                                                ############################## OLD index evals
                                                
                                                cl.defs <- data.frame(group = 1, fields = c("dating", "friends", "serrelationship", "networking"), vals = c(1,1,0,0))
                                                cl.defs <- list()
                                                
                                                cl.defs[[1]][[1]] <- "test"
                                                cl.defs[[1]][[2]] <- list("dating", 1)
                                                cl.defs[[1]][[3]] <- list("friends", 1)
                                                cl.defs[[1]][[4]] <- list("serrelationship", 0)
                                                cl.defs[[1]][[5]] <- list("networking", 0)
                                                cl.defs[[1]][[6]] <- list("relationship_status", "S")
                                                
                                                cl.defs <- list()
                                                cl.defs[[1]] <- list("test1", list("gender", "F"), list("dating", 1), list("friends", 1), list("serrelationship", 0), list("networking", 0), list("relationship_status", "S"))
                                                cl.defs[[2]] <- list("test2", list("gender", "F"), list("dating", 0), list("friends", 0), list("serrelationship", 0), list("networking", 0), list("relationship_status", "S"))
                                                cl.defs[[3]] <- list("test2", list("gender", "M"), list("dating", 0), list("friends", 0), list("serrelationship", 0), list("networking", 0), list("relationship_status", "S"))
                                                ##cl.defs[[3]] <- list("test3", list("gender", "F"), list("domain", "mail.com"))
                                                cl.defs[[4]] <- list("test4", list("gender", "M"), list("reg_source", "ind"))
                                                cl.defs[[5]] <- list("test5", list("gender", "F"), list("reg_source", "ind"))
                                                cl.defs[[6]] <- list("test6", list("gender", "F"), list("reg_source", "rgn"))
                                                cl.defs[[7]] <- list("test7", list("gender", "F"), list("dating", 1), list("friends", 1), list("serrelationship", 1), list("networking", 1), list("relationship_status", "S"))
                                                cl.defs[[8]] <- list("test8", list("gender", "F"), list("dating", 1), list("friends", 0), list("serrelationship", 1), list("networking", 0), list("relationship_status", "S"))
                                                cl.defs[[9]] <- list("test9", list("gender", "F"), list("dating", 0), list("friends", 0), list("serrelationship", 0), list("networking", 0), list("relationship_status", "X"))
                                                cl.defs[[10]] <- list("outlook", list("gender", "F"), list("reg_source", "rgn"), list("domain", "outlook.com"))
                                                
                                                idxs <- data.frame( group = c(), idx1 = c())
                                                idxs <- list()
                                                for (j in cl.defs){
                                                  cl.defs1 <- j
                                                  
                                                  tmp = TRUE
                                                  for (i in 2:length(cl.defs1)){
                                                    tmp <- dat[,cl.defs1[[i]][[1]]] == cl.defs1[[i]][[2]] & tmp
                                                  }
                                                  idxs <- append(idxs, list(list(cl.defs1[[1]], tmp)))
                                                }
                                                
                                                b <- hm[-3,] 
                                                
                                                hm <- merge(date_index, b, by = c("DR"), all.x= TRUE, sort= TRUE)
                                                
                                                
                                                wat2 <-c()
                                                watplots2 <- list()
                                                for (i in idxs){ ##print(1)
                                                  grp <- summarise(group_by(filter(dat[i[[2]],], spam != 1), DR), ct=n())
                                                  
                                                  grpFilled <-merge(date_index, grp,by = c("DR"), all.x= TRUE, sort= TRUE)
                                                  grpFilled[is.na(grpFilled[,2]),2] <-0
                                                  res = shesd(grpFilled[,2])
                                                  
                                                  watplots2 <- append(watplots2, list(list(i[[1]], res$plot)))
                                                  if(!is.null(res$plot))  wat2 <- rbind(wat2, cbind(i[[1]], res$anoms, grpFilled[res$anoms$index,1]))
                                                }
                                                wat2
                                                watplots2[[2]][[2]]
                                                
                                                
                                                ##### SHESD
                                                ##### group input
                                                
                                                shesd <-function(x){
                                                  results <- AnomalyDetectionVec(x, max_anoms=0.2, alpha = .10, period=7, direction='pos', only_last=FALSE, plot=TRUE, e_value =T)
                                                  results$anoms$diff <- round(100*(results$anoms$anoms- results$anoms$expected_value)/(results$anoms$expected_value),1)
                                                  results
                                                }
                                                ##### SHESD
                                                
                                                ##### SHESD2
                                                ##### group input
                                                
                                                shesd2 <-function(x){
                                                  results <- AnomalyDetectionVec(x, max_anoms=0.2, alpha = .05, period=7, direction='pos', only_last=FALSE, plot=TRUE, e_value =T)
                                                  results$anoms$diff <- round(100*(results$anoms$anoms- results$anoms$expected_value)/(results$anoms$expected_value),1)
                                                  results
                                                }
                                                ##### SHESD2
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                
                                                ############################################### clusting ##############################################################################################
                                                dateN <- c("birthdate", "date_registered", "last_login_date", "date_validated")
                                                for (i in (dateN)){
                                                  dat[, i] <- as.numeric(dat[,i])  
                                                }
                                                hm<-  filter(dat, spam != 2, DR >= as.POSIXct('2016-05-05'),  DR <= as.POSIXct('2016-05-07'))
                                                wat <- data.frame(hm[,
                                                                     !(colnames(dat) %in% c("user_id", "date_validated"  ##"last_login_date" , "apps_optout_settings_1", "birthdate"
                                                                     ))])
                                                
                                                for (i in c("date_registered", "birthdate", "last_login_date")){
                                                  wat[, i] <- as.numeric(wat[,i])  
                                                }
                                                
                                                wat <- filter(dat, gender == "M", domain == i, reg_source == 'ind' | reg_source == 'rgn', spam != 1, DR >= as.POSIXct('2016-04-11'),  DR <= as.POSIXct('2016-04-12'))
                                                wat <- filter(dat, spam != 2, DR >= as.POSIXct('2016-05-01'),  DR <= as.POSIXct('2016-05-11'))
                                                rownames(wat)
                                                
                                                fukk <- filter(dat,  gender == "M", domain == i, !(reg_source %in% c('ung', 'ind', 'rgn')), spam != 1, DR == '2016-05-05')
                                                
                                                d <- daisy(wat[,1:24], metric = "gower")
                                                hc <- hclust(d,method="ward.D") 
                                                hr <-hc
                                                
                                                plot(hr)
                                                
                                                summary(daisy(bad[1:5,1:24], metric = "gower"))
                                                
                                                
                                                wat[which( wat[1:50,]$gender == 'F' & wat[1:50,]$cc_iso == 'US' & wat[1:50,]$reg_source == 'rgn'& wat[1:50,]$spam == 1),1:2]
                                                
                                                mycl <- cutree(hc, h=max(hc$height/20))
                                                clusterCols <- rainbow(length(unique(mycl)))
                                                myClusterSideBar <- clusterCols[mycl]
                                                myheatcol <- rev(redgreen(75))
                                                
                                                wat[mycl == 1, ]
                                                
                                                
                                                hm<-which(dat$domain %in% c('outlook.com')) ## &  dat$DR>= as.POSIXct('2016-04-'))
                                                wat <- data.frame(dat[hm,
                                                                      !(colnames(dat) %in% c("user_id", "date_validated"  ##"last_login_date" , "apps_optout_settings_1", "birthdate"
                                                                      ))])
                                                for (i in c("date_registered", "birthdate", "last_login_date")){
                                                  wat[, i] <- as.numeric(wat[,i])  
                                                }
                                                idxs <- list()
                                                for (i in 1:unique(mycl)) {
                                                  bad <- wat[mycl == 4, ]
                                                  
                                                  dissim2 <- daisy(bad[, 1:24], metric = "gower")
                                                  pamM <- pam(dissim2, 1, diss = TRUE)
                                                  medoid.idx <- pamM$medoids
                                                  mm <- pamM$clusinfo[2]
                                                  medoid <- bad[row.names(bad) == medoid.idx,]
                                                  
                                                  newDist <- daisy(rbind(medoid[, 1:24], wat[1:, 1:24]), metric = "gower")[1:nrow(dat)]
                                                  
                                                  idxs <- append(idxs, list(list(i, newDist <= mm)))  
                                                }
                                                
                                                wat2 <-c()
                                                watplots2 <- list()
                                                for (i in idxs){ ##print(1)
                                                  grp <- summarise(group_by(filter(dat[i[[2]],], spam != 2), DR), ct=n())
                                                  
                                                  grpFilled <-merge(date_index, grp,by = c("DR"), all.x= TRUE, sort= TRUE)
                                                  grpFilled[is.na(grpFilled[,2]),2] <-0
                                                  res = shesd(grpFilled[,2])
                                                  
                                                  watplots2 <- append(watplots2, list(list(i[[1]], res$plot)))
                                                  if(!is.null(res$plot))  wat2 <- rbind(wat2, cbind(i[[1]], res$anoms, grp[res$anoms$index,1]))
                                                }
                                                wat2
                                                watplots2[[10]][[2]]
                                                
                                                library(gplots)
                                                heatmap.2(as.matrix(wat[,1:21]), main="Hierarchical Cluster", Rowv=as.dendrogram(hr), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)
                                                
                                                source("http://rattle.googlecode.com/svn/trunk/src/hclust.R")
                                                predict.hclust(hr, wat,wat)
                                                
                                                predict.hclust <- function(object, data, x, nclust=10, ...)
                                                  
                                                  predict.hclust <- function(object, data, x, nclust=10, ...)
                                                    object$centers <- centers.hclust(x, object, nclust=nclust, use.median=FALSE)
                                                
                                                
                                                idxs <- list()
                                                for (i in unique(mycl)){
                                                  idxs <- append(idxs, list(list(i, mycl == i)))  
                                                }
                                                
                                                wat[mycl==4,]
                                                
                                                wat2 <-c()
                                                watplots2 <- list()
                                                for (i in idxs){ ##print(1)
                                                  grp <- summarise(group_by(filter(wat[i[[2]],], spam != 2), DR), ct=n())
                                                  
                                                  grpFilled <-merge(date_index, grp,by = c("DR"), all.x= TRUE, sort= TRUE)
                                                  grpFilled[is.na(grpFilled[,2]),2] <-0
                                                  res = shesd(grpFilled[,2])
                                                  
                                                  watplots2 <- append(watplots2, list(list(i[[1]], res$plot)))
                                                  if(!is.null(res$plot))  wat2 <- rbind(wat2, cbind(i[[1]], res$anoms, grp[res$anoms$index,1]))
                                                }
                                                wat2
                                                
                                                
                                                
                                                
                                                plot(hc)
                                                
                                                groups.3 = cutree(hc,3)
                                                groups.4 = cutree(hc,20)
                                                
                                                table(groups.3)
                                                counts = sapply(2:6,function(ncl)table(cutree(hc,ncl)))
                                                
                                                dat[groups.4 == 3,][1:100,1:6][,1]
                                                
                                                
                                                tmp <- filter(dat, domain == "outlook.com"), cc_iso =="US", reg_source == 'rgn', dating == 0, gender== 'F')
                                          
                                          summarise(tmp$date_registered, n_distinct(zipcode), ct = n())
                                          
                                          
                                          hm[1:100, ]
                                          
                                          
                                          
                                          ##############################here
                                          es <-c("outlook.com", "yahoo.com")
                                          
                                          thor <-list()
                                          loki <-list()
                                          for (i in es){
                                            
                                            print(i)
                                            hm <- filter(dat,  gender == "F", domain == i, spam != 2)
                                            
                                            wat <- data.frame(hm[,
                                                                 !(colnames(hm) %in% c("user_id", "date_validated"  ##"last_login_date" , "apps_optout_settings_1", "birthdate"
                                                                 ))])
                                            
                                            for (j in c("date_registered", "birthdate", "last_login_date")){
                                              wat[, j] <- as.numeric(wat[,j])  
                                            }
                                            
                                            d <- daisy(wat[,1:24], metric = "gower")
                                            hc <- hclust(d,method="ward.D") 
                                            hr <-hc
                                            
                                            
                                            mycl <- cutree(hc, h=max(hc$height/20))
                                            clusterCols <- rainbow(length(unique(mycl)))
                                            myClusterSideBar <- clusterCols[mycl]
                                            myheatcol <- rev(redgreen(75))
                                            
                                            thor <-list()
                                            loki <-list()
                                            mycl.unique = unique(mycl)
                                            for (i in mycl.unique) {
                                              bad <- wat[mycl == i, ]
                                              grp <- summarise(group_by(filter(bad, spam != 2), DR), ct=n())
                                              grpFilled <-merge(date_index, grp,by = c("DR"), all.x= TRUE, sort= TRUE)
                                              grpFilled[is.na(grpFilled[,2]),2] <-0
                                              
                                              if(nrow(bad)){ 
                                                res = shesd(grpFilled[,2])
                                                if(!is.null(res$plot)){
                                                  thor <- rbind(thor, cbind(i, res$anoms, hm[res$anoms$index,1]))
                                                }
                                                loki <- append(loki, list(res$plot))
                                              }else{
                                                loki <- append(loki, list(NA))
                                              } 
                                            }
                                          }  
                                          
                                          Sys.time() -> a
                                          dm <- daisy(rbind(wat[1:10000,1:24]), metric = "gower")
                                          Sys.time() -> b
                                          b-a
                                          
                                          fetchQuery("select * from registration_event where user_id = 6050009102;", config.redshift)
                                          
                                          
                                          select * from photo_upload_event where user_id = 6026176406
                                          244174908                                          
                                          
fetchQuery("
select count(*) from public.lh_midNovSpam2 where 
interaction_country = 'nonUS' and (src_spam = 1 or tgt_spam = 1)
and (src_spam = 0 or tgt_spam = 0)
and (user_id in (select user_id from userdata_light where email like '%mail.ru' or email like '%bk.ru')
or interested_uid in (select user_id from userdata_light where email like '%mail.ru' or email like '%bk.ru'))
")
                                          


dd <- fetchQuery("select * from registration_web_event where 
                 source = 'rgn'
                 and user_agent = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:45.0) Gecko/20100101 Firefox/45.0'
                 and email like '%mail.ru'
                 and gender = 'F'
                 and birthdate >= '1990-01-01'
and locale = 'en_US'
and dt::date >= '1/11/2017'
                 ")
