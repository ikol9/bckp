datt <- fetchQuery("select * from lh_midnovspam5")
datt <- filter(dat2, toupper(gender1) == 'F', toupper(gender2) == 'M' ,dt != '2017-01-23')

datt$DR <- as.Date(as.POSIXct(strptime(datt$dt, "%Y-%m-%d")))

datt <- filter(datt, toupper(gender1) == 'F', toupper(gender2) == 'M' )


dplyr::summarise(group_by(filter(datt), gender, gender), ct=n())

var.splits <- c('gender', 'domain1', 'combo_iso', 'reg_source', 'isp')
var.splits <- c('domain1', 'cc_iso1', 'reg1', 'new')

#'dating', 'friends', 'serrelationship', 'ispdomain'
#'networking', 'relationship_status', 
#'sexual_preference'

#var.combo <- as.data.frame(arrange(expand.grid(a=var.splits,b=var.splits
#                                               ,c = var.splits
#                                               ),a))
var.combo <- t(combn(var.splits,3))
var.combo <- rbind(var.combo, c("reg1",  "reg1", "reg1"))
var.combo <- rbind(var.combo, c( "cc_iso1", "reg1",  "reg1"))
var.combo <- rbind(var.combo, c( "gender1", "reg1",  "reg1"))
                                      
                                      
start_date <- min(datt$DR)
end_date <- max(datt$DR)-1

date_index <- data.frame(DR= as.Date(as.POSIXct(strptime(seq(start_date, end_date , "days"),"%Y-%m-%d"))))

datt[1,"date_reg1"]


datt$DR2 <- as.Date(as.POSIXct(strptime(datt$date_reg1, "%Y-%m-%d")))


datt$new  <- 0
datt$new[as.numeric((datt$DR - datt$DR2 )) <= 90] <-1

dat2$grp <- 0 
dat2[dat2$DR <= '2016-11-01',]$grp <- 1
dat2[dat2$DR >= '2016-12-01',]$grp <- 2


length(unique(filter(dat2, grp== 1)$DR)) ##32
length(unique(filter(dat2, grp== 2)$DR)) ##53


View(arrange(summarize(group_by(filter(datt, gender1 == 'F', gender2 == 'M'), DR), ct= sum(ct)),DR))

aa <- data.frame(arrange(summarize(group_by(filter(dat2, grp == 1), domain, reg1), ct = sum(ct)),reg1, domain))
bb <- data.frame(arrange(summarize(group_by(filter(dat2, grp == 2), domain, reg1), ct = sum(ct)),reg1, domain))

cc <- merge(aa, bb, by = c("domain", "reg1"))
cc$diff <- (cc$ct.y/53)/(cc$ct.x/32)
cc$diff2 <- (cc$ct.y/53)-(cc$ct.x/32)

arrange(filter(cc, diff >= 1.5 | diff2 >= 20000), (diff2))


merge(aa, bb)



###HEREEEEE  
dat2 <- summarise(group_by(filter(datt), 
                           DR, gender1, domain1, cc_iso1, reg1 , new

), ct = sum(ct))
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
  capA <- unique(filter(summarise(group_by(ekk2, idx.name,DR), mct = max(ct)), mct >= 600)[,1])
  jj = nrow(capA)
  jj2 = 0
  for (i in capA[,1][[1]]){
    jj2=jj2+1
    print(paste(g,i,m, mdat, " - ", format(jj2/jj*100,digits=2))) ##(reg_source %in% c('ung', 'ind', 'rgn')),reg_source %in% c('ind', 'rgn')),  cc_iso == "IN", reg_iso != 'US', !(reg_source %in% c('ung', 'ind', 'rgn', 'fwn'))
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


sOs <- '2017-01-01'
ceo <- filter(wat, diff>= 1000, perdiff >= 10, mdate >= sOs )[,1:10]
ceo[ceo[,9] >= sOs & ceo[,5] <= 1 & ceo[,4] >= 5,][,1:9]



#7328503410
##!grepl('(mail.ru)|(Beeline)|(RU-RU)', i)
ceo <- filter(wat, diff>= 400, perdiff >= 1000 , diff !=0 , mdate >= sOs)[,1:10]
arrange(ceo[ceo[,9] >= sOs & ceo[,5] >1,], m)
#arrange(ceo[ceo[,9] >= '2016-08-07',], desc(perdiff))[201:233,1:9]

ceo <- filter(wat, diff>= 1500, perdiff >= 5 , mdate >= sOs)[,1:10]
arrange(ceo[ceo[,9] >= sOs & ceo[,5] >1,], m)
#arrange(ceo[ceo[,9] >= '2016-08-07',], desc(perdiff))[201:233,1:9]

1232
1018
eh = 1232
wat[wat$m==eh,]
watplot[[ eh ]]

kek <- unique(ceo[,2])

j = 0

j=j+1
eh = kek[j]
wat[wat$m==eh,]
watplot[[ eh ]]
kek[j]


14 gmail BO gan; 12/5; 1k; fb ad
41 IE gan; 1/2; 600; fb
41 PY gan; 12/1; 600; fb
85 RO gan; 1/3 1.3k; fb
107 UY gan; 600k; fb




#bucky <-filter(dat, combo_iso == "US-GB", gender == '', spam!=1, pets <=10), DR == '2016-06-14')
hm <- dplyr::summarize(group_by(filter(dat2, reg1 %!in% c('gan'), 
                                       #domain %!in% c('mail.ru', 'mail.ua', 'bk.ru', 'yandex.ru'), 
                                       cc_iso1 %!in% c('US'), new!=2, DR < '2017-01-20'), DR), ct= sum(ct))
#hm <- dplyr::summarise(group_by(filter(dat,  spam!=1, reg_iso %in% c('PH'), cc_iso == 'PH'), DR), ct=n())
grpFilled <-merge(date_index, hm, by = c("DR"), all.x= TRUE, sort= TRUE)
grpFilled[is.na(grpFilled[,2]),2] <-0
res = AnomalyDetectionVec(grpFilled[,2], max_anoms=0.1, alpha = .01, period=7, direction='both', only_last=FALSE, plot=TRUE, e_value =T, title = "bleh", ylabel = "")
res$plot

hm2 <- dplyr::summarize(group_by(filter(dat2, reg1 %in% c('gan'), 
                                       #domain %!in% c('mail.ru', 'mail.ua', 'bk.ru', 'yandex.ru'), 
                                       cc_iso1 %!in% c('US'), new == 0, DR < '2017-01-20'), DR), ct= sum(ct))
#hm <- dplyr::summarise(group_by(filter(dat,  spam!=1, reg_iso %in% c('PH'), cc_iso == 'PH'), DR), ct=n())
grpFilled2 <-merge(date_index, hm2, by = c("DR"), all.x= TRUE, sort= TRUE)
grpFilled2[is.na(grpFilled2[,2]),2] <-0
res2 = AnomalyDetectionVec(grpFilled2[,2], max_anoms=0.1, alpha = .01, period=7, direction='both', only_last=FALSE, plot=TRUE, e_value =T, title = "bleh", ylabel = "")
res2$plot



hm <- dplyr::summarize(group_by(filter(dat2, DR < '2017-01-20'), DR), ct= sum(ct))
#hm <- dplyr::summarise(group_by(filter(dat,  spam!=1, reg_iso %in% c('PH'), cc_iso == 'PH'), DR), ct=n())
grpFilled <-merge(date_index, hm, by = c("DR"), all.x= TRUE, sort= TRUE)
grpFilled[is.na(grpFilled[,2]),2] <-0
res = AnomalyDetectionVec(grpFilled[,2], max_anoms=0.1, alpha = .01, period=7, direction='both', only_last=FALSE, plot=TRUE, e_value =T, title = "bleh", ylabel = "")
res$plot




wat <-c()
watplot <- list()
wat <- rbind(wat, cbind(i, m, res$anoms, grpFilled[res$anoms$index,2], max(grpFilled[res$anoms$index,2])))
watplot <- append(watplot, list(res$plot))



