#
# Get appraised value of car from Edmunds.com using the developer API with R
# Reference: http://developer.edmunds.com/docs/read/The_Vehicle_API
#

# set working dir
setwd('~/R/carvalue')

#load libraries
library(RJSONIO)
library(ggplot2)
library(plyr)
library(RCurl)

# get all makes 
APIkey<-"8n4mg8er5fb5kwxkgbn9nfsu"
# get the URL that gets the JSON file
resURL=paste("https://api.edmunds.com/api/vehicle/v2/makes?fmt=json&api_key=",
             APIkey,sep="")

# getURL data and convert JSON to a list 
makesJSON<-fromJSON(getURLContent(resURL))
# get make names
makes<-sapply(makesJSON$makes,function(x) x$niceName)
makes 

# get makes and models
makeslist<-sapply(makesJSON$makes,function(x) rep(x$niceName,length(x$models)))
modelslist<-sapply(makesJSON$makes,function(x) sapply(x$models,function(y) y$id))
modelslist2<-sapply(makesJSON$makes,function(x) sapply(x$models,function(y) y$niceName))
makesModels<-data.frame(make=unlist(makeslist),modelname=unlist(modelslist),
                       model=unlist(modelslist2))
head(makesModels)

# models for toyota
makesModels[makesModels$make == "toyota",]
# models for chrysler
makesModels[makesModels$make == "subaru",]

# function to get a list of styles for make->model->year 

stylefn<-function(pickmake,pickmodel,pickyr){
  resURL=paste("https://api.edmunds.com/api/vehicle/v2/",
               pickmake,"/",pickmodel,"/",pickyr,"?fmt=json&api_key=",
               APIkey,sep="")
  # convert JSON to list
  Sys.sleep(0.6) #added delay becuase Edmund API doesn't allow request rates >2/sec
  step2<-getURLContent(resURL)
  stylesJSON<-fromJSON(step2)
  # extract style info from the list into a dataframe
  styles<-sapply(stylesJSON$styles,function(x) c(x$id,x$name,x$trim))
  styles<-data.frame(t(styles),stringsAsFactors=FALSE) #t is matrix transpose
  names(styles)<-c("styleid","stylename","trim")
  styles$make<-pickmake
  styles$model<-pickmodel
  styles$year<-pickyr
  return(styles)
}

# test stylefn
pickmake<-"subaru"
pickmodel<-"forester"
pickyr=2010
test<-stylefn(pickmake,pickmodel,pickyr);test

# pick make and model
pickmake<-"toyota"
pickmodel<-"corolla"
# get list of styles for a given set of years for a given model->make
yrlist<-c(2008,2009,2010,2011,2012)
stylelist<-lapply(yrlist, function(x) stylefn(pickmake,pickmodel,x))
styledf=do.call(rbind,stylelist) #converts stylelist from list to DF

# function to get tmv for a style with typically equipped vehicle
valfn<-function(pickstyleid,pickzip){ 
  resURL<-paste("http://api.edmunds.com/v1/api/tmv/tmvservice/calculatetypicallyequippedusedtmv?styleid=",
                pickstyleid,
                "&zip=",pickzip,"&api_key=",APIkey,"&fmt=json",sep="")
  Sys.sleep(0.6)
  tmv<-fromJSON(getURLContent(resURL))
 
  # market value (retail, private party and trade-in)
  tmv<-c(tmvretail=tmv$tmv$totalWithOptions$usedTmvRetail,
        tmvpp=tmv$tmv$totalWithOptions$usedPrivateParty,
        tmvtradein=tmv$tmv$totalWithOptions$usedTradeIn)

  return(tmv)  
}

# test valfn
pickstyleid<-"100884950"
pickzip<-"22203"
test<-valfn(pickstyleid,pickzip);test

# get tmv for a style with typically equipped vehicle for a list of styles
pickzip<-"22203"
tmvlist<-lapply(styledf$styleid, function(x) valfn(x,pickzip))
tmvdf<-do.call(rbind,tmvlist)
tmvdf<-cbind(styledf,tmvdf)

# remove records with zero tmvretail value
tmvdf<-tmvdf[tmvdf$tmvretail > 0,]


# summarize by year and trim
tmvdfS1<-ddply(tmvdf,c("year","trim"),summarize,tmvmin=min(tmvretail),tmvmax=max(tmvretail))
# plot TMV by year and trim

# y range for plot
ymin<-round(min(tmvdfS1$tmvmin),-3)-500
ymax<-round(max(tmvdfS1$tmvmax),-3)+1500

ggplot(data=tmvdfS1,aes(x=factor(year),y=tmvmin,color=trim))+
  geom_crossbar(aes(ymin=tmvmin,ymax=tmvmax,fill=trim,width=0.4),position="dodge")+
  scale_y_continuous(breaks=seq(ymin,ymax,1000))+
  xlab("")+ylab("True Market Value (Retail) ($)")+theme_bw(20)


# function to get tmv for options, condition, mileage for a style
valoptfn<-function(pickstyleid,pickzip,pickmiles,pickcondition,pickoption){
  
  resURL<-paste("http://api.edmunds.com/v1/api/tmv/tmvservice/calculateusedtmv?styleid=",
               pickstyleid,"&condition=",pickcondition,"&mileage=",pickmiles,
               "&zip=",pickzip,"&api_key=",APIkey,
               "&optionid=",pickoption,
               "&fmt=json",sep="")
  tmv<-fromJSON(getURLContent(resURL))
  Sys.sleep(0.6)
  # TMV for options
  tmvopt<-c(tmvretail=tmv$tmv$optionTMVPrices[[1]]$usedTmvRetail,
           tmvpp=tmv$tmv$optionTMVPrices[[1]]$usedPrivateParty,
           tmvtradein=tmv$tmv$optionTMVPrices[[1]]$usedTradeIn)
  
  # TMV for condition adjustment
  tmvcond<-c(tmvretail=tmv$tmv$conditionAdjustment$usedTmvRetail,
            tmvpp=tmv$tmv$conditionAdjustment$usedPrivateParty,
            tmvtradein=tmv$tmv$conditionAdjustment$usedTradeIn)
  
  # TMV for mileage adjustment
  tmvmiles<-c(tmvretail=tmv$tmv$mileageAdjustment$usedTmvRetail,
             tmvpp=tmv$tmv$mileageAdjustment$usedPrivateParty,
             tmvtradein=tmv$tmv$mileageAdjustment$usedTradeIn)
  
  # TMV with all options
  tmvtot<-c(tmvretail=tmv$tmv$totalWithOptions$usedTmvRetail,
           tmvpp=tmv$tmv$totalWithOptions$usedPrivateParty,
           tmvtradein=tmv$tmv$totalWithOptions$usedTradeIn)
  
  return(list(tmvopt=tmvopt,tmvcond=tmvcond,tmvmiles=tmvmiles,
              tmvtot=tmvtot))
  
}

# get a list of options for a given style

# styleid for Toyota Corolla LE 4dr Sedan (1.8L 4cyl 4A) year 2008
pickstyleid<-"100884950" 

resURL<-paste("http://api.edmunds.com/v1/api/vehicle-directory-ajax/vehicle-options/options?styleId=",
             pickstyleid,"&fmt=json&api_key=",
             APIkey,sep="")
optionsJSON<-fromJSON(getURLContent(resURL))
options<-sapply(optionsJSON,function(x) c(x["optionId"],x["optionDescription"]))
options<-data.frame(t(options))
head(options)

# test valoptfn
pickzip<-"22203"
pickmiles<-"120000"
pickcondition<-"clean"
pickoption<-"TMVU1008849501501000083"
test<-valoptfn(pickstyleid,pickzip,pickmiles,pickcondition,pickoption)

# find adjsment for options, condition and miles

# find adjustment for options
adjopt<-apply(options,1,function(x) valoptfn(pickstyleid,pickzip,"40000",
                                            "clean",x["optionId"])$tmvopt)
adjopt2<-cbind(options,t(adjopt))
# plot option adjustment
ggplot(adjopt2,aes(x=reorder(optionDescription,tmvretail),y=tmvretail))+
  xlab("")+ylab("Adjustment for an option ($)")+
  geom_bar(stat="identity")+coord_flip()+theme_bw()

# find adjustment for condition
condlist<-data.frame(condition=c("Rough","Average","Clean","Outstanding"))
adjcond<-apply(condlist,1,function(x) valoptfn(pickstyleid,pickzip,"40000",x,options$optionId[1])$tmvcond)
adjcond2<-cbind(condlist,t(adjcond))
# plot condition adjustment
ggplot(adjcond2,aes(x=reorder(condition,tmvretail),y=tmvretail))+
  xlab("")+ylab("Adjustment for condition ($)")+
  geom_bar(stat="identity")+coord_flip()+theme_bw(20)

# find adjustment by mileage
milelist<-data.frame(miles=seq(10000,90000,by=10000))
#milelist=data.frame(miles=c(90000,110000,120000))
adjmiles<-apply(milelist,1,function(x) valoptfn(pickstyleid,pickzip,x,"clean",options$optionId[1])$tmvmiles)
adjmiles2<-cbind(milelist,t(adjmiles))
# plot mileage adjustment
ggplot(adjmiles2,aes(x=miles,y=tmvretail))+geom_line()+
  xlab("Miles")+ylab("Mileage Adjustment ($)")+
  scale_x_continuous(breaks=seq(10000,90000,by=10000))+
  scale_y_continuous(breaks=seq(-1000,1000,200))+theme_bw(20)