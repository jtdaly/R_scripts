#library(XML)
#library(RCurl)


####################################
# extract relevant part of web page#
####################################

extractor<-function (web_page2){
  #remove all double blank spaces and TAB    
  web_page2<-gsub("  |\t","",page)
  
  #Count how many addresses appear
  add.count<-grep("<address",web_page2);length(add.count)
  #Ditto for prices and acres
  price.count<-grep("\"price",web_page2);length(price.count)
  acres.count<-grep("acres</s",web_page2);length(acres.count)
  price.count;add.count;acres.count
  
  all<-c(add.count,price.count,acres.count)
  all<-sort(all)
  new.webpage<-web_page2[all]
  write.table(new.webpage,file="lf2.txt")
  
  #remplaces nonense with blanks
  new.webpage<-gsub("\t+","",new.webpage) #find a TAB and replace with blank
  #new.webpage<-gsub("<label>Premium</label>|<div>|<dl class=clearfix>|</dt>|<p class=stats>|</ul>|<div class=contentLeft>|</a>|</div>|</p>|<li class=house>Residential Land</li>","",new.webpage)
  new.webpage<-gsub("<.*?>|  ","",new.webpage)
  all<-c(add.count,price.count,acres.count)
  return(new.webpage)
}


new<-as.data.frame(date())
orig.file<-date()
colnames(new)<-"output"
for (i in 1:250){
  link<-paste("http://www.landandfarm.com/search/Virginia-land-for-sale/?CurrentPage=",i,sep="")
  page<- readLines(link)
  orig.file<-c(orig.file,page)
  output<-extractor(page)
  #below line cuts out garbage lines that made it though previous filters
  #junk<-grep("For Sale",output[1])
  output<-as.data.frame(output)
  new<-rbind(new,output)
  }

#str(new)
write.table(orig.file,file="orig_file.txt")
write.table(new,file="lf.txt")

#orig<-(read.table(file="orig_file.txt",colClasses = "character"))
#new<-extractor(orig)


a<-length(new[,1])
address<-grep("VA",new[1:a,])
price<-grep("\\$",new[1:a,])
acres<-grep("acres",new[1:a,])
length(address);length(price);length(acres)
i<-1
j<-1
k<-1
combo<-data.frame(address=character(0),price=character(0),acres=character(0),stringsAsFactors=TRUE)

for (i in 1:length(address)){
    col1<-as.character(new[address[i],])
    if (price[j]==address[i]+1) {
      col2<-as.character(new[price[j],])
      j<-j+1
      if (acres[k]==address[i]+2) {
        col3<-as.character(new[acres[k],])
        k<-k+1
      } else {
        col3<-"NA"
      }
    } else {
      col2<-"NA"
      if (acres[k]==address[i]+1) {
        col3<-as.character(new[acres[k],])
        k<-k+1
    } else {
      col3<-"NA"
    }
}
   # print(col1);print(col2);print(col3)
    #print(address[i]);print(price[j]);print(acres[k])
    a<-cbind(col1,col2,col3)
    combo<-rbind(a,combo)
}
colnames(combo)<-c("Address","Price","Acreage")
combo$Price<-gsub("\\$","",combo$Price)
combo$Price<-gsub("\\,","",combo$Price)
combo$Price<-as.numeric(combo$Price)
combo$Acreage<-gsub(" acres","",combo$Acreage)
combo$Acreage<-as.numeric(combo$Acreage)
combo$PP<-combo$Price/combo$Acreage
write.csv(combo,file="combo.csv")

##################################
#Add Geocode for each address
###################################
getDocNodeVal=function(doc, path){
  sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}

gGeoCode=function(str){
  library(XML)
  u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
  doc = xmlTreeParse(u, useInternal=TRUE)
  str=gsub(' ','%20',str)
  lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
  lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
  #lat<-as.numeric(lat)
  as.numeric(c(lat,lng))
  #c(lat,lng)
}
ll<-0;ll<-as.data.frame(ll)

for (i in 1:500){
  lat<-gGeoCode(combo[i,2])[1]
  #print(i)#;print(gGeoCode(combo[i,2]))
  lng<-gGeoCode(combo[i,2])[2]
  lat<-gGeoCode(combo[i,2])[1]
  #print(lat);print(lng)
  #ll[i,1]<-lat
  #ll[i,2]<-lng
  combo[i,6]<-lat
  combo[i,7]<-lng
}
ll<-as.numeric(tapply(combo[1:2,2],1,gGeoCode))
)###################################################
#ANALYSIS
#From here the script moves from data prep to analysis
##################################################
combo<-read.csv(file="combo.csv")
library("ggplot2")
#Remove all NA in price and acreage
#remove all with PP<100 or >100,000
#Remove prices >10M and acres>1000
ignore<-c(which(is.na(combo$Price)),  
        which(is.na(combo$Acreage)),
        which(combo$PP>100000),
        which(combo$PP<100),
        which(combo$Price>500000),
        which(combo$Acreage<5),  
        which(combo$Acreage>600)) 
ignore<-unique(ignore)
combo.sm<-combo[-ignore,]

fit<-glm(combo.sm$Acreage~combo.sm$Price)
summary(fit)

ggplot(combo.sm, aes(Price,Acreage))+ 
  geom_point()+labs(x="Price($k)",y="Acres") +
  geom_abline(intercept=fit$coefficients[1],slope=fit$coefficients[2])
#plot(combo.sm$Price/1000,combo.sm$Acreage,xlim=c(0,2000),ylim=c(0,1000))
ggplot(combo.sm, aes(PP))+ geom_histogram()+labs(x="Price/Acre($/acre)")


ggplot(combo.sm, aes(Price,Acreage))+ geom_point(aes(x=sort(combo.sm$Price),y=sort(combo.sm$Acreage)))
qqplot(combo.sm$Price,combo.sm$Acreage, xlim=c(0,500000),ylim=c(0,400))
abline(0,1/8000)
qqplot(combo$Price,combo$Acreage, xlim=c(0,20000000),ylim=c(0,1000))

###################################
#trying new website
###################################
page<- readLines("http://www.landwatch.com/default.aspx?ct=R&type=5,77;13,12;268,6843&pg=1")
page
