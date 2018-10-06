#UP Election Data for 2017
# Data Sources
# EC data for Candidates : http://eci.nic.in/eci_main1/ElectionStatistics.aspx
# Criminal and Assets data of candidates :  http://www.myneta.info/uttarpradesh2017/index.php?action=summary&subAction=candidates_analyzed&sort=asset#summary

library(googleVis)
install.packages("rdataviewer")

getwd()


setwd( "C:/Home/Work/GreatLakes/Machine Learning")
#We will merge 2 Excel Files
# Election 2012 and 2017
#However, the Constituency names may have changed


UP.2017.unmatched<-read.csv(file.choose(), header=T)
UP.2012.all<-read.csv(file.choose(), header=T)

UP.2017.unmatched$AC<-UP.2017.unmatched$EC.2017
UP.2012.all$AC<-UP.2012.all$AC_NAME
#install.packages('stringdist')
library(stringdist)
#We will only use LV,DL,JACCARD and JARROW


distance.methods<-c('lv','dl','jaccard','jw')
dist.methods<-list()
for(m in 1:length(distance.methods))
{
  dist.name.enh<-matrix(NA, ncol = length(UP.2017.unmatched$AC),nrow = length(UP.2012.all$AC))
  for(i in 1:length(UP.2017.unmatched$AC)) {
    for(j in 1:length(UP.2012.all$AC)) { 
      dist.name.enh[j,i]<-stringdist(tolower(UP.2017.unmatched[i,]$AC),tolower(UP.2012.all[j,]$AC),
                                     method = distance.methods[m])      
      #adist.enhance(UP.Match[i,]$name,UP_Crime[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

s

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods))
{
  
  dist.matrix.1<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix.1, 1, base::min)
  for(i in 1:nrow(dist.matrix.1))
  {
    s2.i<-match(min.name.enh[i],dist.matrix.1[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=UP.2017.unmatched[s2.i,]$AC, 
                                      s1name=UP.2012.all[s1.i,]$AC, adist=min.name.enh[i],
                                      method=distance.methods[m]),match.s1.s2.enh)
  }
}

library(reshape2)
matched.names.matrix.17with12<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix.17with12)
#Looks like jw does well

write.csv(matched.names.matrix.17with12,"matched.name.matrix.17with12.csv")


##################################
#Clean manually, merge and create the file EC.csv
#Next we import data from MyNeta.com

# Get data from Myneta
install.packages("RCurl")
install.packages("XML")
install.packages("data.table")


library(RCurl)
library(XML)
library(data.table)

#Extract the all candidate data
urlUP<-"http://myneta.info/uttarpradesh2017/index.php?action=summary&subAction=candidates_analyzed&sort=candidate#summary"
#Our table is the third table
candtableUP<-readHTMLTable(urlUP,which=3)
head(candtableUP)

str(candtableUP)
#remove unnecessary rows and columns
candUP<-candtableUP[3:4825,2:7]
str(candUP)
#candUP has Total Assets that are all garbaged
cand_UP<-as.data.table(candUP)

cand_UP$`Total Assets`<-as.character(cand_UP$`Total Assets`)
for (i in 1:nrow(cand_UP)){
  if(cand_UP$`Total Assets`[i]=="Nil"){
    cand_UP$`Total Assets`[i]=0
  }
  else{
    str<-cand_UP$`Total Assets`[i]
    strn<-gsub(",","",substr(str,start = 4,stop=regexpr("~", str)[1]-2))
    cand_UP$`Total Assets`[i]<-strn
  }
}
cand_UP$`Total Assets`<-as.numeric(cand_UP$`Total Assets`)
str(cand_UP)
#Define merging by
cand_UP$cand<-cand_UP$`Candidateâˆ‡`
cand_UP$const<-cand_UP$`Constituency `
cand_UP$party<-cand_UP$`Party `

#Let us do that for Serious Crimes
urlser<-"http://myneta.info/uttarpradesh2017/index.php?action=summary&subAction=serious_crime&sort=candidate#summary"

candtableser<-readHTMLTable(urlser,which=3)
candser<-candtableser[3:706,2:4]
cand_ser<-as.data.table(candser)
str(cand_ser)
names(cand_ser)[1] = 'Candidate'
#Define merging by
cand_ser$cand<-cand_ser$`Candidate`
cand_ser$const<-cand_ser$`Constituency `
cand_ser$party<-cand_ser$`Party `
cand_ser$ser<- 1

#Now Merge with CandUP
data<-merge(cand_UP,cand_ser,by=c("cand","const", "party"), all=TRUE)
data<-subset(data, select=c(1:3,7:9,13))
data$ser[is.na(data$ser)] <- 0
mean(data$ser)


#Let's see the data
View(data)
#Trim it further (not necessary though)
#data.MyNeta<-subset(data, select=c(1:3,7:9,13))
write.csv(data, file = "MyNeta.csv")
#Now let us merge it with EC

EC<-read.csv(file.choose(), header=T)
#Create common fields to merge
EC$cand<-EC$Name
EC$const<-EC$AC_NAME
EC$party<-EC$Party

UP.All<-merge(EC,data,by=c("cand","const", "party"), all=TRUE)
#Problem?
UP.Analysis<-merge(EC,data,by=c("const", "party"), all=TRUE)
