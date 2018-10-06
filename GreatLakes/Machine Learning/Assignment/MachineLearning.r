#11-10-2017

library(stringr)
library(MASS)

getwd()
setwd("C:/Home/Work/GreatLakes/Machine Learning/Assignment")
#setwd(dir = "D:/")

LS2009Electors = read.csv(file = "LS2009Electors.csv", header = T, sep = ",")
LS2009Candidates = read.csv(file = "LS2009Candidate.csv", header = T, sep = ",")
LS2014Electors = read.csv(file = "LS2014Electors.csv", header = T, sep = ",")
LS2014Candidates = read.csv(file = "LS2014Candidate.csv", header = T, sep = ",")
Candidate2014Declaration = read.csv(file = "MyNeta.csv", header = T, sep = ",")

Candidate.Freequency = table(LS2014Candidates$State.name)
Candidate.Freequency = as.data.frame(Candidate.Freequency)
Candidate.Freequency = Candidate.Freequency[with(Candidate.Freequency, order(-Freq)),]

#Uttar Pradesh Candidates
UP = Candidate.Freequency[Candidate.Freequency$Var1 == 'Uttar Pradesh',]
UP_candidates = UP$Freq

#Southindia Candidates
SI = Candidate.Freequency[Candidate.Freequency$Var1 %in% c('Kerala','Karnataka','Andhra Pradesh','Tamil Nadu','Puducherry'),]

si_ctr=0
for(si in 1:nrow(SI))
{
  si_ctr = si_ctr + SI$Freq[si]
}
SI_Candidates = si_ctr

Cadidates = data.frame(UP_candidates,SI_Candidates)
names(Cadidates) = c('Uttar Pradesh','South India')

#We will choose South Indian states
Cadidates

#Data Back-up
SRC_LS2009Electors = LS2009Electors
SRC_LS2009Candidates = LS2009Candidates
SRC_LS2014Electors = LS2014Electors
SRC_LS2014Candidates = LS2014Candidates
SRC_Candidate2014Declaration = Candidate2014Declaration

### Data Filtering from South India States
LS2009Electors = LS2009Electors[LS2009Electors$STATE.CODE %in% c('S01','S10','S11','S22','U07'),]
LS2009Candidates = LS2009Candidates[LS2009Candidates$ST_CODE %in% c('S01','S10','S11','S22','U07'),]
LS2014Electors = LS2014Electors[LS2014Electors$STATE.CODE %in% c('S01','S10','S11','S22','U07'),]
LS2014Candidates = LS2014Candidates[LS2014Candidates$ST_CODE %in% c('S01','S10','S11','S22','U07'),]
Candidate2014Declaration = Candidate2014Declaration[Candidate2014Declaration$ST_CODE %in% c('S01','S10','S11','S22','U07'),]


dim(LS2009Electors)
dim(LS2014Electors)


names(LS2009Candidates)

names(LS2014Candidates)

str(LS2009Electors)
#first level Data cleaning - Converting the STATE and PARLIAMENTARY.CONSTITUENCY and also trimming white spaces
LS2009Electors$STATE = trimws(tolower(LS2009Electors$STATE))
LS2009Electors$PARLIAMENTARY.CONSTITUENCY = trimws(tolower(LS2009Electors$PARLIAMENTARY.CONSTITUENCY))
cleaned.PARLIAMENTARY.CONSTITUENCY = sapply(LS2009Electors$PARLIAMENTARY.CONSTITUENCY, function(x) gsub('[ ]{2,}',' ',x))
LS2009Electors$PARLIAMENTARY.CONSTITUENCY = cleaned.PARLIAMENTARY.CONSTITUENCY


#Sorting the data
LS2009Electors = LS2009Electors[with(LS2009Electors, order(STATE.CODE,PC.NO)),]


str(LS2014Electors)
#Data Cleaning
LS2014Electors$STATE = trimws(tolower(LS2014Electors$STATE))
LS2014Electors$PARLIAMENTARY.CONSTITUENCY = trimws(tolower(LS2014Electors$PARLIAMENTARY.CONSTITUENCY))
cleaned.PARLIAMENTARY.CONSTITUENCY = sapply(LS2014Electors$PARLIAMENTARY.CONSTITUENCY, function(x) gsub('[ ]{2,}',' ',x))
LS2014Electors$PARLIAMENTARY.CONSTITUENCY = cleaned.PARLIAMENTARY.CONSTITUENCY

#Sorting the data
LS2014Electors = LS2014Electors[with(LS2014Electors, order(LS2014Electors$STATE.CODE,LS2014Electors$PC.NO)),]


temp2009 = LS2009Electors
temp2014 = LS2014Electors
names(temp2009) = c("STATE.CODE_2009","STATE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009","Total.voters_2009","Total_Electors_2009","TOT_CONTESTANT_2009","POLL.PERCENTAGE_2009")
names(temp2014) = c("STATE.CODE_2014","STATE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014","Total.voters_2014","Total_Electors_2014","POLL.PERCENTAGE_2014")
temp = merge(x=temp2009,y=temp2014,by.x=c("STATE.CODE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009"), by.y = c("STATE.CODE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014"))


# We will find list of Constituency which has inconsistant naming convention between 2009 and 2014. an we mathc 2014 to match 2009.. This only for Electoral data for now and will do the same for Candidates.
t_stc = c()
t_pcn = c()
t_pc09 = c()
t_pc14 = c()
t_rn = c()
for(ii in 1:nrow(temp2014))
{
  if(!(temp2014$PARLIAMENTARY.CONSTITUENCY_2014[ii] %in% temp$PARLIAMENTARY.CONSTITUENCY_2009))
  {
    #print(ii)
    #print(temp2014$PARLIAMENTARY.CONSTITUENCY_2014[ii])
    #print(temp2009$PARLIAMENTARY.CONSTITUENCY_2009[ii])
    t_rn = append(t_rn, ii)
    t_stc = append(t_stc,temp2014$STATE.CODE_2014[ii])
    t_pcn = append(t_pcn,temp2014$PC.NO_2014[ii])
    t_pc09 = append(t_pc09,temp2009$PARLIAMENTARY.CONSTITUENCY_2009[ii])
    t_pc14 = append(t_pc14,temp2014$PARLIAMENTARY.CONSTITUENCY_2014[ii])
    
  }
}
mismatching_pc_elect_09_14 = data.frame(t_rn,t_stc, t_pcn, t_pc09, t_pc14)
mismatching_pc_elect_09_14


# Assuming 2014 Data is more recent than 2009, updating 2009 to 2014 standard.
for(jj in 1:nrow(mismatching_pc_elect_09_14))
{
  LS2009Electors$PARLIAMENTARY.CONSTITUENCY[mismatching_pc_elect_09_14$t_rn[jj]] = LS2014Electors$PARLIAMENTARY.CONSTITUENCY[mismatching_pc_elect_09_14$t_rn[jj]]
}


#We will Merge the data from 2009 to 2014 to know the results from previopus year.
temp2009 = LS2009Electors
temp2014 = LS2014Electors
names(temp2009) = c("STATE.CODE_2009","STATE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009","Total.voters_2009","Total_Electors_2009","TOT_CONTESTANT_2009","POLL.PERCENTAGE_2009")
names(temp2014) = c("STATE.CODE_2014","STATE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014","Total.voters_2014","Total_Electors_2014","POLL.PERCENTAGE_2014")
temp = merge(y=temp2009,x=temp2014,by.y=c("STATE.CODE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009"), by.x = c("STATE.CODE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014"), all = T)


#Creating first version of Electoral Source 
names(temp)
Electors_src = temp[,c(1,4,2,3,5,6,7,9,10,12,11)]


# Now we will fix candidates data
#candidate Name is something which we are after, we will use that join between 2009 and 2014 we have to clean the name in both

str(LS2009Candidates)
#Name Processing
LS2009Candidates$Candidate.Name = trimws(tolower(LS2009Candidates$Candidate.Name))
LS2009Candidates$Candidate.Name = sapply(LS2009Candidates$Candidate.Name,function(x) gsub('[ ]{2,}', ' ',x))
LS2009Candidates$Candidate.Name = sapply(LS2009Candidates$Candidate.Name,function(x) gsub('[[:punct:]]', '',x, perl = T))


#Data Cleaning
LS2009Candidates$State.name = trimws(tolower(LS2009Candidates$State.name))
LS2009Candidates$PC.name = trimws(tolower(LS2009Candidates$PC.name))
LS2009Candidates$Candidate.Category = trimws(LS2009Candidates$Candidate.Sex)

#Data Sorting
LS2009Candidates = LS2009Candidates[with(LS2009Candidates,order(ST_CODE, PC.Number)),]



str(LS2014Candidates)
#Name Processing
LS2014Candidates$Candidate.Name = trimws(tolower(LS2014Candidates$Candidate.Name))
LS2014Candidates$Candidate.Name = sapply(LS2014Candidates$Candidate.Name,function(x) gsub('[ ]{2,}', ' ',x))
LS2014Candidates$Candidate.Name = sapply(LS2014Candidates$Candidate.Name,function(x) gsub('[[:punct:]]', '',x, perl = T))


#Data Cleaning
LS2014Candidates$State.name = trimws(tolower(LS2014Candidates$State.name))
LS2014Candidates$PC.name = trimws(tolower(LS2014Candidates$PC.name))
LS2014Candidates$Candidate.Category = trimws(LS2014Candidates$Candidate.Sex)

#Data Sorting
  LS2014Candidates = LS2014Candidates[with(LS2014Candidates,order(LS2014Candidates$ST_CODE, LS2014Candidates$PC.Number)),]

rn_2014 = c()
rn_2009 = c()
cname_2014 = c()
cname_2009 = c()
s_dist = c() 
d = 0.0

for(kk in 1:nrow(LS2014Candidates) )
{
  for(ll in 1:nrow(LS2009Candidates))
  {
    if(LS2014Candidates$Candidate.Name[kk]LS2009Candidates$Candidate.Name[ll])
    rn_2014 = append(rn_2014,kk)
    rn_2009 = append(rn_2009,ll)
    cname_2014 = append(cname_2014,LS2014Candidates$Candidate.Name[kk])
    cname_2009 = append(cname_2009,LS2009Candidates$C==andidate.Name[ll])
    
    d = stringdist(LS2014Candidates$Candidate.Name[kk],LS2009Candidates$Candidate.Name[ll],method = 'jaccard', q=1)
    s_dist = append(s_dist, d)
  }
}


for(kk in 1:nrow(LS2014Candidates) )
{
  if(LS2014Candidates$Candidate.Name[kk] %in% LS2009Candidates$Candidate.Name)
  {
    rn_2014 = append(rn_2014,kk)
    rn_2009 = append(rn_2009,ll)
    cname_2014 = append(cname_2014,LS2014Candidates$Candidate.Name[kk])
    cname_2009 = append(cname_2009,LS2009Candidates$Candidate.Name[ll])
    #s_dist = append(s_dist, d)
    
  }
}


stringdist('KHAJA QUAYUM ANWAR','KHAJA QUAYUM ANWAR',method = 'jaccard',q=0)


unique(LS2014Candidates$PC.Type)
unique(trimws(LS2014Candidates$PC.Type))

str(Candidate2014Declaration)
#Data Selection
CandidateInfo = Candidate2014Declaration[,c('Candidate','Criminal.Cases','Education','Total.Assets','Total.Liabilities','ST_CODE','PC_CODE')]