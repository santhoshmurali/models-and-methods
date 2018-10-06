
library(stringr)
library(MASS)
library(stringdist)

getwd()
setwd("C:/Home/Work/GreatLakes/Machine Learning/Assignment")
#setwd(dir = "D:/")

LS2009Electors = read.csv(file = "LS2009Electors.csv", header = T, sep = ",")
LS2009Candidates = read.csv(file = "LS2009Candidate.csv", header = T, sep = ",")
LS2014Electors = read.csv(file = "LS2014Electors.csv", header = T, sep = ",")
LS2014Candidates = read.csv(file = "LS2014Candidate.csv", header = T, sep = ",")
Candidate2014Declaration = read.csv(file = "MyNeta.csv", header = T, sep = ",")

names(LS2009Electors)[1]='STATE.CODE'
names(LS2014Electors)[1] = 'STATE.CODE'

unique(LS2009Electors$STATE)

SRC_LS2009Electors = LS2009Electors
SRC_LS2009Candidates = LS2009Candidates
SRC_LS2014Electors = LS2014Electors
SRC_LS2014Candidates = LS2014Candidates
SRC_Candidate2014Declaration = Candidate2014Declaration

dim(LS2009Electors)

dim(LS2014Electors)

dim(LS2009Candidates)

dim(LS2014Candidates)

dim(Candidate2014Declaration)

str(LS2009Electors)

LS2009Electors$STATE = trimws(tolower(LS2009Electors$STATE))
LS2009Electors$PARLIAMENTARY.CONSTITUENCY = trimws(tolower(LS2009Electors$PARLIAMENTARY.CONSTITUENCY))
cleaned.PARLIAMENTARY.CONSTITUENCY = sapply(LS2009Electors$PARLIAMENTARY.CONSTITUENCY, function(x) gsub('[ ]{2,}',' ',x))
LS2009Electors$PARLIAMENTARY.CONSTITUENCY = cleaned.PARLIAMENTARY.CONSTITUENCY

LS2009Electors = LS2009Electors[with(LS2009Electors, order(STATE.CODE,PC.NO)),]

str(LS2014Electors)

LS2014Electors$STATE = trimws(tolower(LS2014Electors$STATE))
LS2014Electors$PARLIAMENTARY.CONSTITUENCY = trimws(tolower(LS2014Electors$PARLIAMENTARY.CONSTITUENCY))
cleaned.PARLIAMENTARY.CONSTITUENCY = sapply(LS2014Electors$PARLIAMENTARY.CONSTITUENCY, function(x) gsub('[ ]{2,}',' ',x))
LS2014Electors$PARLIAMENTARY.CONSTITUENCY = cleaned.PARLIAMENTARY.CONSTITUENCY


LS2014Electors = LS2014Electors[with(LS2014Electors, order(LS2014Electors$STATE.CODE,LS2014Electors$PC.NO)),]

temp2009 = LS2009Electors
temp2014 = LS2014Electors
names(temp2009) = c("STATE.CODE_2009","STATE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009","Total.voters_2009","Total_Electors_2009","TOT_CONTESTANT_2009","POLL.PERCENTAGE_2009")
names(temp2014) = c("STATE.CODE_2014","STATE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014","Total.voters_2014","Total_Electors_2014","POLL.PERCENTAGE_2014")
temp = merge(x=temp2009,y=temp2014,by.x=c("STATE.CODE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009"), by.y = c("STATE.CODE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014"))

t_stc = c()
t_pcn = c()
t_pc09 = c()
t_pc14 = c()
t_rn = c()
for(ii in 1:nrow(temp2014))
{
  if(!(temp2014$PARLIAMENTARY.CONSTITUENCY_2014[ii] %in% temp$PARLIAMENTARY.CONSTITUENCY_2009))
  {
    t_rn = append(t_rn, ii)
    t_stc = append(t_stc,temp2014$STATE.CODE_2014[ii])
    t_pcn = append(t_pcn,temp2014$PC.NO_2014[ii])
    t_pc09 = append(t_pc09,temp2009$PARLIAMENTARY.CONSTITUENCY_2009[ii])
    t_pc14 = append(t_pc14,temp2014$PARLIAMENTARY.CONSTITUENCY_2014[ii])
    
  }
}
mismatching_pc_elect_09_14 = data.frame(t_rn,t_stc, t_pcn, t_pc09, t_pc14)
print(mismatching_pc_elect_09_14)

for(jj in 1:nrow(mismatching_pc_elect_09_14))
{
  LS2009Electors$PARLIAMENTARY.CONSTITUENCY[mismatching_pc_elect_09_14$t_rn[jj]] = LS2014Electors$PARLIAMENTARY.CONSTITUENCY[mismatching_pc_elect_09_14$t_rn[jj]]
}


temp2009 = LS2009Electors
temp2014 = LS2014Electors
names(temp2009) = c("STATE.CODE_2009","STATE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009","Total.voters_2009","Total_Electors_2009","TOT_CONTESTANT_2009","POLL.PERCENTAGE_2009")
names(temp2014) = c("STATE.CODE_2014","STATE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014","Total.voters_2014","Total_Electors_2014","POLL.PERCENTAGE_2014")
temp = merge(y=temp2009,x=temp2014,by.y=c("STATE.CODE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009"), by.x = c("STATE.CODE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014"), all = T)

names(temp)
Electors_src = temp[,c(1,4,2,3,5,6,7,9,10,12,11)]

str(LS2009Candidates)

LS2009Candidates$Candidate.Name = trimws(tolower(LS2009Candidates$Candidate.Name))
LS2009Candidates$Candidate.Name = sapply(LS2009Candidates$Candidate.Name,function(x) gsub('[ ]{2,}', ' ',x))
LS2009Candidates$Candidate.Name = sapply(LS2009Candidates$Candidate.Name,function(x) gsub('[[:punct:]]', '',x, perl = T))
head(LS2009Candidates$Candidate.Name,10)

LS2009Candidates$State.name = trimws(tolower(LS2009Candidates$State.name))
LS2009Candidates$PC.name = trimws(tolower(LS2009Candidates$PC.name))
LS2009Candidates$Candidate.Category = trimws(LS2009Candidates$Candidate.Sex)

LS2009Candidates = LS2009Candidates[with(LS2009Candidates,order(ST_CODE, PC.Number)),]


str(LS2014Candidates)

LS2014Candidates$Candidate.Name = trimws(tolower(LS2014Candidates$Candidate.Name))
LS2014Candidates$Candidate.Name = sapply(LS2014Candidates$Candidate.Name,function(x) gsub('[ ]{2,}', ' ',x))
LS2014Candidates$Candidate.Name = sapply(LS2014Candidates$Candidate.Name,function(x) gsub('[[:punct:]]', '',x, perl = T))


LS2014Candidates$State.name = trimws(tolower(LS2014Candidates$State.name))
LS2014Candidates$PC.name = trimws(tolower(LS2014Candidates$PC.name))
LS2014Candidates$Candidate.Category = trimws(LS2014Candidates$Candidate.Sex)

LS2014Candidates = LS2014Candidates[with(LS2014Candidates,order(LS2014Candidates$ST_CODE, LS2014Candidates$PC.Number)),]


rn_2014 = c()
cname_2014 = c()


for(kk in 1:nrow(LS2014Candidates) )
{
  if(LS2014Candidates$Candidate.Name[kk] %in% LS2009Candidates$Candidate.Name)
  {
    rn_2014 = append(rn_2014,kk)
    cname_2014 = append(cname_2014,LS2014Candidates$Candidate.Name[kk])
      #s_dist = append(s_dist, d)
    
  }
}

contested_2009_2014 = as.data.frame(rn_2014, cname_2014)

LS2009CandidatesFiltered = LS2009Candidates[contested_2009_2014$rn_2014,c('Candidate.Name','PC.name','Party.Abbreviation','Total.Votes.Polled','Position')]
dim(LS2009CandidatesFiltered)
head(LS2009CandidatesFiltered)

temp = merge(x=LS2014Candidates,y=LS2009CandidatesFiltered,by='Candidate.Name',all.x = T)
Candidate_src = temp

names(Candidate_src)

names(Candidate_src) = c('Candidate.Name','ST_CODE','State.name','Month','Year','PC.Number','PC.Name','PC.Type','Candidate.Sex','Candidate.Category','Candidate.Age','Party.Abbreviation','Total.Votes.Polled','Position','PC.name.2009','Party.Abbreviation.2009','Total.Votes.Polled.2009','Position.2009')
Candidate_src = Candidate_src[,c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,18)]

head(Candidate_src)

 Elector_Candidate = merge(x = Electors_src,y = Candidate_src, by.x = c('STATE.CODE_2014','PC.NO_2014'), by.y = c('ST_CODE','PC.Number'), all = T)

head(Elector_Candidate)

Elector_Candidate = Elector_Candidate[,c(1:12,15:25)]

Elector_Candidate = Elector_Candidate[,c(1,3,2,4,7,8,9,10,11,12,13,14,5,15,16,17,18,6,23,19,20,21,22)]

names(Elector_Candidate) = toupper(names(Elector_Candidate))
names(Elector_Candidate)

head(Candidate2014Declaration)

Candidate2014Declaration$Candidate = trimws(tolower(Candidate2014Declaration$Candidate))
Candidate2014Declaration$Candidate = sapply(Candidate2014Declaration$Candidate,function(x) gsub('[ ]{2,}', ' ',x))
Candidate2014Declaration$Candidate = sapply(Candidate2014Declaration$Candidate,function(x) gsub('[[:punct:]]', '',x, perl = T))

Candidate2014Declaration = Candidate2014Declaration[with(Candidate2014Declaration,order(ST_CODE, PC_CODE)),]

head(Candidate2014Declaration)

OverallData = merge(x=Elector_Candidate,y=Candidate2014Declaration,by.x = c('STATE.CODE_2014','PC.NO_2014','CANDIDATE.NAME'), by.y = c('ST_CODE','PC_CODE','Candidate'))
names(OverallData)

OverallData = OverallData[,c('STATE.CODE_2014','STATE_2014','PC.NO_2014','PARLIAMENTARY.CONSTITUENCY_2014','PC.TYPE','TOTAL.VOTERS_2014','TOTAL_ELECTORS_2014','TOTAL.VOTERS_2009','TOTAL_ELECTORS_2009','POLL.PERCENTAGE_2009','TOT_CONTESTANT_2009','PARTY.ABBREVIATION.2009','TOTAL.VOTES.POLLED.2009','POSITION.2009','PC.NAME.2009','CANDIDATE.NAME','CANDIDATE.SEX','CANDIDATE.CATEGORY','CANDIDATE.AGE','PARTY.ABBREVIATION','Criminal.Cases','Education','Total.Assets','Total.Liabilities','POLL.PERCENTAGE_2014','TOTAL.VOTES.POLLED','POSITION')]
head(OverallData)

OverallData$PC.TYPE = trimws(OverallData$PC.TYPE)

OverallData$M_PCTYPE_ST = ifelse(OverallData$PC.TYPE == 'ST',1,0)
OverallData$M_PCTYPE_SC = ifelse(OverallData$PC.TYPE == 'SC',1,0)
OverallData$M_PCTYPE_GEN = ifelse(OverallData$PC.TYPE == 'GEN',1,0)

TotalVotersPropotion = scale(OverallData$TOTAL.VOTERS_2014)
OverallData$M_TotalVotersPropotion = TotalVotersPropotion


