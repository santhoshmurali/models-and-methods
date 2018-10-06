---
output:
  pdf_document: default
  html_document: default
---

## Submission Deadline- 8th October'17, 11:00 PM.

### Assignment Topic: Making Election Predictions  

If during the early part of 2014 you had to predict the upcoming Lok Sabha Elections, what would be your predictive model?  (Say you wanted to predict the election outcomes six months before the elections were held)

#### To solve this problem…here are some possible stuff you can think of….

* Identify data sources. This will depend upon what factors you think can determine winning elections
* Identifying the right technique/techniques to model that data
* Make predictions, validate and update your model.

#### Submission details:

##### Part 1: Identify the data sources, the variables of interest. Process the data and create a ‘clean’ data file(s).

Marks distribution: 20.

** What needs to be submitted: **
* Text file explaining what variables you are choosing and why, the source codes, explanation etc
* The cleaned data file (s)
* Relevant r codes if any
Marks will be awarded based on what factors you think are important in model building, what data sources you identify and finally how the clean data looks. The clean data has to be submitted as CSV (files).

##### Part 2: Develop the model,present a predictive model.
Marks distribution: 20

** What needs to be submitted: **
* Text file explaining the model (s), why did you do what you did, model performance et
* Relevant R codes if any



## Data Sources are 
### http://eci.nic.in/eci_main1/ElectionStatistics.aspx
The above data source will help us in getting candidates electoral inforormation. As the Question asks about "redict the upcoming Lok Sabha Elections" and does not specify id it is at the particular  candidate level or party level. We will assume it as a candidate level and also assume that Candidates are anaounced for by all parties in all constituency.

** We will take the winning information of 2009 and use as one of the predeicting parameter for 2014. **

#### 2009 data : http://eci.nic.in/eci_main/StatisticalReports/candidatewise/GE2009.xls [ Sheets : electors, Cand_Wise ]
#### 2014 data : http://eci.nic.in/eci_main/StatisticalReports/candidatewise/LS-2014_ElectionResult.xls [Sheets : electors, Cand_Wise ]

#### https://raw.githubusercontent.com/datameet/india-election-data/master/affidavits/myneta.2014.csv
Criminal Cases,Education,ID,Party,Sno,Total Assets,Total Liabilities,Year,ST_CODE,PC_CODE

#### Source Data


```R
library(stringr)
library(MASS)
library(stringdist)
```


```R
getwd()
setwd("C:/Home/Work/GreatLakes/Machine Learning/Assignment")
#setwd(dir = "D:/")
```


'C:/Home/Work/GreatLakes/Machine Learning/Assignment'



```R
LS2009Electors = read.csv(file = "LS2009Electors.csv", header = T, sep = ",")
LS2009Candidates = read.csv(file = "LS2009Candidate.csv", header = T, sep = ",")
LS2014Electors = read.csv(file = "LS2014Electors.csv", header = T, sep = ",")
LS2014Candidates = read.csv(file = "LS2014Candidate.csv", header = T, sep = ",")
Candidate2014Declaration = read.csv(file = "MyNeta.csv", header = T, sep = ",")
```


```R
names(LS2009Electors)[1]='STATE.CODE'
names(LS2014Electors)[1] = 'STATE.CODE'
```


```R
unique(LS2009Electors$STATE)
```


<ol class=list-inline>
	<li>Andhra Pradesh</li>
	<li>Arunachal Pradesh</li>
	<li>Assam</li>
	<li>Bihar</li>
	<li>Goa</li>
	<li>Gujarat</li>
	<li>Haryana</li>
	<li>Himachal Pradesh</li>
	<li>Jammu &amp; Kashmir</li>
	<li>Karnataka</li>
	<li>Kerala</li>
	<li>Madhya Pradesh</li>
	<li>Maharashtra</li>
	<li>Manipur</li>
	<li>Meghalaya</li>
	<li>Mizoram</li>
	<li>Nagaland</li>
	<li>Orissa</li>
	<li>Punjab</li>
	<li>Rajasthan</li>
	<li>Sikkim</li>
	<li>Tamil Nadu</li>
	<li>Tripura</li>
	<li>Uttar Pradesh</li>
	<li>West Bengal</li>
	<li>Chattisgarh</li>
	<li>Jharkhand</li>
	<li>Uttarakhand</li>
	<li>Andaman &amp; Nicobar Islands</li>
	<li>Chandigarh</li>
	<li>Dadra &amp; Nagar Haveli</li>
	<li>Daman &amp; Diu</li>
	<li>NCT OF Delhi</li>
	<li>Lakshadweep</li>
	<li>Puducherry</li>
</ol>



#### Before proceeding any further, it is always better to keep the back-up of originial data.


```R
SRC_LS2009Electors = LS2009Electors
SRC_LS2009Candidates = LS2009Candidates
SRC_LS2014Electors = LS2014Electors
SRC_LS2014Candidates = LS2014Candidates
SRC_Candidate2014Declaration = Candidate2014Declaration
```

##### Dimention Check


```R
dim(LS2009Electors)
```


<ol class=list-inline>
	<li>543</li>
	<li>8</li>
</ol>




```R
dim(LS2014Electors)
```


<ol class=list-inline>
	<li>543</li>
	<li>7</li>
</ol>




```R
dim(LS2009Candidates)
```


<ol class=list-inline>
	<li>8070</li>
	<li>14</li>
</ol>




```R
dim(LS2014Candidates)
```


<ol class=list-inline>
	<li>8794</li>
	<li>14</li>
</ol>




```R
dim(Candidate2014Declaration)
```


<ol class=list-inline>
	<li>8163</li>
	<li>12</li>
</ol>



###### Observations : in 2009 thre are 8 variables for electoral information, but we have only 7 variables in 2014.

## Data Cleaning and Data Preparation

#### first level Data cleaning - Converting the STATE and PARLIAMENTARY.CONSTITUENCY and also trimming white spaces in electoral data

### LS2009Electors


```R
str(LS2009Electors)
```

    'data.frame':	543 obs. of  8 variables:
     $ STATE.CODE                : Factor w/ 35 levels "S01","S02","S03",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ STATE                     : Factor w/ 35 levels "Andaman & Nicobar Islands",..: 2 2 2 2 2 2 2 2 2 2 ...
     $ PC.NO                     : int  1 2 3 4 5 6 7 8 9 10 ...
     $ PARLIAMENTARY.CONSTITUENCY: Factor w/ 540 levels "Adilabad","Agra",..: 1 405 272 385 540 343 331 459 218 121 ...
     $ Total.voters              : int  864165 905332 990646 891508 1017372 1061993 1206223 865357 732212 1086510 ...
     $ Total_Electors            : int  1131211 1315642 1496211 1333271 1359566 1389721 2343050 1574818 1393242 1681664 ...
     $ TOT_CONTESTANT            : int  9 15 15 12 10 9 19 23 23 15 ...
     $ POLL.PERCENTAGE           : num  76.4 68.8 66.2 66.9 74.8 ...
    


```R
LS2009Electors$STATE = trimws(tolower(LS2009Electors$STATE))
LS2009Electors$PARLIAMENTARY.CONSTITUENCY = trimws(tolower(LS2009Electors$PARLIAMENTARY.CONSTITUENCY))
cleaned.PARLIAMENTARY.CONSTITUENCY = sapply(LS2009Electors$PARLIAMENTARY.CONSTITUENCY, function(x) gsub('[ ]{2,}',' ',x))
LS2009Electors$PARLIAMENTARY.CONSTITUENCY = cleaned.PARLIAMENTARY.CONSTITUENCY
```

##### Sort the Data


```R
LS2009Electors = LS2009Electors[with(LS2009Electors, order(STATE.CODE,PC.NO)),]
```

### LS2014Electors


```R
str(LS2014Electors)
```

    'data.frame':	543 obs. of  7 variables:
     $ STATE.CODE                : Factor w/ 35 levels "S01","S02","S03",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ STATE                     : Factor w/ 35 levels "Andaman & Nicobar Islands",..: 2 2 2 2 2 2 2 2 2 2 ...
     $ PC.NO                     : int  1 2 3 4 5 6 7 8 9 10 ...
     $ PARLIAMENTARY.CONSTITUENCY: Factor w/ 540 levels "Adilabad ","Agra",..: 1 405 272 385 540 343 331 459 218 121 ...
     $ Total.voters              : int  1055593 1025194 1127225 1034032 1099784 1193548 1624859 1004763 971770 1322312 ...
     $ Total_Electors            : int  1386282 1425355 1550810 1496193 1445354 1536166 3183083 1893741 1823217 2185164 ...
     $ POLL.PERCENTAGE           : num  76.2 71.9 72.7 69.1 76.1 ...
    


```R
LS2014Electors$STATE = trimws(tolower(LS2014Electors$STATE))
LS2014Electors$PARLIAMENTARY.CONSTITUENCY = trimws(tolower(LS2014Electors$PARLIAMENTARY.CONSTITUENCY))
cleaned.PARLIAMENTARY.CONSTITUENCY = sapply(LS2014Electors$PARLIAMENTARY.CONSTITUENCY, function(x) gsub('[ ]{2,}',' ',x))
LS2014Electors$PARLIAMENTARY.CONSTITUENCY = cleaned.PARLIAMENTARY.CONSTITUENCY

```

##### Sort the Data


```R
LS2014Electors = LS2014Electors[with(LS2014Electors, order(LS2014Electors$STATE.CODE,LS2014Electors$PC.NO)),]
```

### Our Next step will be Joining the 2014 Electors with 2009 and capture previous time's Election result and study if it has an influence


```R
temp2009 = LS2009Electors
temp2014 = LS2014Electors
names(temp2009) = c("STATE.CODE_2009","STATE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009","Total.voters_2009","Total_Electors_2009","TOT_CONTESTANT_2009","POLL.PERCENTAGE_2009")
names(temp2014) = c("STATE.CODE_2014","STATE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014","Total.voters_2014","Total_Electors_2014","POLL.PERCENTAGE_2014")
temp = merge(x=temp2009,y=temp2014,by.x=c("STATE.CODE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009"), by.y = c("STATE.CODE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014"))
```

** We will find list of Constituency which has inconsistent naming convention between 2009 and 2014. and we match 2014 to match 2009.. This only for Electoral data for now and will do the same for Candidates. **


```R
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
```

      t_rn t_stc t_pcn             t_pc09              t_pc14
    1   75     4    17          gopalganj      gopalganj (sc)
    2   79     4    21            hajipur        hajipur (sc)
    3   81     4    23         samastipur     samastipur (sc)
    4   92     4    34            sasaram        sasaram (sc)
    5   96     4    38               gaya           gaya (sc)
    6   98     4    40              jamui          jamui (sc)
    7  308    19    10           ferozpur            firozpur
    8  391    24    13 gautam buddh nagar gautam buddha nagar
    

##### There are no mismatches in PARLIAMENTARY.CONSTITUENCY name between 2009 and 2014

###### If it finds mismatches, here is the way to fix it


```R
for(jj in 1:nrow(mismatching_pc_elect_09_14))
{
  LS2009Electors$PARLIAMENTARY.CONSTITUENCY[mismatching_pc_elect_09_14$t_rn[jj]] = LS2014Electors$PARLIAMENTARY.CONSTITUENCY[mismatching_pc_elect_09_14$t_rn[jj]]
}

```

### Electorals Merging


```R
temp2009 = LS2009Electors
temp2014 = LS2014Electors
names(temp2009) = c("STATE.CODE_2009","STATE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009","Total.voters_2009","Total_Electors_2009","TOT_CONTESTANT_2009","POLL.PERCENTAGE_2009")
names(temp2014) = c("STATE.CODE_2014","STATE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014","Total.voters_2014","Total_Electors_2014","POLL.PERCENTAGE_2014")
temp = merge(y=temp2009,x=temp2014,by.y=c("STATE.CODE_2009","PC.NO_2009","PARLIAMENTARY.CONSTITUENCY_2009"), by.x = c("STATE.CODE_2014","PC.NO_2014","PARLIAMENTARY.CONSTITUENCY_2014"), all = T)
```


```R
names(temp)
Electors_src = temp[,c(1,4,2,3,5,6,7,9,10,12,11)]
```


<ol class=list-inline>
	<li>'STATE.CODE_2014'</li>
	<li>'PC.NO_2014'</li>
	<li>'PARLIAMENTARY.CONSTITUENCY_2014'</li>
	<li>'STATE_2014'</li>
	<li>'Total.voters_2014'</li>
	<li>'Total_Electors_2014'</li>
	<li>'POLL.PERCENTAGE_2014'</li>
	<li>'STATE_2009'</li>
	<li>'Total.voters_2009'</li>
	<li>'Total_Electors_2009'</li>
	<li>'TOT_CONTESTANT_2009'</li>
	<li>'POLL.PERCENTAGE_2009'</li>
</ol>



### Candidate Data Cleaning


```R
str(LS2009Candidates)
```

    'data.frame':	8070 obs. of  14 variables:
     $ ST_CODE           : Factor w/ 35 levels "S01","S02","S03",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ State.name        : Factor w/ 35 levels "Andaman & Nicobar Islands",..: 2 2 2 2 2 2 2 2 2 2 ...
     $ Month             : int  3 3 3 3 3 3 3 3 3 3 ...
     $ Year              : int  2009 2009 2009 2009 2009 2009 2009 2009 2009 2009 ...
     $ PC.Number         : int  1 1 1 1 1 1 1 1 1 2 ...
     $ PC.name           : Factor w/ 540 levels "Adilabad","Agra",..: 1 1 1 1 1 1 1 1 1 405 ...
     $ PC.Type           : Factor w/ 3 levels "GEN","SC","ST": 3 3 3 3 3 3 3 3 3 2 ...
     $ Candidate.Name    : Factor w/ 7786 levels "'AIDS MAN' PRAKASH TATERAO LANDGE",..: 5848 3376 4096 117 5851 930 2211 4585 688 1980 ...
     $ Candidate.Sex     : Factor w/ 3 levels "","F","M": 3 3 3 3 3 3 3 3 3 3 ...
     $ Candidate.Category: Factor w/ 4 levels "","GEN","SC",..: 4 4 4 4 4 4 4 4 4 3 ...
     $ Candidate.Age     : int  43 39 59 55 50 55 36 39 47 51 ...
     $ Party.Abbreviation: Factor w/ 364 levels "","ABAS","ABCD(A)",..: 345 137 255 72 98 138 138 138 138 137 ...
     $ Total.Votes.Polled: int  372268 257181 112930 57931 16471 16441 13378 9157 7824 313748 ...
     $ Position          : int  1 2 3 4 5 6 7 8 9 1 ...
    


```R
LS2009Candidates$Candidate.Name = trimws(tolower(LS2009Candidates$Candidate.Name))
LS2009Candidates$Candidate.Name = sapply(LS2009Candidates$Candidate.Name,function(x) gsub('[ ]{2,}', ' ',x))
LS2009Candidates$Candidate.Name = sapply(LS2009Candidates$Candidate.Name,function(x) gsub('[[:punct:]]', '',x, perl = T))
head(LS2009Candidates$Candidate.Name,10)
```


<ol class=list-inline>
	<li>'rathod ramesh'</li>
	<li>'kotnak ramesh'</li>
	<li>'mesram nago rao'</li>
	<li>'ade tukaram'</li>
	<li>'rathod sadashiv naik'</li>
	<li>'banka sahadevu'</li>
	<li>'ganta pentanna'</li>
	<li>'nethavat ramdas'</li>
	<li>'athram laxman rao'</li>
	<li>'drgvivekanand'</li>
</ol>




```R
LS2009Candidates$State.name = trimws(tolower(LS2009Candidates$State.name))
LS2009Candidates$PC.name = trimws(tolower(LS2009Candidates$PC.name))
LS2009Candidates$Candidate.Category = trimws(LS2009Candidates$Candidate.Sex)
```

##### Sort The data


```R
LS2009Candidates = LS2009Candidates[with(LS2009Candidates,order(ST_CODE, PC.Number)),]

```

### Similarly do it fro 2014 also


```R
str(LS2014Candidates)
```

    'data.frame':	8794 obs. of  14 variables:
     $ ST_CODE           : Factor w/ 35 levels "S01","S02","S03",..: 1 1 1 1 1 1 1 1 1 1 ...
     $ State.name        : Factor w/ 35 levels "Andaman & Nicobar Islands",..: 2 2 2 2 2 2 2 2 2 2 ...
     $ Month             : int  5 5 5 5 5 5 5 5 5 5 ...
     $ Year              : int  2014 2014 2014 2014 2014 2014 2014 2014 2014 2014 ...
     $ PC.Number         : int  1 1 1 1 1 1 1 1 1 2 ...
     $ PC.name           : Factor w/ 540 levels "Adilabad ","Agra",..: 1 1 1 1 1 1 1 1 1 405 ...
     $ PC.Type           : Factor w/ 4 levels "GEN","SC","SC ",..: 4 4 4 4 4 4 4 4 4 2 ...
     $ Candidate.Name    : Factor w/ 7956 levels " ABDUL AMIR AMIRO",..: 2320 4549 5789 5934 4633 4708 4302 5004 957 927 ...
     $ Candidate.Sex     : Factor w/ 4 levels "F","M","NULL",..: 2 2 2 2 2 3 2 2 2 2 ...
     $ Candidate.Category: Factor w/ 5 levels "Gen","GEN","NULL",..: 5 5 5 5 5 3 5 5 5 4 ...
     $ Candidate.Age     : Factor w/ 63 levels "25","26","27",..: 25 13 24 31 20 63 16 9 29 7 ...
     $ Party.Abbreviation: Factor w/ 466 levels "A S P","Aa S P",..: 449 179 443 128 180 291 180 180 180 449 ...
     $ Total.Votes.Polled: int  430847 259557 184198 94420 41032 17084 8859 5055 4787 565496 ...
     $ Position          : int  1 2 3 4 5 6 7 8 9 1 ...
    


```R
LS2014Candidates$Candidate.Name = trimws(tolower(LS2014Candidates$Candidate.Name))
LS2014Candidates$Candidate.Name = sapply(LS2014Candidates$Candidate.Name,function(x) gsub('[ ]{2,}', ' ',x))
LS2014Candidates$Candidate.Name = sapply(LS2014Candidates$Candidate.Name,function(x) gsub('[[:punct:]]', '',x, perl = T))

```


```R
LS2014Candidates$State.name = trimws(tolower(LS2014Candidates$State.name))
LS2014Candidates$PC.name = trimws(tolower(LS2014Candidates$PC.name))
LS2014Candidates$Candidate.Category = trimws(LS2014Candidates$Candidate.Sex)
```


```R
LS2014Candidates = LS2014Candidates[with(LS2014Candidates,order(LS2014Candidates$ST_CODE, LS2014Candidates$PC.Number)),]

```

#### Identify the people who contested in both the times


```R
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
```

### Getting the value from 2009 for merging it with 2014.


```R
LS2009CandidatesFiltered = LS2009Candidates[contested_2009_2014$rn_2014,c('Candidate.Name','PC.name','Party.Abbreviation','Total.Votes.Polled','Position')]
dim(LS2009CandidatesFiltered)
head(LS2009CandidatesFiltered)
```


<ol class=list-inline>
	<li>1128</li>
	<li>5</li>
</ol>




<table>
<thead><tr><th></th><th scope=col>Candidate.Name</th><th scope=col>PC.name</th><th scope=col>Party.Abbreviation</th><th scope=col>Total.Votes.Polled</th><th scope=col>Position</th></tr></thead>
<tbody>
	<tr><th scope=row>26</th><td>vinod kumar boinapally   </td><td>karimnagar               </td><td>TRS                      </td><td>267684                   </td><td> 2                       </td></tr>
	<tr><th scope=row>28</th><td>chandupatla janga reddy  </td><td>karimnagar               </td><td>BJP                      </td><td>122337                   </td><td> 4                       </td></tr>
	<tr><th scope=row>29</th><td>barige gattaiah yadav    </td><td>karimnagar               </td><td>IND                      </td><td> 26026                   </td><td> 5                       </td></tr>
	<tr><th scope=row>37</th><td>gaddam raji reddy        </td><td>karimnagar               </td><td>IND                      </td><td>  5205                   </td><td>13                       </td></tr>
	<tr><th scope=row>47</th><td>dr vsathyanarayana murthy</td><td>nizamabad                </td><td>PPOI                     </td><td> 10296                   </td><td> 8                       </td></tr>
	<tr><th scope=row>54</th><td>malkapuram shiva kumar   </td><td>zahirabad                </td><td>PRAP                     </td><td>112792                   </td><td> 3                       </td></tr>
</tbody>
</table>




```R
temp = merge(x=LS2014Candidates,y=LS2009CandidatesFiltered,by='Candidate.Name',all.x = T)
Candidate_src = temp
```


```R
names(Candidate_src)
```


<ol class=list-inline>
	<li>'Candidate.Name'</li>
	<li>'ST_CODE'</li>
	<li>'State.name'</li>
	<li>'Month'</li>
	<li>'Year'</li>
	<li>'PC.Number'</li>
	<li>'PC.name.x'</li>
	<li>'PC.Type'</li>
	<li>'Candidate.Sex'</li>
	<li>'Candidate.Category'</li>
	<li>'Candidate.Age'</li>
	<li>'Party.Abbreviation.x'</li>
	<li>'Total.Votes.Polled.x'</li>
	<li>'Position.x'</li>
	<li>'PC.name.y'</li>
	<li>'Party.Abbreviation.y'</li>
	<li>'Total.Votes.Polled.y'</li>
	<li>'Position.y'</li>
</ol>




```R
names(Candidate_src) = c('Candidate.Name','ST_CODE','State.name','Month','Year','PC.Number','PC.Name','PC.Type','Candidate.Sex','Candidate.Category','Candidate.Age','Party.Abbreviation','Total.Votes.Polled','Position','PC.name.2009','Party.Abbreviation.2009','Total.Votes.Polled.2009','Position.2009')
Candidate_src = Candidate_src[,c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,18)]
```


```R
head(Candidate_src)
```


<table>
<thead><tr><th scope=col>Candidate.Name</th><th scope=col>ST_CODE</th><th scope=col>State.name</th><th scope=col>PC.Number</th><th scope=col>PC.Name</th><th scope=col>PC.Type</th><th scope=col>Candidate.Sex</th><th scope=col>Candidate.Category</th><th scope=col>Candidate.Age</th><th scope=col>Party.Abbreviation</th><th scope=col>Total.Votes.Polled</th><th scope=col>Position</th><th scope=col>PC.name.2009</th><th scope=col>Party.Abbreviation.2009</th><th scope=col>Total.Votes.Polled.2009</th><th scope=col>Position.2009</th></tr></thead>
<tbody>
	<tr><td>a biju       </td><td>S11          </td><td>kerala       </td><td> 9           </td><td>alathur      </td><td>SC           </td><td>M            </td><td>M            </td><td>36           </td><td>IND          </td><td>1692         </td><td>12           </td><td>NA           </td><td>NA           </td><td>NA           </td><td>NA           </td></tr>
	<tr><td>a devdas     </td><td>S10          </td><td>karnataka    </td><td> 9           </td><td>bellary      </td><td>ST           </td><td>M            </td><td>M            </td><td>45           </td><td>SUCI         </td><td>8486         </td><td> 5           </td><td>NA           </td><td>NA           </td><td>NA           </td><td>NA           </td></tr>
	<tr><td>a irudayadass</td><td>S22          </td><td>tamil nadu   </td><td> 3           </td><td>chennai south</td><td>GEN          </td><td>M            </td><td>M            </td><td>53           </td><td>IND          </td><td> 976         </td><td>12           </td><td>NA           </td><td>NA           </td><td>NA           </td><td>NA           </td></tr>
	<tr><td>a jaiganesh  </td><td>S22          </td><td>tamil nadu   </td><td> 3           </td><td>chennai south</td><td>GEN          </td><td>M            </td><td>M            </td><td>29           </td><td>LSP          </td><td> 518         </td><td>16           </td><td>NA           </td><td>NA           </td><td>NA           </td><td>NA           </td></tr>
	<tr><td>a josekutty  </td><td>S11          </td><td>kerala       </td><td>18           </td><td>kollam       </td><td>GEN          </td><td>M            </td><td>M            </td><td>58           </td><td>IND          </td><td>1702         </td><td>12           </td><td>NA           </td><td>NA           </td><td>NA           </td><td>NA           </td></tr>
	<tr><td>a magesh     </td><td>S22          </td><td>tamil nadu   </td><td> 3           </td><td>chennai south</td><td>GEN          </td><td>M            </td><td>M            </td><td>31           </td><td>IND          </td><td> 121         </td><td>41           </td><td>NA           </td><td>NA           </td><td>NA           </td><td>NA           </td></tr>
</tbody>
</table>



## Merging Candidate data with Electoral Data


```R
 Elector_Candidate = merge(x = Electors_src,y = Candidate_src, by.x = c('STATE.CODE_2014','PC.NO_2014'), by.y = c('ST_CODE','PC.Number'), all = T)
```


```R
head(Elector_Candidate)
```


<table>
<thead><tr><th scope=col>STATE.CODE_2014</th><th scope=col>PC.NO_2014</th><th scope=col>STATE_2014</th><th scope=col>PARLIAMENTARY.CONSTITUENCY_2014</th><th scope=col>Total.voters_2014</th><th scope=col>Total_Electors_2014</th><th scope=col>POLL.PERCENTAGE_2014</th><th scope=col>Total.voters_2009</th><th scope=col>Total_Electors_2009</th><th scope=col>POLL.PERCENTAGE_2009</th><th scope=col>...</th><th scope=col>Candidate.Sex</th><th scope=col>Candidate.Category</th><th scope=col>Candidate.Age</th><th scope=col>Party.Abbreviation</th><th scope=col>Total.Votes.Polled</th><th scope=col>Position</th><th scope=col>PC.name.2009</th><th scope=col>Party.Abbreviation.2009</th><th scope=col>Total.Votes.Polled.2009</th><th scope=col>Position.2009</th></tr></thead>
<tbody>
	<tr><td>S01           </td><td>1             </td><td>andhra pradesh</td><td>adilabad      </td><td>1055593       </td><td>1386282       </td><td>76.15         </td><td>864165        </td><td>1131211       </td><td>76.39         </td><td>...           </td><td>M             </td><td>M             </td><td>48            </td><td>TDP           </td><td>184198        </td><td>3             </td><td>NA            </td><td>NA            </td><td>NA            </td><td>NA            </td></tr>
	<tr><td>S01           </td><td>1             </td><td>andhra pradesh</td><td>adilabad      </td><td>1055593       </td><td>1386282       </td><td>76.15         </td><td>864165        </td><td>1131211       </td><td>76.39         </td><td>...           </td><td>M             </td><td>M             </td><td>49            </td><td>TRS           </td><td>430847        </td><td>1             </td><td>NA            </td><td>NA            </td><td>NA            </td><td>NA            </td></tr>
	<tr><td>S01           </td><td>1             </td><td>andhra pradesh</td><td>adilabad      </td><td>1055593       </td><td>1386282       </td><td>76.15         </td><td>864165        </td><td>1131211       </td><td>76.39         </td><td>...           </td><td>M             </td><td>M             </td><td>40            </td><td>IND           </td><td>  8859        </td><td>7             </td><td>NA            </td><td>NA            </td><td>NA            </td><td>NA            </td></tr>
	<tr><td>S01           </td><td>1             </td><td>andhra pradesh</td><td>adilabad      </td><td>1055593       </td><td>1386282       </td><td>76.15         </td><td>864165        </td><td>1131211       </td><td>76.39         </td><td>...           </td><td>M             </td><td>M             </td><td>44            </td><td>IND           </td><td> 41032        </td><td>5             </td><td>NA            </td><td>NA            </td><td>NA            </td><td>NA            </td></tr>
	<tr><td>S01           </td><td>1             </td><td>andhra pradesh</td><td>adilabad      </td><td>1055593       </td><td>1386282       </td><td>76.15         </td><td>864165        </td><td>1131211       </td><td>76.39         </td><td>...           </td><td>NULL          </td><td>NULL          </td><td>NULL          </td><td>NOTA          </td><td> 17084        </td><td>6             </td><td>NA            </td><td>NA            </td><td>NA            </td><td>NA            </td></tr>
	<tr><td>S01           </td><td>1             </td><td>andhra pradesh</td><td>adilabad      </td><td>1055593       </td><td>1386282       </td><td>76.15         </td><td>864165        </td><td>1131211       </td><td>76.39         </td><td>...           </td><td>M             </td><td>M             </td><td>37            </td><td>INC           </td><td>259557        </td><td>2             </td><td>NA            </td><td>NA            </td><td>NA            </td><td>NA            </td></tr>
</tbody>
</table>




```R
Elector_Candidate = Elector_Candidate[,c(1:12,15:25)]
```


```R
Elector_Candidate = Elector_Candidate[,c(1,3,2,4,7,8,9,10,11,12,13,14,5,15,16,17,18,6,23,19,20,21,22)]
```


```R
names(Elector_Candidate) = toupper(names(Elector_Candidate))
names(Elector_Candidate)
```


<ol class=list-inline>
	<li>'STATE.CODE_2014'</li>
	<li>'STATE_2014'</li>
	<li>'PC.NO_2014'</li>
	<li>'PARLIAMENTARY.CONSTITUENCY_2014'</li>
	<li>'POLL.PERCENTAGE_2014'</li>
	<li>'TOTAL.VOTERS_2009'</li>
	<li>'TOTAL_ELECTORS_2009'</li>
	<li>'POLL.PERCENTAGE_2009'</li>
	<li>'TOT_CONTESTANT_2009'</li>
	<li>'CANDIDATE.NAME'</li>
	<li>'PC.TYPE'</li>
	<li>'CANDIDATE.SEX'</li>
	<li>'TOTAL.VOTERS_2014'</li>
	<li>'CANDIDATE.CATEGORY'</li>
	<li>'CANDIDATE.AGE'</li>
	<li>'PARTY.ABBREVIATION'</li>
	<li>'TOTAL.VOTES.POLLED'</li>
	<li>'TOTAL_ELECTORS_2014'</li>
	<li>'POSITION.2009'</li>
	<li>'POSITION'</li>
	<li>'PC.NAME.2009'</li>
	<li>'PARTY.ABBREVIATION.2009'</li>
	<li>'TOTAL.VOTES.POLLED.2009'</li>
</ol>



### Cleaning MyNeta Data


```R
head(Candidate2014Declaration)
```


<table>
<thead><tr><th scope=col>Candidate</th><th scope=col>Constituency</th><th scope=col>Criminal.Cases</th><th scope=col>Education</th><th scope=col>ID</th><th scope=col>Party</th><th scope=col>Sno</th><th scope=col>Total.Assets</th><th scope=col>Total.Liabilities</th><th scope=col>Year</th><th scope=col>ST_CODE</th><th scope=col>PC_CODE</th></tr></thead>
<tbody>
	<tr><td>Kaushal Yadav                                    </td><td>NAWADA                                           </td><td>8                                                </td><td>Post Graduate                                    </td><td> 148                                             </td><td>JD(U)                                            </td><td>1                                                </td><td>154566136                                        </td><td>2604969                                          </td><td>2014                                             </td><td>S04                                              </td><td>39                                               </td></tr>
	<tr><td>Kiran Sharma                                     </td><td>AZAMGARH                                         </td><td>0                                                </td><td>8th Pass                                         </td><td>9487                                             </td><td>Bhartiya Shakti Chetna Party                     </td><td>2                                                </td><td>  3509407                                        </td><td> 325000                                          </td><td>2014                                             </td><td>S24                                              </td><td>69                                               </td></tr>
	<tr><td>M. Aamir Rashadi                                 </td><td>AZAMGARH                                         </td><td>1                                                </td><td>Others                                           </td><td>9496                                             </td><td>Rashtriya Ulama Council                          </td><td>3                                                </td><td>  2191523                                        </td><td>      0                                          </td><td>2014                                             </td><td>S24                                              </td><td>69                                               </td></tr>
	<tr><td>Rakesh Kumar Giri                                </td><td>MAHARAJGANJ                                      </td><td>0                                                </td><td>Graduate Professional                            </td><td>9706                                             </td><td>IND                                              </td><td>4                                                </td><td>   306023                                        </td><td>      0                                          </td><td>2014                                             </td><td>S24                                              </td><td>63                                               </td></tr>
	<tr><td>(Kuppal)G.Devadoss                               </td><td>CHENNAI SOUTH                                    </td><td>0                                                </td><td>8th Pass                                         </td><td>6912                                             </td><td>IND                                              </td><td>5                                                </td><td>  3630000                                        </td><td> 850000                                          </td><td>2014                                             </td><td>S22                                              </td><td> 3                                               </td></tr>
	<tr><td>(Maj Gen (Retd.) ) Bhuwan Chandra Khanduri (Avsm)</td><td>GARHWAL                                          </td><td>0                                                </td><td>Graduate Professional                            </td><td>  74                                             </td><td>BJP                                              </td><td>6                                                </td><td> 44357368                                        </td><td>      0                                          </td><td>2014                                             </td><td>S28                                              </td><td> 2                                               </td></tr>
</tbody>
</table>




```R
Candidate2014Declaration$Candidate = trimws(tolower(Candidate2014Declaration$Candidate))
Candidate2014Declaration$Candidate = sapply(Candidate2014Declaration$Candidate,function(x) gsub('[ ]{2,}', ' ',x))
Candidate2014Declaration$Candidate = sapply(Candidate2014Declaration$Candidate,function(x) gsub('[[:punct:]]', '',x, perl = T))
```


```R
Candidate2014Declaration = Candidate2014Declaration[with(Candidate2014Declaration,order(ST_CODE, PC_CODE)),]
```


```R
head(Candidate2014Declaration)
```


<table>
<thead><tr><th></th><th scope=col>Candidate</th><th scope=col>Constituency</th><th scope=col>Criminal.Cases</th><th scope=col>Education</th><th scope=col>ID</th><th scope=col>Party</th><th scope=col>Sno</th><th scope=col>Total.Assets</th><th scope=col>Total.Liabilities</th><th scope=col>Year</th><th scope=col>ST_CODE</th><th scope=col>PC_CODE</th></tr></thead>
<tbody>
	<tr><th scope=row>962</th><td>banka sahadev   </td><td>ADILABAD        </td><td>1               </td><td>Graduate        </td><td>7678            </td><td>IND             </td><td> 962            </td><td>  393000        </td><td>     0          </td><td>2014            </td><td>S01             </td><td>1               </td></tr>
	<tr><th scope=row>2255</th><td>godam nagesh    </td><td>ADILABAD        </td><td>0               </td><td>Post Graduate   </td><td>7674            </td><td>TRS             </td><td>2255            </td><td>10378857        </td><td>148784          </td><td>2014            </td><td>S01             </td><td>1               </td></tr>
	<tr><th scope=row>4306</th><td>mosali chinnaiah</td><td>ADILABAD        </td><td>0               </td><td>12th Pass       </td><td>6202            </td><td>IND             </td><td>4306            </td><td> 3167000        </td><td> 40000          </td><td>2014            </td><td>S01             </td><td>1               </td></tr>
	<tr><th scope=row>4561</th><td>naresh          </td><td>ADILABAD        </td><td>0               </td><td>Doctorate       </td><td>7675            </td><td>INC             </td><td>4561            </td><td> 1800000        </td><td>     0          </td><td>2014            </td><td>S01             </td><td>1               </td></tr>
	<tr><th scope=row>4649</th><td>nethawath ramdas</td><td>ADILABAD        </td><td>0               </td><td>Illiterate      </td><td>6201            </td><td>IND             </td><td>4649            </td><td>       0        </td><td>     0          </td><td>2014            </td><td>S01             </td><td>1               </td></tr>
	<tr><th scope=row>5045</th><td>pawar krishna   </td><td>ADILABAD        </td><td>1               </td><td>Post Graduate   </td><td>7677            </td><td>IND             </td><td>5045            </td><td>       0        </td><td>     0          </td><td>2014            </td><td>S01             </td><td>1               </td></tr>
</tbody>
</table>




```R
OverallData = merge(x=Elector_Candidate,y=Candidate2014Declaration,by.x = c('STATE.CODE_2014','PC.NO_2014','CANDIDATE.NAME'), by.y = c('ST_CODE','PC_CODE','Candidate'))
names(OverallData)
```


<ol class=list-inline>
	<li>'STATE.CODE_2014'</li>
	<li>'PC.NO_2014'</li>
	<li>'CANDIDATE.NAME'</li>
	<li>'STATE_2014'</li>
	<li>'PARLIAMENTARY.CONSTITUENCY_2014'</li>
	<li>'POLL.PERCENTAGE_2014'</li>
	<li>'TOTAL.VOTERS_2009'</li>
	<li>'TOTAL_ELECTORS_2009'</li>
	<li>'POLL.PERCENTAGE_2009'</li>
	<li>'TOT_CONTESTANT_2009'</li>
	<li>'PC.TYPE'</li>
	<li>'CANDIDATE.SEX'</li>
	<li>'TOTAL.VOTERS_2014'</li>
	<li>'CANDIDATE.CATEGORY'</li>
	<li>'CANDIDATE.AGE'</li>
	<li>'PARTY.ABBREVIATION'</li>
	<li>'TOTAL.VOTES.POLLED'</li>
	<li>'TOTAL_ELECTORS_2014'</li>
	<li>'POSITION.2009'</li>
	<li>'POSITION'</li>
	<li>'PC.NAME.2009'</li>
	<li>'PARTY.ABBREVIATION.2009'</li>
	<li>'TOTAL.VOTES.POLLED.2009'</li>
	<li>'Constituency'</li>
	<li>'Criminal.Cases'</li>
	<li>'Education'</li>
	<li>'ID'</li>
	<li>'Party'</li>
	<li>'Sno'</li>
	<li>'Total.Assets'</li>
	<li>'Total.Liabilities'</li>
	<li>'Year'</li>
</ol>



OverallData = OverallData[,c('STATE.CODE_2014','STATE_2014','PC.NO_2014','PARLIAMENTARY.CONSTITUENCY_2014','PC.TYPE','TOTAL.VOTERS_2014','TOTAL_ELECTORS_2014','TOTAL.VOTERS_2009','TOTAL_ELECTORS_2009','POLL.PERCENTAGE_2009','TOT_CONTESTANT_2009','PARTY.ABBREVIATION.2009','TOTAL.VOTES.POLLED.2009','POSITION.2009','PC.NAME.2009','CANDIDATE.NAME','CANDIDATE.SEX','CANDIDATE.CATEGORY','CANDIDATE.AGE','PARTY.ABBREVIATION','Criminal.Cases','Education','Total.Assets','Total.Liabilities','POLL.PERCENTAGE_2014','TOTAL.VOTES.POLLED','POSITION')]
head(OverallData)

## Variable Selection

#### PC Type


```R
OverallData$PC.TYPE = trimws(OverallData$PC.TYPE)

OverallData$M_PCTYPE_ST = ifelse(OverallData$PC.TYPE == 'ST',1,0)
OverallData$M_PCTYPE_SC = ifelse(OverallData$PC.TYPE == 'SC',1,0)
OverallData$M_PCTYPE_GEN = ifelse(OverallData$PC.TYPE == 'GEN',1,0)
```

#### TOTAL.VOTERS


```R
TotalVotersPropotion = scale(OverallData$TOTAL.VOTERS_2014)
OverallData$M_TotalVotersPropotion = TotalVotersPropotion
```


```R

```


# Tasks Pending
# Calculate Total Candidates by Aggrigating the data from LS2014Candidates and add it to Electors while merging.

