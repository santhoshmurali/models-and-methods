#Business Requirement of Titanic Project
# The sinking of the RMS Titanic is one of the most infamous shipwrecks in history.  On April 15, 1912, during her maiden voyage, the Titanic sank 
# after colliding with an iceberg, killing 1502 out of 2224 passengers and crew. This sensational tragedy shocked the international community and 
# led to better safety regulations for ships.
# 
# One of the reasons that the shipwreck led to such loss of life was that there were not enough lifeboats for the passengers and crew. Although 
# there was some element of luck involved in surviving the sinking, some groups of people were more likely to survive than others, such as women, 
# children, and the upper-class.
# 
# In this challenge, we ask you to complete the analysis of what sorts of people were likely to survive. In particular, we ask you to apply the 
# tools of machine learning to predict which passengers survived the tragedy.

# We will use CRISP-DM model for analysing this Data.
# CRISP-DM Model stands for CRoss Industry Standard Process for Data Mining
# It has 6 Steps
# 1. Business Understanding
# 2. Data understanding
# 3. Data Preparation
# 4. Modeling
# 5. Evaluation
# 6. Deployment

## Business understanding
#------------------------------------------------------------------------------------------------------------------
#Basically we have to perform survival analysis using the data (variables) provided. We have to predict what sort of people were likely to survive during a ship wrtk
#one more thing to consider is that, tnumber of variables provided is limited and we have to use only those. for example, weather is a key
#factor for survival, which is not part of this data set, so we are ignoring it. Next we will try to understand the data.


## Data understanding 
# ------------------------------------------------------------------------------------------------------------------
# I would like to put this as 2 step process.
# 1.Metadata
# 2.Exploratory Data analysis
# Metadata
# --------
# There are 3 sets of files provided out of which 2 contains passenger data. 1 is for training and the other is for testing.
# test.csv and train.csv both has same variables except 1 in train, Survived. 
# gender_submission.csv has only PassengerId and Survived indicator. We will use this file to compare our prediction accuracy.
data_test_titanic <- read.table(file = "Data/test.csv", header = TRUE, sep=",")
data_train_titanic <- read.table(file = "Data/train.csv", header = TRUE, sep=",")
# Variable Description are as follows.

# PassengerId
# -----------
str(data_train_titanic$PassengerId)
#   Integer vector and auto increment, nominal data.
#   This will help us to uniquely identify the customer.

# Survived
# --------
str(data_train_titanic$Survived)
#   Factor variable and Categorical data, 
#   1 : Survived
#   0 : Not Survived
#   This is the Y variable which we will be using for comparing againt. (We may choose to use C.A.R.T)

# Pclass
# --------
str(data_train_titanic$Pclass)
#   This can be both categorical oe ordinal data. 
#   1 : Passenger belongs to First Class
#   2 : Passenger Belongs to second Class
#   3 : Passenger Belongs to Third Class
#   If we see the description of the Class, it says 1st, 2nd and 3rd.There is an order of 1st, 2nd and 3rd. But it can also be considered as a 1st Category, 2nd Category and 3rdCategory.
#   for conveniance purpose, we will consider this as only Nominal (Categorical) Data

# Name
# ----
str(data_train_titanic$Name)
# This is a Character Vector and lists all the names of the passenger.

# Sex
# ----
str(data_train_titanic$Sex)
# Sex is a Nominal Factor variable with 2 levels, 1 being male amd 2 being female

# Age *********************************************
# ----********************************************* Missing Value Treatment Needs to be performed
str(data_train_titanic$Age)
# Age is a Number variable/Vector, it has several blank values (NA) in it.
# There are 177 records with out Age. We have to perform Missing Value 
# We have to Address Treating the missing value during Data Preparation Step. 

# ---------------
# SibSp &  Parch
# ---------------
# SibSp
str(data_train_titanic$SibSp)
# Number of siblings / spouses aboard the Titanic
# you see there is no distinction between Spouse and Sibling with the given data.
# Both Siblings and Spouses are combined together.

# Parch
str(data_train_titanic$Parch)
# Number of parents / children aboard the Titanic
# Here also, There is no distinction between Parent data and children data.
# Both of them are clubed in same data set.
#
# SibSp - has Sibling and Spouse, is a pair
# ParCh - has Parent and Children, is a pair aswell
# If SibSp is > 0,which means the particular passenger has 1 or more sibling or Spouse in the ship
# If Parch is > 0, then particular passenger has more than 1 parent or children in the ship
#
# SibSp       0
# ParCh       0
# Sibling     n
# Spouse      n
# Parent      n
# Child(ren)  n
#
# SibSp       1
# ParCh       0
# Sibling     can be
# Spouse      can be
# Parent      n
# Child(ren)  n
# The Passenger can be travelling with a Sibling or Spouse
# If she/he is travelling sibling, it does not mean that she/he does not have spouse, it means that she/he does not travel with spouse
# If she/he is travelling spouse, it does not mean that she/he does not have sibling, it means that she/he does not travel with sibling
# If Passenger is travelling with Sibling, mind that Passenger is also a sibling. It applies for Spouse as well.
#
# SibSp       0
# ParCh       1
# Sibling     n
# Spouse      n
# Parent      can be
# Child(ren)  can be
# The Passenger can be travelling with a Parent or One Child
# If Passenger is travelling with Parent, It does not mean Passenger do not have child. It means that Child might not have travelled
# If Passenger is travelling with Child, It does not mean Passenger do not have Parent. It means that parent might not have travelled
# If Passenger is travelling with Parent, mind that for that parent passenger current passenger is Child
# If Passenger is travelling with Child, mind that for the child passenger current passenger is Parent
#
# SibSp	      1
# ParCh       1
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger can be travelling with either Sibling, Spouse, Parent or Child, but only one companion
# The Passenger can be travelling with Sibling and parent: The Passenger is a Sibling and Child
# The Passenger can be travelling with Sibling and Child : The Passenger is a Sibling and might be a Parent
# The Passenger can be travelling with Spouse and Parent : The Passenger is a Spouse and might be a Child
# The Passenger can be travelling with Spouse and Child : The Passenger is a Spouse and might be a Parent
#
# SibSp       2
# ParCh       0
# Sibling     can be
# Spouse      can be
# Parent      n
# Child(ren)  n
# The passenger can be traveling with 2 Siblings : The Passenger is a sibling
# The Passenger can be travling with 1 sibling and 1 spouse :The Passenger is a Sibling and also a Spouse
# The Passenger can be traveling with 2 spouses : The Passenger is a Spouse
#
# SibSp       0
# ParCh       2
# Sibling     n
# Spouse      n
# Parent      can be
# Child(ren)  can be
# Assuming Parent means Single Parent.
# The Passenger can be traveling with Parents (Father and Mother) : The Passenger might be Child
# The Passenger can be traveling with Parent and Child :  The Passenger is a Parent and also a Child
# The Passenger can be traveling with 2 children : The Passenger is a Parent
#
# SibSp       2
# ParCh       1
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger is traveling with 3 people
# The Passenger can be travelling with 2 sibling and 1 Parent : The Passenger is a Sibling and also a Child
# The Passenger can be travelling with 1 sibling and 1 spouse and 1 parent : The Passenger is a Sibling, Spouse and Child
# The Passenger can be travelling with 2 spouse and 1 parent : The Passenger is a Spouse and Child
# The Passenger can be travelling with 2 sibling and 1 Child : The Passenger is a Sibling and Parent
# The Passenger can be travelling with 1 sibling and 1 spouse and 1 Child : The Passenger is a Sibling, Spouse and Parent
# The Passenger can be travelling with 2 spouse and 1 Child : The Passenger is a Spouse and Parent
#
# SibSp       1
# ParCh       2
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger is travelling with three people
# The Passenger can be travelling with 1 sibling and 2 parents : The Passenger is a Sibling and also a child
# The Passenger can be travelling with 1 sibling, 1 parent and 1 child : The Passenger is a Sibling, Child and Parent
# The passenger can be travelling with 1 sibling and 2 children : The Passenger is Sibling and Parent
# The Passenger can be travelling with 1 spouse and 2 parents : The Passenger is a Spouse and a child
# The Passenger can be travelling with 1 spouse, 1 parent and 1 child : The Passenger is a Spouse, Child and Parent
# The Passenger can be travelling with 1 spouse and 2 children : The Passenger is Spouse and Parent
#
# SibSp       2
# ParCh       2
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger is travelling with four people
# The Passenger can be travelling with 2 siblings and parents : The Passenger is a Sibling and also a child
# The Passenger can be travelling with 2 siblings, parent and child : The Passenger is a sibling, child and parent
# The Passenger can be travelling with 2 sibling and 2 children : The Passenger is a sibling and Parent
# The Passenger can be travelling with 2 Spouses and parents : The Passenger is a Spouse and child
# The Passenger can be travelling with 2 spouses, parent and child : The Passenger is a Spouse, Child and Parent
# The Passenger can be travelling with 2 Spouses and 2 children : The Passenger is a Spouse and Parent
# The Passenger can be travelling with 1 sibling and 1 spouse and parents : The Passenger is a sibling, spouse and child
# The Passenger can be travelling with 1 sibling, 1 spouse, parent and child :  The Passenger is a sibling, spouse, child and parent
# The Passenger can be travelling with 1 sibling, 1 spouse and 2 children : The Passenger is a sibling, spouse and Parent
#
# SibSp       2
# ParCh       3
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger is Travelling with 5 people
# The Passenger can be travelling with 2 Siblings, Parents and 1 Child : The Passenger is a sibling, child and parent
# The Passenger can be travelling with 2 Siblings, Parent and 2 Children : The Passenger is a sibling, child and parent
# The Passenger can be travelling with 2 Siblings and 3 Children : The Passenger is a sibling and parent
# The Passenger can be travelling with 2 Spouses, Parents and 1 Child : The Passenger is a Spouse, Child and Parent
# The Passenger can be travelling with 2 Spouses, Parent and 2 Children : The Passenger is a Spouse, Child and Parent
# The Passenger can be travelling with 2 Spouses and 3 Children : The Passenger is Spouse and Parent
# The Passenger can be travelling with 1 sibling, 1 Spouse, Parents and 1 Child : The Passenger Sibling, Spouse, Child and Parent
# The Passenger can be travelling with 1 sibling, 1 Spouse, Parent and 2 Children :  The Passenger Sibling, Spouse, Child and Parent
# The Passenger can be travelling with 1 sibling, 1 Spouse and 3 Children : The Passenger is a sibling, spouse and parent.
#
# SibSp       3
# ParCh       2
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger is Travelling eith 5 people
# The Passenger can be travelling with parents and 3 siblings : The Passenger is a child and a sibling
# The Passenger can be travelling with parents, 2 siblings and 1 spouse : The Passenger is child, sibling and spouse
# The Passenger can be travelling with parents, sibling and 2 spouses : The Passenger is child, sibling and spouse
# The Passenger can be travelling with 2 children and 3 siblings : The Passenger is a Parent and Sibling
# The Passenger can be travelling with 2 children,  2 siblings and 1 Spouse : The Passenger is a Parent, Sibling and Spouse
# The Passenger can be travelling with 2 children, sibling and 2 spouses : The Passenger is a Parent, Sibling and Spouse
# The Passenger can be travelling with parent, child and 3 siblings : The Passenger is a Child, Parent and Sibling
# The Passenger can be travelling with parent, child, 2 siblings and 1 Spouse : The Passenger is a Child, Sibling and Spouse
# The Passenger can be travelling with parent, child, sibling and 2 spouses : The Passenger is a Child, Parent, Sibling, Spouse
#
# SibSp       0
# ParCh       3
# Sibling     n
# Spouse      n
# Parent      can be
# Child(ren)  can be
# The Passenger is Travelling with 3 people 
# The Passenger can be travelling with parents and child : The Passenger is a child and parent
# The passenger can be travelling with parent and 2 children : The Passenger is a child and parent
# The Passenger can be travelling with 3 children : The Passenger is a parent
#
# SibSp       3
# ParCh       0
# Sibling     can be
# Spouse      can be
# Parent      n
# Child(ren)  n
# The Passenger is Travelling with 3 people 
# The Passenger can be Travelling with 2 Spouse and 1 Sibling : The Passenger is a spouse and sibling
# The Passenger can be Travelling with 1 Spouse and 2 Siblings : The Passenger is a spouse and sibling
# The Passenger can be Travelling with 3 Siblings : The Passenger is a Sibling
#
# SibSp       1
# ParCh       3
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The passenger is travelling with 4 people
# The Passenger can be Travelling with Sibling, Parents and Child : The Passenger can be Sibling, Child and Parent
# The Passenger can be Travelling with Sibling, Parent and 2 Children : The Passenger can be a Sibling, Child and Parent
# The Passenger can be Travelling with Siblling and 3 Children : The Passenger can be a Sibling and Parent
# The Passenger can be travelling with Spouse, Parents and Child : The Passenger can be Spouse, Child and Parent
# The Passenger can be travelling with Spouse, parent and 2 children : The Passenger can be Spouse, Child and Parent
# The Passenger can be travelling with Spouse and 3 Children : The Passenger can be a Spouse and Parent
#
# SibSp       3
# ParCh       1
# Sibling     can be
# Spouse      can be
# Parent      can be
# Child(ren)  can be
# The Passenger is Travelling with 4 People
# The Passenger can be Travelling with Parent, Sibling and 2 Spouse : The Passenger can be Child, Sibling and Spouse
# The Passenger can be Travelling with Parent, Siblings and Spouse : The Passenger can be Child, Sibling and Spouse
# The Passenger can be Travelling with Parent & Siblings : The Passenger can be Child and Sibling
# The Passenger can be Travelling with Child, Sibling and 2 Spouses : The Passenger can be Parent, Sibling and Spouse
# The Passenger can be Travelling with Child, Siblings and Spouse : The Passenger can be Parent, Sibling and Spouse
# The Passenger can be Travelling with Child and Siblings : The Passenger can be Parent and Sibling.

# More On Data Understanding about Relationship is in the Excel file in Data folder.
SibSp_ParCh <- read.table("Titanic Machine Learning/Data/SibSp_ParCh.csv",sep = ",", header = T)
View(SibSp_ParCh)





# Ticket

# Fare

# Cabin

# Embarked


