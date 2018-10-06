# Import Titan Dataset
# Summary of Titan Data Set
attach(Titan_Dataset)
summary(Titan_Dataset)
# 
# Sales Person     Old Scheme       New Scheme    
# Min.   : 1.00   Min.   : 28.00   Min.   : 32.00  
# 1st Qu.: 8.25   1st Qu.: 54.00   1st Qu.: 55.00  
# Median :15.50   Median : 67.00   Median : 74.00  
# Mean   :15.50   Mean   : 68.03   Mean   : 72.03  
# 3rd Qu.:22.75   3rd Qu.: 81.50   3rd Qu.: 85.75  
# Max.   :30.00   Max.   :110.00   Max.   :122.00  
#
#Mean of Old Scheme > u1 = 68.03
#Mean of New Scheme > u2 = 72.03
sd_old_scheme = sd(`Old Scheme`)
# Standard Deviation of Old Scheme - S1 = 20.46
sd_new_scheme = sd(`New Scheme`)
# Standard Deviation of New Scheme - S1 = 24.06

cv_old_scheme = sd_old_scheme/mean(`Old Scheme`)
#Co-efficieant Variable of Old Scheme cv1 = 0.30
cv_new_scheme = sd_new_scheme/mean(`New Scheme`)
#Co-efficieant Variable of New Scheme cv2 = 0.33

#exploring the data further using box-plot
boxplot(`Old Scheme`, `New Scheme`, horizontal = TRUE, col=rainbow(2))
mydata = Titan_Dataset
attach(mydata)

names(mydata)[1] = 'Sales.Person'
names(mydata)[2] = 'Old.Scheme'
names(mydata)[3] = 'New.Scheme'
Paired.t.test=t.test(Old.Scheme, New.Scheme, paired = T, alternative = "less")
Paired.t.test
