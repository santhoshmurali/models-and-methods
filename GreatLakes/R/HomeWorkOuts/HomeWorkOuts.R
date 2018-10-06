setwd("C:/Home/Work/GreatLakes/R/HomeWorkOuts")
getwd()

udf_FreeqDistrMean <- function(variables, bin_size, freeqTable){
  # Mean for Freequency Distribution
  Ranges = ceiling(max(variables) - min(variables))
  BinWidth = Ranges/bin_size
  Bins = seq(from = min(variables), to=max(variables) , by = BinWidth )
  Bins_Freeq = cut(variables, Bins)
  Bins_FreeqTable <- table(Bins_Freeq)
  names(Bins_FreeqTable) <- Bins[-1*(length(Bins))]
  Bins_FreeqTable <- data.frame(Bins_FreeqTable)
  names(Bins_FreeqTable) <- c("c_Bins","c_Freeq")
  Bins_FreeqTable$c_Bins <- as.numeric(levels(Bins_FreeqTable$c_Bins))[Bins_FreeqTable$c_Bins]
  Bins_FreeqTable$f <- Bins_FreeqTable$c_Bins + (BinWidth)/2
  Bins_FreeqTable$fx <- Bins_FreeqTable$f * Bins_FreeqTable$c_Freeq
  freeq_mean = sum(Bins_FreeqTable$fx)/sum(Bins_FreeqTable$c_Freeq)
  if (freeqTable == 1)
  {
    return(Bins_FreeqTable)
  }
  else
  {
  return(freeq_mean)
  }
}

udf_FreeqDistrMean(InsectSprays$count,10,0)


# Median for Freequency Distribution
udf_FreqDistribution <- function(u_variables, u_bin_size) {
  #Freequency Distribution
Ranges = ceiling(max(u_variables) - min(u_variables))
BinWidth = Ranges/u_bin_size
Bins = seq(from = min(u_variables), to=max(u_variables) , by = BinWidth )
Bins_Freeq = cut(u_variables, Bins)
Bins_FreeqTable <- table(Bins_Freeq)
names(Bins_FreeqTable) <- Bins[-1*(length(Bins))]
Bins_FreeqTable <- data.frame(Bins_FreeqTable)
names(Bins_FreeqTable) <- c("c_Bins","c_Freeq")
Bins_FreeqTable$c_Bins <- as.numeric(levels(Bins_FreeqTable$c_Bins))[Bins_FreeqTable$c_Bins]
return(Bins_FreeqTable)
}

freq_median_tbl <- udf_FreqDistribution(InsectSprays$count,5)
if(length(freq_median_tbl$c_Bins)%%2 != 0)
{
  u_median = median(freq_median_tbl$c_Bins)
} else
{
  u_median = freq_median_tbl$c_Bins[length(freq_median_tbl$c_Bins)/2]
}



freq_median_tbl$c_Bins[3]-freq_median_tbl$c_Bins[2]
length(freq_median_tbl$c_Bins)%%2
