library(plyr)
summ_groups = function(df,colnumberstart,colnumberend,groupnum) {
  groups = unique(df[,groupnum])
  for (gr in groups) {
    summ_all(df[df[,groupnum]==gr,],colnumberstart,colnumberend,groupnum,filename=gr)
  }
}

summ_all <- function(df0,colnumberstart,colnumberend,groupnum,filename="output"){
  
  #Rearrange data to alphabetical order by groups
  df1 <- df0[order(df0[groupnum]),]
  fulldata <- df1[colnumberstart:colnumberend]
  bygroup <- df1[groupnum]
  
  #initalize variables to run for loop
  total <- (colnumberend - colnumberstart)
  i <- 0
  length.1 <- 0
  unique1 <- 0
  FinalOutput <- 0
  
  for(i in 0:(total)){
    i <- (i + colnumberstart)
    unique2 <- (unique(df1[[i]][i]))
    FinalOutput[i]
    
      if(is.numeric(unique2) == FALSE || (is.element(0,df1[,i]) == TRUE) || (is.element(1,df1[,i]) == TRUE)){
        df1 <- (1 * df1)
        a <- with(df1, sum(df1[i]))
        b <- nrow(df1[i])
        percents <- (a / b)
        title <- (colnames(df1[i]))
        print(paste(title, a, "(",percents,")"))
        FinalOutput[i] <- paste(title," ", a, "(",percents,")")
     }
    
      else{
        q1 <- colwise(quantile)(df1[i], 0.25)
        q3 <- colwise(quantile)(df1[i], 0.75)
        med1 <- colwise(quantile)(df1[i], 0.5)
        title <- (colnames(df1[i]))
        print(paste(title, med1,"(",q1,"-",q3,")"))
        FinalOutput[i] <- paste(title," ", med1,"(",q1,"-",q3,")")
        
      }
  }
  
  #Final print onto .csv file
  rbind(FinalOutput)
  write.csv(x = as.data.frame(FinalOutput), file = paste0(filename,".csv"), row.names=F, na="")
}

