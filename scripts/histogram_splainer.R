require(pacman)
p_load(galacticPubs,galacticEdTools,ggplot2,gganimate,dplyr,readr,skimr)

NA_outliers <- function(df, QUANTILE_RANGE,id=NA,ignore) {
  # df should be a dataframe or matrix; QUANTILE_RANGE should be in the form c(.01,.99);
  # optional id (e.g. "band" or "bandyear") should be column name for reporting which values were switched to NA
  # ignore (not required) should be a vector a la c("length","width") or c(1:9), which specifies columns to ignore;
  # factors are ignored automatically, but you may wish to ignore some numeric columns as well

  if(missing(QUANTILE_RANGE)){QUANTILE_RANGE<-c(0.01,0.99)} #default quantile range

  df.orig<-df #for adding ignored columns back in at the end
  if(!missing(ignore)){
    if(is.numeric(ignore)){df<-df[,-ignore]
      ignames<-names(df.orig)[ignore]
      }else{df<-df[,-match(ignore,names(df))]
      ignames<-ignore}
      IGNORED<-df.orig[,ignames]} #make subset of data frame with selected columns removed, accounting for how columns are specified (numeric or names)

  #For checking data organization
check_class<-function(dataframe){
  #Check that columns are appropriately assigned to factor, numeric, etc
  Class<- sapply(1:length(dataframe),function(x)class(dataframe[,x]))
  ColumnName<-names(dataframe)
  return(cbind(ColumnName,Class))
}


  #Define function for calculating outliers and replacing with NA for vectors (each column)
  vector.outlier.remover<-function(x,QUANTILE_RANGE,na.rm=T,...)
  {
    if(is.numeric(x)) #only runs script on numeric columns
    {
      qnt <- quantile(x, probs=QUANTILE_RANGE, na.rm = na.rm)
      H <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      return(y)
    }else{return(as.character(x))}
  }#end vector.outlier.remover

  OUTPUT<-apply(df,2,function(x) {vector.outlier.remover(x,QUANTILE_RANGE)} )

  #Get indices for reporting changes
  CHANGED.index<-which(is.na(OUTPUT)&!is.na(df),arr.ind=T)
  ###MAke factors in OUTPUT match factors of columns in df
  class.df<-check_class(as.data.frame(df))[,"Class"]
  OUTPUT<-data.frame(OUTPUT,stringsAsFactors = F)
  for(i in 1: length(df))
  {
    if(class.df[i]=="factor"){OUTPUT[,i]<-as.factor(as.character(OUTPUT[,i]))
    }else{class(OUTPUT[,i])<-class.df[i]}
  }
  #Combine with ignored columns, make sure names stay same
  if(!missing(ignore)){
    OUTPUT<-cbind(data.frame(OUTPUT,stringsAsFactors = F),IGNORED)
    }else{OUTPUT<-OUTPUT}



  if(attributes(CHANGED.index)$dim[1]==0){
    return(list(newdata=OUTPUT,changelog="No Changes"))
  }else{
    CHANGED<-t(sapply(1:length(CHANGED.index[,1]),function(x) {
      id_x<-ifelse(is.na(id),
                   paste0("row ", CHANGED.index[x, "row"]),
                   as.character(OUTPUT[CHANGED.index[x, "row"], id]))
      data.frame(ID=id_x,
        COLUMN=names(df)[CHANGED.index[x, "col"]],
        OUTLIER=unlist(signif(df[CHANGED.index[x, "row"], CHANGED.index[x, "col"]], 3)),
        MEAN=signif(mean(unlist(df[, CHANGED.index[x, "col"]]), na.rm = T), 3))
    }
    ))
    CHANGED<-data.frame(CHANGED,stringsAsFactors = F)
    return(list(newdata=OUTPUT,changelog=CHANGED))
  }

}#End NA_outliers



#Read in human datasets

# SOURCE 1: Basic Body Measurements
# Kiru, Muhammad  (2021), “Body Measurements Datasets”, Mendeley Data, V1, doi: 10.17632/bjv6c9pmp4.1
# browseURL("https://data.mendeley.com/datasets/bjv6c9pmp4/1")
#
# UNITS ARE INCHES!
kiru00<-read_csv("data/Body Measurements _ original_CSV.csv")


kiru0<-kiru00 %>% select("Gender","Age","TotalHeight")

# Process Data Source 1 ---------------------------------------------------


skim(kiru0$Age)

#let's filter out younguns under 13
kiru<-kiru0 %>% filter(Age>7) %>% mutate(TotalHeight_cm=.*2.54)

skim(kiru)
NA_outliers(kiru)$changelog
#Looks good (no outliers for height)

(g0 <- kiru %>% ggplot(aes(x=1,y=TotalHeight))+
  scale_x_discrete()+
  theme_galactic()+
  labs(x="X-Axis is Not Meaningful",y="Height (cm)",title="Human Heights"))

(g_stack<-g0+geom_point(pch=21,stroke=.6,col="gray30",size=3))

(g_jit<- g0+geom_point(position=position_jitter(),pch=21,stroke=.6,col="gray30",size=3))

(g_box<-g_jit+geom_boxplot(col="#FFC300",fill="transparent",size=1)+
    geom_boxplot(fill="#FFC300",alpha=.3,col="transparent"))



