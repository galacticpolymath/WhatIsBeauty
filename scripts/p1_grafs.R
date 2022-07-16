
require(pacman)
p_load(galacticPubs,galacticEdTools,patchwork)

# Predictions graphs ------------------------------------------------------
#P1
jitter_data<-function(df,cols,xvar,yvar,jitter_amt=0.05, jitter_noise=0.2,min_spacing=3,seed=4){
  set.seed(seed)
  if(missing(cols)){
    col_classes<-apply(df,2, class)
    cols<-which(col_classes !="factor" & col_classes!="logical" & col_classes !="character" & col_classes !="integer")
  }

  if(missing(xvar)|
     missing(yvar)) {
    if (length(cols) == 2) {
      #for our purposes doesn't matter which is x & y here if there are only 2 columns
      xvar <- names(df)[cols[1]]
      yvar <- names(df)[cols[2]]
    } else{
      stop("Must supply xvar and yvar")
    }
  }

  if(length(xvar)!=length(yvar)){
    stop("xvar and yvar must be same length")
  }

  new0<-apply(df[,cols],2,function(col_i){
    #default to 10% of mean - min
      jitter_mean=(mean(col_i,na.rm=TRUE)-min(col_i,na.rm=TRUE))*jitter_amt
      jitter_sd=jitter_mean*jitter_noise
      jit<-rnorm(length(col_i),jitter_mean,jitter_sd)*  sample(c(-1,1),length(col_i),replace=TRUE) #random signs of jitter
      return(col_i+jit)
  })
  # Iterate over all supplied xvar, yvar pairs to make sure randomly jittered points don't overlap
  for(i in length(xvar)) {
    new <- new0
    #Replace values that are <jitter_amt apart (overlap too much)
    tolerated <- FALSE
    iter <- 1
    while (!tolerated) {
      if (iter > 100) {
        stop("Jitter failed. Try increasing jitter_amt or jitter_noise.")
      }
      dfi <- new[, c(xvar[i], yvar[i])]
      disti <- dist(dfi) %>% as.matrix()
      disti[upper.tri(disti, diag = TRUE)] <- NA
      #can't overlap within min_spacing% of range of distances
      overlaps <- which(disti < (diff(range(disti,na.rm=TRUE))/100*min_spacing), arr.ind = TRUE)
      if(nrow(overlaps)==0){tolerated<-TRUE;break}
      #try to fix overlaps by changing one of each pair randomly
      for (overlap_row in 1:nrow(overlaps)) {
        set.seed(seed + iter + overlap_row) #to make results replicable
        which_col <- sample(1:2, 1) #choose x or y col to change, randomly
        set.seed(seed + iter + overlap_row) #to make results replicable
        if (which_col == 1) {
          #store X replacement value in new
          jitter_meanx = (mean(dfi[, xvar[i]], na.rm = TRUE) - min(dfi[, xvar[i]], na.rm =TRUE)) * jitter_amt
          jitter_sdx = jitter_meanx * jitter_noise
          newjit <-
            rnorm(1, mean = jitter_meanx, sd = jitter_sdx) *  sample(c(-1, 1), 1)
          replacementx<-new[overlaps[overlap_row, which_col], xvar[i]] + newjit
          if(replacementx> max(dfi[, xvar[i]], na.rm = TRUE)+jitter_meanx) {
            #don't replace value if it's too big
          } else{
            new[overlaps[overlap_row, which_col], xvar[i]] <- replacementx
          }
        } else{
          #store Y replacement value in new
          jitter_meany = (mean(dfi[, yvar[i]], na.rm = TRUE) - min(dfi[, yvar[i]], na.rm =
                                                                     TRUE)) * jitter_amt
          jitter_sdy = jitter_meany * jitter_noise
          newjit <-
            rnorm(1, mean = jitter_meany, sd = jitter_sdy) *  sample(c(-1, 1), 1)
          replacementy<- new[overlaps[overlap_row, which_col], yvar[i]] + newjit
          if(replacementy> max(dfi[,yvar[i]],na.rm=TRUE)+jitter_meany){
           #don't replace value if it's too big
          }else{
          new[overlaps[overlap_row, which_col], yvar[i]] <-replacementy
          }

        }
      }
      iter <- iter + 1
    }
  }
  return(dplyr::as_tibble(new))
}

#Custom graph function
graph_pred<-function(data){
  data %>%
  ggplot(aes(cog,emo))+
  geom_point(size=4,pch=21,fill="#363636",alpha=.3)+ #position=position_jitter(width=.1,height=.1)
  geom_point(size=4,pch=21,color="#363636",stroke=.7)+
    theme_galactic("bw",pad.outer = c(30,30,30,30))+
  scale_x_continuous(breaks=0:2,labels = 0:2,expand = expansion(.1),minor_breaks = NULL)+
  scale_y_continuous(expand=expansion(.1),minor_breaks = NULL)+
  coord_cartesian(xlim=c(0,2),ylim=c(-2,2),clip="off")
}

p1_data<-tibble(cog=c(2,2,2,1,2,2,1,2,2,1,2),emo=c(2,2,2,2,2,1,2,1,1,2,2))

(g1<-p1_data %>%  jitter_data(jitter_amt=.05,jitter_noise =.1,seed=7) %>%
  graph_pred+
  labs(x="Cognitive Score",y="Emotional Score",title="Prediction I"))

p2_data<-tibble(cog=c(0,0,0,1,0,0,0,0,0,0,0),emo=c(-2,-2,-2,-2,-2,-1,-2,-1,-1,-2,-2))

(g2<-p2_data %>%  jitter_data(jitter_amt=.07,jitter_noise =.2,seed=7,min_spacing=5) %>%
  graph_pred+
  labs(x="Cognitive Score",y="Emotional Score",title="Prediction II"))

p3_data<-tibble(cog=c(1,0,0,0,0,0,0,0,0,0,0),emo=c(2,1,2,2,2,1,2,1,2,2,2))

(g3<-p3_data %>%  jitter_data(jitter_amt=.05,jitter_noise =.2,seed=7) %>%
  graph_pred+
  labs(x="Cognitive Score",y="Emotional Score",title="Prediction III"))

p4_data<-tibble(cog=c(2,2,2,1,2,2,2,1,2,2,2),emo=c(-2,-2,-2,-2,-2,-1,-2,-1,-1,-2,-2))

(g4<-p4_data %>%  jitter_data() %>%
  graph_pred+
  labs(x="Cognitive Score",y="Emotional Score",title="Prediction IV"))

(g1+g2)/(g3+g4)
gpsave("P1_Predictions Graphs.png",width=10,height=10)
