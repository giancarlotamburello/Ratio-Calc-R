# This function is used to automatically analyse volcanic gas time series to calculate molar ratios between two gas species.
# The ratio is calculated as the slope of the best-fit line between the numerator (Y) and denominator (X) gas time series in a moving window of a certain width.
# The calculation is accomplished if the gas marker concentration (an indicator of the presence of volcanic gas, e.g. SO₂ for magmatic gases) and the correlation coefficients
# are greater than a threshold.
# It works on both single or multiple files, it depends on whether the 'files' variable is a data frame or a list of file addresses.
#
# 'num.col' and 'den.col' are the column indexes in the data table for the numerator and denominator of the targeted gas ratio.
# 'marker.col' is the column index for the gas species that is considered a marker of the presence of volcanic gas (e.g. SO₂ for magmatic gases).
# 'coord.col' is a 2 elements array with the column indexes (e.g. 'c(4,5)') of longitude and latitude in decimal format. It is recommended to use it for traverses or moving measurements.
# 'marker.lim' is the concentration limit of the selected gas marker above which the ratio can be calculated.
# 'max.shift' is the maximum shift among the two gas time series that the algorithm can apply to maximize the correlation between numerator and denominator (set zero if no shift is applied).
# 'cor.lim' is the minimum correlation coefficient (Pearson) for ratio calculation
# 'time.str' is the time string format (must be the first column of the data table) if conversion into time is required (e.g. for 21-03-2022 10:11:01 set '%d-%m-%Y %H:%M:%S').
# 'win.width' is the width in samples of the moving window where each ratio is calculated.
# 'mov.step' are the steps (in samples) of the moving window.
# 'peak.dist' is the distance (in samples) between the window edges and the maximum values of denominator and numerator. It avoids having truncated peaks in the calculation window.




ratiocalc=function(files,
                   sep=";",
                   num.col=1,
                   den.col=2,
                   marker.col=2,
                   coord.col=NA,
                   marker.lim=1,
                   max.shift=0,
                   cor.lim=0.9,
                   time.str=NULL,
                   win.width=60,
                   mov.step=1,
                   peak.dist=5
){  
  
  if (class(files) == "data.frame"){
    df1=files
    files=""
  }else{
    files=list.files(path = files,full.names = T)
  }
  
  results=NULL
  
  for (ff in 1:length(files)) {
    
    #cat(files[ff],"\n")
    
    if(length(files) > 1){
      df1=read.table(file = files[ff],header = T,sep = sep,na.strings = c("NA","na","nan","NaN"))
      if (is.null(time.str)) {
        df1[,1]=as.character(df1[,1])
      }else{
        df1[,1]=strptime(df1[,1],time.str,"GMT")  
      }
      
    }
    
    df1=df1[complete.cases(df1[,c(num.col,den.col)]),]   
    
    pb=txtProgressBar(style = 3)
    
    if (diff(range(df1[,marker.col],na.rm = T))< marker.lim) {
      next()
    }
    
    dt=win.width
    
    
    
    ID=(1+max.shift)
    
    for (ii in seq(1+max.shift,nrow(df1)-dt-1-max.shift,mov.step)) {
      
      setTxtProgressBar(pb,ii/nrow(df1))
       
      if (diff(range(df1[ii:(ii+dt),marker.col]))< marker.lim) {
        ID=ii
        next()
      }
      
      
      if (max.shift !=0) {
        ccf1=ccf(df1[ii:(ii+dt),den.col],df1[ii:(ii+dt),num.col],plot=F)
        ccf1=cbind(ccf1$lag,ccf1$acf)
        ccf1=ccf1[abs(ccf1[,1])<=max.shift,]
        ccf1=ccf1[which.max(ccf1[,2]),1]  
      }else{
        ccf1=0
      }
      
      
      if( abs(which.max(df1[ii:(ii+dt),num.col])-dt) < peak.dist){
        ID=ii
        next()
      }
      
      cor1=cor(df1[(ii+ccf1):(ii+dt+ccf1),den.col],df1[ii:(ii+dt),num.col],use = "na.or.complete")
      if (cor1 < cor.lim){
        ID=ii
      }else{
        
        lm1=lm(df1[ii:(ii+dt),num.col]~df1[(ii+ccf1):(ii+dt+ccf1),den.col])
        ratio=lm1$coefficients[2]
        d.den=diff(range(df1[(ii+ccf1):(ii+dt+ccf1),den.col],na.rm=T))
        d.num=diff(range(df1[ii:(ii+dt),num.col],na.rm=T))
        marker.max=max(df1[(ii+ccf1):(ii+dt+ccf1),marker.col],na.rm = T)
        
        if(is.numeric(coord.col)){
          coords=c(mean(df1[(ii+ccf1):(ii+dt+ccf1),coord.col[1]],na.rm=T),
                   mean(df1[(ii+ccf1):(ii+dt+ccf1),coord.col[2]],na.rm=T))
        }else{
          coords=c(NA,NA)
        }
        
        
        
        results=rbind(results,
                      data.frame(time=df1[ii,1],
                                 ID=ii,
                                 ratio=ratio,
                                 cor=cor1,
                                 d.den=d.den,
                                 d.num=d.num,
                                 marker.max=marker.max,
                                 file=basename(files[ff]),
                                 x=coords[1],
                                 y=coords[2],
                                 row.names = NULL))
        
      }
    }
    
    
  }
  
  #results$ID=as.factor(results$ID)
  try(close(pb))
  return(results)
}
