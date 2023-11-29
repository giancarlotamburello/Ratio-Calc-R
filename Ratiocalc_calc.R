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
                   peak.dist=5,
                   output=NULL
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
      
      if (!is.null(output) & !file.exists(output)) {
        break()
      }
      
      
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
        
        if(!is.na(coord.col)){
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