calc.files=function(folder="unzipped/",
                    sep=";",
                    x=2,
                    y=3,
                    marker=2,
                    conc.lim=0,
                    step.win=20,
                    step.mov=1,
                    min.win=200,
                    max.win=0,
                    cor.lim=0.6,
                    max.shift=20,
                    fltr=c(1E8,1E8),
                    long=NA,
                    lat=NA,
                    peak=0,
                    smmry=F,
                    filename=NA){
  
  
  if (class(folder) == "character") {
    
    
    files=dir(folder)
    
    nn=1
    
    for (ff in files) {
      df=read.table(file = paste(folder,ff,sep = "/"),header = T,sep = sep,na.strings = c("NA","NaN","na","nan"),fill = T,blank.lines.skip = T,stringsAsFactors = F)
      
      if(exists("results")){
        results=rbind(results,
                      calc.ratio(df=df,x=x,y=y,marker=marker,conc.lim=conc.lim,step.win=step.win,step.mov=step.mov,min.win=min.win,max.win=max.win,
                                 cor.lim=cor.lim,max.shift=max.shift,fltr=fltr,long=long,lat=lat,peak=peak,smmry=smmry))
        
        
      }else{
        results=calc.ratio(df=df,x=x,y=y,marker=marker,conc.lim=conc.lim,step.win=step.win,step.mov=step.mov,min.win=min.win,max.win=max.win,
                           cor.lim=cor.lim,max.shift=max.shift,fltr=fltr,long=long,lat=lat,peak=peak,smmry=smmry)
      }
      
      msg=paste("Batch running on", nn,"/",length(files),"files...")
      save(msg,file = filename)
      nn=nn+1
      
    }
    
    return(results)
  
  }else{
    
    results=calc.ratio(df=folder,x=x,y=y,marker=marker,conc.lim=conc.lim,step.win=step.win,step.mov=step.mov,min.win=min.win,max.win=max.win,
               cor.lim=cor.lim,max.shift=max.shift,fltr=fltr,long=long,lat=lat,peak=peak,smmry=smmry)
    return(results)
    
  }
}