 
    rm(list=ls())
    set.seed(87611)
    library(nlme)
    library(VCA)

    p1 <- function(x) {formatC(x, format="f", digits=1)}

    # design
    intercept =100 
    top = 4
    range1     = c(2, 10) 
    range2     = c(5, 10)
    replicates = c(3, 10) 
    
    x1 <- range1[1]
    x2 <- range1[2]
    x3 <- range2[1]
    x4 <- range2[2]
    x5 <- replicates[1]
    x6 <- replicates[2]
    
    # SDs
    a =20 
    b =2 
    c =2 
    d =2
    

    # seems that I need to use both c(x1,x2) c(x1:x2) so sample function works correctly
    
    if (x1==x2) {
      
      middle <-  sample(c(x1,x2),   top, replace=TRUE)    # ditto groups in each top level 6
      
    } else {
      
      middle <-  sample(c(x1:x2),   top, replace=TRUE)    # ditto groups in each top level 6
    }
    
    
    if (x3==x4) {
      
      lower <-   sample(c(x3,x4),   sum(middle), replace=TRUE )
      
    } else {
      
      lower <-   sample(c(x3:x4),   sum(middle), replace=TRUE )
      
    }
    
    if (x5==x6) {
      
      replicates <-  sample(c(x5,x6),   sum(lower), replace=TRUE )
      
    } else {
      
      replicates <-  sample(c(x5:x6),   sum(lower), replace=TRUE ) 
      
    }
    
    
    N <- sum(replicates)
    
    residual <- d
    d <- intercept
   
    
    # random effects
    top.r <-    rnorm(top,          d,                a)    
    middle.r <- rnorm(sum(middle),  0,                b)     
    lower.r <-  rnorm(sum(lower),   0,                c)     
    
    # ids
    lower.id <- rep(seq_len(sum(lower)), replicates )        
    middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
    top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)
    
   # --------------------------------------------------------------------------

    Data <- data.frame( top=top.id, mid=middle.id, low=lower.id,
                        y= rnorm( sum(replicates), top.r[top.id] + 
                                    middle.r[middle.id] + lower.r[lower.id], residual ) )
    
    df <- as.data.frame(Data)
    
    # --------------------------------------------------------------------------
    # Fit the specified regression model
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
        fit.res <-  
        tryCatch(intervals(lme(y ~ 1, random = ~1 |  top/mid/low , data=df, method="REML")), 
                 error=function(e) e)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ###http://stackoverflow.com/questions/8093914
      ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
      
      if (!inherits(fit.res, "error")) {
        
        modelint <- fit.res
        
        emu      <-p1(modelint[['fixed']][2][[1]])  
        etop     <-p1(modelint[['reStruct']][['top']][2][[1]])
        eday     <-p1(modelint[['reStruct']][['mid']][2][[1]])
        erun     <-p1(modelint[['reStruct']][['low']][2][[1]])
        esigma   <-p1(modelint[['sigma']][2][[1]])
        
      } else  {
        
        fit.res <- NULL
        
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      o <- fit.res<- tryCatch(fitVCA(y~top/mid/low, df, "reml"), 
                              error=function(e) e) 
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      if (!inherits(fit.res, "error")) {
        
        fit.res <- VCAinference(fit.res, ci.method = "sas")
        
        x <- as.matrix(o)
        features <- attributes(x)
        
        emu      <- p1(features$Mean) 
        
        o <- as.matrix(o)
        etop     <-p1(o["top","SD"])
        eday     <-p1(o["top:mid","SD"])
        erun     <-p1(o["top:mid:low","SD"])
        esigma   <-p1(o["error","SD"])
        
      } else  {
        
        fit.res <- NULL
        
      }
      
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get the model summary
    if (is.null(fit.res)) {
      
      fit.summary <- NULL
      
    } else {
      
      fit.summary <-  (fit.res)
    }
    
   #---------------------------------------------------------------------------
   # Plot the data  
 
    d1 <- df
    
 
      #base plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # middle groups for plot
      xx<- cumsum(as.vector(table(middle.id)))
      xx <-d1$low[xx]
      
      # top groups for plot
      yy<- cumsum(as.vector(table(top.id)))
      yy <-d1$low[yy]
      
      plot( y ~ factor(low), data=d1 , 
            main=paste("Variability Chart. Truth (estimate): intercept ",intercept,"(",emu,"), top level sd=",
                       a,"(",etop,")", ",\n middle level sd=",
                       b ,"(", eday,"), lowest level sd=",
                       c, "(", erun,") & random error sd=", 
                       residual,"(", esigma,")"),
          #  main="lowest level grouped", 
          xlab="lowest level groups")
      
      abline( v=c(0,xx)+0.5, lty=2, col='green' )
      abline( v=c(0,yy)+0.5, lty=2, col='blue' )
      
      
  
      
      #VCA plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    
      varPlot(y~top/mid/low, d1, 
              BG=list(var="top", 
                      col=c("#f7fcfd","#e5f5f9","#ccece6","#99d8c9",
                            "#66c2a4","#41ae76","#238b45","#006d2c","#00441b"), 
                      col.table=TRUE), 
              VLine=list(var=c("top", "mid"), 
                         col=c("black", "mediumseagreen"), lwd=c(2,1), 
                         col.table=c(TRUE,TRUE)), 
              JoinLevels=list(var="low", col=c("lightblue", "cyan", "yellow"), 
                              lwd=c(2,2,2)), 
              MeanLine=list(var="top", col="blue", lwd=2),
              Title=list(main=paste("Variability Chart. Truth (estimate): intercept ",
                                    intercept,"(",emu,"), top level sd=",
                                    a,"(", etop,")", ",\n middle level sd=",
                                    b ,"(", eday,"), lowest level sd=",
                                     c, "(", erun,") & random error sd=", 
                                    residual,"(", esigma,")")),
              
              # MeanLine=list(var="mid", col="pink", lwd=2),
              Points=list(pch=list(var="mid", pch=c(21, 22, 24)), 
                          bg =list(var="mid", bg=c("lightblue", "cyan", "yellow")), 
                          cex=1.25))    
      
      
  
  
  
  #---------------------------------------------------------------------------
  
    
    fit.summary
    df
    
    
 