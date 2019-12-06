#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a Shiny web application
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
# fda budesonide bioequivalence guidance and more
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    library(shiny)
    library(nlme)
    library(knitr)
    library(VCA)
    library(shinythemes)        # more funky looking apps
    options(max.print=1000000)  # allows printing of long listings
    fig.width <- 1200
    fig.height <- 450
    p1 <- function(x) {formatC(x, format="f", digits=1)}
    p4 <- function(x) {formatC(x, format="f", digits=4)}
    options(width=100)

#-----------------------------------------------
# This is the FDA data copied from the guidance

    lines <- c("1 31 B REF 5.957211 1 31 M REF 5.961802 1 31 E REF 5.967178 1 32 B REF 6.010251 1 32 M REF 6.004711 1 32 E REF 6.004797 1 33 B REF 5.884161 1 33 M REF 5.894085 1 33 E REF 5.895977 1 34 B REF 5.624705 1 34 M REF 5.632991 1 34 E REF 5.614428 1 35 B REF 5.957329 1 35 M REF 5.966059 1 35 E REF 5.968143 1 36 B REF 5.074298 1 36 M REF 5.063063 1 36 E REF 5.058519 1 37 B REF 5.418587 1 37 M REF 5.420591 1 37 E REF 5.418178 1 38 B REF 6.325178 1 38 M REF 6.321954 1 38 E REF 6.303148 1 39 B REF 5.656286 1 39 M REF 5.68025 1 39 E REF 5.675036 1 40 B REF 5.792299 1 40 M REF 5.775161 1 40 E REF 5.793083 2 41 B REF 5.601033 2 41 M REF 5.611223 2 41 E REF 5.601142 2 42 B REF 5.61553 2 42 M REF 5.587412 2 42 E REF 5.591004 2 43 B REF 5.682466 2 43 M REF 5.676472 2 43 E REF 5.671434 2 44 B REF 5.844336 2 44 M REF 5.855172 2 44 E REF 5.862329 2 45 B REF 5.898151 2 45 M REF 5.883657 2 45 E REF 5.878956 2 46 B REF 6.100662 2 46 M REF 6.105463 2 46 E REF 6.108098 2 47 B REF 6.294753 2 47 M REF 6.28534 2 47 E REF 6.302333 2 48 B REF 5.638072 2 48 M REF 5.627372 2 48 E REF 5.623516 2 49 B REF 5.113562 2 49 M REF 5.122454 2 49 E REF 5.109271 2 50 B REF 5.932752 2 50 M REF 5.913438 2 50 E REF 5.912427 3 51 B REF 5.961947 3 51 M REF 5.955332 3 51 E REF 5.943721 3 52 B REF 6.2334 3 52 M REF 6.250689 3 52 E REF 6.219668 3 53 B REF 6.041431 3 53 M REF 6.038234 3 53 E REF 6.080464 3 54 B REF 6.049713 3 54 M REF 6.039759 3 54 E REF 6.054218 3 55 B REF 6.834563 3 55 M REF 6.85264 3 55 E REF 6.857395 3 56 B REF 4.864966 3 56 M REF 4.907521 3 56 E REF 4.891049 3 57 B REF 5.895176 3 57 M REF 5.885851 3 57 E REF 5.874895 3 58 B REF 6.45826 3 58 M REF 6.443113 3 58 E REF 6.435882 3 59 B REF 6.090533 3 59 M REF 6.102835 3 59 E REF 6.077606  3 60 B REF 5.886724 3 60 M REF 5.920949 3 60 E REF 5.915749 4 1 B TEST 6.894594 4 1 M TEST 6.913011 4 1 E TEST 6.895764 4 2 B TEST 5.832334 4 2 M TEST 5.846562 4 2 E TEST 5.832269 4 3 B TEST 6.235755 4 3 M TEST 6.26231 4 3 E TEST 6.245095 4 4 B TEST 5.646185 4 4 M TEST 5.635887 4 4 E TEST 5.63034 4 5 B TEST 5.960711 4 5 M TEST 5.962902 4 5 E TEST 5.961959 4 6 B TEST 5.500354 4 6 M TEST 5.508444 4 6 E TEST 5.513115 4 7 B TEST 6.663099 4 7 M TEST 6.64733 4 7 E TEST 6.651215 4 8 B TEST 5.724774 4 8 M TEST 5.72086 4 8 E TEST 5.71411 4 9 B TEST 6.183375 4 9 M TEST 6.186433 4 9 E TEST 6.182109 4 10 B TEST 5.64053 4 10 M TEST 5.648589 4 10 E TEST 5.626395 5 11 B TEST 6.69764 5 11 M TEST 6.71128 5 11 E TEST 6.699829 5 12 B TEST 6.555609 5 12 M TEST 6.549935 5 12 E TEST 6.551611 5 13 B TEST 5.009683 5 13 M TEST 5.013969 5 13 E TEST 5.010928 5 14 B TEST 5.440976 5 14 M TEST 5.42057 5 14 E TEST 5.447687 5 15 B TEST 6.477609 5 15 M TEST 6.456082  5 15 E TEST 6.448981 5 16 B TEST 6.442601 5 16 M TEST 6.426217 5 16 E TEST 6.436262 5 17 B TEST 5.640496 5 17 M TEST 5.63846 5 17 E TEST 5.640755 5 18 B TEST 6.597718 5 18 M TEST 6.599232 5 18 E TEST 6.609437 5 19 B TEST 6.007241 5 19 M TEST 5.990695 5 19 E TEST 5.984292 5 20 B TEST 6.781806 5 20 M TEST 6.774386 5 20 E TEST 6.784001 6 21 B TEST 5.993852 6 21 M TEST 5.994287 6 21 E TEST 5.993541 6 22 B TEST 6.012322 6 22 M TEST 6.006182 6 22 E TEST 6.017961 6 23 B TEST 5.965969 6 23 M TEST 5.97125 6 23 E TEST 5.967839 6 24 B TEST 5.592609 6 24 M TEST 5.581154 6 24 E TEST 5.588877 6 25 B TEST 6.002182 6 25 M TEST 6.011583 6 25 E TEST 6.018746 6 26 B TEST 5.267014 6 26 M TEST 5.272291 6 26 E TEST 5.265213 6 27 B TEST 5.766104 6 27 M TEST 5.786727 6 27 E TEST 5.773194 6 28 B TEST 6.054975 6 28 M TEST 6.05232 6 28 E TEST 6.061088 6 29 B TEST 5.838689 6 29 M TEST 5.837566 6 29 E TEST 5.842508 6 30 B TEST 5.784255 6 30 M TEST 5.789891 6 30 E TEST 5.788662")
    con <- textConnection(lines)
    cnames <- c("BATCH","SECTOR","REP","PRODUCT","y")
    fda.d <- read.table(con, col.names=cnames)
    close(con)
    
    fda2 <- fda.d
    fda2$exp.y <- exp(fda2$y)  # the bioequiv function logs the data, but the fda data in guidance is already logged


#------------------------------------------------
# function inputs:
# here is my function for performing the analysis
# data
# the number of canisters in a batch x number of batches (separately for test and ref)
# number of reps in each canister (separately for test and ref))
# the response variable
# the independent variable
# the variable that identifies the test and refernce data


bioequiv<- function(foo1=d , nrXlr=10*3, mr=2, ntXlt=10*3, mt=3,
                    response="ISM",indep="CONTAIN", split="PRODUCT", ref="REF", test="TEST") {
    
    mydata <- foo1
    
    # fda constants and alpha level
    thetap = ((log(1.11))^2 +0.01)/(0.1)^2 #2.0891, reg constant
    sigma_TO = 0.1
    alpha=0.05
    
    # denominator
    denom <-  nrXlr*mr
    denom2 <- ntXlt*mt
    
    # create a dataset, log the response
    mydata <- as.data.frame(mydata)
    mydata$Container <- as.factor(mydata[,indep])
    mydata$y <- mydata[,response]
    mydata$Product <- mydata[,split]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if( min(mydata$y) > 0 ) { # this checks if all the data are positive, lab test values should not be negative.
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        mydata$y <- log(mydata$y)
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~STUDY DESIGN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Analysis on the test data
        cat("\n~~~~~~~~~~~~~~~~~~~~Test Product ANOVA~~~~~~~~~~~~~~~~~~~~~~~~\n")
        x <- as.data.frame(mydata)
        foo <- x[x$Product %in% test,]
        print(test <- VCA::anovaVCA(y~Container, foo))
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~test anova~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        x.barT <- mean(foo$y)  
        TANOVA <- anova(lm(y~Container, foo)) 
        TMSB <- TANOVA["Container","Mean Sq"] 
        TMSW <- TANOVA["Residuals","Mean Sq"] 
        
        if (test$NegVCmsg %in% "* VC set to 0") {
            
            SIGMAT <- sqrt(TMSW) # Should use this if between is zero
            
            test.between = 0
            test.note <- "test: the between variance component was set to zero"
        } else {
            
            SIGMAT <- ( TMSB/mt + ((mt-1)*TMSW)/mt )^.5
            
            test.between = 1
            test.note <- "test: the between variance component was measurable"
        }
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #Analysis on the reference data
        cat("\n~~~~~~~~~~~~~~~~~~Reference Product ANOVA~~~~~~~~~~~~~~~~~~~~~\n")
        x <- as.data.frame(mydata)
        foo <- x[x$Product %in% ref,]
        print(ref <- VCA::anovaVCA(y~Container, foo))
        x.barR <- mean(foo$y)
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~ref anova~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        RANOVA <- anova(lm(y~Container, foo)) 
        RMSB <- RANOVA["Container","Mean Sq"] 
        RMSW <- RANOVA["Residuals","Mean Sq"] 
        
        if (ref$NegVCmsg %in% "* VC set to 0") {
            
            SIGMAR <- sqrt(RMSW) # Should use this if between is zero
            ref.note <- "reference: the between variance component was set to zero"
            ref.between = 0
            
        } else {
            
            SIGMAR <- ( RMSB/mr + ((mr-1)*RMSW)/mr )^.5 # 
            ref.between = 1
            ref.note <- "reference: the between variance component was measurable"
            
        }
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Perform analysis
        D <- TMSW/denom2 # within test
        A <- TMSB/denom2 # between test
        E <- RMSW/denom  # within ref
        B <- RMSB/denom  # between ref
        
        rmsb  <- RMSB
        rmsbw <- RMSW
        msb   <- TMSB
        msbw  <- TMSW
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # perform the calculations
        E1b <- (rmsb - 0)/mr # between-container variability for ref product
        ED <- (x.barR - x.barT)^2 # mean diff squared
        
        if (test.between %in% 0) {
            
            E1 <- 0 
            E2 <- msbw 
            E3s <- 0 
            E4s <- -(1+thetap)* rmsbw 
            
        } else if (test.between %in% 1) {
            
            E1 <- (msb - 0)/mt 
            E2 <- (mt-1)*msbw/mt
            E3s <- -(1+thetap)* E1b
            E4s <- -(1+thetap)* (mr-1)*rmsbw/mr
            
        }
        
        EQ <- ED + E1 + E2 + E3s + E4s
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Here is where we adjust the analysis based on negative variance components.
        DFTN0 <- (ntXlt-1)
        DFRN0 <- (nrXlr-1)
        DFT0 <-  (ntXlt*(mt-1))
        DFR0 <-  (nrXlr*(mr-1))
        
        if (test.between %in% 1 & ref.between %in% 1) {
            
            get.df <- DFTN0 + DFRN0 ; se <- sqrt( A + B ) 
            
        } else if (test.between %in% 0 & ref.between %in% 0) {
            
            get.df <- DFT0 + DFR0 ; se <- sqrt( D + E ) 
            
        } else if (test.between %in% 0 & ref.between %in% 1) {
            
            get.df <- DFT0 + DFRN0 ; se <- sqrt( D + B ) 
            
        } else if (test.between %in% 1 & ref.between %in% 0) {
            
            get.df <- DFR0 + DFTN0 ; se <- sqrt( E + A ) 
            
        }
        
        HD <- (abs(ED^0.5) + qt(1-alpha, df=get.df) * se )^2
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        H1 <-  (DFTN0*E1) /  qchisq(alpha,   DFTN0)
        H2 <-  (DFT0*E2)  /  qchisq(alpha,   DFT0)
        H3s <- (DFRN0*E3s)/  qchisq(1-alpha, DFRN0)
        H4s <- (DFR0*E4s) /  qchisq(1-alpha, DFR0)
        
        UD = (HD-ED)^2
        U1 = (H1-E1)^2
        U2 = (H2-E2)^2
        U3s= (H3s-E3s)^2
        U4s= (H4s-E4s)^2
        
        Hmu1 <- EQ + (UD + U1 + U2 + U3s + U4s)^0.5
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # constant scale calcs, point estimates
        
        if (ref.between %in% 0) {
            
            E3c <- 0  
            E4c <- -rmsbw  
            
        } else if (ref.between %in% 1) {
            
            E3c <- - E1b    
            E4c <- - (mr-1)*rmsbw/mr
            
        }
        
        EQc <- ED + E1 + E2 + E3c + E4c - thetap* sigma_TO^2
        
        DFTN0 <- (ntXlt-1)
        DFRN0 <- (nrXlr-1)
        DFT0 <-  (ntXlt*(mt-1))
        DFR0 <-  (nrXlr*(mr-1))
        
        # Constant scaled calculations, confidence bounds
        H3c <- (DFRN0*E3c)/  qchisq(0.95, DFRN0)
        H4c <- (DFR0*E4c) /  qchisq(0.95, DFR0)
        U3c <- (H3c-E3c)^2 
        U4c <- (H4c-E4c)^2 
        
        # Constant scaled 95% upper confidence bound (Hmu2)
        Hmu2 <-EQc + (UD + U1 + U2 + U3c + U4c)^0.5
        
        # Conclusion
        # print all key results for cross checking
        rel <- c(response, indep, nrXlr , mr , ntXlt , mt ,
                 x.barR, x.barT, (x.barT-x.barR)^2,
                 SIGMAR, SIGMAT, SIGMAT/SIGMAR,
                 exp(x.barR), exp(x.barT), exp(x.barT)/exp(x.barR), 
                 HD, EQ, Hmu1)
        
        rel <- (as.data.frame(rel))
        con <- c(rep("-",16),EQc, Hmu2)
        con <- (as.data.frame(con))
        res <- cbind(rel, con)
        
        rownames(res) <- c("Response variable (logged for analysis)", 
                           "ANOVA independent variable",
                           "containers x no of batches, reference",
                           "reps in reference containers", 
                           "containers x no of batches, test",
                           "reps in test containers",
                           "Mean of the logged reference", 
                           "Mean of the logged test", 
                           "ED: (Mean log test-Mean log reference)^2",
                           "Sigma total log reference (SigmaR)",
                           "Sigma total log test",
                           "Sigma total log test/log reference",
                           "Geometric mean reference", "Geometric mean test", 
                           "Geometric mean ratio test/reference",
                           "HD", 
                           "Linearized point estimate" ,
                           "95% upper confidence bound for linearized criteria")
        
        if (SIGMAR > sigma_TO) {
            colnames(res) <- c("reference scale, use this for inference","constant scale")
        } else {
            colnames(res) <- c("reference scale","constant scale, use this for inference")
        }
        
        cat("~~~~~~~~~~~~~~~~Print all the key statistics~~~~~~~~~~~~~~\n")
        print(kable(res))
        
        cat("\n")
        print(test.note); print(ref.note)
        cat("\n")
        
        accept="FAIL"
        
        cat("~~~~~~~~~~~~~~~~~~~~~~Conclusion~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
        cat("\n")
        if (SIGMAR > sigma_TO) {
            
            if (Hmu1 < 0) {accept="PASS"} else {accept="FAIL"}
            result1 <- as.data.frame(cbind(SIGMAR, sigma_TO, EQ, Hmu1, accept))
            names(result1) <- c("Total SD Reference", " FDA constant", " linearized ref. scale point estimate", " upper 95% conf bound", " upper 95% conf bound <0?")
            cat("~~~~~~~~~~~~~~~~Referenced Scaled Approach as SigmaR > 0.1~~~~~~~~~~~~~~\n")
            print(kable(result1, digits=12))
            cat("\n")
            
        } else {
            
            if (Hmu2 < 0) {accept="PASS"} else {accept="FAIL"} 
            result2 <- as.data.frame(cbind(SIGMAR, sigma_TO, EQc, Hmu2, accept))
            names(result2) <- c("Total SD Reference", " FDA constant", " linearized const. scale point estimate", " upper 95% conf bound", " upper 95% conf bound <0?")
            cat("~~~~~~~~~~~~~~~~Constant Scaled Approach as SigmaR <= 0.1~~~~~~~~~~~~~~\n")
            print(kable(result2, digits=12))
            cat("\n")
            
        } } else {
            warn1 <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Negative values in response data not allowed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            warn2 <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            print(warn2);print(warn2);print(warn1);print(warn2)
        }
    
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~
####### Function for 1 rep
#~~~~~~~~~~~~~~~~~~~~~~~~~~

bioequiv.1.rep<- function(foo1=d , nrXlr=10*3, mr=1, ntXlt=10*3, mt=1,
                          response="ISM",indep="CONTAIN", split="PRODUCT", ref="REF", test="TEST"
                          
) {
    
    mydata<- foo1
    
    # fda constants and alpha level
    thetap = ((log(1.11))^2 +0.01)/(0.1)^2 # 2.0891, reg constant
    sigma_TO = 0.1
    alpha=0.05
    
    # denominator
    denom <-  nrXlr*mr
    denom2 <- ntXlt*mt
    
    # create a dataset, log the response
    mydata <- as.data.frame(mydata)
    mydata$Container <- as.factor(mydata[,indep])
    mydata$y <- mydata[,response]
    mydata$Product <- mydata[,split]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    if( min(mydata$y) > 0 ) { # this checks if all the data are positive, lab values should not be negative.
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        mydata$y <- log(mydata$y)
        
        # create a slim dataset
        foo <- mydata[, c("Product", "Container", "y", "SECTOR")]
        mydata<- foo
        
        # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~STUDY DESIGN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Analysis on the test data
        cat("\n~~~~~~~~~~~~~~~~~~~~Test Product ANOVA~~~~~~~~~~~~~~~~~~~~~~~~\n")
        x <- as.data.frame(mydata)
        foo <- x[x$Product %in% test,]
        print(test <- VCA::anovaVCA(y~1, foo))
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~test anova~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        x.barT <- mean(foo$y)  
        TANOVA <- anova(lm(y~1, foo)) 
        TMSW <- TANOVA["Residuals","Mean Sq"] 
        SIGMAT <- sqrt(TMSW)  
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #Analysis on the reference data
        cat("\n~~~~~~~~~~~~~~~~~~Reference Product ANOVA~~~~~~~~~~~~~~~~~~~~~\n")
        x <- as.data.frame(mydata)
        foo <- x[x$Product %in% ref,]
        print(ref <- VCA::anovaVCA(y~1, foo))
        x.barR <- mean(foo$y)
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~ref anova~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        RANOVA <- anova(lm(y~1, foo)) 
        RMSW <- RANOVA["Residuals","Mean Sq"] 
        SIGMAR <- sqrt(RMSW) # Should use this if between is zero
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Perform analysis
        D <- TMSW/denom2 # within test
        A <- D#TMSB/denom2 # between test
        E <- RMSW/denom  # within ref
        B <- E#RMSB/denom  # between ref
        
        rmsb <-RMSW
        rmsbw <- RMSW
        msb <- TMSW
        msbw <- TMSW
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
        # perform the calculations
        E1b <- (rmsb - 0)/mr # between-container variability for ref product
        ED <- (x.barR - x.barT)^2 # mean diff squared
        E1 <- (msb - 0)/mt 
        E2 <- 0
        E3s <- -(1+thetap)* E1b  
        E4s <- 0
        EQ <- ED + E1 + E2 + E3s + E4s
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Here is where we adjust the analysis based on negative variance components.
        DFTN0 <- (ntXlt-1)
        DFRN0 <- (nrXlr-1)
        DFT0 <-  (ntXlt*(mt-1))
        DFR0 <-  (nrXlr*(mr-1))
        
        get.df <- DFTN0 + DFRN0 ; se <- sqrt( A + B ) 
        
        HD <- (abs(ED^0.5) + qt(1-alpha, df=get.df) * se )^2
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        H1 <-  (DFTN0*E1) /  qchisq(alpha,   DFTN0)
        H2 <-  0 # (DFT0*E2)  /  qchisq(alpha,   DFT0)
        H3s <- (DFRN0*E3s)/  qchisq(1-alpha, DFRN0)
        H4s <- 0 #(DFR0*E4s) /  qchisq(1-alpha, DFR0)
        
        UD = (HD-ED)^2
        U1 = (H1-E1)^2
        U2 = 0 #(H2-E2)^2
        U3s= (H3s-E3s)^2
        U4s= 0 #(H4s-E4s)^2
        
        Hmu1 <- EQ + (UD + U1 + U2 + U3s + U4s)^0.5
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # constant scale calcs, point estimates
        E3c <- - E1b    
        E4c <- 0 #- (mr-1)*rmsbw/mr
        
        EQc <- ED + E1 + E2 + E3c + E4c - thetap* sigma_TO^2
        
        # Constant scaled calculations, confidence bounds
        H3c <- (DFRN0*E3c)/  qchisq(0.95, DFRN0)
        H4c <- 0 #(DFR0*E4c) /  qchisq(0.95, DFR0)
        U3c <- (H3c-E3c)^2 
        U4c <- 0 #(H4c-E4c)^2 
        
        # Constant scaled 95% upper confidence bound (Hmu2)
        Hmu2 <-EQc + (UD + U1 + U2 + U3c + U4c)^0.5
        
        # Conclusion
        # print all key results for cross checking
        rel <- c(response, indep, nrXlr , mr , ntXlt , mt ,
                 x.barR, x.barT, (x.barT-x.barR)^2,
                 SIGMAR, SIGMAT, SIGMAT/SIGMAR,
                 exp(x.barR), exp(x.barT), exp(x.barT)/exp(x.barR), 
                 HD, EQ, Hmu1)
        
        rel <- (as.data.frame(rel))
        con <- c(rep("-",16),EQc, Hmu2)
        con <- (as.data.frame(con))
        res <- cbind(rel, con)
        
        rownames(res) <- c("Response variable (logged for analysis)", 
                           "ANOVA independent variable",
                           "containers x no of batches, reference",
                           "reps in reference containers", 
                           "containers x no of batches, test",
                           "reps in test containers",
                           "Mean of the logged reference", 
                           "Mean of the logged test", 
                           "ED: (Mean log test-Mean log reference)^2",
                           "Sigma total log reference (SigmaR)",
                           "Sigma total log test",
                           "Sigma total log test/log reference",
                           "Geometric mean reference", "Geometric mean test", 
                           "Geometric mean ratio test/reference",
                           "HD", 
                           "Linearized point estimate" ,
                           "95% upper confidence bound for linearized criteria")
        
        if (SIGMAR > sigma_TO) {
            colnames(res) <- c("reference scale, use this for inference","constant scale")
        } else {
            colnames(res) <- c("reference scale","constant scale, use this for inference")
        }
        
        cat("~~~~~~~~~~~~~~~~Print all the key statistics~~~~~~~~~~~~~~\n")
        print(kable(res))
        
        cat("\n")
        
        accept="FAIL"
        
        cat("~~~~~~~~~~~~~~~~~~~~~~Conclusion~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
        cat("\n")
        if (SIGMAR > sigma_TO) {
            
            if (Hmu1 < 0) {accept="PASS"} else {accept="FAIL"}
            result1 <- as.data.frame(cbind(SIGMAR, sigma_TO, EQ, Hmu1, accept))
            names(result1) <- c("Total SD Reference", " FDA constant", " linearized ref. scale point estimate", " upper 95% conf bound", " upper 95% conf bound <0?")
            cat("~~~~~~~~~~~~~~~~Referenced Scaled Approach as SigmaR > 0.1~~~~~~~~~~~~~~\n")
            print(kable(result1, digits=12))
            cat("\n")
            
        } else {
            
            if (Hmu2 < 0) {accept="PASS"} else {accept="FAIL"} 
            result2 <- as.data.frame(cbind(SIGMAR, sigma_TO, EQc, Hmu2, accept))
            names(result2) <- c("Total SD Reference", " FDA constant", " linearized const. scale point estimate", " upper 95% conf bound", " upper 95% conf bound <0?")
            cat("~~~~~~~~~~~~~~~~Constant Scaled Approach as SigmaR <= 0.1~~~~~~~~~~~~~~\n")
            print(kable(result2, digits=12))
            cat("\n")
            
        } } else {
            warn1 <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Negative values in response data not allowed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") 
            warn2 <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            print(warn2);print(warn2);print(warn1);print(warn2)
        }
    
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# end rep1 function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- fluidPage(theme = shinytheme("journal"),
                
                shinyUI(pageWithSidebar(
                    
                    headerPanel("FDA Population Bioequivalence (PBE) Statistical Analysis Procedure Used in Bioequivalence Determination of Budesonide Suspension Inhalation Product"),
                    
                    sidebarPanel(
                        strong("Introduction"),
                        div(p("Using R we explore the 'FDA Population Bioequivalence (PBE) Statistical Analysis Procedure Used in Bioequivalence Determination of Budesonide Suspension Inhalation Product' [1]")),  
                        div(p("We analyse the FDA guidance example, replicating the guidance results. We also simulate data and perform the PBE analysis. There is also an option to upload your own data for PBE analysis. A choice of plots is given, a base R plot or a plot using the VCA package. A choice of modelling is given for the tabs 1a and 2a, using the nlme or VCA package. The difference in the two is in the variance components confidence interval calculations. It is advisable to understand the subject matter and understand the design and execution of the experiment before attempting to analyse.")),
                        
                        div(
                            
                            selectInput("Plot",
                                        strong("Select plot preference "),
                                        choices=c("VCA package plot" , "Base plot")),
                            
                            
                            selectInput("Model",
                                        strong("Select modelling preference "),
                                        choices=c("VCA package" , "nlme package")), 
                            
                            br(),br(),
                            div(strong("Tab 1a Plot the FDA example guidance data")),p("The example FDA dataset is plotted (always plot data), variance components are estimated for non subsetted data, just for interest. Note the replicates are linked, replicates labelled B, M and E have a specific meaning which cannot be ignored."),
 
                          #  div(strong("Tab 1a Plot the FDA example guidance data")),p("The example FDA dataset is plotted (always plot data), variance components are estimated for non subsetted data, just for interest. Note the replicates are linked, replicates labelled B, M and E have a specific meaning and cannot be ignored."),
 
                            div(strong("Tab 1b FDA Population Bioequivalence Statistical Analysis")),p("Reproduces the FDA analysis as presented in the guidance, notice differences in decimal places compared to the guidance [1]."),
                            div(strong("Tab 1c List the FDA guidance example data")),p("A simple listing of the FDA example data."),
                            tags$hr(),
                            actionButton("resample", "Simulate a new sample"),
                            br(),br(),
                            div(strong("Tab 2a Simulate data, plot and variance components analysis")),p("Does just that, plot a balanced design based on user inputs, use the slider inputs on this tab"),
                            div(strong("Tab 2b Population Bioequivalence Statistical Analysis - simulated data")),p("Performs the analysis approach based on guidance, but will adjust analysis if negative variance components are encountered. Presents a one way ANOVA on each product, performs PBS analysis and then concludes in a PASS or FAIL"),
                            div(strong("Tab 2c List the simulated data")),p("A simple listing of the simulated data."),
                            tags$hr(),
                            div(strong("Tab 3 Upload your own data for Population Bioequivalence Statistical Analysis")),p("User data can be loaded. PBE analysis takes place, the data plotted and listed. It does not have to be balanced, though it requires a balanced number of replicates within each product. The program will work out the design. Use at your own risk."),
                            tags$hr(),
                            div(strong("Tab 4 Explanation")),p("An explanation of the adjustments to the guidance that are performed if negative variance components are encountered. See the FDA guidance for an explanation of the general approach [1]."),
                            
                            br(),
                            actionButton(inputId='ab1', label="R code here", 
                                         icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Three-level-nested-variance-components-analysis2/master/fda_v3_1rep/app.R', '_blank')"),
                            
                            br(),
                            br(),
                            div(p("References:")),  
                            
                            tags$a(href = "https://www.accessdata.fda.gov/drugsatfda_docs/psg/Budesonide_Inhalation_Sus_20929_RC_09-12.pdf", "[1] FDA Draft Guidance on Budesonide"),
                            div(p(" ")),
                            
                            tags$a(href = "https://www.fda.gov/media/70878/download", "[2] Statistical Information from the June 1999 Draft Guidance and Statistical Information for In Vitro Bioequivalence Data Posted on August 18, 1999"),
                            div(p(" "))
                            
                        ))
                    ,  
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(
                        
                        # https://shiny.rstudio.com/articles/upload.html
                        # https://stackoverflow.com/questions/44222796/shiny-with-multiple-tabs-and-different-sidebar-in-each-tab
                        
                        navbarPage(       
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                            tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: lightgrey;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")), 
                            ##~~~~~~~~~~~~~~~~~~~~~end of section to add colour
                            
                            
                            #~~~~~~~~~~~~~~~~~~
                            tabPanel("1a Plot the FDA example guidance data", 
                                     
                                     div(plotOutput("reg.plot2", width=fig.width, height=fig.height)),  
                                     
                                     p(strong("Arithmetic mean presented above plot when VCA is used otherwise modelled mean
                                      (arithmetic mean and modelled mean will match with a balanced design)")) ,
                                     
                                     div( verbatimTextOutput("reg.summaryf"))
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~          
                            tabPanel("1b FDA Population Bioequivalence Statistical Analysis", 
                                     
                                     div( verbatimTextOutput("bioequivfda1")) #
                                     
                            ) ,
                            #~~~~~~~~~~~~~
                            tabPanel("1c List the FDA guidance example data", 
                                     
                                     div( verbatimTextOutput("bioequivfda2")), #
                                     
                                     p(strong("copy the above data to do your own analysis................."))
                                     
                            ),
                            #~~~~~~~~~~~~          
                            tabPanel("2a Simulate data, plot and variance components analysis", 
                                     
                                     
                                     div(strong("Select true population parameters for simulated data using the sliders below"),
                                         
                                         p("A 3 level nested data set is simulated. A plot of the raw data is generated. 
                    A model is fit to estimate the variance components. or the base plot, the blue dashed lines demark the top level groups. The green dashed lines demark the mid level groups.
                    The boxplots within the green lines demark the lowest level groups. Each boxplot presents the distribution of replicates. 
                    The middle, lowest and replicate numbers are varied randomly based on the slider ranges. The variance components are between blue 'top' groups,
                    between green 'mid' groups (within blue groups), within green 'mid' groups, within 'low' groups (replicates), a.k.a. repeatability. 
                    So we actually estimate 4 components counting the residual error. Product '1' is taken as the reference product.
                    Create a balanced design by reducing all the sliders to one value. 
                    The FDA guidance fits a one way ANOVA to each product's data with the independent variable the lowest level factor. 
                    The mid level 'batch' is not included in the analysis, however 'batch' is taken into account in the evaluation, the degrees of freedom are used in the analysis. 
                    The guidance does not discuss how to proceed in the case in which one or both between variance component(s) is estimated as negative, 
                    that's a bit Pepega, we show what to do in those scenarios, see notes tab for more information. You also have the choice of simulating a new sample. ")),
                                     ## new here -----------------------------------------------------------------------------------
                                     # https://stackoverflow.com/questions/47355043/multiple-column-layout-inside-a-tabpanel
                                     sidebarLayout(
                                         # Sidebar panel for inputs ----
                                         sidebarPanel( 
                                             
                                             list(
                                                 fluidRow(
                                                     
                                                     column(4,
                                                            sliderInput("top",
                                                                        "Number of levels of top (product) component (demarked by blue or thick lines). Must be 2 for PBE analysis.",
                                                                        min=2, max=100, step=1, value=2, ticks=FALSE)),
                                                     column(4,
                                                            sliderInput("range1",
                                                                        "Middle (batch) level: select no. of 'mid' groups within each top level group:", 
                                                                        min = 2, max = 10, step=1, value = c(3,3), ticks=FALSE)),
                                                     column(4,
                                                            sliderInput("range2", 
                                                                        "Lower (sector) level: select\n no. of 'low' groups within each mid level group:",
                                                                        min = 2, max = 10, step=1, value = c(10,10),ticks=FALSE))
                                                 ),
                                                 
                                                 fluidRow(
                                                     
                                                     
                                                     column(4,
                                                            sliderInput("replicates", 
                                                                        "Select number of replicates nested within each boxplot",
                                                                        min = 2, max = 50, step=1, value = c(3,3), ticks=FALSE)),
                                                     column(4,
                                                            sliderInput("intercept",
                                                                        "True intercept",
                                                                        min=0, max=1000, step=.5, value=700, ticks=FALSE)),
                                                     column(4,
                                                            sliderInput("a",
                                                                        "True top level SD",
                                                                        min=0, max=100, step=.5, value=75, ticks=FALSE))
                                                 ),
                                                 
                                                 fluidRow(
                                                     column(4,
                                                            sliderInput("b",
                                                                        "True middle level SD",
                                                                        min=0, max=100, step=.5, value=1, ticks=FALSE)),
                                                     column(4,
                                                            sliderInput("c",
                                                                        "True lower level SD",
                                                                        min=0, max=100, step=.5, value=1, ticks=FALSE)),
                                                     column(4,
                                                            sliderInput("d",
                                                                        "True error",
                                                                        min=0, max=100, step=.5, value=75, ticks=FALSE))

                                                 )
                                             )
              
                                             ,   width = 12 ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(
                                             
                                             div(plotOutput("reg.plot", width=fig.width, height=fig.height)),
                                             
                                             p(strong("Arithmetic mean presented above plot when VCA is used otherwise modelled mean
                                            (arithmetic mean and modelled mean will match with a balanced design)")) ,
                                             
                                             div( verbatimTextOutput("reg.summary"))
                                             
                                             , width = 12 ))  # experiment with this
                                     
                            ) ,
                            #~~~~~~~~~~~~
                            
                            #~~~~~~~~~~~~
                            tabPanel("2b Population Bioequivalence Statistical Analysis - simulated data", 
                                     
                                     div( verbatimTextOutput("bioequiv0")),
                                     
                                     p(strong(""))
                                     
                            ) ,
                            #~~~~~~~~~~~~~
                            tabPanel("2c List the simulated data", 
                                     
                                     div( verbatimTextOutput("summary2"))
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                   
                            tabPanel("3 Upload your own data for Population Bioequivalence Statistical Analysis", fluid = TRUE,
                                     
                                     p(("Upload your own data for determination of population bioequivalence. The top two radio button options are to help load,
                                 the bottom two options are to either show the top six rows of the data or show all the data, and secondly to toggle between PBE analysis or a plot of the data. 
                                 Ensure your data is balanced within product. Use at your own risk.")) ,
                                     
                                     p(("Example data sets (download either file and click 'Browse...' to locate and upload for PBE analysis):")) ,
                                     
                                     tags$a(href = "https://raw.githubusercontent.com/eamonn2014/Three-level-nested-variance-components-analysis2/master/fda", "FDA example data set in guidance [1]"),
                                     div(p(" ")),
                                     
                                     tags$a(href = "https://raw.githubusercontent.com/eamonn2014/Three-level-nested-variance-components-analysis2/master/fda%20B%20rep%20only", "The same FDA data set with only 1 rep per canister"),
                                     div(p(" ")),
                          
                                     sidebarLayout(
                                         
                                         # Sidebar panel for inputs ----
                                         sidebarPanel(
                                            
                                             # Input: Select a file ----
                                             fileInput("file1", "Choose CSV File",
                                                       multiple = TRUE,
                                                       accept = c("text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")),
                                             
                                             # Horizontal line ----
                                             tags$hr(),
                                             
                                             # Input: Checkbox if file has header ----
                                             checkboxInput("header", "Header", TRUE),
                                             
                                             # Input: Select separator ----
                                             radioButtons("sep", "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t",
                                                                      Whitespace = ""),
                                                          selected = ""),
                                             
                                             # Input: Select quotes ----
                                             radioButtons("quote", "Quote",
                                                          choices = c(None = "",
                                                                      "Double Quote" = '"',
                                                                      "Single Quote" = "'"),
                                                          selected = ''),
                                             
                                             # Horizontal line ----
                                             tags$hr(),
                                             
                                             # Input: Select number of rows to display ----
                                             radioButtons("disp", "Display",
                                                          choices = c(Head = "head",
                                                                      All = "all"),
                                                          selected = "head"),
                                             
                                             # Horizontal line ----
                                             tags$hr(),
                                             
                                             # Input: Select number of rows to display ----
                                             radioButtons("what", "Output",
                                                          choices = c(Analysis = "Analysis",
                                                                      Plot = "plot"),
                                                          selected = "Analysis")
                                             
                                         ),
                                         
                                         # Main panel for displaying outputs ----
                                         mainPanel(
                                             
                                             # Output: Data file ----
                                             
                                             
                                             div(verbatimTextOutput("contents2")),
                                             plotOutput("plotx"),
                                             tags$hr(),
                                             
                                             tableOutput("contents") 
                                             
                                             
                                         ),
                                     )
                            ) ,
                            
                            tabPanel("4 Explanation", value=3, 
                                     
                                     HTML(" <strong>When the between variance component is estimated to be negative we apply the following adjustments.</strong>"),
                                     
                                     
                                     p("Since three batches is not sufficient to reliably estimate the between batch component, the total
                                        variances are estimated as the between canister variance of the 'super-batch' consisting of the three
                                        batches combined [2]. It is not uncommon that the mean square between (MSB) is less than the mean square within (MSW) with a one way ANOVA. This results in a negative estimate 
                                        for the between variance component. 
                             Thus concluding there is no additional variability due to the between variance component. In such cases the FDA PBE equations are adjusted. 
                             We have",HTML(" <em>m</em>"),"replicates, 
                             ",HTML(" <em>n</em>")," items per batch and",HTML(" <em>l</em>"),"is the no batches per product (test and reference). Refer to the FDA guidance document."),
                                     br(),
                                     
                                     
                                     HTML(" <strong>Impact to the total variances</strong>"),
                                     withMathJax(
                                         helpText('
                           $${{\\sigma_R = }{\\sqrt{\\frac{MSB_R}{m} + \\frac{(m-1)MSW_R}{m}}}}\\!$$')),   
                                     
                                     
                                     withMathJax(
                                         helpText('This is equal to $${{}\\sigma_R ={\\sqrt{\\frac{MSB_R-MSW_R}{m} + MSW_R}}}\\!$$ In the event that $$MSB_R < MSW_R$$then $$MSB_R - MSW_R < 0$$and 
                                         therefore $$\\sigma_R <  {\\sqrt{MSW_R}}$$
                              This means the total variance is less than the within variance component which cannot be.
                                                  If this is encountered the total variance is set equal to the within variance component. 
                                                  For either or both reference or test product if necessary. This is the first change from the guidance.')),
                                     
                                     withMathJax(
                                         helpText('Therefore if $$MSB_R < MSW_R$$ then')),
                                     
                                     
                                     
                                     withMathJax(
                                         helpText(" $$\\sigma_R =  {\\sqrt{MSW_R}}$$")),
                                 
                                     withMathJax(
                                         helpText('and if $$MSB_T < MSW_T$$ then')),
                                     
                                          
                                     withMathJax(
                                         helpText(" $$\\sigma_T =  {\\sqrt{MSW_T}}$$")),
                                     
                         
                                     HTML(" <strong>Impact to Delta and HD</strong>"),
                                
                                     withMathJax(
                                         helpText('$$ \\hat{\\Delta} = \\bar{y}_T - \\bar{y}_R$$')),
                                        
                                     br(),
                                     
                                     withMathJax(
                                         helpText("We have")),
                                     
                                     withMathJax(
                                         helpText('$$Var(\\bar{y}_T) = \\frac{\\sigma^2_B}{n.l}  +  \\frac{\\sigma^2_W}{n.l.m}  = \\frac{m\\sigma^2_B + \\sigma^2_W}{n.l.m} = \\frac{MSB_T}{n.l.m} $$')),
                                     
                                     withMathJax(
                                         helpText("with degrees of freedom $$n_T.l_T-1$$")),
                                     br(),
                                     withMathJax(
                                         helpText("We have")),
                                     withMathJax(
                                         helpText('$$Var(\\bar{y}_R) = \\frac{\\sigma^2_B}{n.l}  +  \\frac{\\sigma^2_W}{n.l.m}  = \\frac{m\\sigma^2_B + \\sigma^2_W}{n.l.m} = \\frac{MSB_R}{n.l.m} $$')),
                                     
                                     withMathJax(
                                         helpText("with degrees of freedom $$n_R.l_R-1$$")),
                                     
                                     withMathJax(
                                         helpText("The variances of the difference is equal to the sum of the variances, so")),
                                     
                                     withMathJax(
                                         helpText('$$ Var\\hat{\\Delta} =  \\frac{MSB_T}{n.l.m} +  \\frac{MSB_R}{n.l.m} $$')),
                                     
                                     withMathJax(
                                         helpText("with degrees of freedom $$(n_R.l_R-1) + (n_T.l_T-1) = (n_R.l_R + n_T.l_T-2)$$")),
                                     
                                     withMathJax(
                                         helpText("When there is no between variance component for either test or reference then:")),
                                     
                                     withMathJax(
                                         helpText('$$Var(\\bar{y}_T) =  \\frac{MSW_T}{n.l.m} $$')),
                                     
                                     withMathJax(
                                         helpText("with degrees of freedom $$n_T.l_T.(m-1)$$")),
                                     
                                     withMathJax(
                                         helpText("and")),
                                     
                                     withMathJax(
                                         helpText('$$Var(\\bar{y}_R) =  \\frac{MSW_R}{n.l.m} $$')),                                    
                                     
                                     withMathJax(
                                         helpText("with degrees of freedom $$n_R.l_R.(m-1)$$")),
                                     
                                     withMathJax(
                                         helpText("and so depending on if one or both products has a single variance component, there are four scenarios for the calculation of HD including the one in the guidance:")),
                                     
                                     withMathJax(
                                         helpText("I) Both have non negative variance components:")),
                                     
                                     withMathJax(
                                         helpText('$$H_D = \\left(\\lvert\\hat{\\Delta}\\rvert + 
                                        t_{1-\\alpha,n_T.l_T-1 + n_R.l_R-1)}  
                                          (\\frac{MSB_T}{n.l.m} + \\frac{MSB_R}{n.l.m})^.5\\right)^2  $$')),
                                     
                                     withMathJax(
                                         helpText("II) Both have negative variance components:")),
                                     
                                     withMathJax(
                                         helpText('$$H_D = \\left(\\lvert\\hat{\\Delta}\\rvert + 
                                        t_{1-\\alpha,n_T.l_T.(m-1) + n_R.l_R.(m-1)}  
                                          (\\frac{MSW_T}{n.l.m} + \\frac{MSW_R}{n.l.m})^.5\\right)^2  $$')),
                                     
                                     withMathJax(
                                         helpText("III) Test only has negative variance component:")),
                                     
                                     withMathJax(
                                         helpText('$$H_D = \\left(\\lvert\\hat{\\Delta}\\rvert + 
                                        t_{1-\\alpha,n_T.l_T.(m-1) + n_R.l_R-1)}  
                                          (\\frac{MSW_T}{n.l.m} + \\frac{MSB_R}{n.l.m})^.5\\right)^2  $$')),
                                     
                                     withMathJax(
                                         helpText("IV) Reference only has negative variance component:")),
                                     
                                     withMathJax(
                                         helpText('$$H_D = \\left(\\lvert\\hat{\\Delta}\\rvert + 
                                         t_{1-\\alpha,n_T.l_T-1  + n_R.l_R.(m-1)}  
                                          (\\frac{MSB_T}{n.l.m} + \\frac{MSW_R}{n.l.m})^.5\\right)^2  $$')),
                                     
                                     HTML(" <strong>Impact on FDA parameters E1 and E2</strong>"),
                                     
                                     withMathJax(
                                         helpText("When $$MSB_T >= MSW_T$$ we have")),
                                     
                                     withMathJax(
                                         helpText("$$E1 + E2 = \\frac{MSB_T}{m} + \\frac{(m-1) MSW_T }{m } = \\frac{MSB_T - MSW_T }{m}  +  MSW_T = \\hat{\\sigma^2_B} + \\hat{\\sigma^2_W}$$ 
                                                 which is the total variance of test products. 
                                                 If we encounter a negative between variance component for the test product we have shown above the total variance is estimated by the mean squares within. 
                                                 
                                                 So let $$E1 = 0, E2 = MSW_T$$ and H2 and U2 are unaltered.")), 
                                     
                                     HTML(" <strong>Impact on FDA parameters E3s and E4s, reference scaling</strong>"),
                                     
                                     withMathJax(
                                         helpText("When $$MSB_ r >= MSW_R$$ we have")),
                                     
                                     withMathJax(
                                         helpText("$$E3s + E4s = -(1+\\theta_p)\\frac{MSB_R}{m} -(1+\\theta_p) \\frac{(m-1) MSW_R }{m } = -(1+\\theta_p)\\left(\\frac{MSB_R - MSW_R }{m}  +
                                        MSW_R\\right) = -(1+\\theta_p)\\left(\\hat{\\sigma^2_B} + \\hat{\\sigma^2_W}\\right)$$ 
                                        
                                                 If we encounter a negative between variance component for the reference product we have shown above the total variance is estimated by the mean squares within. 
                                                 
                                                 So let $$E3s = 0, E4s = -(1+\\theta_p)MSW_R$$ and H4s and U4s are unaltered.")), 
                                     
                                     HTML(" <strong>Impact on FDA parameters E3c and E4c, constant scaling</strong>"),
                                     
                                     withMathJax(
                                         helpText("When $$MSB_ r >= MSW_R$$ we have")),
                                     
                                     withMathJax(
                                         helpText("$$E3c + E4c = -\\frac{MSB_R}{m} + \\frac{(m-1) MSW_R }{m } = -\\left(\\frac{MSB_R - MSW_R }{m}  +  MSW_R\\right) = -\\left(\\hat{\\sigma^2_B} +
                                        \\hat{\\sigma^2_W}\\right)$$ 
                                                   
                                                 If we encounter a negative between variance component for the test product we have shown above the total variance is estimated by the mean squares within. 
                                                 
                                                 So let $$E3c = 0, E4c = -MSW_R$$ and H4c and U4c are unaltered.")), 
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br())
                            
                            
                            
                            
                        )
                    )
                ) 
                )
)
#~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~


server <- shinyServer(function(input, output) {
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated, only random noise is required to be generated
    random.sample <- reactive({
        
        # Dummy line to trigger off button-press
        foo <- input$resample
        
        x1 <- input$range1[1]
        x2 <- input$range1[2]
        x3 <- input$range2[1]
        x4 <- input$range2[2]
        x5 <- input$replicates[1]
        x6 <- input$replicates[2]
        
        top <-  input$top
        
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
        
        a=input$a
        b=input$b
        c=input$c
        d=input$intercept
        residual <- input$d
        
        # random effects
        top.r <-    rnorm(top,          d,                a)    
        middle.r <- rnorm(sum(middle),  0,                b)    
        lower.r <-  rnorm(sum(lower),   0,                c)    
        
        # ids
        lower.id <- rep(seq_len(sum(lower)), replicates )       
        middle.id <- cut(lower.id, c(0,cumsum(lower)),  labels=FALSE)
        top.id   <- cut(middle.id, c(0,cumsum(middle)), labels=FALSE)
        
        return(list(top.r=top.r, middle.r=middle.r, lower.r=lower.r, intercept=d, residual=residual,
                    middle=middle, top=top, lower=lower, replicates=replicates,
                    N=N, top.id=top.id, middle.id=middle.id, lower.id=lower.id, 
                    a=a, b=b, c=c))
        
    }) 
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    make.regression <- reactive({
        
        sample <- random.sample()
        
        top <-        sample$top
        middle <-     sample$middle
        lower <-      sample$lower
        replicates <- sample$replicates
        
        # random effects
        top.r <-    sample$top.r 
        middle.r <- sample$middle.r 
        lower.r <-  sample$lower.r 
        
        # ids
        lower.id <- sample$lower.id
        middle.id <- sample$middle.id
        top.id   <- sample$top.id 
        
        Data <- data.frame( top=top.id, mid=middle.id, low=lower.id,
                            y= rnorm( sum(replicates), top.r[top.id] + 
                                          middle.r[middle.id] + lower.r[lower.id], sample$residual ) )
        
        df <- as.data.frame(Data)
        return(list(df=df , middle.id=middle.id, top.id=top.id)) 
        
    })  
    # --------------------------------------------------------------------------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Fit the specified regression model on simulated data  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fit.regression <- reactive({
        
        data <- make.regression()
        
        df <- data$df
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Conditionally fit the model
        
        if (input$Model == "nlme package") {
            
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
        } else {                   
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
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # Get the model summary
        if (is.null(fit.res)) {
            
            fit.summary <- NULL
            
        } else {
            
            fit.summary <-  (fit.res)
        }
        
        return(list(emu=emu, etop=etop, eday=eday, erun=erun,
                    esigma=esigma, fit.res=fit.res, fit.summary=fit.summary)) 
        
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Plot a scatter of the simulated data  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data1 <- make.regression()
        
        d1 <- data1$df
        
        # Conditionally plot
        if (input$Plot == "Base plot") {
            
            #base plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # middle groups for plot
            xxx<- cumsum(as.vector(table(data1$middle.id)))
            xxx <-d1$low[xxx]
            
            # top groups for plot
            yyy<- cumsum(as.vector(table(data1$top.id)))
            yyy <-d1$low[yyy]
            
            plot( y ~ factor(low), data=d1 , 
                  main=paste("Variability Chart. Truth (estimate): intercept ",input$intercept,"(",fit.regression()$emu,"), top level sd=",
                             input$a,"(",fit.regression()$etop,")", ",\n middle level sd=",
                             input$b ,"(",fit.regression()$eday,"), lowest level sd=",
                             input$c, "(",fit.regression()$erun,") & random error sd=", 
                             input$d,"(",fit.regression()$esigma,")"),
                  main="lowest level grouped", xlab="lowest level groups")
            
            abline( v=c(0,xxx)+0.5, lty=2, col='green' )
            abline( v=c(0,yyy)+0.5, lty=2, col='blue' )
            
            
        } else {
            
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
                    Title=list(main=paste("Variability Chart. Truth (estimate): intercept ",input$intercept,"(",fit.regression()$emu,"), top level sd=",
                                          input$a,"(",fit.regression()$etop,")", ",\n middle level sd=",
                                          input$b ,"(",fit.regression()$eday,"), lowest level sd=",
                                          input$c, "(",fit.regression()$erun,") & random error sd=", 
                                          input$d,"(",fit.regression()$esigma,")")),
                    
                    # MeanLine=list(var="mid", col="pink", lwd=2),
                    Points=list(pch=list(var="mid", pch=c(21, 22, 24)), 
                                bg =list(var="mid", bg=c("lightblue", "cyan", "yellow")), 
                                cex=1.25))    
            
            
        }
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # analyse fda variance components for plot title
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    vc.fda <-  reactive({
        
        df <- fda.d
        
        #df$y <- log(df$y)  # log the fda data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Conditionally fit the model
        
        if (input$Model == "nlme package") {
            
            fit.res <-  
                tryCatch(intervals(lme(y ~ 1, random = ~1 |  PRODUCT/BATCH/SECTOR , data=df, method="REML")), 
                         error=function(e) e)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ###http://stackoverflow.com/questions/8093914
            ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
            
            if (!inherits(fit.res, "error")) {
                
                modelint <- fit.res
                
                emu      <-p4(modelint[['fixed']][2][[1]])  
                etop     <-p4(modelint[['reStruct']][['PRODUCT']][2][[1]])
                eday     <-p4(modelint[['reStruct']][['BATCH']][2][[1]])
                erun     <-p4(modelint[['reStruct']][['SECTOR']][2][[1]])
                esigma   <-p4(modelint[['sigma']][2][[1]])
                
            } else  {
                
                fit.res <- NULL
                
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        } else {                   
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            o <- fit.res<- tryCatch(fitVCA(y~PRODUCT/BATCH/SECTOR, df, "reml"), 
                                    error=function(e) e) 
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
            if (!inherits(fit.res, "error")) {
                
                fit.res <- VCAinference(fit.res, ci.method = "sas")
                
                x <- as.matrix(o)
                features <- attributes(x)
                
                emu      <- p4(features$Mean) 
                
                o <- as.matrix(o)
                etop     <-p4(o["PRODUCT","SD"])
                eday     <-p4(o["PRODUCT:BATCH","SD"])
                erun     <-p4(o["PRODUCT:BATCH:SECTOR","SD"])
                esigma   <-p4(o["error","SD"])
                
            } else  {
                
                fit.res <- NULL
                
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # Get the model summary
        if (is.null(fit.res)) {
            
            fit.summary <- NULL
            
        } else {
            
            fit.summary <-  (fit.res)
        }
        
        return(list(emu=emu, etop=etop, eday=eday, erun=erun,
                    esigma=esigma, fit.res=fit.res, fit.summary=fit.summary))
        
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # plot fda data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$reg.plot2 <- renderPlot({         
        
        # Get the fda  data
        
        d1 <- fda.d
        
        
        # Conditionally plot
        if (input$Plot == "Base plot") {
            
            #base plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # middle groups for plot
            xxx<- cumsum(as.vector(table(d1$BATCH)))
            xxx <-d1$SECTOR[xxx]
            
            # top groups for plot
            yyy<- cumsum(as.vector(table(d1$PRODUCT)))
            yyy <-d1$SECTOR[yyy]
            
            plot( y ~ factor(SECTOR), data=d1 , 
                  main=paste("Variability Chart of FDA data. (Estimate in brackets): intercept (",vc.fda()$emu,"), top level sd=(",vc.fda()$etop,")", 
                             ",\n middle level sd=(",vc.fda()$eday,"), lowest level sd=(",vc.fda()$erun,") & random error sd=("
                             ,vc.fda()$esigma,")"),
                  main="lowest level grouped", xlab="lowest level groups", ylab="Guidance response (log y)")
            
            abline( v=c(0,xxx)+0.5, lty=2, col='green' )
            abline( v=c(0,yyy)+0.5, lty=2, col='blue' )
            
            
        } else {
            
            # VCA plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            varPlot(y~PRODUCT/BATCH/SECTOR/REP, d1, 
                    YLabel = list(text =
                                      "Guidance response (log y)", side = 2, line = 3.5, cex = 1.5),
                    BG=list(var="PRODUCT", 
                            col=c("#f7fcfd","#e5f5f9","#ccece6","#99d8c9",
                                  "#66c2a4","#41ae76","#238b45","#006d2c","#00441b"), 
                            col.table=TRUE), 
                    VLine=list(var=c("PRODUCT", "BATCH"), 
                               col=c("black", "mediumseagreen"), lwd=c(2,1), 
                               col.table=c(TRUE,TRUE)), 
                    JoinLevels=list(var="SECTOR", col=c("lightblue", "cyan", "yellow"), 
                                    lwd=c(2,2,2)), 
                    MeanLine=list(var="PRODUCT", col="blue", lwd=2),
                    Title=list(main=paste("Variability Chart of FDA data. (Estimate in brackets): intercept (",vc.fda()$emu,"), top level sd=(",vc.fda()$etop,")", 
                                          ",\n middle level sd=(",vc.fda()$eday,"), lowest level sd=(",vc.fda()$erun,") & random error sd=("
                                          ,vc.fda()$esigma,")")),
                    
                    # MeanLine=list(var="mid", col="pink", lwd=2),
                    Points=list(pch=list(var="BATCH", pch=c(21, 22, 24)), 
                                bg =list(var="BATCH", bg=c("lightblue", "cyan", "yellow")), 
                                cex=1.25))    
            
        }
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # execute PBE data analysis on simulation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bioequivf <- reactive({
        
        data1 <- make.regression()
        
        foo <- data1$df
        foo$top <-    ifelse(foo$top %in% 1, "REF", "TEST")  # arbitrarily make 1 the reference product
        foo$top <-    factor(foo$top)
        foo$SECTOR <- foo$low
        foo$BATCH <-  foo$mid
        
        A <- unique(input$range1)*unique(input$range2)
        B <- unique(input$replicates)
        
        res <- bioequiv(foo1=foo, nrXlr=A, mr= B, 
                        ntXlt=A, mt= B,
                        response="y",indep="low", split="top", ref="REF", test="TEST")
        
        return(list(res=res))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # execute fda analysis
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    bioequivfdax <- reactive({
        
        
        foo8 <- fda.d
        
        foo8$y <- exp(foo8$y)  # the bioequiv function logs the data, but the fda data is already logged
        
        res.fda <- bioequiv(foo1=foo8 , nrXlr=10*3, mr= 3, 
                            ntXlt=10*3, mt= 3,
                            response="y",indep="SECTOR", split="PRODUCT", ref="REF", test="TEST")
        
        return(res.fda)
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # collect fda data for listing
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    
    
    fda.data <- reactive({
        
        foo99 <- fda2
        
        return(foo99)
        
    })
    
    #---------------------------------------------------------------------------
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # fda data var comp output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$reg.summaryf <- renderPrint({
        
        summary <- vc.fda()$fit.summary
        
        if (!is.null(summary)) {
            
            return(vc.fda()$fit.summary)
            
        }
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # simulated data var comp output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression()$fit.summary
        
        if (!is.null(summary)) {
            
            return(fit.regression()$fit.summary)
            
        }
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # lsting of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$summary2 <- renderPrint({
        
        return(make.regression()$df)
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # collect simulation analysis
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$bioequiv0 <- renderPrint({
        
        return(bioequivf()$res)
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # collect the fda data listing
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$bioequivfda2 <- renderPrint({
        
        return(fda2)
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # collect the fda analysis
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$bioequivfda1 <- renderPrint({
        
        return(bioequivfdax()$res.fda)
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # loading in user data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        df<- NULL
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        df <- as.data.frame(df)
        
        if(input$disp == "head") {
            
            return(head(df))
        }
        else {
            
            return(df)
        } 

    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # PBE analysis of user uploaded data 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$contents2 <- renderPrint({
        
        if(input$what == "Analysis"){
            
            df<-NULL
            req(input$file1)
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
            
            df<- as.data.frame(df)
            
            df$y <- exp(df$y) 
            
            REF <-  df[df$PRODUCT %in% "REF",]
            TEST <- df[df$PRODUCT %in% "TEST",]
            
            #work out the design
            mr <- unique(with(REF, table(SECTOR)))
            mt <- unique(with(TEST, table(SECTOR))) 
            
            x <- REF[,c(1,2)]
            x <- unique(x)
            nrXlr <- sum(as.vector(table(x$BATCH)))
            
            
            x <- TEST[,c(1,2)]
            x <- unique(x)
            ntXlt <- sum(as.vector(table(x$BATCH)))
            
            ##new 
            if(mr ==1 & mt==1) {
                
                
             user.analysis<- bioequiv.1.rep(foo1=df , 
                                                   nrXlr=nrXlr, mr=mr, 
                                                   ntXlt=ntXlt, mt=mt,
                               response="y",indep="SECTOR", split="PRODUCT", ref="REF", test="TEST")
               
            } else {
            
            user.analysis<- bioequiv(foo1=df , 
                                     nrXlr=nrXlr, mr= mr, 
                                     ntXlt=ntXlt, mt= mr,
                                     response="y",indep="SECTOR", split="PRODUCT", ref="REF", test="TEST")
            
            }
        }})
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # analyse on user data, variance components for plot title
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    vc.user <-  reactive({
        
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        df<- as.data.frame(df)
        
        REF <-  df[df$PRODUCT %in% "REF",]
        TEST <- df[df$PRODUCT %in% "TEST",]
        
        #work out the design
        mr <- unique(with(REF, table(SECTOR)))
        mt <- unique(with(TEST, table(SECTOR))) 
        
        x <- REF[,c(1,2)]
        x <- unique(x)
        nrXlr <- sum(as.vector(table(x$BATCH)))
        
        x <- TEST[,c(1,2)]
        x <- unique(x)
        ntXlt <- sum(as.vector(table(x$BATCH)))
        
        ##new 
        if(mr ==1 & mt==1) {

            #   df$y <- log(df$y)  # log the fda data
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Conditionally fit the model
            
            if (input$Model == "nlme package") {
                
                fit.res <-  
                    tryCatch(intervals(lme(y ~ 1, random = ~1 |  PRODUCT/BATCH , data=df, method="REML")), 
                             error=function(e) e)
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                ###http://stackoverflow.com/questions/8093914
                ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
                
                if (!inherits(fit.res, "error")) {
                    
                    modelint <- fit.res
                    
                    emu      <-p4(modelint[['fixed']][2][[1]])  
                    etop     <-p4(modelint[['reStruct']][['PRODUCT']][2][[1]])
                    eday     <-p4(modelint[['reStruct']][['BATCH']][2][[1]])
                   # erun     <-p4(modelint[['reStruct']][['SECTOR']][2][[1]])
                    esigma   <-p4(modelint[['sigma']][2][[1]])
                    
                } else  {
                    
                    fit.res <- NULL
                    
                }
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            } else {                   
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                o <- fit.res<- tryCatch(fitVCA(y~PRODUCT/BATCH, df, "reml"), 
                                        error=function(e) e) 
                
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                if (!inherits(fit.res, "error")) {
                    
                    fit.res <- VCAinference(fit.res, ci.method = "sas")
                    
                    x <- as.matrix(o)
                    features <- attributes(x)
                    
                    emu      <- p4(features$Mean) 
                    
                    o <- as.matrix(o)
                    etop     <-p4(o["PRODUCT","SD"])
                    eday     <-p4(o["PRODUCT:BATCH","SD"])
                    #erun     <-p4(o["PRODUCT:BATCH:SECTOR","SD"])
                    esigma   <-p4(o["error","SD"])
                    
                } else  {
                    
                    fit.res <- NULL
                    
                }
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       # MORE THAN 1 REP   
                
        } else {
            
        #   df$y <- log(df$y)  # log the fda data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Conditionally fit the model
        
        if (input$Model == "nlme package") {
            
            fit.res <-  
                tryCatch(intervals(lme(y ~ 1, random = ~1 |  PRODUCT/BATCH/SECTOR , data=df, method="REML")), 
                         error=function(e) e)
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            ###http://stackoverflow.com/questions/8093914
            ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
            
            if (!inherits(fit.res, "error")) {
                
                modelint <- fit.res
                
                emu      <-p4(modelint[['fixed']][2][[1]])  
                etop     <-p4(modelint[['reStruct']][['PRODUCT']][2][[1]])
                eday     <-p4(modelint[['reStruct']][['BATCH']][2][[1]])
                erun     <-p4(modelint[['reStruct']][['SECTOR']][2][[1]])
                esigma   <-p4(modelint[['sigma']][2][[1]])
                
            } else  {
                
                fit.res <- NULL
                
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        } else {                   
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            o <- fit.res<- tryCatch(fitVCA(y~PRODUCT/BATCH/SECTOR, df, "reml"), 
                                    error=function(e) e) 
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
            if (!inherits(fit.res, "error")) {
                
                fit.res <- VCAinference(fit.res, ci.method = "sas")
                
                x <- as.matrix(o)
                features <- attributes(x)
                
                emu      <- p4(features$Mean) 
                
                o <- as.matrix(o)
                etop     <-p4(o["PRODUCT","SD"])
                eday     <-p4(o["PRODUCT:BATCH","SD"])
                erun     <-p4(o["PRODUCT:BATCH:SECTOR","SD"])
                esigma   <-p4(o["error","SD"])
                
            } else  {
                
                fit.res <- NULL
                
            }
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        }  # end

        ##new 
        if(mr ==1 & mt==1) { 
            
        # Get the model summary
        if (is.null(fit.res)) {
            
            fit.summary <- NULL
            
        } else {
            
            fit.summary <-  (fit.res)
        }
        
        return(list(emu=emu, etop=etop, eday=eday, #erun=NA,
                    esigma=esigma, fit.res=fit.res, fit.summary=fit.summary))
            
        } else {
            
            if (is.null(fit.res)) {
                
                fit.summary <- NULL
                
            } else {
                
                fit.summary <-  (fit.res)
            }
            
            return(list(emu=emu, etop=etop, eday=eday, erun=erun,
                        esigma=esigma, fit.res=fit.res, fit.summary=fit.summary))
           
        }  
            
    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plotting user uploaded data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$plotx <- renderPlot({
        if(input$what == "plot"){
            
            df<-NULL
            req(input$file1)
            df <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
            
            df<- as.data.frame(df)
            
            REF <-  df[df$PRODUCT %in% "REF",]
            TEST <- df[df$PRODUCT %in% "TEST",]
            
            #work out the design
            mr <- unique(with(REF, table(SECTOR)))
            mt <- unique(with(TEST, table(SECTOR))) 
            
            x <- REF[,c(1,2)]
            x <- unique(x)
            nrXlr <- sum(as.vector(table(x$BATCH)))
            
            x <- TEST[,c(1,2)]
            x <- unique(x)
            ntXlt <- sum(as.vector(table(x$BATCH)))
            
            ##new 
            if(mr ==1 & mt==1) {
            # df$y <- log(df$y)  # log the fda data
            
                varPlot(y~PRODUCT/BATCH/SECTOR/REP, df, 
                        BG=list(var="PRODUCT", 
                                col=c("#f7fcfd","#e5f5f9","#ccece6","#99d8c9",
                                      "#66c2a4","#41ae76","#238b45","#006d2c","#00441b"), 
                                col.table=TRUE), 
                        VLine=list(var=c("PRODUCT", "BATCH"), 
                                   col=c("black", "mediumseagreen"), lwd=c(2,1), 
                                   col.table=c(TRUE,TRUE)), 
                        JoinLevels=list(var="SECTOR", col=c("lightblue", "cyan", "yellow"), 
                                        lwd=c(2,2,2)), 
                        MeanLine=list(var="PRODUCT", col="blue", lwd=2),
                        Title=list(main=paste("Variability Chart. Estimate: intercept (", vc.user()$emu,"), top level sd= (",vc.user()$etop,")"
                                              , ",\n lower level sd= (",vc.user()$eday,"),  random error sd= (",vc.user()$esigma,")")),
                        
                        # MeanLine=list(var="mid", col="pink", lwd=2),
                        Points=list(pch=list(var="BATCH", pch=c(21, 22, 24)), 
                                    bg =list(var="BATCH", bg=c("lightblue", "cyan", "yellow")), 
                                    cex=1.25)) 
           }  else {   
               
               varPlot(y~PRODUCT/BATCH/SECTOR/REP, df, 
                       BG=list(var="PRODUCT", 
                               col=c("#f7fcfd","#e5f5f9","#ccece6","#99d8c9",
                                     "#66c2a4","#41ae76","#238b45","#006d2c","#00441b"), 
                               col.table=TRUE), 
                       VLine=list(var=c("PRODUCT", "BATCH"), 
                                  col=c("black", "mediumseagreen"), lwd=c(2,1), 
                                  col.table=c(TRUE,TRUE)), 
                       JoinLevels=list(var="SECTOR", col=c("lightblue", "cyan", "yellow"), 
                                       lwd=c(2,2,2)), 
                       MeanLine=list(var="PRODUCT", col="blue", lwd=2),
                       Title=list(main=paste("Variability Chart. Estimate: intercept (", vc.user()$emu,"), top level sd= (",vc.user()$etop,")"
                                             , ",\n middle level sd= (",vc.user()$eday,"), lowest level sd= (",vc.user()$erun,") & random error sd= (",vc.user()$esigma,")")),
                       
                       # MeanLine=list(var="mid", col="pink", lwd=2),
                       Points=list(pch=list(var="BATCH", pch=c(21, 22, 24)), 
                                   bg =list(var="BATCH", bg=c("lightblue", "cyan", "yellow")), 
                                   cex=1.25)) 
            }
            
        }  
    })   
    
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run the application 
shinyApp(ui = ui, server = server)
