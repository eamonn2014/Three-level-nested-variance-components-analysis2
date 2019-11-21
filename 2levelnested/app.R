# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/

library(shiny)
library(nlme)
library(VCA)
options(max.print=1000000)
fig.width <- 1200
fig.height <- 450
library(shinythemes)        # more funky looking apps
p1 <- function(x) {formatC(x, format="f", digits=1)}




ui <- fluidPage(theme = shinytheme("journal"),
                
                shinyUI(pageWithSidebar(

#ui <-shinyUI(pageWithSidebar(
    
    headerPanel("Three level nested variance components analysis"),
    
    sidebarPanel(
     
        div(p("Frequentist variance component modelling and a plot of the data. A choice of plots is given, a base plot or a plot using the VCA package 
              (VCA plot shows individual data points rather than boxplots of replicate distributions). A choice of modelling is given, estimation using nlme or VCA package. 
              The difference in the two is in the variance components confidence interval calculation approach.")),
        
        div(
            
          #p("blah"),
            selectInput("Plot",
                        strong("Select plot preference "),
                        choices=c("VCA package plot" , "Base plot")),
     
           
             selectInput("Model",
                         strong("Select modelling preference "),
                       choices=c("VCA package" , "nlme package")),
               
            
            actionButton("resample", "Simulate a new sample"),
            br(),br(),
            
            div(strong("Select true population parameters"),p("A 3 level nested data set is simulated. A plot of the raw data is generated. 
                  A model is fit to estimate the variance components. The design does not have to be balanced. For the base plot, 
                  the blue dashed lines demark the top level groups. The green dashed lines demark the mid level groups.
                  The boxplots within the green lines demark the lowest level groups. Each boxplot presents the distribution of replicates. 
                  The middle, lowest and replicate numbers are varied randomly based on the slider ranges. The variance components are between blue 'top' groups,
between green 'mid' groups (within blue groups), within green 'mid' groups, within 'low' groups (replicates), aka repeatability. 
So we actually estimate 4 components counting the residual error. Create a balanced design by reducing all the sliders to one value.
                  You also have the choice of simulating a new sample. ")),

        
            br(),
            actionButton(inputId='ab1', label="R code here", 
                         icon = icon("th"), 
                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Three-level-nested-variance-components-analysis/master/three-level-nested-variance-components-analysis/app.R', '_blank')"),
            
            br(),
            br(),
            #p(strong("Generate true population parameters:")),   
            sliderInput("intercept",
                        "True intercept",
                        min=0, max=1000, step=.5, value=100, ticks=FALSE),
            
            sliderInput("top",
                        "Number of levels of top component (demarked by blue or thick lines)",
                        min=2, max=100, step=1, value=4, ticks=FALSE),
            
            sliderInput("range1", "Middle level: Randomly select using range or precisely select no of 'mid' groups within each top level group:", 
                        min = 2, max = 10, value = c(2, 10), ticks=FALSE) ,
            
            sliderInput("range2", "Lower level: Randomly select using range or precisely select no of 'low' groups within each mid level group:",
                        min = 2, max = 10, value = c(5, 10),ticks=FALSE),
        
            sliderInput("replicates", "Randomly select using range or precisely select no of replicates",
                        min = 2, max = 50, value = c(3, 10), ticks=FALSE),
            
            sliderInput("a",
                        "True top level SD",
                        min=1, max=100, step=.5, value=20, ticks=FALSE),
            sliderInput("b",
                        "True middle level SD",
                        min=1, max=100, step=.5, value=2, ticks=FALSE),
            sliderInput("c",
                        "True lower level SD",
                        min=1, max=100, step=.5, value=2, ticks=FALSE),
            sliderInput("d",
                        "True error",
                        min=1, max=100, step=.5, value=2, ticks=FALSE)
            
           
        
               )
    ),
    
     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
    mainPanel(
        
      #~~~~~~~~~~~~~
  #    tabsetPanel(type = "tabs", 
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
        #~~~~~~~~~~~~          
        tabPanel("Plot and analysis", 
        
        div(plotOutput("reg.plot", width=fig.width, height=fig.height)),  
    
        p(strong("Arithmetic mean is presented as the intercept above the plot when VCA package is used, otherwise the modelled mean is presented.
                 (Artithmetic mean and modelled mean will match with a balanced design)")) ,
        
        div( verbatimTextOutput("reg.summary"))
        
        ) ,
    
        #~~~~~~~~~~~~
        tabPanel("List the data", 
                 
          div( verbatimTextOutput("summary2")),
       
         p(strong("copy the above data to do your own analysis..."))

        
        )
        #~~~~~~~~~~~~~
    
         )
      #~~~~~~~~~~~~~
      
    )
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels
    
    
)
)
)

server <- shinyServer(function(input, output) {
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated only random noise is required to be generated
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
        top.r <-    rnorm(top,          d,                a)    # 6
        middle.r <- rnorm(sum(middle),  0,                b)    # 43 middle random effects
        lower.r <-  rnorm(sum(lower),   0,                c)    # 739
        
        # ids
        lower.id <- rep(seq_len(sum(lower)), replicates )       # expand lower (1:723) by replicates 
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
    # Fit the specified regression model
    fit.regression <- reactive({
        
        #p1 <- function(x) {formatC(x, format="f", digits=1)}
        
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
            } else {            #if (input$model == "VCA") {          
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
    
    #---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data1 <- make.regression()
        
        d1 <- data1$df
        
        # Conditionally plot
        if (input$Plot == "Base plot") {
        
        #base plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # middle groups for plot
        xx<- cumsum(as.vector(table(data1$middle.id)))
        xx <-d1$low[xx]
        
        # top groups for plot
        yy<- cumsum(as.vector(table(data1$top.id)))
        yy <-d1$low[yy]
        
        plot( y ~ factor(low), data=d1 , 
              main=paste("Variability Chart. Truth (estimate): intercept ",input$intercept,"(",fit.regression()$emu,"), top level sd=",
                         input$a,"(",fit.regression()$etop,")", ",\n middle level sd=",
                         input$b ,"(",fit.regression()$eday,"), lowest level sd=",
                         input$c, "(",fit.regression()$erun,") & random error sd=", 
                         input$d,"(",fit.regression()$esigma,")"),
              main="lowest level grouped", xlab="lowest level groups")
        
        abline( v=c(0,xx)+0.5, lty=2, col='green' )
        abline( v=c(0,yy)+0.5, lty=2, col='blue' )
        
        
        } else {
          
        #VCA plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          
            require(VCA)
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

    
    #---------------------------------------------------------------------------
    # Show the summary for the 
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression()$fit.summary
        
        if (!is.null(summary)) {
            
            return(fit.regression()$fit.summary)
            
        }
        
    })
    
    # the data to print
    output$summary2 <- renderPrint({
      
      return(make.regression()$df)
      
      
    })
    
    
})



# Run the application 
shinyApp(ui = ui, server = server)