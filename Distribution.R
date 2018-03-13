# Title: Distribution
# Author: Thomas Verliefde
# Date: 2018/03/13
# Version: 0.4
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages = c("shiny","ggplot2","stats","sn","tidyr","dplyr","shinyjs","magrittr");new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];if(length(new.packages)){install.packages(new.packages,repos="http://cran.us.r-project.org")};lapply(list.of.packages,require,character.only=T);rm(list.of.packages,new.packages)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  fluidRow(
    column(
      1,
      actionButton('resample','Resample',icon('refresh'),
                   style="position:relative;
                        font-size:115%;
                        margin-top:10px;
                        font-family:Lucida Console, sans-serif;
                   ")
    ),
    column(
      6,
      offset=1,
      'F-Distribution & Violations of Assumptions',
      style="position:relative;
            margin-top:8px;
            font-family:Lucida Console, sans-serif;
            font-weight:500;
            font-variant: small-caps;
            font-size:200%;"
    ),
    column(
      4,
      selectInput(
        'distribution',NULL,
        c('Normal Distribution','Something Else Non-Implemented')
        ),
      style="position:relative;
            margin-top:10px;
            font-size:115%;
            font-family:Lucida Console, sans-serif"
    )

    
  ),
  
  fluidRow(
    column(
      4,
      plotOutput('distplot',width='250px'),
      tags$style(type = "text/css", "
                .irs {max-height:30px;width:250px}
                .irs-single {display:none;}
                .js-irs-0 .irs-single {display:inline;background:#c8cfa1;color:#000000;}
                .irs-grid {display:none !important;}
                .irs-bar {background:#c8cfa1;border:1px solid #999fbd;}
                .irs-bar-edge {background:#c8cfa1;border-left:1px solid #999fbd;
                               border-top:1px solid #999fbd;
                               border-bottom:1px solid #999fbd;}
                .irs-min {visibility:hidden !important;
                          left:-22px;}
                .irs-max {visibility:hidden !important;}
                .js-irs-0 .irs-min {visibility:visible !important;
                                    left:0px;background:#ffffff;
                                    font-size:80%;}
                .js-irs-0 .irs-max {visibility:visible !important;
                                    background:#ffffff;
                                    font-size:80%;}
                .js-irs-1 .irs .irs-min:after {content:'Unbalanced';}
                .js-irs-1 .irs .irs-max:after {content:'Balanced';}
                .js-irs-2 .irs .irs-min:after {content:'Heterogeneity';}
                .js-irs-2 .irs .irs-max:after {content:'Homogeneity';}
                .js-irs-3 .irs .irs-min:after {content:'Heterogeneity';}
                .js-irs-3 .irs .irs-max:after {content:'Homogeneity';}
                .js-irs-4 .irs .irs-min:after {content:'Dependence';}
                .js-irs-4 .irs .irs-max:after {content:'Independence';}
                .irs-min:after {visibility: visible !important;}
                .irs-max:after {visibility: visible !important;}
                .irs-all {top:-10px;}
                "),
      sliderInput(
        'samplesize',
        'Sample Size (n)',
        min=16,
        max=100,
        value=40,
        step=4
      ),
      sliderInput(
        'balance',
        'Group Diff: n',
        min=0.01,
        max=1,
        value=1,
        step=.01
      ),
      sliderInput(
        'mu',
        HTML("Group Diff: &mu;"),
        min=0.01,
        max=1,
        value=1,
        step=.01
      ),
      sliderInput(
        'var',
        HTML('Group Diff: &sigma;²'),
        min=0.01,
        max=1,
        value=1,
        step=.01
      # ),
      # sliderInput(
      #   'independence',
      #   'Dependence',
      #   min=0.01,
      #   max=1,
      #   value=1,
      #   step=.01
      )
    ),
    column(
      4,
      'Col2'
    ),
  column(
    4,
    'COL3'
  )
   )
 
)

# Define server logic
server <- function(input, output) {

  #
  Palette = c('#e66101','#fdb863','#b2abd2','#5e3c99')
  Lims=c(.01,.99)
  
  LinTrans = function(x,y) {
    output = abs(x-1)*y
    return(output)
  }
  
  ExpTrans = function(x,y) {
    output = (1 / exp(x-1))^y
    return(output)
  }
  
  SeqTrans = function(x,y,z) {
    output = (x / z) %>% {seq((.^y),(.*2 - 1), length.out = z )}
  }
  
  
  MuVec = reactive(
    sapply(
      seq(-9,9,length.out=4),
      function(y) LinTrans(input$mu,y)
    )
    )
  VarVec = reactive(
    sapply(
      c(0,seq(1,2.5,length.out=3)),
      function(y) ExpTrans(input$var,y)
    )
  )
  
  DistFunc=reactive({
    switch(input$distribution,
           'Normal Distribution' = c(dnorm,qnorm,rnorm)
    )}
  )
  
  DataDist=reactive({
    tibble(
      x = c(
        DistFunc()[[2]](Lims,MuVec()[[1]],VarVec()[[1]]),
        DistFunc()[[2]](Lims,MuVec()[[2]],VarVec()[[2]]),
        DistFunc()[[2]](Lims,MuVec()[[3]],VarVec()[[3]]),
        DistFunc()[[2]](Lims,MuVec()[[4]],VarVec()[[4]])
      ) %>% {c(min(.),max(.))}
    )
  })
  
  output$distplot = renderPlot(
    ggplot(DataDist(),aes(x=x)) +
      stat_function(fun=DistFunc()[[1]],n=101,args=list(MuVec()[[1]],VarVec()[[1]]),colour=Palette[1]) +
      stat_function(fun=DistFunc()[[1]],n=101,args=list(MuVec()[[2]],VarVec()[[2]]),colour=Palette[2]) +
      stat_function(fun=DistFunc()[[1]],n=101,args=list(MuVec()[[3]],VarVec()[[3]]),colour=Palette[3]) +
      stat_function(fun=DistFunc()[[1]],n=101,args=list(MuVec()[[4]],VarVec()[[4]]),colour=Palette[4]) +
      labs(x=NULL,y=NULL) +
      scale_x_continuous(
        breaks=function(x) c(x[1],mean(c(x[1],mean(x))),mean(x),mean(c(x[2],mean(x))),x[2]-.01),
        expand=c(0,0)) +
      guides(colour=F) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(colour='black'),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(.3,"cm"),
        axis.line.x = element_line(colour='black'),
        plot.margin = unit(c(0,0,.5,0),'cm')
      )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

