# Title: Distribution
# Author: Thomas Verliefde
# Date: 2018/03/13
# Version: 0.5
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
  
  fixedRow(
    column(
      2,
      plotOutput('distplot',width='100%'),
      tags$style(type = "text/css", "
                .irs {max-height:30px;width:100%}
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
        value=.75,
        step=.01
      ),
      sliderInput(
        'var',
        HTML('Group Diff: &sigma;²'),
        min=0.01,
        max=1,
        value=.75,
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
      5,
      plotOutput('sampleplot',height='200px',width='100%')
    ),
  column(
    5,
    'COL3'
  )
   )
 
)

# Define server logic
server <- function(input, output, session) {

  #
  Palette = c('#e66101','#fdb863','#b2abd2','#5e3c99')
  Lims=c(.01,.99)
  Iterations=10
  Group=seq(4)
  MuMin=-9
  MuMax=9
  VarMin=1
  VarMax=12
  
  MuTrans = function(x,y) {
    output = abs(x-1)*y
    return(output)
  }
  
  VarTrans = function(x,y) {
    output = y^(1-x)
    return(output)
  }
  
  BalTrans = function(x,y,z) {
    output = (x / z) %>% {seq((.^y),(.*2 - .^y), length.out = z )} %>% round
    return(output)
  }
  
  MuVec = reactive(
    sapply(
      seq(MuMin,MuMax,along.with=Group),
      function(y) MuTrans(input$mu,y)
    )
    )
  VarVec = reactive(
    sapply(
      seq(VarMin,VarMax,along.with=Group),
      function(y) VarTrans(input$var,y)
    )
  )
  
  BalVec = reactive(
    BalTrans(input$samplesize,input$balance,length(Group))
  )
  
  DistFunc=reactive({
    switch(input$distribution,
           'Normal Distribution' = c(dnorm,qnorm,rnorm)
    )}
  )
  
  DataDist=reactive({
    tibble(
      x = sapply(
        Group,
        function(x) DistFunc()[[2]](Lims,MuVec()[[x]],VarVec()[[x]])
      ) %>% {c(min(.),max(.))}
    )
  })
  
  output$distplot = renderPlot(
    ggplot(DataDist(),aes(x=x)) +
      sapply(
        Group,
        function(x) stat_function(
          fun=DistFunc()[[1]],n=101,args=list(MuVec()[[x]],VarVec()[[x]]),colour=Palette[x])
      ) +
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
  
  SampledData=
    reactive(
      replicate(
        Iterations,
        expr = sapply( 
          Group,
          function(a) replicate(
            BalVec()[[a]],
            expr = MuVec()[[a]] + 0 + DistFunc()[[3]](1,0,VarVec()[[a]])
          )
        ) %>% unlist %>% as_tibble %>% unlist %T>% {input$resample} # fake dependency on button
      ) %>% as_tibble %>% mutate(Grouping = rep(Group,BalVec()) %>% as.factor)
    )
  
  eventReactive(input$resample,SampledData())
  
  output$sampleplot = renderPlot(
    ggplot(SampledData(),aes(y=Grouping,colour=Grouping)) +
      geom_jitter(aes(x=V1),position = position_jitter(w=0,h=0.1)) +
      scale_colour_manual(values=Palette) +
      labs(x=NULL,y=NULL)+
      guides(colour=F) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill='gray98'),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)