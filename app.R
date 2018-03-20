# Title: Distribution
# Author: Thomas Verliefde
# Date: 2018/03/20
# Version: 0.92
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages = c("shiny","ggplot2","stats","tidyr","dplyr","shinyjs","magrittr",'cowplot');new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];if(length(new.packages)){install.packages(new.packages,repos="http://cran.us.r-project.org")};lapply(list.of.packages,require,character.only=T);rm(list.of.packages,new.packages)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  fluidRow(
    column(
      1,
      actionButton('sample','Sample',icon('refresh'),
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
        c('Normal Distribution')
        ),
      style="position:relative;
            margin-top:10px;
            font-size:115%;
            font-family:Lucida Console, sans-serif"
    ) %>% disabled

    
  ),
  fluidRow(
    div(
    plotOutput('distplot',width='30%'),
    plotOutput('sampleplot',width='30%'),
    plotOutput('fplot',width='30%'),
    class='shiny-flow-layout',
    style='margin-left:15px;
           margin-top:10px;'
    )
    
  ),
  
  fluidRow(
    column(
      4,
      tags$style(type = "text/css", "
                .irs {max-height:30px;}
                .irs-single {display:none;}
                .js-irs-4 .irs-single {display:inline;background:#c8cfa1;color:#000000;}
                .js-irs-3 .irs-single {display:inline;background:#c8cfa1;color:#000000;}
                .js-irs-2 .irs-single {display:inline;background:#c8cfa1;color:#000000;}
                .js-irs-6 .irs-single {display:inline;background:#c8cfa1;color:#000000;}
                .irs-grid {display:none !important;}
                .irs-bar {background:#c8cfa1;border:1px solid #999fbd;}
                .irs-bar-edge {background:#c8cfa1;border-left:1px solid #999fbd;
                               border-top:1px solid #999fbd;
                               border-bottom:1px solid #999fbd;}
                .irs-min {visibility:hidden !important;
                          left:-22px;}
                .irs-max {visibility:hidden !important;}
                .js-irs-4 .irs-min {visibility:visible !important;
                                    left:0px;background:#ffffff;
                                    font-size:80%;}
                .js-irs-4 .irs-max {visibility:visible !important;
                                    background:#ffffff;
                                    font-size:80%;}
                .js-irs-3 .irs-min {visibility:visible !important;
                                    left:0px;background:#ffffff;
                                    font-size:80%;}
                .js-irs-3 .irs-max {visibility:visible !important;
                                    background:#ffffff;
                                    font-size:80%;}
                .js-irs-2 .irs-min {visibility:visible !important;
                                    left:0px;background:#ffffff;
                                    font-size:80%;}
                .js-irs-2 .irs-max {visibility:visible !important;
                                    background:#ffffff;
                                    font-size:80%;}
                .js-irs-5 .irs .irs-min:after {content:'Unbalanced';}
                .js-irs-5 .irs .irs-max:after {content:'Balanced';}
                .js-irs-0 .irs .irs-min:after {content:'Heterogeneity';}
                .js-irs-0 .irs .irs-max:after {content:'Homogeneity';}
                .js-irs-1 .irs .irs-min:after {content:'Heteroscedasticity';}
                .js-irs-1 .irs .irs-max:after {content:'Homoscedasticity';}
                .js-irs-7 .irs .irs-min:after {content:'Dependence';}
                .js-irs-7 .irs .irs-max:after {content:'Independence';}
                .irs-min:after {visibility: visible !important;}
                .irs-max:after {visibility: visible !important;}
                .irs-all {top:-10px;}
                "),
      sliderInput(
        'mu',
        HTML("Group Diff: &mu;"),
        min=0.01,
        max=1,
        value=.99,
        step=.01
      ),
      sliderInput(
        'var',
        HTML('Group Diff: &sigma;²'),
        min=0.01,
        max=1,
        value=1,
        step=.01
      ),
      sliderInput(
        'groups',
        'Groups',
        min=2,
        max=6,
        value=4,
        step=1
      ) %>% disabled
      ),
    column(
      4,
      sliderInput(
        'iter',
        'Iterations',
        min=10,
        max=500,
        value=250,
        step=10
      ),
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
      )
    ),
  column(
    4,
    sliderInput(
      'alpha',
      HTML('Significance: &alpha;'),
      min=0,
      max=1,
      value=.95,
      step=.01
    ) %>% disabled,
    sliderInput(
      'independence',
      'Dependence',
      min=0.01,
      max=1,
      value=1,
      step=.01
    ) %>% disabled
  )
  )
)

# Define server logic
server <- function(input, output, session) {

  DistFunc=reactive({
    switch(input$distribution,
           'Normal Distribution' = c(dnorm,qnorm,rnorm,df,qf)
    )}
  )
  
  Palette = c('#e66101','#fdb863','#b2abd2','#5e3c99')
  Lims=c(.01,.99)
  # Iterations=isolate(input$iter)
  Group=isolate(seq(input$groups))
  MuMin=-5
  MuMax=5
  VarMin=1
  VarMax=10
  Df1=isolate(input$groups-1)
  Df2=isolate(input$samplesize-input$groups)
  SignifVal=isolate(DistFunc()[[5]](input$alpha,Df1,Df2))
  
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
      labs(x=NULL,y=NULL,title='Theoretical Distributions') +
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
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(colour='black'),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(.3,"cm"),
        axis.line.x = element_line(colour='black'),
        plot.margin = unit(c(0.1,0,.5,0),'cm')
      )
  )
  

  SampledData =
    reactive(
      isolate(
        replicate(
          input$iter,
          expr = sapply(
            Group,
            function(a) replicate(
              BalVec()[[a]],
              expr = MuVec()[[a]] + 0 + DistFunc()[[3]](1,0,VarVec()[[a]])
            )
          ) %>% unlist %>% as_tibble %>% unlist
        ) %>% as_tibble %>% mutate(Grouping = rep(Group,BalVec()) %>% as.factor)
      ) %T>% {input$sample}
    )
  
  ListPlots = reactive(
    lapply(
      SampledData() %>% select(1:4),
      function(a) {ggplot(SampledData(),aes(x=a,y=Grouping,colour=Grouping)) +
          geom_jitter(position = position_jitter(w=0,h=0.1)) +
          scale_colour_manual(values=Palette) +
          labs(x=NULL,y=NULL)+
          guides(colour=F) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill='gray98'),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank()
          )}
    )
  )
  
  output$sampleplot = renderPlot(
    plot_grid(
      ggdraw() +
        draw_label('Sampled Distributions',fontface='bold'),
      ListPlots()[[1]],
      ListPlots()[[2]],
      ListPlots()[[3]],
      ggdraw() +
        draw_label('. . .',size=30,fontface='bold'),
      ListPlots()[[4]],
      ncol=1,
      align='v',
      rel_heights=c(0.2,1,1,1,.3,1)
    )
  )
  
  FData = reactive(
    SampledData() %>%
      summarize_at(
        vars(-Grouping),
        function(x) aov(x ~ Grouping,data=.) %>%
          summary %>% unlist(recursive=F) %$% `F value`[[1]]
      ) %>% gather
  )
  
  # Fdata2 = SampledData2 %>%
  #   summarize_at(
  #     vars(-Grouping),
  #     function(x) aov(x ~ Grouping,data=.) %>%
  #       summary %>% unlist(recursive=F) %$% `F value`[[1]]
  #   ) %>% gather

  output$fplot = renderPlot(
    ggplot(FData(),aes(x=value)) +
      geom_histogram(aes(y=..density..),
                     bins=50,
                     fill='transparent',
                     colour='black') +
      stat_function(
        fun=DistFunc()[[4]],n=101,
      args=list(df1=Df1,df2=Df2),colour='red1'
      ) +
      stat_function(geom='density',
                    fun=DistFunc()[[4]],n=101,
                    args=list(df1=Df1,df2=Df2),fill='red1',colour='transparent',
                    xlim=c(SignifVal,max(FData()$value)),
                    alpha=.5) +
      labs(x=NULL,y=NULL,title='F-Distribution') +
      scale_x_continuous(
        breaks=function(x) c(x[1],mean(c(x[1],mean(x))),mean(x),mean(c(x[2],mean(x))),x[2]-.01),
        expand=c(0,0.7)) +
      guides(fill=FALSE) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(colour='black'),
        axis.text.x = element_blank(),
        axis.ticks.length = unit(.3,"cm"),
        axis.line.x = element_line(colour='black'),
        plot.margin = unit(c(0.15,0,.5,0),'cm')
      )
  )

  
  # ggplot(Fdata2,aes(x=value)) +
  #   stat_density(geom='line')+
  #   geom_histogram(
  #     aes(y=..density..,fill=sig),
  #     colour='black',
  #     bins=50
  #     ) +
  #   scale_fill_manual(values=c('transparent','green'))
  
  # 
  # balvectest = BalTrans(40,1,length(Group))
  # distfunctest = c(dnorm,qnorm,rnorm)
  # muvectest = sapply(
  #   seq(MuMin,MuMax,along.with=Group),
  #   function(y) MuTrans(.85,y)
  # )
  # varvectest =  sapply(
  #   seq(VarMin,VarMax,along.with=Group),
  #   function(y) VarTrans(.85,y)
  # )
  # 
  # 
  # SampledData2=
  #     replicate(
  #       Iterations,
  #       expr = sapply(
  #         Group,
  #         function(a) replicate(
  #           balvectest[[a]],
  #           expr = muvectest[[a]] + 0 + distfunctest[[3]](1,0,varvectest[[a]])
  #         )
  #       ) %>% unlist %>% as_tibble %>% unlist
  #     ) %>% as_tibble %>% mutate(Grouping = rep(c(1,2,3,4),balvectest))
  # 
  # ggplot(data = SampledData2, aes(x=V1,y=Grouping,colour=Grouping %>% as.factor)) +
  #   geom_jitter(position=position_jitter(w=0,h=0.1)) +
  #   scale_colour_manual(values=Palette) +
  #   guides(colour=F)

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

