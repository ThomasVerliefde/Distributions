# Title: Distribution
# Author: Thomas Verliefde
# Date: 2018/04/19
# Version: 1.00
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# INSTALLS ALL NECESSARY PACKAGES

list.of.packages = c("shiny","ggplot2","tidyr","dplyr","shinyjs","magrittr",'cowplot','sn');new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];if(length(new.packages)){install.packages(new.packages,repos="http://cran.us.r-project.org")};lapply(list.of.packages,require,character.only=T);rm(list.of.packages,new.packages)



# Define UI
ui <- fluidPage(
  # Enables the use of shinyjs-package functions (e.g. disabled)
  useShinyjs(),
  
  fluidRow(
    column(
      4,
      # Select which distribution to use for the theoretical distributions
      selectInput(
        'distribution',NULL,
        c('Normal Distribution'='Norm','Skew-Normal Distribution'='SN')
      ),
      style="position:relative;
      margin-top:10px;
      font-size:115%;
      font-family:Lucida Console, sans-serif"
    ),
    column(
      6,
      # The title
      HTML('F-Distribution & Violations of Assumptions'),
      style="position:relative;
            margin-top:8px;
            font-family:Lucida Console, sans-serif;
            font-weight:500;
            font-variant: small-caps;
            font-size:200%;"
    ),
    column(
      1,
      # The sample button.
      # Pressing this will resample with the chosen variable values.
      actionButton('sample','Sample',icon('refresh'),
                   style="position:relative;
                   font-size:115%;
                   margin-top:10px;
                   font-family:Lucida Console, sans-serif;
                   ")
      )
  ),
  fluidRow(
    # The 3 plots displayed
    # distplot shows the theoretical plots
    # sampleplot shows the sampled data
    # fplot shows the calculated F-distribution with theoretical overlay
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
      # This changes the appearance of the sliders (.irs elements)
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
                # .js-irs-2 .irs-min {visibility:visible !important;
                #                     left:0px;background:#ffffff;
                #                     font-size:80%;}
                # .js-irs-2 .irs-max {visibility:visible !important;
                #                     background:#ffffff;
                #                     font-size:80%;}
                .js-irs-5 .irs .irs-max:after {content:'Unbalanced';}
                .js-irs-5 .irs .irs-min:after {content:'Balanced';}
                .js-irs-0 .irs .irs-max:after {content:'Heterogeneity';}
                .js-irs-0 .irs .irs-min:after {content:'Homogeneity';}
                .js-irs-1 .irs .irs-max:after {content:'Heteroscedasticity';}
                .js-irs-1 .irs .irs-min:after {content:'Homoscedasticity';}
                .js-irs-7 .irs .irs-max:after {content:'Dependence';}
                .js-irs-7 .irs .irs-min:after {content:'Independence';}
                .irs-min:after {visibility: visible !important;}
                .irs-max:after {visibility: visible !important;}
                .irs-all {top:-10px;}
                "),
      # Slider indicating group differences on mu/mean/location
      sliderInput(
        'mu',
        HTML("Group Diff: &mu;"),
        min=-1,
        max=-.01,
        value=-.95,
        step=.01
      ),
      # Slider indicating group differences on sigma/variance/scale
      sliderInput(
        'var',
        HTML('Group Diff: &sigma;²'),
        min=-1,
        max=-.01,
        value=-1,
        step=.01
      ),
      # Slider indicating the amount of groups
      # This slider is not constructed or tested to work with any other value than 4
      # As such, this slider is disabeld, and only used as an indicator.
      sliderInput(
        'groups',
        'Groups',
        min=2,
        max=6,
        value=4,
        step=1
      ) %>% disabled # '%>% disabled' makes this slider not clickable
      ),
    column(
      4,
      # Slider indicating how many times a group of samples is drawn.
      # Setting this low will lead to unreliable results.
      sliderInput(
        'iter',
        'Iterations',
        min=10,
        max=500,
        value=250,
        step=10
      ),
      # Slider indicating the total sample size of 1 iteration of samples.
      sliderInput(
        'samplesize',
        'Sample Size (n)',
        min=16,
        max=100,
        value=40,
        step=4
      ),
      # Slider indicating how balanced the groups are.
      # At value = 1 (default), all groups are balanced and have an equal amount of samples.
      # At value = 0.01 (minimum), sample sizes differ greatly between groups.
      sliderInput(
        'balance',
        'Group Diff: n',
        min=-1,
        max=-.01,
        value=-1,
        step=.01
      )
    ),
  column(
    4,
    # Checkbox to show the non-central F-distribution
    # The non-centrality-parameter (lambda) is based on the group difference on mu/location.
    # Note that I'm using checkboxGroupInput, instead of checkboxInput.
    # The first returns the 'choiceValues', while the latter returns TRUE/FALSE.
    # Getting values makes it easier to use the 'switch()' function.
    checkboxGroupInput(
      'noncentral',NULL,
      choiceNames=list(HTML('<b>Show Non-Central F-Dist?</b>')),
      choiceValues=list('noncentral')
    ),
    # Slider indicating the significance level
    # This slider should technically work with different values.
    # But this has not been tested, and was not the aim.
    # As such, this slider is disabled, and forms merely an indicator.
    sliderInput(
      'alpha',
      HTML('Significance: &alpha;'),
      min=0,
      max=1,
      value=.95,
      step=.01
    ) %>% disabled,
    checkboxInput(
      'enableDependence',NULL,
      label=HTML('<b>Enabling Dependence - Lock Balance</b>'),
      value=FALSE
    ),
    # Slider indicating the level of interdependence.
    # Not implemented (yet).
    sliderInput(
      'dependence',
      'Dependence',
      min=0,
      max=1,
      value=0,
      step=.01
    ) %>% disabled
  )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Observing the enableDependence checkbox.
  # If TRUE (checked), then it resets the balance slider
  # If FALSE (unchecked), then it resets the dependence slider
  observe(
    if(input$enableDependence) {
      reset('balance')
    } else if(input$enableDependence %>% not) {
      reset('dependence')
    }
  )
  
  # These two lines enable or disable the 'dependence' and 'balance' sliders.
  # This is conditional on enableDependence.
  # If TRUE (checked), then balance is disabled, and dependence is enabled.
  # If FALSE (unchecked), then balance is enabled, and dependence is disabled.
  observe(toggleState('dependence', input$enableDependence))
  observe(toggleState('balance', input$enableDependence %>% not))

  # Reactive switch
  # Based on the selected distribution,
  # it will select a list of functions to be used.
  # Note that currently, this serves little value,
  #  as for both 'Norm' and 'SN' we are using the same
  #  functions.
  # It could be extended later.
  DistFunc=reactive({
    switch(input$distribution,
           'Norm' = c(dsn,qsn,rsn,df,qf),
           'SN' = c(dsn,qsn,rsn,df,qf)
    )}
  )

  # Colour palette that will be used for the different groups.
  # One of the reasons the amount of groups only works for 4.
  Palette = c('#e66101','#fdb863','#b2abd2','#5e3c99')
  # Used in DataDist()
  Lims=c(.01,.99)
  # sequence of groups, should be c(1,2,3,4), as input$groups is set at 4.
  Group=isolate(seq(input$groups))
  # MuMin, MuMax are the minimum and maximum values for mu.
  # If input$mu (difference between groups on mu) is at abs(0.01),
  #  1 group should be at -1, and another group at +1.
  MuMin=-1
  MuMax=1
  # VarMin, VarMax are the minimum and maximum for variance.
  # If input$var (difference between groups on var) is at abs(0.01),
  #  1 group should be at 1, and another group at 10.
  VarMin=1
  VarMax=10
  # Df1, Df2 determine the degrees of freedom for the F-distributions
  Df1=isolate(input$groups-1)
  Df2=isolate(input$samplesize-input$groups)
  # Non-centrality parameter, changes with group difference on mu (input$mu)
  Lambda = reactive(input$samplesize*0.39*(1-abs(input$mu)))
  # Skewness factor (called alpha by the sn-distribution functions e.g. sn::dsn)
  # Set to 4 if the Skew-normal Distribution is selected, otherwise is at 0.
  Skew = reactive({
    switch(
      input$distribution,
      'Norm' = 0,
      'SN' = 4
    )}
  )
  
  # Transformation function for mu, to make the 0.01-1 scale function
  MuTrans = function(x,y) {
    output = (1-x)*y
    return(output)
  }
  
  # Transformation function for var, to make the 0.01-1 scale function
  VarTrans = function(x,y) {
    output = y^(1-x)
    return(output)
  }
  
  # Transformation function for balance, to make the 0.01-1 scale function
  BalTrans = function(x,y,z) {
    output = (x / z) %>% {seq((.^y),(.*2 - .^y), length.out = z )} %>% round
    return(output)
  }
  
  # Create a vector of means, 1 for each group
  MuVec = reactive(
    sapply(
      seq(MuMin,MuMax,along.with=Group),
      function(y) MuTrans(abs(input$mu),y)
    )
    )
  
  # Create a vector of variances, 1 for each group
  VarVec = reactive(
    sapply(
      seq(VarMin,VarMax,along.with=Group),
      function(y) VarTrans(abs(input$var),y)
    )
  )
  
  # Create a vector of sample sizes, 1 for each group
  BalVec = reactive(
    BalTrans(input$samplesize,abs(input$balance),length(Group))
  )

  # Find the minimum and maximum values based on the q-plot of the chosen distribution
  # This is used as the minimum and maximum values of the x-axis
  #  for the theoretical plot.
  DataDist=reactive({
    tibble(
      x = sapply(
        Group,
        function(x) DistFunc()[[2]](Lims,MuVec()[[x]],VarVec()[[x]])
      ) %>% {c(min(.),max(.))}
    )
  })
  
  # Create the distplot, showing the theoretical plots.
  # There are 4 stat_function plots, 1 for each group.
  output$distplot = renderPlot(
    ggplot(DataDist(),aes(x=x)) +
      sapply(
        Group,
        function(x) stat_function(
          fun=DistFunc()[[1]],n=101,args=list(MuVec()[[x]],VarVec()[[x]],alpha=Skew()),
          colour=Palette[x])
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
  
  # This is the system that computes the sampled data.
  # It is only reactive on 'input$sample'.
  # Normally, something like this should also work with
  #  eventReactive(), but I didn't get it to work, so I got a workaround.
  SampledData =
    reactive(
      isolate(
        replicate(
          input$iter,
          expr = sapply(
            Group,
            function(a) replicate(
              BalVec()[[a]],
              expr = MuVec()[[a]] + DistFunc()[[3]](1,0,VarVec()[[a]])
            )
          ) %>%
            # This part adds a dependence value to all values in a row (over groups).
            # Note that if the dependence == 0, this value will always be 0.
            # The arbitrary factor 3 is to make sure that the effect is noticable.
            apply(
              1,
              function(b) {
                add(
                  b,
                  DistFunc()[[3]](1,0,input$dependence*3)
                )
              }
            ) %>% t %>% unlist %>% as_tibble %>% unlist
        ) %>% as_tibble %>% mutate(Grouping = rep(Group,BalVec()) %>% as.factor)
      ) %T>% {input$sample}
    )
  
  # This creates a list of plots showing the sampled data.
  # This will be used in the output$sampleplot.
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
  
  # Creates the output object for the sampled plots.
  # By utilizing ListPlots, it makes it easier to insert ... after 3.
  # This is also the only place (I think) that needs the cowplot package.
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
  
  # Computing the relevant F-distribution data
  FData = reactive(
    SampledData() %>%
      summarize_at(
        vars(-Grouping),
        function(x) aov(x ~ Grouping,data=.) %>%
          summary %>% unlist(recursive=F) %$% `F value`[[1]]
      ) %>% gather
  )
  
  # FFunc is code for either a central F-distribution, or
  #  for a noncentral F-distribution, depending on the checkbox earlier.
  # The main difference is the addition of ncp=Lambda.
  # This would probably also work the other way around, by changing Lambda
  #  based on the checkbox.
  FFunc=reactive({
    if(is.null(input$noncentral)) {
      list(
        "stat_function(
        fun=DistFunc()[[4]],n=101,
        args=list(df1=Df1,df2=Df2),colour='red1')",
        "stat_function(geom='density',
        fun=DistFunc()[[4]],n=101,
        args=list(df1=Df1,df2=Df2),fill='red1',colour='transparent',
        xlim=c(DistFunc()[[5]](input$alpha,Df1,Df2),max(FData()$value)),
        alpha=.5)",
        "labs(x=NULL,y=NULL,title='F-Distribution')"
      )
    } else {
      list(
        "stat_function(
        fun=DistFunc()[[4]],n=101,
        args=list(df1=Df1,df2=Df2,ncp=Lambda()),colour='red1')",
        "stat_function(geom='density',
        fun=DistFunc()[[4]],n=101,
        args=list(df1=Df1,df2=Df2,ncp=Lambda()),fill='red1',colour='transparent',
        xlim=c(DistFunc()[[5]](input$alpha,Df1,Df2,Lambda()),max(FData()$value)),
        alpha=.5)",
        "labs(x=NULL,y=NULL,title='Non-Central F-Distribution')"
      )
    }
  })

  # Creates the third plot, the f-distribution sampled histogram,
  #  with overlay of the theoretical F-distribution.
  # These overlayplots can be found in FFunc()[1] and FFunc()[2].
  output$fplot = renderPlot(
    ggplot(FData(),aes(x=value)) +
      geom_histogram(aes(y=..density..),
                     bins=50,
                     fill='transparent',
                     colour='black') +
      eval(parse(text=FFunc()[1])) +
      eval(parse(text=FFunc()[2])) +
      eval(parse(text=FFunc()[3])) +
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

}

# Run the application 
shinyApp(ui = ui, server = server)

