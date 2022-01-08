library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(chron)
library(lubridate)
library(ggplot2)


fires <- read_csv("final_fires.csv")

# Styling
font = list(
  # sans = "Helvetica",
  # family = "sans",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "white",
  bordercolor = "transparent",
  font = font
)

### ------------- New df's for graphs -------------------------

freq <- fires %>%
  group_by(FIRE_YEAR) %>%
  count()

# For Choropleth
mx_fire_sz <- fires %>%
  group_by(FIRE_YEAR, STATE) %>% 
  summarise(incidents = n(),
            time = which.max(time_taken),
            sz = FIRE_SIZE[time]) %>%
  mutate(hover = paste0(STATE, "\n$", "Number of fire incident ->",
                        incidents,"\n$", "Fire Size ->",
                        sz, "acres\n$", "Time taken -> ", time, "hours"))

inc_by_state <- fires %>%
  group_by(STATE) %>%
  summarise(incidents = n()) %>%
  arrange(incidents)

by_month <- fires %>% 
  mutate(mt = factor(format(mdy_hm(discovery_date_time), '%B'), levels=month.name)) %>%
  group_by(mt) %>%
  count() %>%
  arrange(mt)

human <- c(
  "Arson", "Campfire", "Children", "Equipment Use", "Fireworks", "Debris Burning", "Smoking"
)

natural <- c(
  "Lightning", "Powerline", "Structure", "Railroad"
)

'%!in%' <- function(x,y)!('%in%'(x,y))

fire_filt_cs <- fires %>%
              filter(STAT_CAUSE_DESCR %!in% c("Miscellaneous","Missing/Undefined"))

fire_filt <- fire_filt_cs %>% 
                mutate(cause_mode=if_else(STAT_CAUSE_DESCR %in% human, "Human", "Natural"))

yr_cas <- fire_filt %>%
            group_by(FIRE_YEAR, STAT_CAUSE_DESCR) %>% 
            summarise(
              time = mean(time_taken),
              size = mean(FIRE_SIZE), 
              incident = n()
            )

hvn_cause <- fire_filt %>%
                group_by(FIRE_YEAR, cause_mode) %>% 
                summarise(
                  time = mean(time_taken),
                  size = mean(FIRE_SIZE),
                  incident = n()
                )

cause_ls <- c(unique(fire_filt$STAT_CAUSE_DESCR))

# Funs ---------------
year_plot <- function(){
  plot_ly(fires,
          x = ~freq$FIRE_YEAR,
          y = ~ freq$n,
          type='scatter',
          mode='lines') %>% 
    layout(
      title=list(text='Fire Incidents over past years'),
      xaxis=list(
        tickmode='linear',
        title="Year",
        tickangle = -90
      ),
      yaxis=list(
        title = "Number of Fire Incidents"
      )
    ) %>%
    config(displayModeBar=FALSE)
}

state_plot <- function(){
  plot_ly(inc_by_state,
          x = ~reorder(STATE, incidents),
          y = ~incidents,
          type='bar',
          color=I('burlywood')
  ) %>%
    layout(
      title="Number of Fire incidents per State",
      xaxis=list(title="State"),
      yaxis=list(title="Number of Incidents")
    ) %>%
    config(displayModeBar=FALSE)
}

plotType <- function(type){
  switch(type,
         Year = year_plot(),
         State = state_plot())
}

time_plot <- function(){
    ggplot(hvn_cause, aes(x=FIRE_YEAR, y=time))+
      geom_line(aes(color=cause_mode))+
      xlab("Years")+
      ylab("Average time taken")%>%
      config(displayModeBar=FALSE)
}

size_plot <- function(){
    ggplot(hvn_cause, aes(x=FIRE_YEAR, y=size))+
      geom_line(aes(color=cause_mode))+
      xlab("Years")+
      ylab("Average Size of fires")%>%
      config(displayModeBar=FALSE)
}

incident_plot <- function(){
    ggplot(hvn_cause, aes(x=FIRE_YEAR, y=incident))+
      geom_line(aes(color=cause_mode))+
      xlab("Years")+
      ylab("Number of Incidents")%>%
      config(displayModeBar=FALSE)
}

plotType2 <- function(type){
  switch(type,
         Time = time_plot(),
         Size = size_plot(),
         Incident = incident_plot(),
         )
}

##Function to plot freq of based on State filter
state_filter <- function(state){
  new <- filter(fires,STATE==state) %>%
    group_by(FIRE_YEAR) %>%
    count()
  
  plot_ly(fires,
          x = ~new$FIRE_YEAR,
          y = ~ new$n,
          type='scatter',
          mode='lines'
      )%>%
        layout(
          title=list(text=str_glue('Frequency distribution of <b> {state} </b> over time')),
          xaxis=list(
            tickangle=-90,
            title="YEARS",
            range=c(1992, 2015),
            tickmode='linear'
          ),
          yaxis=list(
            title="Frequency"
          )
        )%>%
          config(displayModeBar=FALSE)
}

#Function to plot freq based in Year filter 
year_filter <- function(year){
  new_year <- filter(fires,FIRE_YEAR==year) %>%
    group_by(STATE) %>%
    summarise(incidents= n()) %>%
    arrange(incidents)
  
  plot_ly(new_year,
          x = ~reorder(STATE, incidents),
          y = ~ incidents,
          type='bar',
          color=I('burlywood')
        )%>%
        layout(
          title=list(text=str_glue('Frequency distribution of incidents across States during <b> {year} </b>')),
          xaxis=list(
            tickangle=90,
            title="STATES"
          ),
          yaxis=list(
            title="Frequency"
          )
        )%>%
        config(displayModeBar=FALSE)
}

fire_cause <- function(year, cause){
  new_year <- filter(fires,FIRE_YEAR==year, STAT_CAUSE_DESCR == cause) %>%
    group_by(STATE) %>%
    summarise(incidents= n()) %>%
    arrange(incidents)
  
  plot_ly(new_year,
          x = ~reorder(STATE, incidents),
          y = ~ incidents,
          type='bar',
          color=I('burlywood')
  )%>%
    layout(
      title=list(text=str_glue('Total incidents of <b>{cause}</b> for <b>{year}</b>')),
      xaxis=list(
        tickangle=90,
        title="STATES"
      ),
      yaxis=list(
        title="Frequency"
      )
    )%>%
    config(displayModeBar=FALSE)
}

cause_check <- function(causes){
  yr_cas_an <- yr_cas %>%
                filter(STAT_CAUSE_DESCR %in% causes)

  plot_ly(fire_filt,
      x=~yr_cas_an$FIRE_YEAR,
      y=~yr_cas_an$incident,
      color=~yr_cas_an$STAT_CAUSE_DESCR,
      type='scatter',
      mode='lines'
    )%>%
    layout(
      title="Time taken to control fire for causes",
      xaxis=list(
        tickangle=-90,
        title="YEARS",
        range=c(1992, 2015),
        tickmode='linear'
      ),
      yaxis=list(
            title="Number of Incident"
          )
    ) %>%
    config(displayModeBar=FALSE)
}


# UI
ui <- dashboardPage(
  skin = "yellow",
  # tags$head(tags$style(".rightAlign{float:right;}")),
  dashboardHeader(title = "Wild Fire in US From 1992 to 2015"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Choropleth", tabName = "choropleth"),
      menuItem("State level", tabName = "stateLevel"),
      menuItem("Causes of Fire",tabName="causefire")
      # menuItem("Time taken for fires",tabName="times")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("choropleth",
              plotlyOutput("maps"),
      ),
      tabItem("stateLevel",
              
              box(box(plotlyOutput("incident"), width=10),
                  box(radioButtons('yearstate', "Select Mode", choices=c("Year", "State")), style="align:right", width=2), width=12),
              
              box(box(selectInput("state","STATE:",
                              c(unique(fires$STATE))),style="align:right",width=2),
                  box(plotlyOutput("State"), width=10, style='align:left'), width=12),

              box(box(selectInput("year","YEAR:",
                              c(sort(unique(fires$FIRE_YEAR)))),style="align:right",width=2),
                  box(plotlyOutput("years"), width=10, style='align:left'), width=12)
      ),
      tabItem("causefire",
            h3("All Causes"),
              box(
                  box(plotlyOutput("time_to_size"), width=9),
                  box(
                    actionLink("selectall","Select All") ,
                    checkboxGroupInput("cause_select", "Filter by cause:", cause_ls), width=3), 
              width=12
              ),
              h3("Human vs natural Causes"),
              box(
                box(plotlyOutput("hvnCause"), width=10),
                box(radioButtons('cse', "Select Mode", choices=c("Time", "Size", "Incident")), style="align:right", width=2), 
                width=12
              ),
              h3("Filter by Year and Cause"),
              box(box(box(selectInput("year2","YEAR:",
                                  c(sort(unique(fires$FIRE_YEAR)))),style="align:right",width=6),
                      box(selectInput("cause","Cause of Fire:",
                                      c(sort(unique(fires$STAT_CAUSE_DESCR)))),style="align:right",width=6),
                      width=12),
                  box(plotlyOutput("causes"), width=12, style='align:left'),
              width=12)
      )
    ) 
  )
)

# Server
server <- function(input, output, session){
  output$maps <- renderPlotly({
    plot_geo(mx_fire_sz, 
             locationmode = 'USA-states',
             frame = ~FIRE_YEAR) %>%
      add_trace(locations = ~STATE,
                z = ~time,
                zmin=0,
                zmax=max(mx_fire_sz$time),
                color = ~time,
                colorscale='Viridis',
                text= ~hover,
                hoverinfo='text') %>%
      layout(geo = list(scope = 'usa'),
             title = "Max Time taken to Control fire for each state\n1992 - 2015") %>%
      config(displayModeBar=FALSE)%>%
      style(hoverlabel = label) 
  })
  output$incident <- renderPlotly({
    plotType(input$yearstate)
  })
  output$State <- renderPlotly({
    state_filter(input$state)
  })
  output$years <- renderPlotly({
    year_filter(input$year)
  })
  # observe({
  #   updateCheckboxGroupInput(
  #     session, 'cause_select', choices=cause_ls,
  #     selected = if(input$selectall) cause_ls
  #   )
  # })
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"cause_select","Choose cause(s):",choices=cause_ls)
    }
    else
    {
      updateCheckboxGroupInput(session,"cause_select","Choose cause(s):",choices=cause_ls,selected=cause_ls)
    }
  })
  output$time_to_size <- renderPlotly({
    cause_check(input$cause_select)
  })
  output$causes <- renderPlotly({
    fire_cause(input$year2, input$cause)
  })
  output$hvnCause <- renderPlotly({
      plotType2(input$cse)
  })
}

shinyApp(ui, server)

hvn_cause[is.null(hvn_cause$cause_mode)]
hvn_cause[is.null(hvn_cause$cause_mode),]
