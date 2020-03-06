#----------------- Packages -----------------
library(shiny)
library(bs4Dash)
library(mapproj)
library(TSstudio)
library(dplyr)
library(tidyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggmap)
#install.packages("googleAuthR")

#----------------- Functions -----------------
`%>%` <- magrittr::`%>%`

top10<- function(){
    ddff<-coronavirus %>% 
        filter(date == max(date)) %>%
        select(country = Country.Region, type, cases) %>%
        group_by(country, type) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    
    ddff<- ddff[1:10,]
    m<-barplot(ddff$confirmed, names.arg = ddff$country, horiz = T, las=2,   border = NA , col = c('Gold','Blue','Red','Grey'))+
        mtext(text = "Top 10 Countries with highest cases of Corona Virus", line = 1, side = 3, cex = 1.2)
    return(m)
}

plotBar<- function(){
    ddff<-getLatestStat()
    
    ddff<- ddff[1:5,]
    fig <- plot_ly(ddff, x = ~Country, y = ~confirmed, type = 'bar', 
                   marker = list(color = c('Red','Gold','Green','blue','Grey'),
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5)))
    fig <- fig %>% layout(title = "Confirmed Cases in Top 5 Countries",
                          xaxis = list(title = ""),
                          yaxis = list(title = ""))
    
    fig
}
## Loading and Wrangling Corona Virus Data from John Hopkins University Research

getCoronaData <- function(){
    
    # Setting functions
    `%>%` <- magrittr::`%>%`
    #----------------------------------------------------
    # Pulling confirmed cases
    
    raw_conf <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
                         stringsAsFactors = FALSE)
    # Transforming the data from wide to long
    # Creating new data frame
    df_conf <- raw_conf[, 1:4]
    
    for(i in 5:ncol(raw_conf)){
        print(i)
        raw_conf[,i] <- as.integer(raw_conf[,i])
        raw_conf[,i] <- ifelse(is.na(raw_conf[, i]), 0 , raw_conf[, i])
        
        if(i == 5){
            df_conf[[names(raw_conf)[i]]] <- raw_conf[, i]
        } else {
            df_conf[[names(raw_conf)[i]]] <- raw_conf[, i] - raw_conf[, i - 1]
        }
    }
    
    
    df_conf1 <-  df_conf %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                                 names_to = "date_temp",
                                                 values_to = "cases_temp")
    
    # Parsing the date
    df_conf1$month <- sub("X", "",
                          strsplit(df_conf1$date_temp, split = "\\.") %>%
                              purrr::map_chr(~.x[1]) )
    
    df_conf1$day <- strsplit(df_conf1$date_temp, split = "\\.") %>%
        purrr::map_chr(~.x[2])
    
    
    df_conf1$date <- as.Date(paste("2020", df_conf1$month, df_conf1$day, sep = "-"))
    
    # Aggregate the data to daily
    df_conf2 <- df_conf1 %>%
        dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
        dplyr::summarise(cases = sum(cases_temp)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(type = "confirmed",
                      Country.Region = trimws(Country.Region),
                      Province.State = trimws(Province.State))
    
    head(df_conf2)
    tail(df_conf2)
    #----------------------------------------------------
    # Pulling death cases
    
    raw_death <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
                          stringsAsFactors = FALSE)
    # Transforming the data from wide to long
    # Creating new data frame
    df_death <- raw_death[, 1:4]
    
    for(i in 5:ncol(raw_death)){
        print(i)
        raw_death[,i] <- as.integer(raw_death[,i])
        raw_death[,i] <- ifelse(is.na(raw_death[, i]), 0 , raw_death[, i])
        
        if(i == 5){
            df_death[[names(raw_death)[i]]] <- raw_death[, i]
        } else {
            df_death[[names(raw_death)[i]]] <- raw_death[, i] - raw_death[, i - 1]
        }
    }
    
    
    df_death1 <-  df_death %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                                   names_to = "date_temp",
                                                   values_to = "cases_temp")
    
    # Parsing the date
    df_death1$month <- sub("X", "",
                           strsplit(df_death1$date_temp, split = "\\.") %>%
                               purrr::map_chr(~.x[1]) )
    
    df_death1$day <- strsplit(df_death1$date_temp, split = "\\.") %>%
        purrr::map_chr(~.x[2])
    
    
    df_death1$date <- as.Date(paste("2020", df_death1$month, df_death1$day, sep = "-"))
    
    # Aggregate the data to daily
    df_death2 <- df_death1 %>%
        dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
        dplyr::summarise(cases = sum(cases_temp)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(type = "death",
                      Country.Region = trimws(Country.Region),
                      Province.State = trimws(Province.State))
    
    head(df_death2)
    tail(df_death2)
    #----------------------------------------------------
    # Pulling recovered cases
    
    raw_rec <- read.csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",
                        stringsAsFactors = FALSE)
    # Transforming the data from wide to long
    # Creating new data frame
    df_rec <- raw_rec[, 1:4]
    
    for(i in 5:ncol(raw_rec)){
        print(i)
        raw_rec[,i] <- as.integer(raw_rec[,i])
        raw_rec[,i] <- ifelse(is.na(raw_rec[, i]), 0 , raw_rec[, i])
        
        if(i == 5){
            df_rec[[names(raw_rec)[i]]] <- raw_rec[, i]
        } else {
            df_rec[[names(raw_rec)[i]]] <- raw_rec[, i] - raw_rec[, i - 1]
        }
    }
    
    
    df_rec1 <-  df_rec %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                               names_to = "date_temp",
                                               values_to = "cases_temp")
    
    # Parsing the date
    df_rec1$month <- sub("X", "",
                         strsplit(df_rec1$date_temp, split = "\\.") %>%
                             purrr::map_chr(~.x[1]) )
    
    df_rec1$day <- strsplit(df_rec1$date_temp, split = "\\.") %>%
        purrr::map_chr(~.x[2])
    
    
    df_rec1$date <- as.Date(paste("2020", df_rec1$month, df_rec1$day, sep = "-"))
    
    # Aggregate the data to daily
    df_rec2 <- df_rec1 %>%
        dplyr::group_by(Province.State, Country.Region, Lat, Long, date) %>%
        dplyr::summarise(cases = sum(cases_temp)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(type = "recovered",
                      Country.Region = trimws(Country.Region),
                      Province.State = trimws(Province.State))
    
    head(df_rec2)
    tail(df_rec2)
    
    coronavirus <- dplyr::bind_rows(df_conf2, df_death2, df_rec2) %>%
        dplyr::arrange(date) %>% dplyr::ungroup() %>%
        dplyr::filter(cases != 0)
    head(coronavirus)
    tail(coronavirus)
    
    
    
    #usethis::use_data(coronavirus, overwrite = TRUE)
    save(coronavirus, file = 'coronavirus.rda')
    
    write.csv(coronavirus, "coronavirus_dataset.csv", row.names = FALSE)
    #writexl::write_xlsx(x = coronavirus, path = "data/coronavirus_dataset.xlsx", col_names = TRUE)
    return(coronavirus)
    
}

#Returns country specific data
getCountrydata<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, Lat, Long) %>%
        group_by(State, type,Lat, Long) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    countryD$confirmed[is.na(countryD$confirmed)]<-0
    countryD$death[is.na(countryD$death)]<-0
    countryD$recovered[is.na(countryD$recovered)]<-0
    return(countryD)
}

#Returns Country data with date
getCountrydataWD<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, date) %>%
        group_by(State, type,date) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    return(countryD)
}
getCountrydataWDA<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, date) %>%
        group_by(State, type,date) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    countryD<- countryD[rev(order(as.Date(countryD$date))),]
    return(countryD[1:30,])
}

getLatestStat<- function(){
    f<- coronavirus %>% select(Country = Country.Region, type, cases) %>% group_by(Country, type) %>% summarize(totalCases = sum(cases, na.rm = T)) %>%
        pivot_wider(names_from = type, values_from = totalCases)%>% arrange(-confirmed)
    f$confirmed[is.na(f$confirmed)]<-0
    f$death[is.na(f$death)]<-0
    f$recovered[is.na(f$recovered)]<-0
    
    return(f)
}

getLatestStat5<- function(){
    f<- coronavirus %>% select(Country = Country.Region,State = Province.State, type, cases, date) %>% group_by(Country,State, type, date) %>% summarize(totalCases = sum(cases, na.rm = T)) %>%
        pivot_wider(names_from = type, values_from = totalCases) 
    f<- f[rev(order(as.Date(f$date))),]
    return(f[1:50,])
}


register_google(key = "AIzaSyC0jJJJcidZm4d7T-_HsSYXTkcvb8IxNaY")
getCountrydata<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, Lat, Long) %>%
        group_by(State, type,Lat, Long) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    return(countryD)
}

tsPlot<- function(){
    d<- coronavirus %>% select( type, cases, date) %>% group_by( type, date) %>% summarize(totalCases = sum(cases, na.rm = T)) %>%
        pivot_wider(names_from = type, values_from = totalCases)%>% arrange(-confirmed) 
    d$confirmed[is.na(d$confirmed)]<-0
    d$recovered[is.na(d$recovered)]<-0
    d$death[is.na(d$death)]<-0
    d
    
    ggplot(data = d, mapping = aes(date,confirmed))+
        geom_line(data = d, mapping = aes(date,death, col='Death', label = "death"))+
        geom_point(data = d, mapping = aes(date,death, col='Death', label = 'death'), alpha= 0.2)+
        geom_line(data = d, mapping = aes(date,confirmed, col='Confirmed'))+
        geom_point(data = d, mapping = aes(date,confirmed, col='Confirmed'), alpha= 0.2)+
        geom_line(data = d, mapping = aes(date,recovered, col='Recovered'))+
        geom_point(data = d, mapping = aes(date,recovered, col='Recovered'), alpha= 0.2)+
        xlab("Date") + ylab("Cases") 
    
}

tsPlotCountry<- function(country){
    d<- coronavirus %>% filter(Country.Region == country) %>% select( type, cases, date) %>% group_by( type, date) %>% summarize(totalCases = sum(cases, na.rm = T)) %>%
        pivot_wider(names_from = type, values_from = totalCases)%>% arrange(-confirmed) 
    d$confirmed[is.na(d$confirmed)]<-0
    d$recovered[is.na(d$recovered)]<-0
    d$death[is.na(d$death)]<-0
    d
    ggplot(data = d, mapping = aes(date,confirmed))+
        geom_line(data = d, mapping = aes(date,death, col='Death', label = "death"))+
        geom_point(data = d, mapping = aes(date,death, col='Death', label = 'death'), alpha= 0.2)+
        geom_line(data = d, mapping = aes(date,confirmed, col='Confirmed'))+
        geom_point(data = d, mapping = aes(date,confirmed, col='Confirmed'), alpha= 0.2)+
        geom_line(data = d, mapping = aes(date,recovered, col='Recovered'))+
        geom_point(data = d, mapping = aes(date,recovered, col='Recovered'), alpha= 0.2)+
        xlab("Date") + ylab("Cases")
}

loadMap<- function(country){
    df<- getCountrydata(country)
    #lon <- df$Long
    #lat <- df$Lat
    #df <- as.data.frame(cbind(lon,lat))
    
    # getting the map
    register_google(key = "AIzaSyC0jJJJcidZm4d7T-_HsSYXTkcvb8IxNaY")
    mapG <- get_map(location = c(lon = mean(df$Long), lat = mean(df$Lat)), zoom = 4,
                    maptype = "roadmap", scale = 2)
    
    # plotting the map with some points on it
    mapMe<-ggmap(mapG) +
        geom_point(data = df, aes(x=Long, y=Lat, fill='black',size=confirmed, color=confirmed, text=State, alpha=confirmed), shape = 21) +
        scale_size_continuous(range=c(1,30)) +
        scale_color_viridis(option="inferno", trans="log" ) +
        scale_alpha_continuous(trans="log") +
        theme_void() +
        
        theme(legend.position = "right")
        guides(fill=FALSE, alpha=FALSE, size=FALSE)
    mapMe
}
library(plotly)
library(viridis)
loadMapMe<- function(country){
    df<- getCountrydata(country)
    #lon <- df$Long
    #lat <- df$Lat
    #df <- as.data.frame(cbind(lon,lat))
    
    register_google(key = "AIzaSyC0jJJJcidZm4d7T-_HsSYXTkcvb8IxNaY")
    mapG <- get_map(location = c(lon = mean(df$Long), lat = mean(df$Lat)), zoom = 4,
                    maptype = "roadmap", scale = 2)

    
    # plotting the map with some points on it
    p<-ggmap(mapG) +
        geom_point(data= df, aes(x=Long, y=Lat, size=confirmed, color=confirmed, text=State, alpha=confirmed) ) +
        scale_size_continuous(range=c(1,30)) +
        scale_color_viridis(option="inferno", trans="log" ) +
        scale_alpha_continuous(trans="log") +
        theme_void() +
        
        theme(legend.position = "none")
    
    
    p <- ggplotly(p, tooltip="text")
    p
    
}

getCases<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, Lat, Long) %>%
        group_by(State, type,Lat, Long) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    
    return(sum(countryD$confirmed,na.rm = TRUE))
    
}

getDeath<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, Lat, Long) %>%
        group_by(State, type,Lat, Long) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    
    return(sum(countryD$death, na.rm = TRUE))
    
}

getRecovery<- function(country){
    countryD<- coronavirus %>%
        filter(Country.Region == country) %>%
        select(State = Province.State, type, cases, Lat, Long) %>%
        group_by(State, type,Lat, Long) %>%
        summarise(total_cases = sum(cases)) %>%
        pivot_wider(names_from = type,
                    values_from = total_cases) %>%
        arrange(-confirmed)
    
    return(sum(countryD$recovered, na.rm = TRUE))
    

}



library(plotly)

plotPieC<- function(country){
    
    data<- getCountrydata(country)
    group<- c('Confirmed', 'Recovered','Death')
    value=c(sum(data$confirmed, na.rm = T),sum(data$recovered, na.rm = T),sum(data$death, na.rm = T))
    dataMap <- data.frame(
        group,
        value
    )
    
    # USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
    # data <- USPersonalExpenditure[, c('Categorie', 'X1960')]
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    fig <- plot_ly(dataMap, labels = ~group, values = ~value, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste('$', value, ' billions'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE)
    fig <- fig %>% layout(title = c('Ratio between Confirmed/Recovered/Death Cases in ', country),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
}



#----------------- Data -----------------
coronavirus<- getCoronaData()
#setwd('~/Desktop/Corona/')
#load(file = 'data/coronavirus.rda')
countries<- coronavirus$Country.Region
udf<- coronavirus

ddff<-coronavirus %>% 
    filter(date == max(date)) %>%
    select(country = Country.Region, type, cases) %>%
    group_by(country, type) %>%
    summarise(total_cases = sum(cases)) %>%
    pivot_wider(names_from = type,
                values_from = total_cases) %>%
    arrange(-confirmed)
totalDeaths<-sum(which(coronavirus$type =='death'))
totalRecovery<- sum(which(coronavirus$type =='recovered'))
totalCases <- sum(coronavirus$cases)


#----------------- UI -----------------
ui <- bs4Dash::dashboardPage(
    enable_preloader = FALSE,
    title = "CoronaVirus Dashboard",
    navbar = bs4Dash::dashboardHeader(
        "Corona Virus Details Dashboard",
        titleWidth = 12,
        skin = "light",
        status = NULL,
        border = TRUE,
        sidebarIcon = "bars",
        compact = FALSE,
        controlbarIcon = "th",
        leftUi = NULL,
        rightUi = NULL,
        fixed = FALSE
    ),
    footer = bs4Dash::dashboardFooter(
        "Designed by Ayush (https://ayushpkumar.com)    |      Data retrieved from John Hopkins University(https://github.com/CSSEGISandData/COVID-19)   | ",
        copyrights = NULL,
        right_text = 'March, 2020'
    ),
    #----------------- Sidebar -----------------
    sidebar = bs4Dash::bs4DashSidebar(inputId = NULL,
                                      disable = T,
                                      
                                      skin = "dark",
                                      status = "primary",
                                      brandColor = NULL,
                                      url = NULL,
                                      src = NULL,
                                      elevation = 4,
                                      opacity = 0.8,
                                      expand_on_hover = F),
    
    #----------------- Body -----------------
    body = bs4Dash::dashboardBody(
        bs4Dash::bs4TabSetPanel(
            id = "panel1",
            side = "left",
            #----------------- Tab 1 -----------------
            bs4Dash::bs4TabPanel(
                tabName =  "Summary",
                fluidRow(
                    bs4Dash::infoBox(
                        title = "Total Cases",
                        status = "info",
                        icon = NULL,
                        value = totalCases
                        
                    ),
                    infoBox(
                        title = "Total Deaths",
                        status = "danger",
                        icon = "skull-crossbones",
                        value = as.numeric(totalDeaths)
                        
                    ),
                    infoBox(
                        title = "Total Recovered",
                        gradientColor = "success",
                        icon = NULL,
                        value = totalRecovery
                    )
                ),
                fluidRow(
                    bs4Dash::bs4Card(
                        inputId = "id9",
                        title = "About",
                        closable = FALSE,
                        maximizable = F,
                        width = 5,
                        
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        fluidPage(
                            print("This dashboard uses data from Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
                                    Created to inform people about the spread of corona virus through out world. The data is wrangled using R, and this dashboard is created
                                    using Shiny Dashboard, FlexDashboard and BS4JDash( AdminLTE Themed)
                                  ")
                          
                         #   DT::DTOutput("latest")
                        
                        )
                    ),
                    bs4Dash::bs4Card(
                        inputId = "id9",
                        title = "Latest Reported Case",
                        closable = FALSE,
                        maximizable = TRUE,
                        width = 7,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        fluidPage(
                            DT::DTOutput("latest")
                        )
                    )
                ),
                fluidRow(
                    bs4Dash::bs4Card(
                        inputId = "id2",
                        title = "Top 5 Countries with Maximum Cases",
                        closable = FALSE,
                        maximizable = TRUE,
                        width = 6,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        dropdownIcon = "wrench",
                        plotlyOutput("plot2")),
                    
                    bs4Dash::bs4Card(
                        inputId = "id2",
                        title = "Trend Of Cases",
                        closable = FALSE,
                        maximizable = TRUE,
                        width = 6,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        dropdownIcon = "wrench",
                        plotlyOutput("tsplot"))
                )
            ),
            #----------------- Tab 2 -----------------
            bs4Dash::bs4TabPanel(
                tabName =  "Country Specific",
                plotOutput("distPlot", width="100%", height="100%"
                ),
                fluidRow(
                    bs4Dash::bs4Card(
                        inputId = "id3",
                        title = "Check the Country Specific Data Here",
                        closable = FALSE,
                        maximizable = TRUE,
                        width = 7,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        selectInput("country", "Select Country", choices = unique(countries), selected = "United States"),
                        fluidRow(
                            bs4Dash::infoBoxOutput("Confirmed"),
                            infoBoxOutput("Death"),
                            infoBoxOutput("Recovered"))
                        ,
                        plotly::plotlyOutput("plot3")
                    ),
                    bs4Dash::bs4Card(
                        inputId = "id9",
                        title = "Country Specific",
                        closable = FALSE,
                        maximizable = F,
                        width = 5,
                        
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        fluidPage(
                            print("This page consists of an interactive map which gives an option to select a particular country data. The tables are then populated as different views.
                                  ")
                            
                            #   DT::DTOutput("latest")
                            
                        )
                    ),
                    
                ),
                fluidRow(
                    bs4Dash::bs4Card(
                        inputId = "id9",
                        title = "5 Latest Cases",
                        closable = FALSE,
                        maximizable = F,
                        width = 6,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        fluidPage(
                            DT::DTOutput("countryDD")
                        )
                    ),
                        bs4Dash::bs4Card(
                            inputId = "id12",
                            title = "Trend of Cases in Selected Country",
                            closable = FALSE,
                            maximizable = F,
                            width = 6,
                            status = "dark",
                            solidHeader = FALSE,
                            collapsible = T,
                            
                            dropdownIcon = "wrench",
                            plotlyOutput("tsplotC")
                            )
                        
                    
                ),
                fluidRow(
                    bs4Dash::bs4Card(
                        inputId = "id9",
                        title = "Country Data",
                        closable = FALSE,
                        maximizable = F,
                        width = 6,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        fluidPage(
                            DT::DTOutput("countryD")
                        )
                    ),
                    bs4Dash::bs4Card(
                        inputId = "id9",
                        title = "Distrubution Of Cases",
                        closable = FALSE,
                        maximizable = F,
                        width = 6,
                        status = "dark",
                        solidHeader = FALSE,
                        collapsible = F,
                        
                        dropdownIcon = "wrench",
                        plotlyOutput('piePlot')
                    )
                )
            ),
            
            #----------------- Tab 3 -----------------
            
            
            
            
            bs4Dash::bs4TabPanel(
                tabName =  "Check Summary Data",
                fluidPage(
                    DT::DTOutput("sum")
                )
            ),
            
            #----------- Tab 4 ---------
            bs4Dash::bs4TabPanel(
                tabName =  "Check out the Data",
                fluidPage(
                    DT::DTOutput("coronavirus")
                )
            )
            
            
        )
        
    ),
)



#----------------- Server -----------------
server <- function(input, output, session){
    #----------------- Tab 1 -----------------
    
    output$plot2 <- renderPlotly({
        plotBar()
    })
    
    
    output$latest <- DT::renderDT({
        FF <- getLatestStat5()
        DT::datatable(FF,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))})
    
    output$tsplot<- renderPlotly({
        tsPlot()
    })
    
    #----------------- Tab 2 -----------------
    
    
    
    
    
    output$plot3 <- plotly::renderPlotly({
        
        loadMapMe(input$country)
    })
    output$Confirmed <- renderInfoBox({
        infoBox(
            title = "Confirmed Cases",
            getCases(input$country),
            status = "info",
            icon = shiny::icon("hospitals")
            
        )})
    
    output$Death <- renderInfoBox({
        infoBox(
            title = "Death",
            getDeath(input$country),
            status = "danger",
            icon = shiny::icon("skull-crossbones")
            
        )
    })
    output$Recovered <- renderInfoBox({
        infoBox(
            title = "Recovered",
            getRecovery(input$country),
            status = "success",
            icon = shiny::icon("thumbs-up")
            
        )
    })

    #output$countryD <- DT::renderDT(DT::datatable(getCountrydataWD(input$country)),options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
    output$countryD <- DT::renderDT({
        tmp<- getCountrydataWD(input$country)
        DT::datatable(tmp,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))})
    output$countryDD <- DT::renderDT({
        tmp<- getCountrydataWDA(input$country)
        DT::datatable(tmp,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))})
    output$tsplotC<- renderPlotly({
        tsPlotCountry(input$country)
    })
    
    output$piePlot<-renderPlotly({
        plotPieC(input$country)
    })
    
    
    
    
    #----------------- Tab 3 -----------------
    
    output$sum <- DT::renderDT(DT::datatable(getLatestStat(),options = list(lengthMenu = c(5, 30, 50), pageLength = 50)))
    
    #----------------- Tab 4 -----------------
    
    output$coronavirus <- DT::renderDT(DT::datatable(coronavirus,options = list(lengthMenu = c(5, 30, 50), pageLength = 50)))
    
    
    
    
    
    
}


shinyApp(ui, server)
#rsconnect::showLogs()

