### Dependencies ----

# install.packages(tidyverse)
# install.packages(readxl)
# install.packages(shiny)
library(tidyverse)
library(readxl)
library(shiny)

### Data read in ----

# 1) Setup: sheets with each year's data for individual teams are in sheets 3-83 of the excel file.
#    Put them in a list for storage
team_dflist = rep(list(list()), length(81))
for(i in 3:83){
    team_dflist[[i-1]] = read_xlsx("Data/RushingPassing.xlsx", sheet = i)
}
team_dflist[[1]] = NULL

# 2) We're going to need to bind all these together eventually, 
#    so rename all columns the same,
#    and add a variable for year (so we can distinguish teams between years)
y1 = seq(2012,1932,by = -1)
for(i in 1:81){
    names(team_dflist[[i]]) = c("rank", "team", "points", "pass", "rush")
    team_dflist[[i]]$year = y1[i]
}

# 3) Bind all of the individual team data by year into one dataframe "teams"
teams = do.call(rbind, team_dflist)

# 4) Get rid of all unnecessary rows (those not associated with teams
teams = teams %>% 
    filter(!(rank == "CORRELATION"))

### Add field for number games/year ----

# from http://www.profootballhof.com/news/nfl-regular-season-games-played-per-season/:
# the NFL did not have fixed length seasons prior to 1935. 
# so, we'll only look at data from 1935 on. 
# Number of games per season can be found at the site, we'll just enter it here:
teams = teams %>% filter(year >= 1935)
games = rep(NA, nrow(teams))
games[teams$year >= 1983] = 16
games[teams$year == 1982] = 9
games[teams$year >= 1978 & teams$year <= 1981] = 16
games[teams$year >= 1961 & teams$year <= 1977] = 14
games[teams$year >= 1947 & teams$year <= 1960] = 12
games[teams$year == 1946] = 11
games[teams$year >= 1943 & teams$year <= 1945] = 10
games[teams$year >= 1937 & teams$year <= 1942] = 11
games[teams$year >= 1935 & teams$year <= 1936] = 12
teams$games = games

# Now create per-game fields
teams = teams %>% mutate(Passing = pass/games,
                         Rushing = rush/games)

### Align some team names ----

# Teams have changed names/locations throughout history, we need to to account for that
# The following changes are sourced from the franchise timeline image at 
# https://en.wikipedia.org/wiki/Timeline_of_the_National_Football_League
t = teams$team
t[t == "St. Louis Rams" | t == "Cleveland Rams"] = "Los Angeles Rams"
t[t == "Tennessee Oilers" | t == "Houston Oilers"] = "Tennessee Titans"
t[t == "Los Angeles Raiders"] = "Oakland Raiders"
t[t == "Phoenix Cardinals" | t == "St. Louis Cardinals" | t == "Chicago Cardinals" | t == "Chi/Pit Cards/Steelers"] = "Arizona Cardinals"
t[t == "Baltimore Colts"] = "Indianapolis Colts"
t[t == "Boston Patriots"] = "New England Patriots"
t[t == "San Diego Chargers"] = "Los Angeles Chargers"
t[t == "Phi/Pit Eagles/Steelers"] = "Philadelphia Eagles"
t[t == "Pittsburgh Pirates"] = "Pittsburgh Steelers"
t[t == "Boston Redskins"] = "Washington Redskins"
teams$current = t

# Get rid of teams who have no modern tie (i.e. those who eventually went defunct)
# For our purposes, we only care about the history of MODERN teams that we can trace back
u = unique(teams$team)[1:32]
u[u == "St. Louis Rams"] = "Los Angeles Rams"
u[u == "San Diego Chargers"] = "Los Angeles Chargers"
teams = teams %>% filter(current %in% u)

### Add a field for colors ----

# This will allow us to assign a natural color to each team for plotting
# Hex colors sourced from this link: https://www.playoffmagic.com/nfl/teams/
# Primary and secondary colors chosen to avoid overlap as much as possible
# But getting some, closeness in colors is inevitable when working with 32 teams
# Goal was to have pretty obvious differentiation within divisions, 
# because those would be the common comparisons (at least expected, bc they include rivalries and most common games)
# Divisions are every 4 teams below (i.e. 1-4, 4-8, so on in the ordering below)
color = rep(NA, nrow(teams))
color[teams$current == "New York Jets"] = "#0C371D"        #green
color[teams$current == "New England Patriots"] = "#C60C30" #red
color[teams$current == "Miami Dolphins"] = "#006666"       #teal
color[teams$current == "Buffalo Bills"] = "#00338D"        #bright blue
color[teams$current == "Indianapolis Colts"] = "#003B7B"   #nice normal blue
color[teams$current == "Tennessee Titans"] = "#648FCC"     #light blue
color[teams$current == "Houston Texans"] = "#02253A"       #navy
color[teams$current == "Jacksonville Jaguars"] = "#007198" #aqua
color[teams$current == "Baltimore Ravens"] = "#280353"     #dark purple
color[teams$current == "Pittsburgh Steelers"] = "#F2C800"  #yellow
color[teams$current == "Cincinnati Bengals"] = "#FB4F14"   #dark orange
color[teams$current == "Cleveland Browns"] = "#26201E"     #brown
color[teams$current == "Los Angeles Chargers"] = "#EEC607" #gold
color[teams$current == "Denver Broncos"] = "#002244"       #slightly brighter navy
color[teams$current == "Oakland Raiders"] = "#000000"      #black
color[teams$current == "Kansas City Chiefs"] = "#FFB612"   #yellow-orange
color[teams$current == "Philadelphia Eagles"] = "#003B48"  #dark green, almost blue green
color[teams$current == "Washington Redskins"] = "#773141"  #maroon
color[teams$current == "Dallas Cowboys"] = "#002244"       #navy
color[teams$current == "New York Giants"] = "#CA001A"      #reddish
color[teams$current == "Carolina Panthers"] = "#0088CE"    #light-blue teal thing
color[teams$current == "Atlanta Falcons"] = "#BD0D18"      #red
color[teams$current == "New Orleans Saints"] = "#D2B887"   #tan
color[teams$current == "Tampa Bay Buccaneers"] = "#89765F" #off brown
color[teams$current == "Minnesota Vikings"] = "#3B0160"    #purple
color[teams$current == "Green Bay Packers"] = "#213D30"    #dark green
color[teams$current == "Detroit Lions"] = "#006DB0"        #like a sky blue
color[teams$current == "Chicago Bears"] = "#03202F"        #real deep navy
color[teams$current == "Seattle Seahawks"] = "#4EAE47"     #lime green
color[teams$current == "San Francisco 49ers"] = "#AF1E2C"  #a more brown-y red
color[teams$current == "Los Angeles Rams"] = "#C9AF74"     #dark tan
color[teams$current == "Arizona Cardinals"] = "#870619"    #dark red
teams$color = color

### Obtain df of averages/year ----

# Create a separate data frame for the league averages
# Do this based on the existing data: we have years and averages for each team!
pass_avg = teams %>%
    group_by(year) %>%
    summarize(Passing = mean(Passing))
rush_avg = teams %>%
    group_by(year) %>%
    summarize(Rushing = mean(Rushing))
average = as.tibble(merge(pass_avg, rush_avg, by = "year"))

### Declutter and organize ----

teams = teams[,c(2,10,6,8,9,7,4,5,3,1,11)]
average$current = rep("Yearly NFL Average",78)
average$color = rep("#808080", 78)
average$Y = rep("Yes", 78) # this ~might~ be useful for the shiny interactive
average = average[,c(4,1,2,3,5,6)]

# To be able to have a legend for statistic when plotting both, 
# we'll also need dataframes gathered by yards
teams_both = gather(teams, "statistic", "yards", 4:5)
average_both = gather(average, "statistic", "yards", 3:4)

### SHINY INTERACTION! UI ----

# The plot will take inputs of a particular range of years, and output a time series
# plot of the average per-game rushing and passing yards over that range of years

ui <- fluidPage(
    titlePanel(
        strong("Historical Trends in the NFL Offense")
    ),  
    sidebarPanel(
        selectInput(inputId = "statistic",
                    label = "Select your statistic(s):",
                    choices = c("Passing", "Rushing", "Both"),
                    selected = "Both"),
        br(),
        radioButtons(inputId = "avg",
                     label = "Do you wish to show the league average in the plot?",
                     choices = c("Yes", "No"),
                     selected = "Yes",
                     inline = TRUE),
        br(),
        selectizeInput(inputId = "franchise",
                       label = "Select your team(s) (up to 4):",
                       choices = sort(unique(teams$current)),
                       multiple = TRUE,
                       options = list(maxItems = 4,
                                      placeholder = "Teams of Interest")),
        br(),
        h5(strong("Below, the year of entry into the NFL for each selected team will appear.
              Please select your range of years to plot accordingly.")),
        textOutput("history1"),
        textOutput("history2"),
        textOutput("history3"),
        textOutput("history4"),
        br(),
        width = 3
    ),
    mainPanel(
        width = 9,
        plotOutput("plot"),
        hr(),
        fluidRow(
            column(12,
                   fluidRow(
                       column(6,
                              sliderInput(inputId = "year1",
                                          label = "Select your minimum year:",
                                          min = 1935, max = 2012, value = 1966,
                                          sep = "")
                       ),
                       column(6,
                              sliderInput(inputId = "year2", label = "Select your maximum year:",
                                          min = 1935, max = 2012, value = 2012,
                                          sep = ""),
                              h6("Note: must be greater than your chosen minimum year!")
                       )
                   )
                   
            )
        )
    )
)

### SHINY INTERACTION! SERVER ----

server = function(input, output){
    output$history1 = renderPrint(
        if(length(input$franchise) >= 1){
            state = paste(input$franchise[1], ": ", min(teams$year[teams$current == input$franchise[1]]), sep = "")
            cat(state)
        } else {
            cat(" ")
        }
    )
    output$history2 = renderPrint(
        if(length(input$franchise) >= 2){
            state = paste(input$franchise[2], ": ", min(teams$year[teams$current == input$franchise[2]]), sep = "")
            cat(state)
        } else {
            cat(" ")
        }
    )
    output$history3 = renderPrint(
        if(length(input$franchise) >= 3){
            state = paste(input$franchise[3], ": ", min(teams$year[teams$current == input$franchise[3]]), sep = "")
            cat(state)
        } else {
            cat(" ")
        }
    )
    output$history4 = renderPrint(
        if(length(input$franchise) >= 4){
            state = paste(input$franchise[4], ": ", min(teams$year[teams$current == input$franchise[4]]), sep = "")
            cat(state)
        } else {
            cat(" ")
        }
    )
    output$plot = renderPlot(
        if(input$statistic == "Both"){
            ggplot() +
                geom_path(data = teams_both[teams_both$current %in% input$franchise,], 
                          aes(x = year, y = yards, color = current, linetype = statistic)) +
                geom_path(data = teams_both[teams_both$current %in% input$franchise,], 
                          aes(x = year, y = yards, color = current, linetype = statistic)) +
                geom_path(data = average_both[average_both$Y %in% input$avg,], 
                          aes(x = year, y = yards, color = current, linetype = statistic)) +
                geom_path(data = average_both[average_both$Y %in% input$avg,], 
                          aes(x = year, y = yards, color = current, linetype = statistic)) +
                scale_x_continuous(name = "Year",
                                   limits = c(input$year1, input$year2),
                                   breaks = seq(input$year1, input$year2, by = 2)) +
                scale_y_continuous(name = "Yards per Game",
                                   limits = c(0, 350),
                                   breaks = seq(0,350,by=25)) +
                labs(title = paste("Passing and Rushing Yards per Game between", input$year1, "and", input$year2)) +
                scale_color_manual(name = "Team",
                                   values = c(teams$color[match(sort(input$franchise), teams$current)],
                                              unique(average$color))) +
                scale_linetype_manual(name = "Statistic",
                                      labels = c("Passing Yards", "Rushing Yards"),
                                      values = c("longdash", "solid")) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank())
        } else if(input$statistic == "Passing"){
            ggplot() +
                geom_path(data = teams[teams$current %in% input$franchise,], 
                          aes(x = year, y = Passing, color = current), linetype = "longdash") +
                geom_path(data = average[average$Y %in% input$avg,], 
                          aes(x = year, y = Passing, color = current), linetype = "longdash") +
                scale_x_continuous(name = "Year",
                                   limits = c(input$year1, input$year2),
                                   breaks = seq(input$year1, input$year2, by = 2)) +
                scale_y_continuous(name = "Yards per Game",
                                   limits = c(0, 350),
                                   breaks = seq(0,350,by=25)) +
                labs(title = paste("Passing Yards per Game between", input$year1, "and", input$year2)) +
                scale_color_manual(name = "Team",
                                   values = c(teams$color[match(sort(input$franchise), teams$current)],
                                              unique(average$color))) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank())
        } else if(input$statistic == "Rushing"){
            ggplot() +
                geom_path(data = teams[teams$current %in% input$franchise,], 
                          aes(x = year, y = Rushing, color = current), linetype = "solid") +
                geom_path(data = average[average$Y %in% input$avg,], 
                          aes(x = year, y = Rushing, color = current), linetype = "solid") +
                scale_x_continuous(name = "Year",
                                   limits = c(input$year1, input$year2),
                                   breaks = seq(input$year1, input$year2, by = 2)) +
                scale_y_continuous(name = "Yards per Game",
                                   limits = c(0, 250),
                                   breaks = seq(0,350,by=25)) +
                labs(title = paste("Rushing Yards per Game between", input$year1, "and", input$year2)) +
                scale_color_manual(name = "Team",
                                   values = c(teams$color[match(sort(input$franchise), teams$current)],
                                              unique(average$color))) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank())
        }
    )
}

### Deploying the app
shinyApp(ui, server)

