## Visualizing Historical Trends in the NFL Offense Trends

The code in this repository produces an interactive plot for tracing average rushing and passing yards per game for NFL teams throughout the league's history, and comparing these statistics between teams. 

This interactive visualization is built as a Shiny App, and is hosted [here.](https://aaronweinstock.shinyapps.io/NFL_Offense_Trends/) Please visit this link to interact with the plot!


### **Plot Options**

___

Below, the options for the interactive portion of this plot are detailed:

1. **Select your statistic(s):** Select *Passing* to display just passing yards/game, *Rushing* to display just rushing yards/game, or *Both* to display both rushing yards/game and passing yards/game. Default is *Both*.

2. **Do you wish to show the league average in the plot?** Select *Yes* to display the league average for the selected statistics on the plot, or *No* to hide the league average for the selected statistics. Default is *Yes*.

3. **Select your team(s) (up to 4):** Select up to four teams for which to display the selected statistics on the plot and make comparisons. This parameter is searchable, so team names may be typed into the selection box or selected from the drop down menu; teams will be plotted with either their primary or secondary color, and will automatically be added to a plot legend upon selection. Furthermore, when teams are selected, their year of entry into the NFL will be displayed below the selection box, to help guide choices for minimum and maximum years.

4. **Select your minimum year:** Slide the bar to the earliest desired year for which to display data. Default is *1966* (beginning of the Super Bowl Era in the NFL).

5. **Select your maximum year:** Slide the bar to the latest desired year for which to display data. Default is *2012* (most recent year available in the data used).

Refer to `Example_Interaction.png` in the main directory for an example of a visualization produced by the interacting with the app in the above described ways. This plot compares the rushing yards/game of the New York Jets and New England Patriots (with a baseline of the annual NFL average) between 1970 -- the year these two teams entered the NFL -- and 2012. This was acheived with the parameter selections *Rushing, Yes, New York Jets New England Patriots, 1966, and 2012* (respectively, for options 1 through 5). 


### **Code and Files**

___

The folder `NFL_Offense_Trends` contains two files used for the creation of this plot.

1. `Data/RushingPassing.xlsx` contains data on rushing and passing offense (total yards) for each team in each season between 1932 and 2012.

2. `app.R` cleans the above data for plotting, produces a yards/game statistic, and creates the Shiny application.