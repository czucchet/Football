Backlog: # = Completed 
1)  Get player stats of games played in season
2)  Understand prior winning record for each player prior to start of match 
#3)  Check when team was first promoted to add flag for model. Complete. Analysis able to go back prior year to understand if team was in EPL. If not, promoted flag added and given 40 points as PY total
#4)  Need to clean the comma off the subbed off field as when exporting to text file, other apps unable to read it. Complete. Scraping of player data means much fewer carriage returns issues noted
#5)  To add accumulated points to fixture detail. Complete. Have a running total of points at start of a round 
#6)  Add in the points taken from last 5 games for each team -  Complete. Option to add more, but as it stands, last 5 games. 
7)  Add in points diff at starting postition from teams played over last 5 games per round. For instance, Man city
#8)  Add in flags when club is likely to be in relegation zone or champions league based on points at point in season -> Champions league: 68.71/38 = 1.808 Relegation: 36.92/38 = 	0.972. Complete
10) Removal of outliers
#11) Include ladder position of teams playing each other. Complete
12) Metric that captures performance against teams that are/are not doing as well
13) Relative ladder position last year to understand if team is CL, europa etc
14) Run model on all of training set to then export into tableau for further analysis
15) Import betting odds on games to apply to a back-dated strategy
#16) Changed key that get running points for team per round to know be done on Season-Date - previously done on Seasons-Rounds. Issue with Seasons-Rounds is that matches get postponed


Hypothesis:
1) Do predictions improve throughout the season?
2) What type of predictions tend to consistently underperform? Are these outliers and can they can removed from the data?
3) Can a betting strategy be put together in line with anything found within the analysis?
4) How does the model go against the most recent games?
  
  
  
