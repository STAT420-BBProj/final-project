library(dplyr)

salaries = read.csv("Salaries.csv")
teams = read.csv("Teams.csv")
batting = read.csv("Batting.csv")

#Reduce data to Year >=2000 thru 2016
teams = teams[teams$yearID >= 2000 & teams$yearID < 2017,]
teams = droplevels(teams)
salaries = salaries[salaries$yearID >= 2000 & salaries$yearID < 2017,]
salaries = droplevels(salaries)
batting = batting[batting$yearID >= 2000 & batting$yearID < 2017,]
batting = droplevels(batting)

#Check identity levels
identical(levels(salaries$teamID), levels(teams$teamID))
identical(levels(batting$teamID), levels(teams$teamID))

# sum salaries by year by team
team_salaries = salaries %>% group_by(yearID, teamID) %>% summarize(salary = sum(as.numeric(salary)))

# sum batting by year by team
team_batting = batting %>% group_by(yearID, teamID) %>% 
                  summarise_at( 
                    .vars = vars(RBI, GIDP, IBB),
                    .funs = sum)

# Now join it together
teams = inner_join(teams, team_salaries)
teams = inner_join(teams, team_batting)

# Calculate team total bases
# Since a doubles, triples, and home runs are already counted as hits,
# multiply the number of bases in the extra-base hit by (no. bases - 1)
teams$TB = teams$H + teams$X2B + 2 * teams$X3B + 3 * teams$HR

# Calculate team slugging percentage
teams$SLG = teams$TB / teams$AB

# Calculate team on-base percentage
teams$OBP = (teams$H + teams$BB + teams$HBP) / (teams$AB + teams$BB + teams$HBP + teams$SF)

# Calculate OPS
teams$OPS = teams$SLG + teams$OBP

# Calculate team WHIP
teams$WHIP = (teams$BBA + teams$HA) / (teams$IPouts / 3)

# Calculate team batting average on balls in play
teams$BABIP = (teams$H - teams$HR) / (teams$AB - teams$SO - teams$HR + teams$SF)

# Calculate runs created
teams$RC = teams$TB * (teams$H + teams$BB) / (teams$AB + teams$BB)

# Calucate team singles
teams$X1B = teams$H - teams$X2B - teams$X3B - teams$HR

# Calculate team unintentional walks
teams$uBB = teams$BB - teams$IBB

# Calculate team wOBA
teams$wOBA = (0.69 * teams$uBB + 0.72 * teams$HBP + 0.89 * teams$X1B + 
                1.27 * teams$X2B + 1.62 * teams$X3B + 2.10 * teams$HR) / 
  (teams$AB + teams$BB - teams$IBB + teams$SF + teams$HBP)

write.csv(teams, file = "bbproj.csv", row.names = FALSE)
