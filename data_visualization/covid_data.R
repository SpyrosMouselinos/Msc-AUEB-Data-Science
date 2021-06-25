library("readxl")
library("dplyr")
library("ggplot2")
library("digest")
library("viridis")
library("scales")
library("maps")
library("directlabels")
library("gridExtra")

# Create non-Greece EU stats for plot 3 #
#non_greece_eu_data = europe %>% filter(countriesAndTerritories !="Greece") %>% group_by(dateRep) %>% 
#                                                        summarise(Daily_cases_sum=sum(cases),
#                                                         Daily_deaths_sum=sum(deaths),
#                                                         Daily_cases_avg=mean(cases),
#                                                         Daily_deaths_avg=mean(deaths)) %>% 
#                                                         mutate(Continent="Non-GR-Europe")


#Set directory Here
setwd("C:\\Users\\Guldan\\Desktop\\datavisualization")

# Declare the file name
fileName <- "COVID-19-geographic-disbtribution-worldwide.xlsx"

# Read the data into a Data Frame
df <- read_excel(fileName)

df <- df %>% rename(cumsum14 = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`)

### -------------- START OF EDA -------------- ###

# Get Europe frame
europe = df %>% filter(continentExp == "Europe")

# Get Europe Total Population
europe_population_per_country = europe %>% group_by(countriesAndTerritories) %>% summarise(EU_Population=mean(popData2019))

# Greece Population
greece_total_population = europe_population_per_country %>% filter(countriesAndTerritories=="Greece") 
greece_total_population = greece_total_population$EU_Population

# Total Europe Population
europe_total_population = sum(europe_population_per_country$EU_Population)



# Get Europe Daily Stats
europe_daily_stats = europe %>% group_by(dateRep) %>% summarise(Daily_cases_sum=sum(cases),
                                                                Daily_deaths_sum=sum(deaths),
                                                                Daily_cases_avg=  sum(cases)  / europe_total_population,
                                                                Daily_deaths_avg= sum(deaths) / europe_total_population) %>% mutate(Continent="Europe")
                                                      

# Get Greece
greece = df %>% filter(countriesAndTerritories=="Greece")


# Create Dummy Greece Continent to compare on continent 
greece_daily_stats = greece %>% mutate(Daily_cases_sum=cases,
                                       Daily_deaths_sum=deaths,
                                       Daily_cases_avg=cases / greece_total_population,
                                       Daily_deaths_avg=deaths / greece_total_population,
                                       Continent="Greece") %>% select(dateRep,Daily_cases_sum,
                                                                      Daily_deaths_sum,Daily_cases_avg,
                                                                      Daily_deaths_avg,
                                                                      Continent)

# Get Asia frame
asia = df %>% filter(continentExp == "Asia")

# Get Asia Total Population
asia_population_per_country = asia %>% group_by(countriesAndTerritories) %>% summarise(AS_Population=mean(popData2019))

# Total Asia Population
asia_total_population = sum(asia_population_per_country$AS_Population)

# Get Asia Daily Stats
asia_daily_stats = asia %>% group_by(dateRep) %>% summarise(Daily_cases_sum=sum(cases),
                                                            Daily_deaths_sum=sum(deaths),
                                                            Daily_cases_avg=  sum(cases)/asia_total_population,
                                                            Daily_deaths_avg= sum(deaths)/asia_total_population)  %>% mutate(Continent="Asia")

# Get Africa frame
africa = df %>% filter(continentExp =="Africa")

# Get Africa Total Population
africa_population_per_country = africa %>% group_by(countriesAndTerritories) %>% summarise(AF_Population=mean(popData2019))

# Total Africa Population
africa_total_population = sum(africa_population_per_country$AF_Population)

# Get Africa Daily Stats
africa_daily_stats = africa %>% group_by(dateRep) %>%  summarise(Daily_cases_sum=sum(cases),
                                                                 Daily_deaths_sum=sum(deaths),
                                                                 Daily_cases_avg=  sum(cases) / africa_total_population,
                                                                 Daily_deaths_avg= sum(deaths)/ africa_total_population) %>% mutate(Continent="Africa")

# Get America frame
america = df %>% filter(continentExp =="America")

# Get America Total Population
america_population_per_country = america %>% group_by(countriesAndTerritories) %>% summarise(AM_Population=mean(popData2019))

# Total America Population
america_total_population = sum(america_population_per_country$AM_Population)

# Get America Daily Stats
america_daily_stats = america %>% group_by(dateRep) %>%  summarise(Daily_cases_sum=sum(cases),
                                                                   Daily_deaths_sum=sum(deaths),
                                                                   Daily_cases_avg=  sum(cases) / america_total_population,
                                                                   Daily_deaths_avg= sum(deaths) / america_total_population) %>% mutate(Continent="America")

# Get Oceania frame
# Note:# There is a missing value in the field "Wallis and Futuna"
# However according to 
# https://www.worldometers.info/world-population/wallis-and-futuna-islands-population/
# The 2019 population was 11,432
# So we proceed to fill the missing value and continue our research
oceania = df %>% filter(continentExp =="Oceania")

oceania$popData2019 <- ifelse(oceania$countriesAndTerritories == 'Wallis_and_Futuna' & is.na(oceania$popData2019), 11432, oceania$popData2019)  

# Get Oceania Total Population
oceania_population_per_country = oceania %>% group_by(countriesAndTerritories) %>% summarise(OC_Population=mean(popData2019))
oceania_total_population = sum(oceania_population_per_country$OC_Population)

# Get Oceania Daily Stats
oceania_daily_stats = oceania %>% group_by(dateRep) %>%  summarise(Daily_cases_sum=sum(cases),
                                                                   Daily_deaths_sum=sum(deaths),
                                                                   Daily_cases_avg=   sum(cases)  / oceania_total_population,
                                                                   Daily_deaths_avg=  sum(deaths) / oceania_total_population) %>% mutate(Continent="Oceania")

# Group all countries into 1 DF for plots 1 and 2#
global_data = rbind(europe_daily_stats, asia_daily_stats, africa_daily_stats, america_daily_stats, oceania_daily_stats)

# Greece vs Global AVG's #
gr_vs_world = rbind(global_data, greece_daily_stats)


### -------------- END OF EDA -------------- ###


#### -------------- PLOT 1 -------------- ####
#### Global Case Evolution Between Continents ####
ggplot(global_data, aes(x=dateRep, y=Daily_cases_sum / 1000, fill=Continent)) +
  geom_area(alpha=0.55 , size=0.01, colour="black") +
  scale_fill_manual(name = 'Continents', values = c('Africa'='cyan','America'='blue', 'Asia'='orange','Europe'='red', 'Oceania'='magenta'), labels = c('Africa','America','Asia','Europe','Oceania'))+
  ggtitle("Total Number of Global Daily Cases")+
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("\nCases in Thousands\n") +
  xlab("\nTime\n")



#### -------------- PLOT 2 -------------- ####
#### Global Death Evolution Between Continents ####
ggplot(global_data, aes(x=dateRep, y=Daily_deaths_sum / 1000, fill=Continent)) +
  geom_area(alpha=0.55 , size=0.01, colour="black") +
  scale_fill_manual(name = 'Continents', values = c('Africa'='cyan','America'='blue', 'Asia'='orange','Europe'='red', 'Oceania'='magenta'), labels = c('Africa','America','Asia','Europe','Oceania'))+
  ggtitle("Total Number of Global Daily Deaths")+
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("\nDeaths in Thousands\n") +
  xlab("\nTime\n") +
  ylim(0, NA)

#### -------------- PLOT 3 -------------- ####
#### Global Case Evolution Between Greece and AVG Continents ####
p1 = ggplot(gr_vs_world, aes(dateRep, 100 * Daily_cases_avg, color = Continent))+
  geom_point(size = 1)+
  geom_line()+
  ylab("\nPercentage of of total Population (%) \n") +
  xlab("\nTime\n")+
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  scale_color_manual(name = 'Continents', values = c('Africa'='cyan','America'='blue', 'Asia'='orange','Europe'='red', 'Oceania'='magenta', 'Greece'='darkgreen'), labels = c('Africa','America','Asia','Europe','Oceania',"Greece"))+
  labs(title= "Daily Cases: Greece vs World")

direct.label(p1, "last.qp")

#### -------------- PLOT 4 -------------- ####
#### Global Death Evolution Between Greece and AVG Continents ####
p2 = ggplot(gr_vs_world, aes(dateRep, 100 * Daily_deaths_avg, color = Continent))+
  geom_point(size = 1)+
  ylim(0, NA) +
  geom_line()+
  ylab("\nPercentage of of total Population (%) \n") +
  xlab("\nTime\n")+
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  scale_color_manual(name = 'Continents', values = c('Africa'='cyan','America'='blue', 'Asia'='orange','Europe'='red', 'Oceania'='magenta', 'Greece'='darkgreen'), labels = c('Africa','America','Asia','Europe','Oceania',"Greece"))+
  labs(title= "Daily Deaths: Greece vs World")

direct.label(p2, "last.qp")


#### -------------- PLOT 5 -------------- ####
#### EU Countries ####
europe = europe %>% mutate(countriesAndTerritories=case_when( countriesAndTerritories=="United_Kingdom"~"UK",
                                                              countriesAndTerritories=="North_Macedonia"~"Macedonia",
                                                              countriesAndTerritories=="Czechia"~"Czech Republic",
                                                              countriesAndTerritories=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                                              countriesAndTerritories=="Isle_of_Man"~"Isle of Man",
                                                              countriesAndTerritories=="Faroe_Islands"~"Faroe Islands",
                                                              countriesAndTerritories=="San_Marino"~"San Marino",
                                                              TRUE~countriesAndTerritories))

# Get Latest Cases and Deaths per EU Country
europe_latest_stats = europe  %>% 
                      filter(!is.na(cases)) %>% 
                      group_by(countriesAndTerritories) %>% 
                      summarise(total_cases=sum(cases, na.rm=TRUE), geoId=geoId) %>%  select(countriesAndTerritories,total_cases,geoId)
                                 

eu_countries = setdiff(unique(europe$countriesAndTerritories),c('Andorra','Russia','Azerbaijan','Georgia','Armenia', 'Jersey', 'Guernsey'))

maps = map_data("world", region = eu_countries)

maps = left_join(maps, europe_latest_stats, by=c('region'='countriesAndTerritories'))
# Compute the centroid as the mean longitude and latitude
# Used as label coordinate for country's names
region_data = maps %>%
  group_by(geoId) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = total_cases))+
  geom_text(aes(label = geoId), data = region_data,  size = 3, color='black')+
  ggtitle("Total Cases since Outbreak: Greece vs Europe") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_gradient(low='yellow', high='red')+
  theme_void() +
  ylim(NA, 72) +
  xlim(-25,40)
  

#### -------------- PLOT 6 -------------- ####
#### EU Countries ####
  
  europe_latest_stats = europe  %>% 
    filter(!is.na(cases)) %>% 
    group_by(countriesAndTerritories) %>% 
    summarise(total_cases=sum(cases, na.rm=TRUE), geoId=geoId, population=mean(popData2019)) %>%
    summarise(total_pcases=100*total_cases/population, geoId=geoId) %>%
    select(countriesAndTerritories,total_pcases,geoId)
  

  eu_countries = setdiff(unique(europe$countriesAndTerritories),c('Andorra','Russia','Azerbaijan','Georgia','Armenia', 'Jersey', 'Guernsey'))
  
  maps = map_data("world", region = eu_countries)
  
  maps = left_join(maps, europe_latest_stats, by=c('region'='countriesAndTerritories'))
  # Compute the centroid as the mean longitude and latitude
  # Used as label coordinate for country's names
  region_data = maps %>%
    group_by(geoId) %>%
    summarise(long = mean(long), lat = mean(lat))
  
  ggplot(maps, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = total_pcases))+
    geom_text(aes(label = geoId), data = region_data,  size = 3, color='black')+
    ggtitle("Total Percentage Cases since Outbreak: Greece vs Europe") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_fill_gradient(low='yellow', high='red')+
    theme_void()+
    ylim(NA, 72) +
    xlim(-25,40)
  
  
#### -------------- PLOT 7 -------------- ####
#### EU Countries ####
  europe = europe %>% mutate(countriesAndTerritories=case_when( countriesAndTerritories=="United_Kingdom"~"UK",
                                                                countriesAndTerritories=="North_Macedonia"~"Macedonia",
                                                                countriesAndTerritories=="Czechia"~"Czech Republic",
                                                                countriesAndTerritories=="Bosnia_and_Herzegovina"~"Bosnia and Herzegovina",
                                                                countriesAndTerritories=="Isle_of_Man"~"Isle of Man",
                                                                countriesAndTerritories=="Faroe_Islands"~"Faroe Islands",
                                                                countriesAndTerritories=="San_Marino"~"San Marino",
                                                                TRUE~countriesAndTerritories))
  
  # Get Latest Cases and Deaths per EU Country
  europe_latest_stats = europe  %>% 
    filter(!is.na(deaths)) %>% 
    group_by(countriesAndTerritories) %>% 
    summarise(total_deaths=sum(deaths, na.rm=TRUE), geoId=geoId) %>%  select(countriesAndTerritories,total_deaths,geoId)
  
  
  eu_countries = setdiff(unique(europe$countriesAndTerritories),c('Andorra','Russia','Azerbaijan','Georgia','Armenia', 'Jersey', 'Guernsey'))
  
  maps = map_data("world", region = eu_countries)
  
  maps = left_join(maps, europe_latest_stats, by=c('region'='countriesAndTerritories'))
  # Compute the centroid as the mean longitude and latitude
  # Used as label coordinate for country's names
  region_data = maps %>%
    group_by(geoId) %>%
    summarise(long = mean(long), lat = mean(lat))
  
  ggplot(maps, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = total_deaths))+
    geom_text(aes(label = geoId), data = region_data,  size = 3, color='black')+
    ggtitle("Total Deaths since Outbreak: Greece vs Europe") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_fill_gradient(low='green', high='purple') +
    theme_void()+
    ylim(NA, 72) +
    xlim(-25,40)
  
  
#### -------------- PLOT 8 -------------- ####
#### EU Countries ####
  
  europe_latest_stats = europe  %>% 
    filter(!is.na(deaths)) %>% 
    group_by(countriesAndTerritories) %>% 
    summarise(total_deaths=sum(deaths, na.rm=TRUE), geoId=geoId, population=mean(popData2019)) %>%
    summarise(total_pdeaths=100*total_deaths/population, geoId=geoId) %>%
    select(countriesAndTerritories,total_pdeaths,geoId)
  
  
  eu_countries = setdiff(unique(europe$countriesAndTerritories),c('Andorra','Russia','Azerbaijan','Georgia','Armenia', 'Jersey', 'Guernsey'))
  
  maps = map_data("world", region = eu_countries)
  
  maps = left_join(maps, europe_latest_stats, by=c('region'='countriesAndTerritories'))
  # Compute the centroid as the mean longitude and latitude
  # Used as label coordinate for country's names
  region_data = maps %>%
    group_by(geoId) %>%
    summarise(long = mean(long), lat = mean(lat))
  
  ggplot(maps, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = total_pdeaths))+
    geom_text(aes(label = geoId), data = region_data,  size = 3, color='black')+
    ggtitle("Total Percentage Deaths since Outbreak: Greece vs Europe") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_fill_gradient(low='green', high='purple')+
    theme_void()+
    ylim(NA, 72) +
    xlim(-25,40)

#### -------------- PLOT 9 -------------- ####

# Get Latest Cases and Deaths per EU Country
europe = europe  %>% 
  mutate(isGreece = case_when(countriesAndTerritories=="Greece"~1,
                              TRUE~0))


europe_rank_before_cases = europe %>% 
  filter(!is.na(cases) & (month < 9)) %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(total_cases=sum(cases, na.rm=TRUE), isGreece) %>%  select(countriesAndTerritories,total_cases, isGreece)


europe_rank_after_cases = europe  %>% 
  filter(!is.na(cases) & (month > 8)) %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(total_cases=sum(cases, na.rm=TRUE), isGreece) %>%  select(countriesAndTerritories,total_cases, isGreece)

europe_rank_before_deaths = europe  %>% 
  filter(!is.na(deaths) & (month < 9)) %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(total_deaths=sum(deaths, na.rm=TRUE), isGreece) %>%  select(countriesAndTerritories,total_deaths, isGreece)


europe_rank_after_deaths = europe  %>% 
  filter(!is.na(deaths) & (month > 8)) %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(total_deaths=sum(deaths, na.rm=TRUE), isGreece) %>%  select(countriesAndTerritories,total_deaths, isGreece)


#### RANKING IN CASES ####

before_august_cases = ggplot(europe_rank_before_cases, aes(x = reorder(countriesAndTerritories, total_cases), y =total_cases / 10000))+
                geom_bar(stat='identity', aes(fill=isGreece), width=.5) +
                labs(title="EU Rank by cases in tens of thousands - Before August", x=NULL, y="Total Cases in Tens of Thousands")+
                annotate("text", x ='Greece', y = 1, label = "29th Rank ", color="black", size=3, hjust=-0.5) + 
                guides(fill = FALSE)+
                coord_flip()
before_august_cases
  
after_august_cases = ggplot(europe_rank_after_cases, aes(x = reorder(countriesAndTerritories, total_cases), y =total_cases / 10000))+
                geom_bar(stat='identity', aes(fill=isGreece), width=.5) +
                labs(title="EU Rank by total number of Cases in tens of thousands - After August", x=NULL, y="Total Cases in Tens of Thousands")+
                annotate("text", x ='Greece', y = 1, label = "28th Rank ", color="black", size=3, hjust=-0.5) + 
                scale_color_brewer(palette = 'Dark2')+      
                guides(fill = FALSE)+
                coord_flip()
after_august_cases

#### RANKING IN DEATHS ####
before_august_deaths = ggplot(europe_rank_before_deaths, aes(x = reorder(countriesAndTerritories, total_deaths), y = total_deaths / 1000))+
  geom_bar(stat='identity', aes(fill=isGreece), width=.5) +
  labs(title="EU Rank by deaths in thousands - Before August", x=NULL, y="Total Deaths in Thousands")+
  annotate("text", x ='Greece', y = 1, label = "32nd Rank ", color="black", size=3, hjust=-0.5) + 
  guides(fill = FALSE)+
  coord_flip()

before_august_deaths

after_august_deaths = ggplot(europe_rank_after_deaths, aes(x = reorder(countriesAndTerritories, total_deaths), y =total_deaths / 1000))+
  geom_bar(stat='identity', aes(fill=isGreece), width=.5) +
  labs(title="EU Rank by total number of deaths in thousands - After August", x=NULL, y="Total Deaths in Thousands")+
  annotate("text", x ='Greece', y = 1, label = "20th Rank ", color="black", size=3, hjust=-0.5) + 
  scale_color_brewer(palette = 'Dark2')+      
  guides(fill = FALSE)+
  coord_flip()

after_august_deaths


#### Timeline 1 Case Plot ####
### Take 20 Countries with Population closest to Greece ###
countries_continents = df %>% group_by(countriesAndTerritories) %>% slice(1) %>%filter(!is.na(popData2019))  %>% select(countriesAndTerritories, continentExp, popData2019)
countries_continents$popData2019 = abs(countries_continents$popData2019 - greece_total_population) 
countries_continents = tail(countries_continents[order(countries_continents$popData2019, decreasing=TRUE), ], 20)
over_1 = df %>% filter(!is.na(cases)) %>% filter(cases > 1)

over_1 = over_1 %>% 
    group_by(countriesAndTerritories) %>% 
    summarise(dateRep = min(dateRep)) %>% 
    ungroup() %>% 
    select(countriesAndTerritories:dateRep)

over_1 = left_join(countries_continents, over_1 , by=c("countriesAndTerritories"))
over_1 = over_1 %>% mutate(continentExp=case_when( countriesAndTerritories=="Greece"~"Greece",
                                                              TRUE~continentExp))
continent_levels <- c("Africa", "America", "Asia", "Europe","Oceania","Greece")
continent_colors <- c("cyan", "blue", "orange", "red","magenta","darkgreen")

over_1$continentExp <- factor(over_1$continentExp, levels=continent_levels, ordered=TRUE)

positions <- c(0.1,-0.1,0.3,-0.3, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2,-2,3,-3,4,-4,5,-5)
#positions <- c(0.5,-0.5,2,-2,5,-5)
directions <- c(1, -1)

line_pos <- data.frame(
  "dateRep"=unique(over_1$dateRep),
  "position"=rep(positions, length.out=length(unique(over_1$dateRep))),
  "direction"=rep(directions, length.out=length(unique(over_1$dateRep)))
)

dft <- left_join(over_1, line_pos, by=c("dateRep"), all = TRUE)
dft <- dft[with(dft, order(dateRep, continentExp)), ]

text_offset <- 0.3

dft$month_count <- ave(dft$dateRep==dft$dateRep, dft$dateRep, FUN=cumsum)
dft$text_position <- (dft$month_count * text_offset * dft$direction) + dft$position


month_buffer <- 1

month_date_range <- seq(min(dft$dateRep) - lubridate::dmonths(month_buffer), max(dft$dateRep) + lubridate::dmonths(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

### Bug - Fix for February ###
month_df$month_format = c("Dec","Jan","Feb","Mar","Apr","May")


timeline_plot<-ggplot(dft,aes(x=dateRep, y=0, col=continentExp, label=countriesAndTerritories))
timeline_plot<-timeline_plot+labs(col="Country Continent")
timeline_plot<-timeline_plot+scale_color_manual(values=continent_colors, labels=continent_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_get()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=dft[dft$month_count == 1,], aes(y=position,yend=0,xend=dateRep), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "left"
)
### Bug fix 


# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.8,label=month_format),size=3, vjust=3, color='black', angle=90)
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=countriesAndTerritories),size=2)
timeline_plot<-timeline_plot +labs(title="First Case (Globe vs Greece)")
timeline_plot




#### Timeline 100  Cases plot ####
countries_continents = df %>% group_by(countriesAndTerritories) %>% slice(1) %>%filter(!is.na(popData2019))  %>% select(countriesAndTerritories, continentExp, popData2019)
countries_continents$popData2019 = abs(countries_continents$popData2019 - greece_total_population) 
countries_continents = tail(countries_continents[order(countries_continents$popData2019, decreasing=TRUE), ], 20)
### When Broke 100 Barrier ###
over_1 = df %>% filter(!is.na(cases)) %>% filter(cases > 100)

over_1 = over_1 %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(dateRep = min(dateRep)) %>% 
  ungroup() %>% 
  select(countriesAndTerritories:dateRep)

over_1 = left_join(countries_continents, over_1 , by=c("countriesAndTerritories"))
over_1 = over_1 %>% mutate(continentExp=case_when( countriesAndTerritories=="Greece"~"Greece",
                                                   TRUE~continentExp))
continent_levels <- c("Africa", "America", "Asia", "Europe","Oceania","Greece")
continent_colors <- c("cyan", "blue", "orange", "red","magenta","darkgreen")

over_1$continentExp <- factor(over_1$continentExp, levels=continent_levels, ordered=TRUE)

positions <- c(0.1,-0.1,0.3,-0.3, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2,-2,3,-3,4,-4,5,-5)
#positions <- c(0.5,-0.5,2,-2,5,-5)
directions <- c(1, -1)

line_pos <- data.frame(
  "dateRep"=unique(over_1$dateRep),
  "position"=rep(positions, length.out=length(unique(over_1$dateRep))),
  "direction"=rep(directions, length.out=length(unique(over_1$dateRep)))
)

dft <- left_join(over_1, line_pos, by=c("dateRep"), all = TRUE)
dft <- dft[with(dft, order(dateRep, continentExp)), ]

text_offset <- 0.3

dft$month_count <- ave(dft$dateRep==dft$dateRep, dft$dateRep, FUN=cumsum)
dft$text_position <- (dft$month_count * text_offset * dft$direction) + dft$position


month_buffer <- 1

month_date_range <- seq(min(dft$dateRep) - lubridate::dmonths(month_buffer), max(dft$dateRep) + lubridate::dmonths(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

### Bug - Fix for February ###
month_df$month_format = c("Dec","Jan","Feb","Mar","Apr","May")

timeline_plot<-ggplot(dft,aes(x=dateRep, y=0, col=continentExp, label=countriesAndTerritories))
timeline_plot<-timeline_plot+labs(col="Country Continent")
timeline_plot<-timeline_plot+scale_color_manual(values=continent_colors, labels=continent_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_get()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=dft[dft$month_count == 1,], aes(y=position,yend=0,xend=dateRep), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "left"
)

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.8,label=month_format),size=3, vjust=3, color='black', angle=90)
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=countriesAndTerritories),size=2)
timeline_plot<-timeline_plot +labs(title="First 100 Cases (Globe vs Greece)")
timeline_plot






#### Timeline 1 Death plot ####
countries_continents = df %>% group_by(countriesAndTerritories) %>% slice(1) %>%filter(!is.na(popData2019))  %>% select(countriesAndTerritories, continentExp, popData2019)
countries_continents$popData2019 = abs(countries_continents$popData2019 - greece_total_population) 
countries_continents = tail(countries_continents[order(countries_continents$popData2019, decreasing=TRUE), ], 20)
### When Broke 100 Barrier ###
over_1 = df %>% filter(!is.na(deaths)) %>% filter(deaths > 1)

over_1 = over_1 %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(dateRep = min(dateRep)) %>% 
  ungroup() %>% 
  select(countriesAndTerritories:dateRep)

over_1 = left_join(countries_continents, over_1 , by=c("countriesAndTerritories"))
over_1 = over_1 %>% mutate(continentExp=case_when( countriesAndTerritories=="Greece"~"Greece",
                                                   TRUE~continentExp))
continent_levels <- c("Africa", "America", "Asia", "Europe","Oceania","Greece")
continent_colors <- c("cyan", "blue", "orange", "red","magenta","darkgreen")

over_1$continentExp <- factor(over_1$continentExp, levels=continent_levels, ordered=TRUE)

positions <- c(0.1,-0.1,0.3,-0.3, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2,-2,3,-3,4,-4,5,-5)
#positions <- c(0.5,-0.5,2,-2,5,-5)
directions <- c(1, -1)

line_pos <- data.frame(
  "dateRep"=unique(over_1$dateRep),
  "position"=rep(positions, length.out=length(unique(over_1$dateRep))),
  "direction"=rep(directions, length.out=length(unique(over_1$dateRep)))
)

dft <- left_join(over_1, line_pos, by=c("dateRep"), all = TRUE)
dft <- dft[with(dft, order(dateRep, continentExp)), ]

text_offset <- 0.3

dft$month_count <- ave(dft$dateRep==dft$dateRep, dft$dateRep, FUN=cumsum)
dft$text_position <- (dft$month_count * text_offset * dft$direction) + dft$position


month_buffer <- 1

month_date_range <- seq(min(dft$dateRep) - lubridate::dmonths(month_buffer), max(dft$dateRep) + lubridate::dmonths(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

### Bug - Fix for February ###
month_df$month_format = c("Dec","Jan","Feb","Mar","Apr","May")

timeline_plot<-ggplot(dft,aes(x=dateRep, y=0, col=continentExp, label=countriesAndTerritories))
timeline_plot<-timeline_plot+labs(col="Country Continent")
timeline_plot<-timeline_plot+scale_color_manual(values=continent_colors, labels=continent_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_get()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=dft[dft$month_count == 1,], aes(y=position,yend=0,xend=dateRep), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "left"
)

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.8,label=month_format),size=3, vjust=3, color='black', angle=90)
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=countriesAndTerritories),size=2)
timeline_plot<-timeline_plot +labs(title="First Death (Globe vs Greece)")
timeline_plot

#### Timeline 10 Death plot ####
countries_continents = df %>% group_by(countriesAndTerritories) %>% slice(1) %>%filter(!is.na(popData2019))  %>% select(countriesAndTerritories, continentExp, popData2019)
countries_continents$popData2019 = abs(countries_continents$popData2019 - greece_total_population) 
countries_continents = tail(countries_continents[order(countries_continents$popData2019, decreasing=TRUE), ], 20)
over_1 = df %>% filter(!is.na(deaths)) %>% filter(deaths > 10)

over_1 = over_1 %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(dateRep = min(dateRep)) %>% 
  ungroup() %>% 
  select(countriesAndTerritories:dateRep)

over_1 = left_join(countries_continents, over_1 , by=c("countriesAndTerritories"))
over_1 = over_1 %>% mutate(continentExp=case_when( countriesAndTerritories=="Greece"~"Greece",
                                                   TRUE~continentExp))
continent_levels <- c("Africa", "America", "Asia", "Europe","Oceania","Greece")
continent_colors <- c("cyan", "blue", "orange", "red","magenta","darkgreen")

over_1$continentExp <- factor(over_1$continentExp, levels=continent_levels, ordered=TRUE)

positions <- c(0.1,-0.1,0.3,-0.3, 0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2,-2,3,-3,4,-4,5,-5)
#positions <- c(0.5,-0.5,2,-2,5,-5)
directions <- c(1, -1)

line_pos <- data.frame(
  "dateRep"=unique(over_1$dateRep),
  "position"=rep(positions, length.out=length(unique(over_1$dateRep))),
  "direction"=rep(directions, length.out=length(unique(over_1$dateRep)))
)

dft <- left_join(over_1, line_pos, by=c("dateRep"), all = TRUE)
dft <- dft[with(dft, order(dateRep, continentExp)), ]

text_offset <- 0.3

dft$month_count <- ave(dft$dateRep==dft$dateRep, dft$dateRep, FUN=cumsum)
dft$text_position <- (dft$month_count * text_offset * dft$direction) + dft$position


month_buffer <- 0

month_date_range <- seq(min(dft$dateRep, na.rm=TRUE) - lubridate::dmonths(month_buffer), max(dft$dateRep, na.rm=TRUE) + lubridate::dmonths(month_buffer), by='month')
#month_format <- c("Mar","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")

month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)


timeline_plot<-ggplot(dft,aes(x=dateRep, y=0, col=continentExp, label=countriesAndTerritories))
timeline_plot<-timeline_plot+labs(col="Country Continent")
timeline_plot<-timeline_plot+scale_color_manual(values=continent_colors, labels=continent_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_get()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=dft[dft$month_count == 1,], aes(y=position,yend=0,xend=dateRep), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "left"
)

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.8,label=month_format),size=3, vjust=3, color='black', angle=90)
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=countriesAndTerritories),size=2)
timeline_plot<-timeline_plot +labs(title="First 10 Deaths (Globe vs Greece)")
timeline_plot
