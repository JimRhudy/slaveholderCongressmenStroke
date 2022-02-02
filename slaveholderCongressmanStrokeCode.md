---
title: "slaveholderCongressmanStrokeCode"
author: "Jim Rhudy"
date: "2/2/2022"
output: 
  html_document:
    keep_md: true
---



## Background

This work is submitted in partial fulfillment of requirements for the Developing Data Products massively open online course (MOOC), component of the Data Science Specialization, hosted by Johns Hopkins University. The data at hand are the products of a team of journalists at the Washington Post. Detailed background and citation of data sources, along with considerations for reproducibility of my transformation from raw to tidy data, are available in the accompanying final.Rpres document in my project Github repo. Throughout my emphasis will be on reproducibility of my findings.

The unjust practice of enslaving humans for forced labor has been tragically with us since the beginning of recorded history. The history of colonization of the American continent is no exception. This analysis was mindfully begun during the Dr. Martin Luther King, Jr., holiday weekend of 2022, and was completed and is submitted in mindful anticipation of observance of Black History Month 2022 in the USA. The twofold purpose of this submission is to (1) demonstrate how open-source software and public-domain data can be used to answer important questions and (2) offer my perspective on an important and timely issue as a scientist and native of the American Deep South.

Readers interested solely in peer review of my web application may scroll down, noting visualization of my intermediate data products, to the section labelled "Embedded Shiny Web Application". You will find my tidy data posted in my Github repo. Readers who are not fluent in R, or do not need to consider reproducibility, may skip my code chunks without missing anything. Readers who are fluent in R and wish to consider reproducibility are welcome to fork my repo and proceed; all I ask is that you inform me that you have done so, and please feel free to push your work and share it! Please also go to the Washington Post source publication and data. This project is a splendid example of scholarly journalism, among many others hosted by this newspaper.

## For Reviewers and Interested Readers from beyond the USA

The USA is governed at the federal level, and the states are represented, by three branches: executive, legislative, and judicial. The legislative branch comprises two houses of Congress: the Senate and the House of Representatives. Senators are elected to a term of six years and Representatives are elected to a term of two years. Each Congress runs for two years. Our  history of congressional representation begins in the year 1789 with the first Congress and continues to the present with the 117th Congress. As our Union has grown from the original thirteen colonies to the current fifty states, the District of Columbia, and several territories, and as colonization has grown generally from east to west, historical representation (proportional to population and to longevity within the Union) has become very large in the New England and mid-Atlantic regions and sparse in the plains and west coast. 

## Data Preprocessing

The raw data curated by the Washington Post team require preprocessing, accomplished as follows:


```r
# setwd locally in toolbar; run ls()
rawDat<-read.csv(file='slaveowners.csv', header=TRUE, sep=',')
library(tidyverse)
tidyDat<-rawDat %>% select(bioguide, states_served, congresses_served, is_slaveholder) %>% filter(congresses_served>0) %>% separate(col=congresses_served, into=letters, sep=',') %>% pivot_longer(letters, names_to = "Which") %>% filter(value>0) %>% select(-Which) %>% rename(Congress=value) %>% group_by(Congress, states_served, is_slaveholder) %>% summarize(count=n()) %>% mutate(Congress=as.numeric(Congress)) # head(tidyDat)
```

## Scatter Pie map for Bird's-Eye View of Data

The map below will orient the reader to a bird's-eye view of the cumulative historic representation in Congress by members who either held slaves, did not hold slaves, or whose status on slave ownership is unknown. The appearance of the map is the result of my compromise of cartographic best practices with my intent to convey a message. The range of state-level Congressional representation is from 70 to 2602 member-Congresses. This range cannot be symbolized satisfactorily on a map without mathematical transformation. For this reason I employed a square-root transformation and divided the square root by 25. This preserves the visual impression that states to the East have high historical representation and states to the West do not. Next, I symbolized each state's magnitude of historical Congressional representation as the radius of a state-level pie chart and the proportion of that representation who held slaves as the angle of the red-colored wedge within each pie. Finally, I labelled each state by its postal service abbreviation and repelled the label so that the extent of the red wedges would not be obscured. The reader will note that mid-Atlantic, mid-South and Deep South states have the largest red wedges. The hopeful result I intend is that all readers, regardless of familiarity with US history and emergent geography, will understand which states have or have not had high levels of historical Congressional representation with slaveholder sensibilities. 


```r
# source for states polygons:https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2021&layergroup=States+%28and+equivalent%29

# source for guidance regarding procedures: https://www.coursera.org/learn/jhu-advanced-data-visualization-r/lecture/VyiGE/simple-features-maps

# define states represented by at least one slaveholder congressman
library(sf)
poly<-read_sf("tl_2021_us_state.shp")
bigstatesList<-tidyDat %>% filter(is_slaveholder=="true") %>% distinct(states_served) 
slavePoly <- poly %>% filter(STUSPS %in% bigstatesList$states_served) # head(slavePoly)

# solve for coordinates of polygon centroids
polyCent <- st_centroid(slavePoly) # head(polyCent)

# prepare data for join with centroids
mapDat <- tidyDat %>%
     filter(str_length(states_served)<3) %>%
     pivot_wider(names_from = is_slaveholder, values_from = count) %>%
     mutate(total=sum(c_across(c(false, true, unknown)), na.rm = TRUE)) %>%
     group_by(states_served) %>%
     summarise(
          sumFalse=sum(false, na.rm = TRUE),
          sumTrue=sum(true, na.rm = TRUE),
          sumUnknown=sum(unknown, na.rm = TRUE),
          sumTotal=sum(total, na.rm = TRUE)
     ) %>%
     filter(sumTrue>0) # head(mapDat)

# join mapDat to centroids
mapDatPoints<-merge(polyCent, mapDat, by.x="STUSPS", by.y="states_served", all.x=TRUE, all.y=TRUE) # head(mapDatPoints); dim(mapDatPoints)

############## prepare data for scatterpie
# prepare data; initialize data frame
long <- as.numeric(mapDatPoints$INTPTLON)
lat <- as.numeric(mapDatPoints$INTPTLAT)
d <- data.frame(long=long, lat=lat)
d$state <- mapDatPoints$STUSPS
d$slaveowner_true <- mapDatPoints$sumTrue
d$slaveowner_false <- mapDatPoints$sumFalse
d$slaveowner_unknown <- mapDatPoints$sumUnknown
d$total <- mapDatPoints$sumTotal
d <- d %>%
     filter(total>5) # %>%  filter out one anomalous row

############## make scatterpie
library(scatterpie); library(ggrepel)
range(d$total)
```

```
## [1]   70 2602
```

```r
d$radius <- round(sqrt(d$total)/25, digits = 3) # explain square root transformation
range(d$radius)
```

```
## [1] 0.335 2.040
```

```r
#head(d); str(d)

# visualize data on a map
state <- map_data("state")

p <- ggplot(state, aes(long, lat, region)) +
geom_map(map=state, aes(map_id=region), fill=NA, color="black") + 
coord_quickmap() + labs(title="A Bird's-Eye View of Historical Slaveholder Congressional Representation", caption = "Radius of  pies = size of historical representation; angle of red wedges = proportion who ever held slaves.")

p + geom_scatterpie(aes(x=long, y=lat, group=state, r=radius), data=d, cols=c("slaveowner_true", "slaveowner_false", "slaveowner_unknown"), alpha=.8) +
#geom_scatterpie_legend(d$radius, x=-130, y=25) +
geom_label_repel(aes(x=long, y=lat, group=state, r=radius, label=state, label.size=0.01), data=d, stat='identity') + theme(legend.position="none")
```

<img src="slaveholderCongressmanStrokeCode_files/figure-html/map-1.png" width="100%" />

## Animated Stacked Bar Graph

Below I present an animated stacked bar graph of state level magnitude of Congressional representation, along with color-coded symbolization of the proportion who had ever held slaves in red, those who had not in green, and those whose status on slave ownership is unknown in yellow. The animation runs for about three minutes. The history of our Union is depicted here in four eras. The first era (1789-1863) begins with the first Congress and ends with the Emancipation Proclamation issued by president Abraham Lincoln in 1863, during the 37th Congress. This proclamation declared that all persons formerly enslaved would be henceforth free forever. The second era (1863-1923) spans the interval beginning with the Emancipation Proclamation and ending with the last Congress including any representative who had ever held slaves (the 67th). The third era (1923-1973) spans the years for which data are available wherein no Congressman had ever held slaves. The fourth, and modern, era, spans the years from the (putative) implementation of contemporary civil rights to the present.

Viewing the animation will take about three minutes. Each Congress is displayed in two frames (one per second) because each Congress comprises two years. The reader is invited to mindfully note (1) from the beginning how many years (about 75) and how large any red bars are present, indicating slave owner representation; (2) how many years since Emancipation (37th Congress; about 60 years) representation still reflected slave owner sensibilities; (3) how many years since then have elapsed in the data at hand when no slave holder sensibilities were officially represented (67th Congress; about 50 years); and (4) the years since Congressional representation is no longer reflected in the data at hand. The animation runs for about three minutes; each Congress is run for two frames, since each Congress comprises two years; and the entire sequence depicted comprises 184 years.


```r
# filter out rows indicating more than one state served
tidyDat<-tidyDat %>% filter(str_length(states_served)<3, states_served!="OL") # %>% glimpse 

# plot animated bargraph
library(gganimate)

barAnim<- ggplot(tidyDat, aes(x=factor(states_served), y=count, fill=is_slaveholder)) + geom_bar(stat="identity") + transition_states(Congress) + scale_fill_manual(values=c("green", "red", "yellow")) + labs(title = 'The animation scrolls through each Congress as follows: {closest_state}') + theme(axis.text.x = element_text(angle = 60, hjust = 1))

animate(barAnim, nframes = 190, fps = 1, height = 400, width = 1200)
```

<img src="slaveholderCongressmanStrokeCode_files/figure-html/graph-1.gif" width="100%" />

```r
# anim_save("barAnim.gif",animation=barAnim)
```

## State-Level Historical Slaveholder Representation and Contemporary Stroke Mortality

The stroke mortality data at hand are curated as a three-year average as of 2015 of state-level stroke mortality among adults aged greater than 35 per 100,000 population. These data were joined with slaveholder data to enable plotting with state-level historical slaveholder representation on the x-axis and state-level contemporary stroke mortality on the y-axis. Since there are many US states where historical representation by slave-owner Congressmen is zero or very low, this variable was log-transformed to render it mathematically accessible for plotting and to spread out these very low values for exploration.


```r
# source for stroke data and metadata: https://catalog.data.gov/dataset/stroke-mortality-data-among-us-adults-35-by-state-territory-and-county-8fc3c
# name of download changed to stroke.csv for ease of manipulation
stroke<-read.csv("stroke.csv", header = TRUE, sep = ',')

# prepare stroke data for join
stroke<-stroke %>%
     select(LocationAbbr, GeographicLevel, Data_Value, Stratification1, Stratification2, LocationID, Location.1) %>%
     filter(GeographicLevel=="State" & Stratification1=="Overall" & Stratification2=="Overall") %>%
     arrange(desc(Data_Value)) # %>% glimpse
# dim(stroke); names(stroke); head(stroke); str(stroke)

# prepare slaveholder data for join
plotDat<-tidyDat %>%
     group_by(states_served) %>%
     filter(str_length(states_served)<3, states_served!="OL") %>%
     mutate(True=ifelse(is_slaveholder=="true", 1, 0)) %>%
     summarise(sumTrue=sum(True), sumCount=sum(count)) %>%
     mutate(propTrue=sumTrue/sumCount) %>%
     # explain log transformation
     mutate(logPropTrue=round(log(propTrue+1), digits = 3)) # %>% glimpse
# dim(plotDat); names(plotDat); head(plotDat); str(plotDat)

# join data
joinStroke<-merge(stroke, plotDat, by.x="LocationAbbr", by.y="states_served", all.x=FALSE, all.y=FALSE)
# head(joinStroke); dim(joinStroke)
# write.csv (joinStroke,'joinStroke.csv')
```

## Render static plot

Inspection of a static scatterplot of state-level contemporary stroke mortality on the y-axis by the logarithm of the proportion of each state's historical Congressional representation who have ever held slaves on the x-axis, overlaid by a regression line with a default 95% confidence interval, yields a strong visual impression of a positive slope, indicating that as historical slaveholder representation increases at the state level, contemporary stroke mortality also increases. 


```r
pStroke<-ggplot(joinStroke, aes(x=logPropTrue, y=Data_Value, label=LocationAbbr)) + geom_jitter() + geom_smooth(method="lm")
pStroke
```

![](slaveholderCongressmanStrokeCode_files/figure-html/static plot-1.png)<!-- -->

## Render interactive plot

Inspection of an interactive scatterplot programmed as above but with the added capability that the viewer may hover over each point and identify which state is represented, along with the values on the x and y axes, supports the above interpretation.


```r
library(plotly)
pStrokeLy<-ggplotly(pStroke)
pStrokeLy
```

```{=html}
<div id="htmlwidget-2c0d4a0f303f5b318f09" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-2c0d4a0f303f5b318f09">{"x":{"data":[{"x":[0.000203146183490753,0.0822378972655162,0.0881410054827109,0.000262793185189366,0.00791738726813346,0.000188745345920324,0.0398557357899845,0.000273711217567324,0.169633465202898,0.13991113016922,0.0689459438614547,0.000156087135896087,0.00419546322710812,0.000207520782761276,0.0270317305356264,0.0123042787663639,-0.000335652311146259,0.062852446064353,0.0842291294768453,0.00529364586435258,0.0708299822688103,0.0210589838726446,0.000326321608386934,-0.000173210876435041,0.0600448582861573,0.0958598320234567,0.000393975907005369,0.0548429719954729,0.000322338478080928,0.000239393847621977,0.0120180341083556,0.0376680813884363,0.0418825119940564,-0.000101848800480366,0.0113413152610883,0.0086218964567408,-0.000134547486528754,0.0299181418001652,0.0118628277825192,0.000314105685427785,0.0500538740167394,0.0760909704046324,-9.46878194808961e-06,0.069050697401166,0.0408014730094001,0.0527124934837222,0.0417860895277932,0.0133187142780051,0.00717731269672513,0.0158800011524931,0.0309751794109121,0.0131389426345006],"y":[70.1372272051498,98.2999304204807,88.9084794485197,57.5368919812329,69.3389048674144,67.0259621557407,52.620273382552,71.7844704180956,77.6759184976667,69.4627976623923,85.507412751019,68.7654018954188,64.421245784536,71.6916806838103,73.4837698019296,77.7102162282169,75.4036010889895,79.6110310605541,88.8799724937976,55.1681648718938,74.7752895279415,64.6939668037742,74.0794143436104,64.7277410851419,79.1049065736681,98.1036007349193,66.3099794649333,84.6169346985221,65.4150014213659,65.8722652839869,53.8953477762453,60.1172095524147,66.3875571413152,69.0336477054469,50.1618388899043,78.4334210902825,82.726812770348,73.0036892046221,72.8395299145021,52.7911690145172,51.613312458545,87.9724984676763,69.8664610553905,88.9330796767399,81.711140884012,74.7082289507985,73.3191456592269,63.1052414345369,67.6846693603695,66.9280863454193,84.5926206480898,59.3963126090728],"text":["logPropTrue: 0.000<br />Data_Value: 70.1<br />LocationAbbr: AK","logPropTrue: 0.082<br />Data_Value: 98.3<br />LocationAbbr: AL","logPropTrue: 0.088<br />Data_Value: 88.9<br />LocationAbbr: AR","logPropTrue: 0.000<br />Data_Value: 57.5<br />LocationAbbr: AZ","logPropTrue: 0.008<br />Data_Value: 69.3<br />LocationAbbr: CA","logPropTrue: 0.000<br />Data_Value: 67.0<br />LocationAbbr: CO","logPropTrue: 0.040<br />Data_Value: 52.6<br />LocationAbbr: CT","logPropTrue: 0.000<br />Data_Value: 71.8<br />LocationAbbr: DC","logPropTrue: 0.170<br />Data_Value: 77.7<br />LocationAbbr: DE","logPropTrue: 0.140<br />Data_Value: 69.5<br />LocationAbbr: FL","logPropTrue: 0.069<br />Data_Value: 85.5<br />LocationAbbr: GA","logPropTrue: 0.000<br />Data_Value: 68.8<br />LocationAbbr: HI","logPropTrue: 0.004<br />Data_Value: 64.4<br />LocationAbbr: IA","logPropTrue: 0.000<br />Data_Value: 71.7<br />LocationAbbr: ID","logPropTrue: 0.027<br />Data_Value: 73.5<br />LocationAbbr: IL","logPropTrue: 0.012<br />Data_Value: 77.7<br />LocationAbbr: IN","logPropTrue: 0.000<br />Data_Value: 75.4<br />LocationAbbr: KS","logPropTrue: 0.063<br />Data_Value: 79.6<br />LocationAbbr: KY","logPropTrue: 0.084<br />Data_Value: 88.9<br />LocationAbbr: LA","logPropTrue: 0.005<br />Data_Value: 55.2<br />LocationAbbr: MA","logPropTrue: 0.071<br />Data_Value: 74.8<br />LocationAbbr: MD","logPropTrue: 0.021<br />Data_Value: 64.7<br />LocationAbbr: ME","logPropTrue: 0.000<br />Data_Value: 74.1<br />LocationAbbr: MI","logPropTrue: 0.000<br />Data_Value: 64.7<br />LocationAbbr: MN","logPropTrue: 0.060<br />Data_Value: 79.1<br />LocationAbbr: MO","logPropTrue: 0.096<br />Data_Value: 98.1<br />LocationAbbr: MS","logPropTrue: 0.000<br />Data_Value: 66.3<br />LocationAbbr: MT","logPropTrue: 0.055<br />Data_Value: 84.6<br />LocationAbbr: NC","logPropTrue: 0.000<br />Data_Value: 65.4<br />LocationAbbr: ND","logPropTrue: 0.000<br />Data_Value: 65.9<br />LocationAbbr: NE","logPropTrue: 0.012<br />Data_Value: 53.9<br />LocationAbbr: NH","logPropTrue: 0.038<br />Data_Value: 60.1<br />LocationAbbr: NJ","logPropTrue: 0.042<br />Data_Value: 66.4<br />LocationAbbr: NM","logPropTrue: 0.000<br />Data_Value: 69.0<br />LocationAbbr: NV","logPropTrue: 0.011<br />Data_Value: 50.2<br />LocationAbbr: NY","logPropTrue: 0.009<br />Data_Value: 78.4<br />LocationAbbr: OH","logPropTrue: 0.000<br />Data_Value: 82.7<br />LocationAbbr: OK","logPropTrue: 0.030<br />Data_Value: 73.0<br />LocationAbbr: OR","logPropTrue: 0.012<br />Data_Value: 72.8<br />LocationAbbr: PA","logPropTrue: 0.000<br />Data_Value: 52.8<br />LocationAbbr: PR","logPropTrue: 0.050<br />Data_Value: 51.6<br />LocationAbbr: RI","logPropTrue: 0.076<br />Data_Value: 88.0<br />LocationAbbr: SC","logPropTrue: 0.000<br />Data_Value: 69.9<br />LocationAbbr: SD","logPropTrue: 0.069<br />Data_Value: 88.9<br />LocationAbbr: TN","logPropTrue: 0.041<br />Data_Value: 81.7<br />LocationAbbr: TX","logPropTrue: 0.053<br />Data_Value: 74.7<br />LocationAbbr: UT","logPropTrue: 0.042<br />Data_Value: 73.3<br />LocationAbbr: VA","logPropTrue: 0.013<br />Data_Value: 63.1<br />LocationAbbr: VT","logPropTrue: 0.007<br />Data_Value: 67.7<br />LocationAbbr: WA","logPropTrue: 0.016<br />Data_Value: 66.9<br />LocationAbbr: WI","logPropTrue: 0.031<br />Data_Value: 84.6<br />LocationAbbr: WV","logPropTrue: 0.013<br />Data_Value: 59.4<br />LocationAbbr: WY"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0.00215189873417722,0.00430379746835443,0.00645569620253165,0.00860759493670886,0.0107594936708861,0.0129113924050633,0.0150632911392405,0.0172151898734177,0.0193670886075949,0.0215189873417722,0.0236708860759494,0.0258227848101266,0.0279746835443038,0.030126582278481,0.0322784810126582,0.0344303797468354,0.0365822784810127,0.0387341772151899,0.0408860759493671,0.0430379746835443,0.0451898734177215,0.0473417721518987,0.049493670886076,0.0516455696202532,0.0537974683544304,0.0559493670886076,0.0581012658227848,0.060253164556962,0.0624050632911392,0.0645569620253165,0.0667088607594937,0.0688607594936709,0.0710126582278481,0.0731645569620253,0.0753164556962025,0.0774683544303797,0.079620253164557,0.0817721518987342,0.0839240506329114,0.0860759493670886,0.0882278481012658,0.090379746835443,0.0925316455696203,0.0946835443037975,0.0968354430379747,0.0989873417721519,0.101139240506329,0.103291139240506,0.105443037974684,0.107594936708861,0.109746835443038,0.111898734177215,0.114050632911392,0.11620253164557,0.118354430379747,0.120506329113924,0.122658227848101,0.124810126582278,0.126962025316456,0.129113924050633,0.13126582278481,0.133417721518987,0.135569620253165,0.137721518987342,0.139873417721519,0.142025316455696,0.144177215189873,0.146329113924051,0.148481012658228,0.150632911392405,0.152784810126582,0.154936708860759,0.157088607594937,0.159240506329114,0.161392405063291,0.163544303797468,0.165696202531646,0.167848101265823,0.17],"y":[66.9306141916678,67.2544465124369,67.5782788332059,67.902111153975,68.2259434747441,68.5497757955132,68.8736081162822,69.1974404370513,69.5212727578204,69.8451050785895,70.1689373993586,70.4927697201276,70.8166020408967,71.1404343616658,71.4642666824349,71.788099003204,72.111931323973,72.4357636447421,72.7595959655112,73.0834282862803,73.4072606070494,73.7310929278184,74.0549252485875,74.3787575693566,74.7025898901257,75.0264222108947,75.3502545316638,75.6740868524329,75.997919173202,76.321751493971,76.6455838147401,76.9694161355092,77.2932484562783,77.6170807770474,77.9409130978164,78.2647454185855,78.5885777393546,78.9124100601237,79.2362423808928,79.5600747016618,79.8839070224309,80.2077393432,80.5315716639691,80.8554039847382,81.1792363055072,81.5030686262763,81.8269009470454,82.1507332678145,82.4745655885835,82.7983979093526,83.1222302301217,83.4460625508908,83.7698948716599,84.0937271924289,84.417559513198,84.7413918339671,85.0652241547362,85.3890564755053,85.7128887962743,86.0367211170434,86.3605534378125,86.6843857585816,87.0082180793507,87.3320504001197,87.6558827208888,87.9797150416579,88.303547362427,88.627379683196,88.9512120039651,89.2750443247342,89.5988766455033,89.9227089662724,90.2465412870414,90.5703736078105,90.8942059285796,91.2180382493487,91.5418705701178,91.8657028908868,92.1895352116559,92.513367532425],"text":["logPropTrue: 0.000000000<br />Data_Value: 66.93061","logPropTrue: 0.002151899<br />Data_Value: 67.25445","logPropTrue: 0.004303797<br />Data_Value: 67.57828","logPropTrue: 0.006455696<br />Data_Value: 67.90211","logPropTrue: 0.008607595<br />Data_Value: 68.22594","logPropTrue: 0.010759494<br />Data_Value: 68.54978","logPropTrue: 0.012911392<br />Data_Value: 68.87361","logPropTrue: 0.015063291<br />Data_Value: 69.19744","logPropTrue: 0.017215190<br />Data_Value: 69.52127","logPropTrue: 0.019367089<br />Data_Value: 69.84511","logPropTrue: 0.021518987<br />Data_Value: 70.16894","logPropTrue: 0.023670886<br />Data_Value: 70.49277","logPropTrue: 0.025822785<br />Data_Value: 70.81660","logPropTrue: 0.027974684<br />Data_Value: 71.14043","logPropTrue: 0.030126582<br />Data_Value: 71.46427","logPropTrue: 0.032278481<br />Data_Value: 71.78810","logPropTrue: 0.034430380<br />Data_Value: 72.11193","logPropTrue: 0.036582278<br />Data_Value: 72.43576","logPropTrue: 0.038734177<br />Data_Value: 72.75960","logPropTrue: 0.040886076<br />Data_Value: 73.08343","logPropTrue: 0.043037975<br />Data_Value: 73.40726","logPropTrue: 0.045189873<br />Data_Value: 73.73109","logPropTrue: 0.047341772<br />Data_Value: 74.05493","logPropTrue: 0.049493671<br />Data_Value: 74.37876","logPropTrue: 0.051645570<br />Data_Value: 74.70259","logPropTrue: 0.053797468<br />Data_Value: 75.02642","logPropTrue: 0.055949367<br />Data_Value: 75.35025","logPropTrue: 0.058101266<br />Data_Value: 75.67409","logPropTrue: 0.060253165<br />Data_Value: 75.99792","logPropTrue: 0.062405063<br />Data_Value: 76.32175","logPropTrue: 0.064556962<br />Data_Value: 76.64558","logPropTrue: 0.066708861<br />Data_Value: 76.96942","logPropTrue: 0.068860759<br />Data_Value: 77.29325","logPropTrue: 0.071012658<br />Data_Value: 77.61708","logPropTrue: 0.073164557<br />Data_Value: 77.94091","logPropTrue: 0.075316456<br />Data_Value: 78.26475","logPropTrue: 0.077468354<br />Data_Value: 78.58858","logPropTrue: 0.079620253<br />Data_Value: 78.91241","logPropTrue: 0.081772152<br />Data_Value: 79.23624","logPropTrue: 0.083924051<br />Data_Value: 79.56007","logPropTrue: 0.086075949<br />Data_Value: 79.88391","logPropTrue: 0.088227848<br />Data_Value: 80.20774","logPropTrue: 0.090379747<br />Data_Value: 80.53157","logPropTrue: 0.092531646<br />Data_Value: 80.85540","logPropTrue: 0.094683544<br />Data_Value: 81.17924","logPropTrue: 0.096835443<br />Data_Value: 81.50307","logPropTrue: 0.098987342<br />Data_Value: 81.82690","logPropTrue: 0.101139241<br />Data_Value: 82.15073","logPropTrue: 0.103291139<br />Data_Value: 82.47457","logPropTrue: 0.105443038<br />Data_Value: 82.79840","logPropTrue: 0.107594937<br />Data_Value: 83.12223","logPropTrue: 0.109746835<br />Data_Value: 83.44606","logPropTrue: 0.111898734<br />Data_Value: 83.76989","logPropTrue: 0.114050633<br />Data_Value: 84.09373","logPropTrue: 0.116202532<br />Data_Value: 84.41756","logPropTrue: 0.118354430<br />Data_Value: 84.74139","logPropTrue: 0.120506329<br />Data_Value: 85.06522","logPropTrue: 0.122658228<br />Data_Value: 85.38906","logPropTrue: 0.124810127<br />Data_Value: 85.71289","logPropTrue: 0.126962025<br />Data_Value: 86.03672","logPropTrue: 0.129113924<br />Data_Value: 86.36055","logPropTrue: 0.131265823<br />Data_Value: 86.68439","logPropTrue: 0.133417722<br />Data_Value: 87.00822","logPropTrue: 0.135569620<br />Data_Value: 87.33205","logPropTrue: 0.137721519<br />Data_Value: 87.65588","logPropTrue: 0.139873418<br />Data_Value: 87.97972","logPropTrue: 0.142025316<br />Data_Value: 88.30355","logPropTrue: 0.144177215<br />Data_Value: 88.62738","logPropTrue: 0.146329114<br />Data_Value: 88.95121","logPropTrue: 0.148481013<br />Data_Value: 89.27504","logPropTrue: 0.150632911<br />Data_Value: 89.59888","logPropTrue: 0.152784810<br />Data_Value: 89.92271","logPropTrue: 0.154936709<br />Data_Value: 90.24654","logPropTrue: 0.157088608<br />Data_Value: 90.57037","logPropTrue: 0.159240506<br />Data_Value: 90.89421","logPropTrue: 0.161392405<br />Data_Value: 91.21804","logPropTrue: 0.163544304<br />Data_Value: 91.54187","logPropTrue: 0.165696203<br />Data_Value: 91.86570","logPropTrue: 0.167848101<br />Data_Value: 92.18954","logPropTrue: 0.170000000<br />Data_Value: 92.51337"],"type":"scatter","mode":"lines","name":"fitted values","line":{"width":3.77952755905512,"color":"rgba(51,102,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0.00215189873417722,0.00430379746835443,0.00645569620253165,0.00860759493670886,0.0107594936708861,0.0129113924050633,0.0150632911392405,0.0172151898734177,0.0193670886075949,0.0215189873417722,0.0236708860759494,0.0258227848101266,0.0279746835443038,0.030126582278481,0.0322784810126582,0.0344303797468354,0.0365822784810127,0.0387341772151899,0.0408860759493671,0.0430379746835443,0.0451898734177215,0.0473417721518987,0.049493670886076,0.0516455696202532,0.0537974683544304,0.0559493670886076,0.0581012658227848,0.060253164556962,0.0624050632911392,0.0645569620253165,0.0667088607594937,0.0688607594936709,0.0710126582278481,0.0731645569620253,0.0753164556962025,0.0774683544303797,0.079620253164557,0.0817721518987342,0.0839240506329114,0.0860759493670886,0.0882278481012658,0.090379746835443,0.0925316455696203,0.0946835443037975,0.0968354430379747,0.0989873417721519,0.101139240506329,0.103291139240506,0.105443037974684,0.107594936708861,0.109746835443038,0.111898734177215,0.114050632911392,0.11620253164557,0.118354430379747,0.120506329113924,0.122658227848101,0.124810126582278,0.126962025316456,0.129113924050633,0.13126582278481,0.133417721518987,0.135569620253165,0.137721518987342,0.139873417721519,0.142025316455696,0.144177215189873,0.146329113924051,0.148481012658228,0.150632911392405,0.152784810126582,0.154936708860759,0.157088607594937,0.159240506329114,0.161392405063291,0.163544303797468,0.165696202531646,0.167848101265823,0.17,0.17,0.17,0.167848101265823,0.165696202531646,0.163544303797468,0.161392405063291,0.159240506329114,0.157088607594937,0.154936708860759,0.152784810126582,0.150632911392405,0.148481012658228,0.146329113924051,0.144177215189873,0.142025316455696,0.139873417721519,0.137721518987342,0.135569620253165,0.133417721518987,0.13126582278481,0.129113924050633,0.126962025316456,0.124810126582278,0.122658227848101,0.120506329113924,0.118354430379747,0.11620253164557,0.114050632911392,0.111898734177215,0.109746835443038,0.107594936708861,0.105443037974684,0.103291139240506,0.101139240506329,0.0989873417721519,0.0968354430379747,0.0946835443037975,0.0925316455696203,0.090379746835443,0.0882278481012658,0.0860759493670886,0.0839240506329114,0.0817721518987342,0.079620253164557,0.0774683544303797,0.0753164556962025,0.0731645569620253,0.0710126582278481,0.0688607594936709,0.0667088607594937,0.0645569620253165,0.0624050632911392,0.060253164556962,0.0581012658227848,0.0559493670886076,0.0537974683544304,0.0516455696202532,0.049493670886076,0.0473417721518987,0.0451898734177215,0.0430379746835443,0.0408860759493671,0.0387341772151899,0.0365822784810127,0.0344303797468354,0.0322784810126582,0.030126582278481,0.0279746835443038,0.0258227848101266,0.0236708860759494,0.0215189873417722,0.0193670886075949,0.0172151898734177,0.0150632911392405,0.0129113924050633,0.0107594936708861,0.00860759493670886,0.00645569620253165,0.00430379746835443,0.00215189873417722,0,0],"y":[63.2954266915554,63.7185307728511,64.1372799294353,64.5513040607696,64.9602116612259,65.3635928501069,65.7610238801218,66.1520733204928,66.5363100270881,66.9133128767221,67.2826820558688,67.6440514675549,67.9971015817582,68.3415718467416,68.6772716523644,69.0040888380553,69.3219948939645,69.6310463061943,69.9313819006575,70.223216471406,70.5068313565624,70.782562882179,71.0507896982468,71.3119199862057,71.5663793583123,71.8146000458128,72.0570117343302,72.29403418922,72.5260716431657,72.7535088005196,72.9767082446581,73.1960090066741,73.4117260548273,73.6241504836647,73.8335502109407,74.0401710231015,74.2442378420516,74.4459561148286,74.6455132525942,74.8430800657522,75.03881215826,75.2328512567949,75.4253264599548,75.6163553996794,75.8060453121107,75.9944940186075,76.1817908199696,76.368017308406,76.5532481026345,76.7375515119063,76.9209901348429,77.10362139886,77.2854980457031,77.4666685682876,77.647177603663,77.8270662865304,78.0063725673506,78.1851314987054,78.3633754932175,78.5411345560008,78.7184364943093,78.8953071067704,79.0717703543389,79.2478485148738,79.4235623230418,79.5989310970603,79.7739728536331,79.9487044122845,80.1231414901654,80.2972987882905,80.4711900700631,80.6448282328487,80.8182253732836,80.9913928469243,81.1643413227866,81.3370808332619,81.5096208198473,81.6819701750842,81.8541372810565,82.0261300447644,82.0261300447644,103.000605020086,102.524933142255,102.049435606689,101.574120320388,101.098995665435,100.624070534373,100.149354368697,99.6748572007993,99.200589699696,98.7265632209435,98.2527898611779,97.7792825177649,97.3060549541076,96.8331218712208,96.3604989862555,95.8882031187358,95.4162522853657,94.9446658043625,94.4734644103927,94.0026703813157,93.532307678086,93.0624020993312,92.5929814523051,92.1240757421218,91.6557173814038,91.187941422733,90.7207858165703,90.2542916976166,89.7885037029216,89.3234703254005,88.8592443067989,88.3958830745326,87.933449227223,87.4720110741212,87.0116432339452,86.5524272989038,86.0944525697969,85.6378168679833,85.1826274296051,84.7290018866018,84.2770693375715,83.8269715091913,83.3788640054187,82.9329176366576,82.4893198140695,82.0482759846922,81.61001107043,81.1747708577293,80.7428232643443,80.3144593848221,79.8899941874225,79.4697667032383,79.0541395156458,78.6434973289975,78.2382443759766,77.838800421939,77.4455951525075,77.0590607989282,76.6796229734579,76.3076898575363,75.9436401011545,75.5878100303649,75.2404809832899,74.9018677539815,74.5721091683526,74.2512617125053,73.93929687659,73.6361025000352,73.3414879727003,73.0551927428483,72.7768972804568,72.5062354885527,72.2428075536098,71.9861923524427,71.7359587409194,71.4916752882623,71.2529182471804,71.0192777369766,70.7903622520226,70.5658016917802,63.2954266915554],"text":["logPropTrue: 0.000000000<br />Data_Value: 66.93061","logPropTrue: 0.002151899<br />Data_Value: 67.25445","logPropTrue: 0.004303797<br />Data_Value: 67.57828","logPropTrue: 0.006455696<br />Data_Value: 67.90211","logPropTrue: 0.008607595<br />Data_Value: 68.22594","logPropTrue: 0.010759494<br />Data_Value: 68.54978","logPropTrue: 0.012911392<br />Data_Value: 68.87361","logPropTrue: 0.015063291<br />Data_Value: 69.19744","logPropTrue: 0.017215190<br />Data_Value: 69.52127","logPropTrue: 0.019367089<br />Data_Value: 69.84511","logPropTrue: 0.021518987<br />Data_Value: 70.16894","logPropTrue: 0.023670886<br />Data_Value: 70.49277","logPropTrue: 0.025822785<br />Data_Value: 70.81660","logPropTrue: 0.027974684<br />Data_Value: 71.14043","logPropTrue: 0.030126582<br />Data_Value: 71.46427","logPropTrue: 0.032278481<br />Data_Value: 71.78810","logPropTrue: 0.034430380<br />Data_Value: 72.11193","logPropTrue: 0.036582278<br />Data_Value: 72.43576","logPropTrue: 0.038734177<br />Data_Value: 72.75960","logPropTrue: 0.040886076<br />Data_Value: 73.08343","logPropTrue: 0.043037975<br />Data_Value: 73.40726","logPropTrue: 0.045189873<br />Data_Value: 73.73109","logPropTrue: 0.047341772<br />Data_Value: 74.05493","logPropTrue: 0.049493671<br />Data_Value: 74.37876","logPropTrue: 0.051645570<br />Data_Value: 74.70259","logPropTrue: 0.053797468<br />Data_Value: 75.02642","logPropTrue: 0.055949367<br />Data_Value: 75.35025","logPropTrue: 0.058101266<br />Data_Value: 75.67409","logPropTrue: 0.060253165<br />Data_Value: 75.99792","logPropTrue: 0.062405063<br />Data_Value: 76.32175","logPropTrue: 0.064556962<br />Data_Value: 76.64558","logPropTrue: 0.066708861<br />Data_Value: 76.96942","logPropTrue: 0.068860759<br />Data_Value: 77.29325","logPropTrue: 0.071012658<br />Data_Value: 77.61708","logPropTrue: 0.073164557<br />Data_Value: 77.94091","logPropTrue: 0.075316456<br />Data_Value: 78.26475","logPropTrue: 0.077468354<br />Data_Value: 78.58858","logPropTrue: 0.079620253<br />Data_Value: 78.91241","logPropTrue: 0.081772152<br />Data_Value: 79.23624","logPropTrue: 0.083924051<br />Data_Value: 79.56007","logPropTrue: 0.086075949<br />Data_Value: 79.88391","logPropTrue: 0.088227848<br />Data_Value: 80.20774","logPropTrue: 0.090379747<br />Data_Value: 80.53157","logPropTrue: 0.092531646<br />Data_Value: 80.85540","logPropTrue: 0.094683544<br />Data_Value: 81.17924","logPropTrue: 0.096835443<br />Data_Value: 81.50307","logPropTrue: 0.098987342<br />Data_Value: 81.82690","logPropTrue: 0.101139241<br />Data_Value: 82.15073","logPropTrue: 0.103291139<br />Data_Value: 82.47457","logPropTrue: 0.105443038<br />Data_Value: 82.79840","logPropTrue: 0.107594937<br />Data_Value: 83.12223","logPropTrue: 0.109746835<br />Data_Value: 83.44606","logPropTrue: 0.111898734<br />Data_Value: 83.76989","logPropTrue: 0.114050633<br />Data_Value: 84.09373","logPropTrue: 0.116202532<br />Data_Value: 84.41756","logPropTrue: 0.118354430<br />Data_Value: 84.74139","logPropTrue: 0.120506329<br />Data_Value: 85.06522","logPropTrue: 0.122658228<br />Data_Value: 85.38906","logPropTrue: 0.124810127<br />Data_Value: 85.71289","logPropTrue: 0.126962025<br />Data_Value: 86.03672","logPropTrue: 0.129113924<br />Data_Value: 86.36055","logPropTrue: 0.131265823<br />Data_Value: 86.68439","logPropTrue: 0.133417722<br />Data_Value: 87.00822","logPropTrue: 0.135569620<br />Data_Value: 87.33205","logPropTrue: 0.137721519<br />Data_Value: 87.65588","logPropTrue: 0.139873418<br />Data_Value: 87.97972","logPropTrue: 0.142025316<br />Data_Value: 88.30355","logPropTrue: 0.144177215<br />Data_Value: 88.62738","logPropTrue: 0.146329114<br />Data_Value: 88.95121","logPropTrue: 0.148481013<br />Data_Value: 89.27504","logPropTrue: 0.150632911<br />Data_Value: 89.59888","logPropTrue: 0.152784810<br />Data_Value: 89.92271","logPropTrue: 0.154936709<br />Data_Value: 90.24654","logPropTrue: 0.157088608<br />Data_Value: 90.57037","logPropTrue: 0.159240506<br />Data_Value: 90.89421","logPropTrue: 0.161392405<br />Data_Value: 91.21804","logPropTrue: 0.163544304<br />Data_Value: 91.54187","logPropTrue: 0.165696203<br />Data_Value: 91.86570","logPropTrue: 0.167848101<br />Data_Value: 92.18954","logPropTrue: 0.170000000<br />Data_Value: 92.51337","logPropTrue: 0.170000000<br />Data_Value: 92.51337","logPropTrue: 0.170000000<br />Data_Value: 92.51337","logPropTrue: 0.167848101<br />Data_Value: 92.18954","logPropTrue: 0.165696203<br />Data_Value: 91.86570","logPropTrue: 0.163544304<br />Data_Value: 91.54187","logPropTrue: 0.161392405<br />Data_Value: 91.21804","logPropTrue: 0.159240506<br />Data_Value: 90.89421","logPropTrue: 0.157088608<br />Data_Value: 90.57037","logPropTrue: 0.154936709<br />Data_Value: 90.24654","logPropTrue: 0.152784810<br />Data_Value: 89.92271","logPropTrue: 0.150632911<br />Data_Value: 89.59888","logPropTrue: 0.148481013<br />Data_Value: 89.27504","logPropTrue: 0.146329114<br />Data_Value: 88.95121","logPropTrue: 0.144177215<br />Data_Value: 88.62738","logPropTrue: 0.142025316<br />Data_Value: 88.30355","logPropTrue: 0.139873418<br />Data_Value: 87.97972","logPropTrue: 0.137721519<br />Data_Value: 87.65588","logPropTrue: 0.135569620<br />Data_Value: 87.33205","logPropTrue: 0.133417722<br />Data_Value: 87.00822","logPropTrue: 0.131265823<br />Data_Value: 86.68439","logPropTrue: 0.129113924<br />Data_Value: 86.36055","logPropTrue: 0.126962025<br />Data_Value: 86.03672","logPropTrue: 0.124810127<br />Data_Value: 85.71289","logPropTrue: 0.122658228<br />Data_Value: 85.38906","logPropTrue: 0.120506329<br />Data_Value: 85.06522","logPropTrue: 0.118354430<br />Data_Value: 84.74139","logPropTrue: 0.116202532<br />Data_Value: 84.41756","logPropTrue: 0.114050633<br />Data_Value: 84.09373","logPropTrue: 0.111898734<br />Data_Value: 83.76989","logPropTrue: 0.109746835<br />Data_Value: 83.44606","logPropTrue: 0.107594937<br />Data_Value: 83.12223","logPropTrue: 0.105443038<br />Data_Value: 82.79840","logPropTrue: 0.103291139<br />Data_Value: 82.47457","logPropTrue: 0.101139241<br />Data_Value: 82.15073","logPropTrue: 0.098987342<br />Data_Value: 81.82690","logPropTrue: 0.096835443<br />Data_Value: 81.50307","logPropTrue: 0.094683544<br />Data_Value: 81.17924","logPropTrue: 0.092531646<br />Data_Value: 80.85540","logPropTrue: 0.090379747<br />Data_Value: 80.53157","logPropTrue: 0.088227848<br />Data_Value: 80.20774","logPropTrue: 0.086075949<br />Data_Value: 79.88391","logPropTrue: 0.083924051<br />Data_Value: 79.56007","logPropTrue: 0.081772152<br />Data_Value: 79.23624","logPropTrue: 0.079620253<br />Data_Value: 78.91241","logPropTrue: 0.077468354<br />Data_Value: 78.58858","logPropTrue: 0.075316456<br />Data_Value: 78.26475","logPropTrue: 0.073164557<br />Data_Value: 77.94091","logPropTrue: 0.071012658<br />Data_Value: 77.61708","logPropTrue: 0.068860759<br />Data_Value: 77.29325","logPropTrue: 0.066708861<br />Data_Value: 76.96942","logPropTrue: 0.064556962<br />Data_Value: 76.64558","logPropTrue: 0.062405063<br />Data_Value: 76.32175","logPropTrue: 0.060253165<br />Data_Value: 75.99792","logPropTrue: 0.058101266<br />Data_Value: 75.67409","logPropTrue: 0.055949367<br />Data_Value: 75.35025","logPropTrue: 0.053797468<br />Data_Value: 75.02642","logPropTrue: 0.051645570<br />Data_Value: 74.70259","logPropTrue: 0.049493671<br />Data_Value: 74.37876","logPropTrue: 0.047341772<br />Data_Value: 74.05493","logPropTrue: 0.045189873<br />Data_Value: 73.73109","logPropTrue: 0.043037975<br />Data_Value: 73.40726","logPropTrue: 0.040886076<br />Data_Value: 73.08343","logPropTrue: 0.038734177<br />Data_Value: 72.75960","logPropTrue: 0.036582278<br />Data_Value: 72.43576","logPropTrue: 0.034430380<br />Data_Value: 72.11193","logPropTrue: 0.032278481<br />Data_Value: 71.78810","logPropTrue: 0.030126582<br />Data_Value: 71.46427","logPropTrue: 0.027974684<br />Data_Value: 71.14043","logPropTrue: 0.025822785<br />Data_Value: 70.81660","logPropTrue: 0.023670886<br />Data_Value: 70.49277","logPropTrue: 0.021518987<br />Data_Value: 70.16894","logPropTrue: 0.019367089<br />Data_Value: 69.84511","logPropTrue: 0.017215190<br />Data_Value: 69.52127","logPropTrue: 0.015063291<br />Data_Value: 69.19744","logPropTrue: 0.012911392<br />Data_Value: 68.87361","logPropTrue: 0.010759494<br />Data_Value: 68.54978","logPropTrue: 0.008607595<br />Data_Value: 68.22594","logPropTrue: 0.006455696<br />Data_Value: 67.90211","logPropTrue: 0.004303797<br />Data_Value: 67.57828","logPropTrue: 0.002151899<br />Data_Value: 67.25445","logPropTrue: 0.000000000<br />Data_Value: 66.93061","logPropTrue: 0.000000000<br />Data_Value: 66.93061"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(153,153,153,0.4)","hoveron":"points","hoverinfo":"x+y","showlegend":false,"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":43.1050228310502},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.00885243492670357,0.178516782615557],"tickmode":"array","ticktext":["0.00","0.05","0.10","0.15"],"tickvals":[0,0.05,0.1,0.15],"categoryorder":"array","categoryarray":["0.00","0.05","0.10","0.15"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"logPropTrue","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[47.5199005833953,105.642543326595],"tickmode":"array","ticktext":["50","60","70","80","90","100"],"tickvals":[50,60,70,80,90,100],"categoryorder":"array","categoryarray":["50","60","70","80","90","100"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Data_Value","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"365467403782":{"x":{},"y":{},"label":{},"type":"scatter"},"365450577463":{"x":{},"y":{},"label":{}}},"cur_data":"365467403782","visdat":{"365467403782":["function (y) ","x"],"365450577463":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Fit, Interpret, and Diagnose Linear Model

The reader will note that the slope and intercept of this model are 150.49 and 66.93, respectively, and that this model explains about 24% of the variability in contemporary stroke mortality among the data at hand. Inspection of the diagnostic plots indicates that the linear model may not be the best fit for these data but let's put this on the back burner for now.


```r
# fit linear model
fit1<-lm(Data_Value~logPropTrue, data = joinStroke)
# interpret model
summary(fit1)
```

```
## 
## Call:
## lm(formula = Data_Value ~ logPropTrue, data = joinStroke)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.855  -5.490   1.360   8.257  19.029 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    66.93       1.81  36.981  < 2e-16 ***
## logPropTrue   150.49      36.46   4.128 0.000139 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.995 on 50 degrees of freedom
## Multiple R-squared:  0.2542,	Adjusted R-squared:  0.2392 
## F-statistic: 17.04 on 1 and 50 DF,  p-value: 0.0001387
```

```r
# diagnose linear model
par(mfrow=c(2, 2), mar=c(2, 2, 2, 2))
plot(fit1)
```

![](slaveholderCongressmanStrokeCode_files/figure-html/model-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```

## Embedded Shiny Web Application

The code below will render my web application. The app will render a scatterplot of state-level contemporary stroke mortality by log-transformed proportion of Congressional representation who have ever held slaves. By using the mouse to drag a rectangle around all the plotted points, the reader will note that the slope and intercept reproduce exactly the slope and intercept of the model fit1 reported above. If the reader wishes to exclude the states which were never represented by slaveowner Congressmen, he or she may sweep the left border of the rectangle slightly to the right to avoid the stack of points representing zero on the x-axis.  The reader will then note that excluding the states never represented by slaveowner Congressmen, the association between those states with historical slaveowner representation and contemporary stroke mortality grows stronger.


```r
library(shiny)
shinyApp(

  # Define UI for application that draws a scatterplot
ui <- fluidPage(
     titlePanel("State Level Contemporary Stroke Mortality by Historical Congressman Slaveholder Status"),
     sidebarLayout(
          sidebarPanel(
               h3("Slope"),
               textOutput("slopeOut"),
               h3("Intercept"),
               textOutput("intOut")
          ),
          mainPanel(
               h5("The user may draw a rectangle with the mouse to plot any subset; please include at least two points. Note in particular that the stack of points labeled zero on the x axis represent those states with no historical slaveholder Congressional representation; and the complement include those states whose representation has historically included at least one member with slaveholder sensibilities."),
               plotOutput("plot1", brush=brushOpts(
                    id="brushP"
               ))
          )
     )
),

# Define server logic required to draw a scatterplot
server <- function(input, output) {
     # Create a reactive expression
     model <- reactive({
          brushed_data<-brushedPoints(joinStroke, input$brushP, xvar="logPropTrue", yvar="Data_Value")
          if(nrow(brushed_data) < 2){
               return(NULL)
          }
          lm(Data_Value ~ logPropTrue, data = brushed_data)
     })
     output$slopeOut <- renderText({
          if(is.null(model())){
               "No Model Found"
          } else {
               model()[[1]][2]
          }
     })
     output$intOut <- renderText({
          if(is.null(model())){
               "No Model Found"
          } else {
               model()[[1]][1]
          }
     })
     output$plot1<-renderPlot({
          plot(joinStroke$logPropTrue, joinStroke$Data_Value, xlab="Log Proportion of Representation with True Slaveholder Status", ylab="Stroke Mortality", main="Stroke Mortality by Congressman Slaveholder Status", cex=1.5, pch=16, bty="n")
          if(!is.null(model())){
               abline(model(), col="blue", lwd=2)
          }
     })
},

  options = list(height = 500)
)
```

`<div style="width: 100% ; height: 500px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}

## Conclusion

I do not intend to teach anyone that stroke mortality is high in the American southeast; that fact is already well documented, and others are continuing to investigate it. What I really want people to understand is that historical exposure to chattel slavery, which has been illegal for 159 years, is hazardous to one's contemporary health and that this historical exposure has somehow arisen from a mechanism which has become at least in part geographically operative. Stroke is a very complex disease, co-determined by many factors; however, my finding that this simple model explains 24% of the variability in stroke mortality in the data at hand is important. Any model of complex disease which includes only one candidate predictor is woefully mis-specified. This proportion of variability explained is unusual under these circumstances.

## On a Professional Note

I have conducted an ordinary least squares regression analysis on a merged data set from two convenient sources. These skills were acquired over a decade of study and practice. I have also made it possible for the reader to fit this linear model and replicate my analysis by simply drawing a rectangle over a plot in a web application embedded in this document. This is a handy superpower which one can use to communicate with one's boss, PhD committee, or promotion and tenure committee. I have done this using only open-source software, data in the public domain, and online resources available at a very modest cost. Most people interested in this kind of analysis will have access to SAS or SPSS but you may wish to work with those who do not. My work will continue to enrich this web app with further capabilities. I welcome your feedback as the work proceeds.

## On a Personal Note

I happen to be a grandfather and a person of faith whose duty includes love of neighbor. Primarily for these two reasons, but including many others, I believe very deeply that racial reconciliation is in everyone's best interest. Many would approach this by labeling our grandchildren as 'oppressor' or 'oppressed'. I believe this to be a mistake; children will either learn to associate success with guilt or they will learn a convenient excuse not to function. In either case they will not learn to learn; they will learn to despise themselves. It is a sad fact that many racist notions are deeply embedded in the institutions that shape our common life. Anyone who doubts this or fails to teach it is simply making a different kind of mistake. *There is no one still alive who is responsible* for this sad heritage. Children must be taught this history or it may repeat itself. Let's work together to strike the proper balance.


