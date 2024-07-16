#SCRIPT for Essential Biodiversity Variables analysis####
#Anne Kristin Eilrich 05/2023

#Housekeeping####
setwd("C:/Users/jan/OneDrive/Postdoc HIFMB/Biodiversity Policy Targets Project/FinalData/FinalData")

library(networkD3)
library(htmlwidgets)
library(webshot)

install.packages("wesanderson")
library("wesanderson")

# See all palettes
names(wes_palettes)
pal <- wes_palette("Zissou1", 6, type = "continuous")
pal
as.character(pal)
#_####
#RESEARCH####

#Import data####
Research <- read.csv("Research.CSV", sep=";")
head(Research)

#Sankey dataframe####
#create link dataframe for sankey plot
#list of links between nodes (variables & targets) with intensity for each link (no. of indicators)
links1 <- data.frame(
 source=Research$Source, 
 target=Research$Variable, 
 value=as.character(Research$Records))

#create node dataframe that lists every entities involved in links (variables & targets)
nodes1 <- data.frame(name=unique(c(links1$source, links1$target)))

#Coloration####
#set colours for links: add a 'group' column to each connection
variable <- Research$Variable
variable
links1$group <- as.factor(variable)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to mgive them the same colour
nodes1$group <- as.factor(c("Var"))

#doublecheck the node ID (node$name) to make sure the ID matches the name in the data frame but is written without underscore
nodes1$name
nodes1$NodeID = c("Marine Biodiversity Research", "Genetic composition", "Species populations", "Species traits", "Community composition", "Ecosystem structure", "Ecosystem function")
print(nodes1)

# Set colours for each group:
sankey.col <- 'd3.scaleOrdinal() .domain(["Genetic composition", "Species populations", "Species_ traits", "Community composition", "Ecosystem structure", "Ecosystem_function", "Var"]) .range(["#BDC881", "#4472c4", "#000080", "#32806E", "#F11B00", "#E3B710", "silver"])'


#Combine####
#links and node dataframes are matched to each other
links1$IDsource <- match(links1$source, nodes1$name)-1 
links1$IDtarget <- match(links1$target, nodes1$name)-1

#Plot####
plot.r <- sankeyNetwork(Links = links1, Nodes = nodes1,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "NodeID",
                        iterations = 0,
                        colourScale = sankey.col,
                        LinkGroup = "group",
                        NodeGroup = "group",
                        fontSize = 16, fontFamily = "arial",
                        nodeWidth=10,
                        sinksRight = FALSE,
                        nodePadding = 3)

plot.r

#for this plot, we want to have the node labels on the left side of the plot, therefore use the onRender function
plot.r <- onRender(plot.r, 'function(el,x)
         {d3.selectAll(".node text")
         .filter(d => d.group)
         .attr("text-anchor", "end")
         .attr("x",-5);}')

#Save####
#the html plot can be adjusted manually in order to sort the nodes in the right order
saveNetwork(plot.r,"Research.html", selfcontained = FALSE)
webshot("Research.html", "Research.pdf")

#_####
#PRE2020 TARGETS####

#Import data####
Targets <- read.csv("Pre2020_targets.CSV", sep=";")
head(Targets)

#Sankey dataframe####
#create link dataframe for sankey plot
#list of links between nodes (variables & targets) with intensity for each link (no. of indicators)
links2 <- data.frame(
  source=Targets$Variable, 
  target=Targets$Target, 
  value=as.character(Targets$Indicators))

#create node dataframe that lists every entities involved in links (variables & targets)
nodes2 <- data.frame(name=unique(c(links2$source, links2$target)))

#Coloration####
#set colours for links: add a 'group' column to each connection
variable <- Targets$Variable
variable
links2$group <- as.factor(variable)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to mgive them the same colour
nodes2$group <- as.factor(c("Var"))

#doublecheck the node ID (node$name) to make sure the ID matches the name in the data frame but is written without underscore
nodes2$name
nodes2$NodeID = c("Genetic composition", "Species populations", "Species traits", "Community composition", "Ecosystem structure", "Ecosystem function", "Not considered", "Aichi 1", "Aichi 4", "Aichi 5", "Aichi 6", "Aichi 8", "Aichi 9", "Aichi 10", "Aichi 11", "Aichi 12", "Aichi 14", "Aichi 15", "Aichi 17", "Aichi 19")
print(nodes2)

# Set colours for each group:
sankey.col <- 'd3.scaleOrdinal() .domain(["Genetic composition", "Species populations", "Species_ traits", "Community composition", "Ecosystem structure", "Ecosystem_function", "Var"]) .range(["#BDC881", "#4472c4", "#000080", "#32806E", "#F11B00", "#E3B710", "silver"])'


#Combine####
#links and node dataframes are matched to each other
links2$IDsource <- match(links2$source, nodes2$name)-1 
links2$IDtarget <- match(links2$target, nodes2$name)-1

#Plot####
plot.t <- sankeyNetwork(Links = links2, Nodes = nodes2,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "NodeID",
                        iterations = 0,
                        colourScale = sankey.col,
                        LinkGroup = "group",
                        NodeGroup = "group",
                        fontSize = 16, fontFamily = "arial",
                        nodeWidth=10,
                        sinksRight = FALSE,
                        nodePadding = 3)

plot.t

#Save####
#the html plot can be adjusted manually in order to sort the nodes in the right order
saveNetwork(plot.t,"Pre2020_targets.html", selfcontained = FALSE)
webshot("Pre2020_targets.html", "Pre2020_targets.pdf")
#_####
#PRE2020 ASSESSMENTS####

#Import data####
Assessments <- read.csv("Pre2020_assessments.CSV", sep=";")
head(Assessments)

#Sankey dataframe####
#create link dataframe for sankey plot
#list of links between nodes (variables & targets) with intensity for each link (no. of indicators)
links3 <- data.frame(
 source=Assessments$Variable, 
 target=Assessments$Assessment, 
 value=as.character(Assessments$Indicators))

#create node dataframe that lists every entities involved in links (variables & targets)
nodes3 <- data.frame(name=unique(c(links3$source, links3$target)))

#Coloration####
#set colours for links: add a 'group' column to each connection
variable <- Assessments$Variable
variable
links3$group <- as.factor(variable)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to mgive them the same colour
nodes3$group <- as.factor(c("Var"))

#doublecheck the node ID (node$name) to make sure the ID matches the name in the data frame but is written without underscore
nodes3$name
nodes3$NodeID = c("Genetic composition",  "Species populations", "Species traits", "Community composition", "Ecosystem structure", "Ecosystem function","Not considered", "CBD", "CITES", "CMS", "IPBES", "RAMSAR", "SDG")
print(nodes3)

# Set colours for each group:
sankey.col <- 'd3.scaleOrdinal() .domain(["Genetic composition", "Species populations", "Species_ traits", "Community composition", "Ecosystem structure", "Ecosystem_function", "Var"]) .range(["#BDC881", "#4472c4", "#000080", "#32806E", "#F11B00", "#E3B710", "silver"])'

#Combine####
#links and node dataframes are matched to each other
links3$IDsource <- match(links3$source, nodes3$name)-1 
links3$IDtarget <- match(links3$target, nodes3$name)-1

#Plot####
plot.a <- sankeyNetwork(Links = links3, Nodes = nodes3,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "NodeID",
                        iterations = 0,
                        colourScale = sankey.col,
                        LinkGroup = "group",
                        NodeGroup = "group",
                        fontSize = 16, fontFamily = "arial",
                        nodeWidth=10,
                        sinksRight = FALSE,
                        nodePadding = 3)

plot.a

#Save####
#the html plot can be adjusted manually in order to sort the nodes in the right order
saveNetwork(plot.a,"Pre2020_assessments.html", selfcontained = FALSE)
webshot("Pre2020_assessments.html", "Pre2020_assessments.pdf")

#_####
#POST2020 MARINE TARGETS####

#Import data####
Marine <- read.csv("Post2020_marine.CSV", sep=";")
head(Marine)

#Sankey dataframe####
#create link dataframe for sankey plot
#list of links between nodes (variables & targets) with intensity for each link (no. of indicators)
links5 <- data.frame(
 source=Marine$Variable, 
 target=Marine$Target, 
 value=as.character(Marine$Indicators))

#create node dataframe that lists every entities involved in links (variables & targets)
nodes5 <- data.frame(name=unique(c(links5$source, links5$target)))

#Coloration####
#set colours for links: add a 'group' column to each connection
variable <- Marine$Variable
variable
links5$group <- as.factor(variable)

# Add a 'group' column to each node. Here I decide to put all of them in the same group to mgive them the same colour
nodes5$group <- as.factor(c("Var"))

#doublecheck the node ID (node$name) to make sure the ID matches the name in the data frame but is written without underscore
nodes5$name
nodes5$NodeID = c("Genetic composition", "Species populations", "Community composition", "Ecosystem structure", "Ecosystem function", "Species traits", "A", "B", "1", "2", "3", "4", "5", "7", "9", "11")
print(nodes5)

# Set colours for each group:
sankey.col <- 'd3.scaleOrdinal() .domain(["Genetic composition", "Species populations", "Community composition", "Ecosystem structure", "Ecosystem function", "Species traits", "Var"]) .range(["#BDC881", "#4472c4", "#32806E", "#F11B00", "#E3B710", "#000080", "silver"])'


#Combine####
#links and node dataframes are matched to each other
links5$IDsource <- match(links5$source, nodes5$name)-1 
links5$IDtarget <- match(links5$target, nodes5$name)-1

#Plot####
plot.m <- sankeyNetwork(Links = links5, Nodes = nodes5,
                        Source = "IDsource", Target = "IDtarget",
                        Value = "value", NodeID = "NodeID",
                        iterations = 0,
                        colourScale = sankey.col,
                        LinkGroup = "group",
                        NodeGroup = "group",
                        fontSize = 16, fontFamily = "arial",
                        nodeWidth=10,
                        sinksRight = FALSE,
                        nodePadding = 3)

plot.m

#Save####
#the html plot can be adjusted manually in order to sort the nodes in the right order
saveNetwork(plot.m,"Post2020_marine.html", selfcontained = FALSE)
webshot("Post2020_marine.html", "Post2020_marine.pdf")
