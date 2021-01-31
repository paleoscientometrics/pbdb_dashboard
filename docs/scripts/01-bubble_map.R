library(plotly)
library(rgeos)
library(rworldmap)
library(tidyverse)

pbdb <- readRDS(file.path("data", "pbdb.rds"))
pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

# per country
colls <- pbdb %>% distinct(country, collection_no) %>% 
	group_by(country) %>% 
	tally() %>% 
	ungroup()

# get country centre

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
df$country <- row.names(df)

colls <- merge(colls, df, all.x=TRUE, all.y=FALSE)
colls <- na.omit(colls)

g <- list(
	scope = 'world',
	showcountries=TRUE, 
	lakecolor = toRGB('white'),
	landcolor = toRGB("gray85"),
	countrycolor = toRGB("white"),
	projection_type="natural earth")

fig <- plot_geo(colls, sizes = c(1, 250))
fig <- fig %>% add_markers(
	x = ~x, y = ~y, size = ~n,  
	marker = list(
		color = paste("rgba(", paste(as.vector(col2rgb(pal[5])), collapse = ","), ",0.7)"),
		line = list(
			color = 'rgba(255, 255, 255, 0)'
		)
	),
	hoverinfo = "text",
	text = ~paste("Number of collections:", "<br />", colls$n)
)

fig <- fig %>% layout(title = '<b>Number of fossil collections per country</b>', geo = g)
fig



