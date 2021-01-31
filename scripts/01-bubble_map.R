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

colls$country[grep("United States", colls$country)] <- "United States of America" 
colls <- merge(colls, df, all.x=TRUE, all.y=FALSE)

colls$iso3c <- countrycode::countrycode(colls$country, "country.name", "iso3c")
colls$region <- countrycode::countrycode(colls$iso3c, "iso3c", "region23")
colls[is.na(colls$region),]$region[1] <- "Antarctica"

colls <- na.omit(colls)

fig <- plot_geo(colls, sizes = c(1, 5000))

g <- list(
	scope = 'world',
	showcountries = TRUE, countrycolor="white",
	showframe = FALSE,
	showcoastlines=FALSE,
	showland=TRUE, landcolor=toRGB("grey85"),
	showocean=FALSE,
	showlakes=TRUE, lakecolor="white",
	projection = list(type = 'robinson')
)

fig<- 
	fig %>% layout(title = '<b>Number of fossil collections per country</b>', 
				   geo = g)


fig_fn <- fig %>% add_markers(
	x = ~x, y = ~y, size = ~n,  
	marker = list(
		color = toRGB(pal[5], alpha=0.8),
		line = list(color=toRGB("white"))
	),
	hoverinfo = "text",
	text = ~paste(sprintf("<b>%s</b></br>", colls$country), "Number of fossil collections:", "<br />", prettyNum(colls$n, big.mark = ","))
)


fig_fn

