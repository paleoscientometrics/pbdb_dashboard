library(plotly)
library(tidyverse)
library(packcircles)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)

# Data --------------------------------------------------------------------

load("data/refs.RData")
manual_check <- read.csv("data/language_check.csv")

all_refs <- all_refs[all_refs$reference_no %in% unique(completed_refs$reference_no),]
all_refs <- all_refs[!all_refs$reference_no %in% manual_check$reference_no,]

lang <- c(all_refs$language, manual_check$language)
lang <- sort(lang, decreasing = T)
lang[lang==""] <- "Unknown"
lang[lang=="other"] <- "Unknown"

data <- data.frame(table(lang))


# Plot --------------------------------------------------------------------

# * Language --------------------------------------------------------------


set.seed(40)
data <- data[sample(1:nrow(data), nrow(data)),] 
packing <- circleProgressiveLayout(data$Freq, sizetype='area')

# Add a column with the text you want to display for each bubble:
data$text <- paste("Language: ",data$lang, "\n", 
				   "Number of publications:", prettyNum(data$Freq, big.mark = ","))
data$text[data$Freq < 50] <- NA
# We can add these packing information to the initial data frame
data <- cbind(data, packing)


# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
p <- ggplot() + 
	
	# Make the bubbles
	geom_polygon_interactive(data = dat.gg, 
							 aes(x, y, group = id, fill=data$Freq[id], 
							 	tooltip = data$text[id], data_id = id), 
							 colour = "black", alpha = 0.8) +
	
	# Add text in the center of each bubble + control its size
	geom_text(data = data, aes(x, y, size=Freq, label = lang)) +
	scale_size_continuous(range = c(1,10)) +
	scale_fill_gradientn(colours = colorRampPalette(brewer.pal(9, "Greens"))(4)) +
	# General theme:
	theme_void() + 
	theme(legend.position="none") +
	coord_equal()

widg <- ggiraph(ggobj = p, fonts="sans")
widg <- girafe_options(widg,
					opts_zoom(min = .7, max = 10)
					) 

# EPI ---------------------------------------------------------------------

epi <- read.csv("data/2019_EPI.csv")
epi$code <- countrycode::countrycode(epi$countries, "country.name", "iso3c")

gdp <- read.csv(file.path("data", "2021-02-03_GDP_percapita_WorldBank.csv"), skip=4)[,-c(3:4)]
colnames(gdp)[c(1:2)] <- c("country", "code")
gdp_long <- reshape2::melt(gdp, id.vars = c("country", "code"), variable.name = "year", value.name = "gdp")
gdp_long$year <- as.numeric(gsub("X", "", as.character(gdp_long$year)))

epi <- merge(epi, gdp_long[gdp_long$year == 2019,], by="code")
epi <- merge(epi, colls_n)


fig <- plot_ly(epi, x = ~eep_9th_ed, y = ~gdp, text = 
			   	~paste(sprintf("<b>%s</b></br>", epi$country),
			   		   "<br><i>GDP:</i>",
			   		   "$", prettyNum(epi$gdp, big.mark = ","),
			   		   
			   		   "<br><i>EEP Score:</i>", epi$eep_9th_ed,
			   		   "<br><i>Number of fossil collections:</i>", 
			   		   prettyNum(epi$freq, big.mark = ",")
			   	),  
			   size = 10,
			   type = 'scatter', mode = 'markers', 
			   color = ~freq, colors = colorRampPalette(pal[-2], bias=4)(10) 
)


f <- list(
	family = "Roboto Mono",
	size = 14,
	color = "#7f7f7f"
)
x <- list(
	title = "English Proficiency Index",
	titlefont = f, 
	showgrid=FALSE, 
	type="log"
)
y <- list(
	title = "Gross Domestic Product\n(GDP)per capita",
	titlefont = f,
	showgrid=FALSE
)

fig_eng <- fig %>% layout(title = 'Relationship between English skills and GDP',
					  xaxis = x,
					  yaxis = y,
					  autosize = T, width = 550, height=250,
					  annotations = 
					  	list(x = 0.5, y = -0.5, #position of text adjust as needed 
					  		 text = "All values given in the current international $", 
					  		 showarrow = F, xref='paper', yref='paper', 
					  		 xanchor='right', yanchor='auto', xshift=0, yshift=0,
					  		 font=list(size=10, color="grey")),
					  plot_bgcolor="rgba(0,0,0,0)",
					  paper_bgcolor='rgba(0,0,0,0)',
					  margin = m <- list(
					  	b=75, pad=1
					  ),
					  showlegend = FALSE
)

