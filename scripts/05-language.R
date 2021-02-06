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
lang[lang==""] <- "Unknown"

data <- data.frame(table(lang))


# Plot --------------------------------------------------------------------

packing <- circleProgressiveLayout(data$Freq, sizetype='area')

# Add a column with the text you want to display for each bubble:
data$text <- paste("Language: ",data$lang, "\n", 
				   "Number of publications:", prettyNum(data$Freq, big.mark = ","))

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

widg <- ggiraph(ggobj = p)
