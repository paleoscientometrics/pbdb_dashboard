library(dplyr)
library(reshape2)
library(plotly)
library(ggplot2)

# Data --------------------------------------------------------------------

HDI <- read.csv("data/2021_HDI_UNDP.csv", skip=5)[,-1]
HDI <- HDI[,-grep("X\\.", colnames(HDI))]
GPI <- read.csv("data/2019_GPI.csv")

HDI$code <- countrycode::countrycode(HDI$Country, origin="country.name", "iso3c")
hdi_long <- melt(HDI, id.vars = c("Country", "code"), 
				 variable.name = "year", value.name = "hdi")

hdi_long$year <- as.numeric(gsub("X", "", as.character(hdi_long$year)))
hdi_long$hdi <- as.numeric(gsub("\\.\\.", NA, hdi_long$hdi))


hdi <- hdi_long %>% filter(year > 1989) %>% 
	group_by(Country, code) %>% 
	summarise(hdi=mean(hdi, na.rm = TRUE))

GPI$code <- countrycode::countrycode(GPI$Countries, origin="country.name", "iso3c")
colnames(GPI)[2] <- "GPI"

gdp <- read.csv(file.path("data", "2021-02-03_GDP_percapita_WorldBank.csv"), skip=4)[,-c(3:4)]
colnames(gdp)[c(1:2)] <- c("country", "code")

gdp_long <- melt(gdp, id.vars = c("country", "code"), variable.name = "year", value.name = "gdp")
gdp_long$year <- as.numeric(gsub("X", "", as.character(gdp_long$year)))

gdp_long <- gdp_long %>% 
	filter(year > 1989) %>% 
	group_by(country, code) %>% 
	summarise(gdp=mean(gdp, na.rm = TRUE)) %>% 
	ungroup()

epi <- read.csv("data/2019_EPI.csv")
native <- readLines("data/native.txt")
epi <- rbind(epi, cbind(countries=native, eep_8th_ed=80, eep_9th_ed=80, change=0))
epi$code <- countrycode::countrycode(epi$countries, "country.name", "iso3c")

df.summary <- merge(
	merge(
		merge(
			merge(hdi, GPI), 
			colls_n), 
		gdp_long[,-1]),
	setNames(epi[,c("code", "eep_9th_ed")], c("code", "epi"))
)

df.summary$epi <- as.numeric(df.summary$epi)

#Use the ideal sizeref value
desired_maximum_marker_size <- 80
your_list_of_size_values <- df.summary$freq
sizeref <- 2.0 * max(your_list_of_size_values) / (desired_maximum_marker_size**2)

f <- list(
	family = "Roboto Mono",
	size = 14,
	color = "#7f7f7f"
)
x <- list(
	title = "Human Development Index",
	titlefont = f, 
	showgrid=FALSE, 
	type="log"
)
y <- list(
	title = "Global Peace Index",
	titlefont = f,
	showgrid=FALSE,
	autorange="reversed"
)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
opac <- range01(df.summary$epi)

fig_hdi <- plot_ly(df.summary, colors = colorRampPalette(pal[-2], bias=2)(10),
				   height=500)

fig_hdi <- fig_hdi %>% layout(title = 'Global Peace, Economic Growth,\nHuman Development and Fossil Collections',
							  xaxis = x,
							  yaxis = y,
							  legend=list(title="GDP per capita")) %>% 
	add_segments(x = 0.7, xend = 0.7, y = 1, yend = 3.5, showlegend=F, 
				 line=list(width=1,
				 		  color='DarkSlateGrey')) %>%
	add_segments(x = 0, xend = 1, y = 2, yend = 2, showlegend=F, color="black",
				 line=list(width=1,
				 		  color='DarkSlateGrey'))

fig_hdi <- fig_hdi %>%  add_markers(x = ~hdi, y = ~GPI, 
									type = 'scatter',
									marker = list(size = ~freq, opacity = 0.5, sizemode="area", sizeref=sizeref,
												  line=list(width=2,
												  		  color=paste0('rgba(0,0,0,',opac,')'))
									),
									color = ~gdp, 
									text = 
										~paste(sprintf("<b>%s</b></br>", df.summary$Country),
											   "<br><i>HDI:</i>",
											   "$", round(df.summary$hdi, 2),
											   
											   "<br><i>GPI:</i>",
											   "$", round(df.summary$GPI, 2),
											   
											   "<br><i>GDP per capita:</i>",
											   "$", prettyNum(df.summary$gdp, big.mark = ","),
											   
											   "<br><i>English Proficiency Score:</i>",
											   df.summary$epi,
											   
											   
											   "<br><i>Number of fossil collections:</i>", 
											   prettyNum(df.summary$freq, big.mark = ",")
											   
											   
										))

a <- list(
	x = -0.22,
	y = 3,
	text = "China and Russia both \nfall short of GPI and HDI\nbut remain global economic\npowerhouses.",
	showarrow=F,
	xref = "x",
	yref = "y",
	font=list(size=10),
	align="left"
)

fig_hdi <- fig_hdi %>% 
	layout(annotations = a) %>% 
	add_segments(x = 0.6, xend = df.summary$hdi[df.summary$Countries=="China"], 
				 y = 2.8, yend = df.summary$GPI[df.summary$Countries=="China"], showlegend=F, 
				 line=list(width=1,
				 		  color='DarkSlateGrey')) %>% 
add_segments(x = 0.68, xend = df.summary$hdi[df.summary$Countries=="Russia"], 
			 y = df.summary$GPI[df.summary$Countries=="Russia"], 
			 yend =  df.summary$GPI[df.summary$Countries=="Russia"], showlegend=F, 
			 line=list(width=1,
			 		  color='DarkSlateGrey'))

b <- list(
	x = -0.05,
	y = 3.4,
	xref = "x",
	yref = "y",
	text = "The US despite having lower GPI \nhas one of the highest HDI in the world, \nand is the global leader in research \nin palaeontology",
	showarrow=F,
	font=list(size=10),
	align="left"
)

fig_hdi <- fig_hdi %>% 
	layout(annotations = b) %>% 
	add_segments(x = 0.8, xend = df.summary$hdi[df.summary$code=="USA"], 
										 y = 3.2, yend = df.summary$GPI[df.summary$code=="USA"], showlegend=F, 
										 line=list(width=1,
										 		  color='DarkSlateGrey'))

# How to ------------------------------------------------------------------

