library(dplyr)
library(reshape2)
library(plotly)

# Economic indicators -----------------------------------------------------

gdp <- read.csv(file.path("data", "2021-02-03_GDP_percapita_WorldBank.csv"), skip=4)[,-c(3:4)]
research_fund <- read.csv(file.path("data", "2020-05-28_RD_WorldBank.csv"))[,-3] # % of GDP
colnames(gdp)[c(1:2)] <- colnames(research_fund)[c(1:2)] <- c("country", "code")

gdp_long <- melt(gdp, id.vars = c("country", "code"), variable.name = "year", value.name = "gdp")
research_long <- melt(research_fund, id.vars = c("country", "code"), 
					  variable.name = "year", value.name = "RD")

df <- merge(gdp_long, research_long)
df$RD <- df$RD * df$gdp # actual numbers

df$year <- as.numeric(gsub("X", "", as.character(df$year)))

df.summary <- df %>% 
	filter(year > 1989) %>% 
	group_by(country, code) %>% 
	summarise(gdp=mean(gdp, na.rm = TRUE), RD = mean(RD, na.rm = TRUE)) %>% 
	ungroup()

df.summary <- merge(df.summary, colls_n) # Add collection no

desired_maximum_marker_size <- 40
your_list_of_size_values <- df.summary$logfreq
sizeref <- max(your_list_of_size_values) / (desired_maximum_marker_size**2)

df.summary$logfreq <- log(df.summary$freq)

fig <- plot_ly(df.summary, x = ~gdp, y = ~RD, text = 
			   	~paste(sprintf("<b>%s</b></br>", df.summary$country),
			   		   "<br><i>GDP:</i>",
			   		   "$", prettyNum(df.summary$gdp, big.mark = ","),
			   		   
			   		   "<br><i>Research Funding:</i>",
			   		   "$", prettyNum(df.summary$RD, big.mark = ","),
			   		   
			   		   
			   		   "<br><i>Number of fossil collections:</i>", 
			   		   prettyNum(df.summary$freq, big.mark = ",")
			   		   
			   		   
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
	title = "Gross Domestic Product (GDP) per capita",
	titlefont = f, 
	showgrid=FALSE, 
	type="log"
)
y <- list(
	title = "Research Funding \nper capita",
	titlefont = f,
	showgrid=FALSE
)

fig <- fig %>% layout(title = 'Relationship between GDP and Research Funding',
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
					  )
)
