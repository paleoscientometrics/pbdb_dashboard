library(dplyr)
library(reshape2)
library(plotly)

# Economic indicators -----------------------------------------------------

gdp <- read.csv(file.path("data", "2020-05-28_GDP_WorldBank.csv"))[,-3]
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

desired_maximum_marker_size <- 20
your_list_of_size_values <- df.summary$freq
sizeref <- 2.0 * max(your_list_of_size_values) / (desired_maximum_marker_size**2)



fig <- plot_ly(df.summary, x = ~gdp, y = ~RD, text = ~country,  sizes = c(1, 50),
			   type = 'scatter', mode = 'markers', color = ~freq, colors = 'Greens'  #,
			   # marker = list(size = ~freq, opacity = 0.8, sizemode = 'area', sizeref = sizeref)
			   )

fig <- fig %>% layout(title = 'Relationship between GDP and Research Funding',
					  xaxis = list(showgrid = FALSE),
					  yaxis = list(showgrid = FALSE),
					  autosize = F, width = 500, height = 200)
