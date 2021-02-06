library(plotly)
library(rgeos)
library(rworldmap)
library(tidyverse)
library(countrycode)


# Load data ---------------------------------------------------------------
pbdb <- readRDS(file.path("data", "pbdb.rds"))
pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")
income <- read.csv(file.path("data", "2019_income_classification_worldbank.csv"))
researchers <- read.csv(file.path("data", "2021-02-06_researchers_Worldbank.csv"), skip=4)

load(file.path("data", "refs.RData"))

#cleaning
completed_refs$aff_country <- gsub("Brasil", "Brazil", completed_refs$aff_country)
completed_refs$aff_country <- gsub("^Chi$", "China", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Denkmark", "Denmark", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Yemen Arab Republic", "Yemen", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Columbia", "Colombia", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Morroco", "Morocco", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Phillipines", "Philippines", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Northern Ireland", "UK", completed_refs$aff_country)
completed_refs$aff_country <- gsub("ISA", "USA", completed_refs$aff_country)
completed_refs$aff_country <- gsub("ISA", "USA", completed_refs$aff_country)
completed_refs$aff_country <- gsub("Pland", "Poland", completed_refs$aff_country)
completed_refs$aff_cc <- countrycode(completed_refs$aff_country, origin="country.name", destination = "iso3c")
completed_refs <- completed_refs[!is.na(completed_refs$aff_cc),]

pbdb <- pbdb[pbdb$reference_no %in% all_refs$reference_no,] # 1989 - 2021 only
df <- merge(pbdb, completed_refs[,c("reference_no", "aff_country", "aff_cc")])

# per country
colls <- df %>% distinct(aff_cc, collection_no) %>% 
  group_by(aff_cc) %>% 
  tally() %>% 
  ungroup()

# get country centre

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE, id=wmap$ISO3)

# get a data.frame with centroids
df <- as.data.frame(centroids)
df$aff_cc <- row.names(df)

colls <- merge(colls, df, all.x=T, all.y=F)
colls <- na.omit(colls)
colls$country <- countrycode(colls$aff_cc, "iso3c", "country.name")

# add income
colls <- merge(colls, income[,c("code", "classification")], 
               by.x="aff_cc", by.y="code", 
               all.x=TRUE, all.y=FALSE)
colls$classification <- factor(colls$classification, levels=c("L", "LM", "UM", "H"), labels=c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"))

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
  fig %>% layout(title = '<b>Number of fossil collections per affiliated countries </b>', 
                 geo = g, 
                 margin = list(b=50), ##bottom margin in pixels
                 annotations = 
                   list(x = 0.3, y = -0.05, #position of text adjust as needed 
                        text = "Copyright © 2021 Pal(a)eoScientometrics", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=8, color="grey")
                   )
  )


fig <- fig %>% add_markers(
  x = ~x, y = ~y, size = ~n,color=~classification, colors = "Greens",
  marker = list(
    
    line = list(color=toRGB("white"))
  ),
  hoverinfo = "text",
  text = ~paste(sprintf("<b>%s</b></br>", colls$country), 
                "Number of fossil collections:", "<br />", 
                prettyNum(colls$n, big.mark = ",")
  ),
  showlegend=TRUE
)

fig_fn2 <- fig %>% layout(legend=list(itemsizing="constant"))


# Researchers -------------------------------------------------------------
researchers <- researchers[grep("income$", researchers$Country.Name),]
researchers <- researchers[researchers$Country.Name != "Low & middle income",]

researchers <- reshape2::melt(researchers, id.vars=c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code"),
               variable.name = "year")
researchers$year <- as.numeric(gsub("^X", "", researchers$year))

researchers <- researchers %>% 
  filter(year > 1990) %>% 
  group_by(Country.Name) %>% 
  summarise(value=mean(value, na.rm=TRUE))

researchers$value[2] <- 0
researchers$classification <- factor(researchers$Country.Name, 
                                     levels=c("Low income", "Lower middle income", 
                                              "Upper middle income", "High income"), 
                                     labels=c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"))


# Plot --------------------------------------------------------------------

f <- list(
  family = "Roboto Mono",
  size = 14,
  color = "#7f7f7f"
)

x <- list(
  title = "",
  titlefont = f, 
  showgrid=FALSE
)

y <- list(
  title = "",
  titlefont = f,
  showgrid=FALSE)


fig2 <- plot_ly(researchers, y = ~classification, x = ~value, orientation = 'h', 
                type = 'bar', color=~classification, colors ="Greens", 
                marker=list(width=0.8), width = 350, height=250,
                hoverinfo = "text",
                text = ~paste(sprintf("<b>%s</b> per million people", round(researchers$value)))
                )


fig2 <- fig2 %>% layout(title = list(text='Number of researchers \nper million people',
                        font=list(size=16)),
                      xaxis = x,
                      yaxis = y,
                      autosize = T,
                      plot_bgcolor="rgba(0,0,0,0)",
                      paper_bgcolor='rgba(0,0,0,0)',
                      margin = m <- list(
                        t=50, b=10, l=50, r=50, pad=1
                      ),
                      showlegend = FALSE,
                      annotations = 
                        list(x = 1400, y = 0, #position of text adjust as needed 
                             text = "No data available", 
                             showarrow = F, xref='x', yref='y', 
                             align="center")
)


