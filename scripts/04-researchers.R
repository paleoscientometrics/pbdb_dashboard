library(plotly)
library(rgeos)
library(rworldmap)
library(tidyverse)
library(countrycode)


# Load data ---------------------------------------------------------------
pbdb <- readRDS(file.path("data", "pbdb.rds"))
pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

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
                        text = "© 2021 Pal(a)eoScientometrics", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=8, color="grey"))
  )


fig_fn2 <- fig %>% add_markers(
  x = ~x, y = ~y, size = ~n,  
  marker = list(
    color = toRGB(pal[4], alpha=0.8),
    line = list(color=toRGB("white"))
  ),
  hoverinfo = "text",
  text = ~paste(sprintf("<b>%s</b></br>", colls$country), 
                "Number of fossil collections:", "<br />", 
                prettyNum(colls$n, big.mark = ",")
  )
)
