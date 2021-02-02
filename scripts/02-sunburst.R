library(dplyr)
library(widgetframe)
library(sunburstR)

pal <- c("#f0ffe9", "#ffe599", "#bbe487", "#4e9755", "#173109")

# Load data --------------------------------------------------------------
dat <- readRDS(file.path("data", "pbdb.rds"))
load(file.path("data", "refs.RData"))
income <- read.csv(file.path("data", "2019_income_classification_worldbank.csv"))
gdp <- read.csv(file.path("data", "2020-05-28_GDP_WorldBank.csv"))

dat$country2 <- countrycode::countryname(dat$country)
dat$code <- countrycode::countrycode(dat$country2, origin="country.name", destination = "iso3c")
dat$code <- as.character(dat$code)

#check if any missing
check <- countrycode::codelist_panel
dat$code[grep("Micronesia", dat$country)] <- unique(check$iso3c)
cc <- unique(dat[,c("country", "country2", "code")])

# No of collections per country

colls <- unique(dat[,c("collection_no", "code")])

colls_n <- data.frame(table(dat$code), stringsAsFactors = FALSE)
colnames(colls_n) <- c("code", "freq")

coll_n <- merge(merge(colls_n, cc), income[,-2], all.x=TRUE, all.y=TRUE, by="code")
coll_n$region <- countrycode::countrycode(coll_n$code, origin="iso3c",
										  destination="continent")

coll_n <- na.omit(coll_n)

coll_n$code <- as.character(coll_n$code)
coll_n$freq <- as.numeric(coll_n$freq)
coll_n$reg.inc <- paste(coll_n$region, coll_n$classification, sep=".")

coll_n <- coll_n %>% mutate(classification=recode(classification,
						 "H"="High Income",
						 "UM"="Upper Middle Income",
						 "LM"="Lower Middle Income",
						 "L"="Low Income"))

coll_n$pathString = paste("World", coll_n$region, coll_n$classification, coll_n$country, sep="-")

seq <- coll_n[,c("pathString", "freq")]

# colors

# match those colors to leaf names, matched by index
labels <- c("World", 
			"Africa", "Asia", "Oceania", "Europe", "Americas",
			"Low Income", "Lower Middle Income", "Upper Middle Income", "High Income",
			coll_n[order(coll_n$freq),]$country)

colpal <- colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"), bias=1)
colours <- c(pal[5], colpal(5), 
			 colpal(4),
			 colpal(nrow(coll_n)))

fig_sb <- sunburst(seq, count=TRUE,
				   colors = list(range = colours,
				   			  domain = labels), 
				   legend=FALSE,
				   percent = FALSE)
fig_sb

