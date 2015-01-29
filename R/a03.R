library(dplyr)
library(noncensus)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(statebins)
library(gridExtra)

# For the map ggplot theme ------------------------------------------------

devtools::source_gist("33baa3a79c5cfef0f6df")

# Read in NSF data --------------------------------------------------------

dat <- read.csv("data/NSF_master_table.csv", stringsAs=FALSE)

data(zip_codes)

# Restrict records to those with valid states -----------------------------

dat %>%
  filter(dat$state != "  ") %>%
  select(year=published_year, STUSPS=state) -> dat

# First aggregration ------------------------------------------------------

dat %>% count(year, STUSPS) -> by_state

# Get shapefile -----------------------------------------------------------

us <- readOGR(dsn="data/state.geojson", "OGRGeoJSON")

# Convert to Winkel-Tripel projection -------------------------------------

us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# Move some states/territories around -------------------------------------

alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1700000))
proj4string(hawaii) <- proj4string(us_aea)

puerto_rico <- us_aea[us_aea$STATEFP=="72",]
puerto_rico <- elide(puerto_rico, shift=c(-2410000, 0))
proj4string(puerto_rico) <- proj4string(us_aea)

us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"),]
us_aea <- rbind(us_aea, alaska, hawaii, puerto_rico)

# Turn into something ggplot can use --------------------------------------

map <- fortify(us_aea, region="STUSPS")

by_state %>%
  count(STUSPS, wt=n) %>%
  select(id=STUSPS, n) -> state_totals


# Plot animation panel by year using bars for cumulative data -------------

i <- 0

for (YEAR in sort(unique(by_state$year))) {

  by_state %>%
    filter(year==YEAR) %>%
    select(id=STUSPS, n) -> awards

# Make the map ------------------------------------------------------------

  gg <- ggplot()
  gg <- gg + geom_map(data=map, map=map,
                      aes(x=long, y=lat, map_id=id, group=group),
                      fill="#ffffff", color="#0e0e0e", size=0.15)
  gg <- gg + geom_map(data=awards, map=map,
                      aes(map_id=id, fill=n),
                      color="#0e0e0e", size=0.15)
  gg <- gg + scale_fill_distiller(palette="BuGn", name="# Awards")
  gg <- gg + labs(title=sprintf("NSF 'Nuclear' Awards by State, %s", YEAR))
  gg <- gg + coord_equal()
  gg <- gg + theme_map()
  gg <- gg + theme(legend.position="right")
  gg <- gg + theme(plot.title=element_text(size=26))
  gg <- gg + theme(legend.position="top")
  choro <- gg

# Make the bars -----------------------------------------------------------

  by_state %>%
    filter(year<=YEAR) %>%
    count(STUSPS, wt=n) %>%
    select(state=STUSPS, n) %>%
    right_join(data.frame(state=state.abb, stringsAsFactors=FALSE), by="state") %>%
    mutate(n=ifelse(is.na(n), 0, n),
           col=factor(as.numeric(cut(n, breaks=c(0,30,60,90))))) -> cumulative_awards

  gg <- ggplot(data=cumulative_awards, aes(x=state, y=n))
  gg <- gg + geom_bar(stat="identity", aes(fill=col), color="#00441b", width=0.75, size=0.5)
  gg <- gg + scale_y_continuous(limits=c(0, 90))
  gg <- gg + scale_fill_brewer(palette="BuGn")
  gg <- gg + labs(x=NULL, y=NULL, title=NULL)
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position="none")
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(axis.text.x=element_text(size=7))
  bars <- gg

# Output file -------------------------------------------------------------

  png(sprintf("output/panel-%03d.png", i), width=800, height=600)
  grid.arrange(choro, bars, ncol=1, heights=c(3,1))
  dev.off()

  i <- i + 1

}

# Make the last panel -----------------------------------------------------

by_state %>%
  count(STUSPS, wt=n) %>%
  select(id=STUSPS, n) -> state_totals

gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="#ffffff", color="#0e0e0e", size=0.15)
gg <- gg + geom_map(data=state_totals, map=map,
                    aes(map_id=id, fill=n),
                    color="#0e0e0e", size=0.15)
gg <- gg + scale_fill_distiller(palette="BuGn", name="# Awards")
gg <- gg + labs(title="NSF 'Nuclear' Awards by State (Cumulative)")
gg <- gg + coord_equal()
gg <- gg + theme_map()
gg <- gg + theme(legend.position="right")
gg <- gg + theme(plot.title=element_text(size=26))
gg <- gg + theme(legend.position="top")
choro <- gg

state_totals %>%
  select(state=id, n) %>%
  right_join(data.frame(state=state.abb, stringsAsFactors=FALSE), by="state") %>%
  mutate(n=ifelse(is.na(n), 0, n),
         col=factor(as.numeric(cut(n, breaks=c(0,30,60,90))))) -> cumulative_awards

gg <- ggplot(data=cumulative_awards, aes(x=state, y=n))
gg <- gg + geom_bar(stat="identity", aes(fill=col), color="#00441b", width=0.75, size=0.5)
gg <- gg + scale_y_continuous(limits=c(0, 90))
gg <- gg + scale_fill_brewer(palette="BuGn")
gg <- gg + labs(x=NULL, y=NULL, title=NULL)
gg <- gg + theme_bw()
gg <- gg + theme(legend.position="none")
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=7))
bars <- gg

png(sprintf("output/panel-%03d.png", i), width=800, height=600)
grid.arrange(choro, bars, ncol=1, heights=c(3,1))
dev.off()

# Convert to animated gif (needs ImageMagick) -----------------------------

wd <- getwd()
setwd("output/")
system("convert -delay 100 -loop 1 panel*g nuclear.gif")
setwd(wd)
