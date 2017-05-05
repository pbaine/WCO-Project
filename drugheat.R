theme_clean <- function(base_size = 12) {
      require(grid)
      theme_grey(base_size) %+replace%
            theme(
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank(),
                  panel.grid = element_blank(),
                  panel.spacing = unit(0, "lines"),
                  plot.margin = unit(c(0,0,0,0),"lines"),
                  complete = TRUE)
}

library(maps)
library(RColorBrewer)
library(extrafont)
library(devtools)
library(dplyr)
library(ggalt)
library(ggplot2)

####2015 DRUG TRAFFICKING INSTANCE HEAT####

capwords <- function(s, strict = FALSE) {
      cap <- function(s) paste(toupper(substring(s, 1, 1)),
                               {s <- substring(s, 2); if(strict) tolower(s) else s},
                               sep = "", collapse = " " )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

route <- read.csv('2015route.csv')
drugs <- read.csv('Drugs/2015drugs.csv')

drugid <- select(drugs, T_CASE_FK)
route <- left_join(drugid, route, by = "T_CASE_FK")
names(route) <- tolower(names(route))

route$country_full <- capwords(as.character(route$country_full), strict = T)
route$country_full <- gsub(' And ', ' and ', route$country_full)
route$country_full <- gsub(' Of ',' of ', route$country_full)
route$country_full <- gsub(' Of', ' of', route$country_full)
route$country_full <- gsub(' The ',' the ', route$country_full)
route$country_full <- gsub(' The', ' the', route$country_full)
route$country_full <- gsub('Iran, Islamic Republic of', 'Iran', route$country_full)
route$country_full <- gsub('Russian Federation', 'Russia', route$country_full)
route$country_full <- gsub('Hong Kong, China', 'China', route$country_full)
route$country_full <- gsub('United States', 'USA', route$country_full)
route$country_full <- gsub('United Kingdom', 'UK', route$country_full)
route$country_full <- gsub('The Former Yugoslav Republic of Macedonia', 'Macedonia', route$country_full)
route$country_full[route$country_full == "Union of Myanmar (republic of the)"] <- "Myanmar"
route$country_full <- gsub("Lao People's Democratic Republic",'Laos', route$country_full)
route$country_full <- gsub("Afghanistan, Islamic Republic of",'Afghanistan', route$country_full)
route$country_full <- gsub('Moldova, Republic of','Moldova', route$country_full)
route$country_full <- gsub('Korea, Republic of','South Korea', route$country_full)
route$country_full <- gsub("Taiwan, Province of China",'Taiwan', route$country_full)
route$country_full <- gsub("Guinea-bissau",'Guinea-Bissau', route$country_full)
route$country_full <- gsub("British Virgin Islands","Virgin Islands", route$country_full)
route$country_full <- gsub("Syrian Arab Republic",'Syria', route$country_full)
route$country_full <- gsub('Congo, Republic of the','Republic of Congo', route$country_full)
route$country_full <- gsub('Us Virgin Islands','Virgin Islands', route$country_full)
route$country_full <- gsub("Cote D'ivoire",'Ivory Coast', route$country_full)
route$country_full <- gsub("Macao, China","China", route$country_full)

countrylist <- as.character(unique(route$country_full))

departure <- filter(route, route_order == 'Departure')
departure <- as.data.frame(table(departure$country_full))
names(departure) <- c('Country', 'Departure')

countrydf <- as.data.frame(countrylist)
names(countrydf) <- c('Country')
transit <- countrydf
for(i in 1:7){
      trans <- filter(route, route_order == paste0('Transit_',i))
      trans <- as.data.frame(table(trans$country_full))
      names(trans) <- c('Country', paste0('Transit_',i))
      transit <- merge(transit, trans, by = 'Country', all = T)
}

transit[is.na(transit)] <- 0
transit <- transit %>% mutate(Transit = rowSums(transit[,2:8])) %>% select(Country, Transit)

destination <- filter(route, route_order == 'Destination')
destination <- as.data.frame(table(destination$country_full))
names(destination) <- c('Country', 'Destination')

routecount <- merge(merge(merge(countrydf, departure, by = 'Country', all = T), transit, by = 'Country', all = T), destination, by = 'Country', all = T)
routecount[is.na(routecount)] <- 0
routecount$Sum <- rowSums(routecount[,2:4])

####

world_map <- map_data("world")
world_map <- subset(world_map, region!="Antarctica")

world_map$region <- gsub('Sint Maarten','Netherlands Antilles',world_map$region)
world_map$region <- gsub('Trinidad','Trinidaad and Tobaago',world_map$region)
world_map$region <- gsub('Tobago','Trinidaad and Tobaago',world_map$region)
world_map$region <- gsub('Trinidaad and Tobaago','Trinidad and Tobago',world_map$region)
world_map$region <- gsub('Antigua','Antiigua and Baarbuda',world_map$region)
world_map$region <- gsub('Barbuda','Antiigua and Baarbuda',world_map$region)
world_map$region <- gsub("Antiigua and Baarbuda",'Antigua and Barbuda',world_map$region)
world_map$region <- gsub('Bonaire','Boonaire, Sint Euustatius, and Saaba',world_map$region)
world_map$region <- gsub('Sint Eustatius','Boonaire, Sint Euustatius, and Saaba',world_map$region)
world_map$region <- gsub('Saba','Boonaire, Sint Euustatius, and Saaba',world_map$region)
world_map$region <- gsub('Boonaire, Sint Euustatius, and Saaba','Bonaire, Sint Eustatius and Saba',world_map$region)

heat <- merge(world_map, routecount, by.y = "Country", by.x = "region", all = T)
heat <- arrange(heat, group, order)

dev_mode(on = T)

library(ggalt)
library(ggplot2)
library(extrafont)

gg <- ggplot(data = heat, aes(x = long, y = lat, group = group, fill = Sum)) + 
      geom_cartogram(dat = world_map, map = world_map, aes(map_id = region), color = "gray20", fill = "white", lwd = .05) + 
      theme_clean() #+ coord_proj("+proj=robin")

gg <- gg + geom_cartogram(map = world_map, aes(map_id = region, fill = Sum), color = "gray20", lwd = .05) + 
      scale_fill_distiller(palette = "Greens", na.value = "gray87", direction = 1) + 
      theme(legend.position = c(0.5, 0.12),
            legend.direction = "horizontal",
            legend.background = element_rect(color = "grey20"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Gill Sans MT"),
            legend.key.height = unit(.08,"in"),
            legend.key.width = unit(.25,"in"),
            panel.background = element_rect(size = 2, color = "black"))

dev_mode(on = F)

png(file = "2015drugheatTI.png", units="in", width=10, height=5, res=600)

gg

dev.off()