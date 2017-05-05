library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)

drugs <- fread('Drugs/2016drugs.csv')
names(drugs) <- tolower(names(drugs))

x <- select(drugs, t_case_fk, category, quantity, unit)

case <- read.csv('2016_other/2016case.csv')
names(case) <- tolower(names(case))
y <- select(case, t_case_id, offence_date, offence_month)
y <- rename(y, t_case_fk = t_case_id)

z <- left_join(x,y,by = "t_case_fk")
z$offence_date <- as.Date(z$offence_date)
z <- filter(z, category != "Other-Specify")

pdf('2016drugcategorybydate_facet.pdf')

ggplot(z, aes(x = offence_date, fill = category)) + 
      geom_histogram(binwidth = 1) + 
      facet_grid(category~.) + guides(fill = FALSE) +
      scale_x_date(date_breaks = '1 month', date_labels = '%B') + 
      labs(x = "", y = 'Seizure Count', title = "2015 Drug Seizures by Category") + 
      coord_cartesian(xlim = as.Date(c('2016-01-01','2016-12-31'))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

pdf('2016categoryseizcount.pdf')
ggplot(z, aes(x = factor(category, levels = c('Khat','Cannabis','Psy drugs','Cocaine','New Psychoactive Substances','Opiates','Other')), fill = category)) + 
      geom_histogram(stat = 'count') + labs(x = "", y="Seizure Count",title = "Total 2015 Drug Seizures by Category") + 
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


####Transit Information -----------------------------------------------------------------------

capwords <- function(s, strict = FALSE) {
      cap <- function(s) paste(toupper(substring(s, 1, 1)),
                               {s <- substring(s, 2); if(strict) tolower(s) else s},
                               sep = "", collapse = " " )
      sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

route <- fread('2016_other/2016route.csv')
drugs <- fread('Drugs/2016drugs.csv')

drugid <- select(drugs, T_CASE_FK)
route <- left_join(drugid, route, by = "T_CASE_FK")
names(route) <- tolower(names(route))

route$country_full <- capwords(as.character(route$country_full), strict = T)
route$country_full <- gsub(' And ', ' and ', route$country_full)
route$country_full <- gsub(' Of ',' of ', route$country_full)
route$country_full <- gsub(' Of', ' of', route$country_full)
route$country_full <- gsub(' The ',' the ', route$country_full)
route$country_full <- gsub(' The', ' the', route$country_full)

countrylist <- as.character(unique(route$country_full))

departure <- filter(route, route_order == 'Departure')
departure <- as.data.frame(table(departure$country_full))
names(departure) <- c('Country', 'Departure')

countrydf <- as.data.frame(countrylist)
names(countrydf) <- c('Country')
transit <- countrydf
for(i in 1:5){
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

transgraph <- data.frame(subset(routecount, Sum > 500 & Sum < 10000))
transgraph <- transform(transgraph, Country = reorder(Country, Sum))
transgraph <- select(transgraph, Country, Departure, Transit, Destination)

stack <- melt(data = transgraph, id = "Country")

gg <- ggplot(data = stack, aes(x = Country, y = value, fill = factor(variable, levels = rev(levels(variable))))) + 
      geom_bar(stat = "identity") + 
      theme_bw() + coord_flip() +
      scale_fill_brewer(palette = 'Set1', direction = -1, breaks = levels(stack$variable)) +
      scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
      ylab("Trafficking Instance Count") +
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.major.x = element_line(size = .5), 
            axis.title.y = element_blank(), 
            axis.title.x = element_text(family = "Gill Sans MT", size = 12, color = "black"),
            axis.text = element_text(family = "Gill Sans MT", size = 12, color = "black"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Gill Sans MT", size = 12, color = "black"))

ggsave("2016drugtrans2.pdf", gg, device = cairo_pdf, width = 11, height = 8.5, units = "in")


####Drugs Weight####

drugs$unit[is.na(drugs$unit)] <- "None"
drugs[,kilo := 0]
for(i in 1:nrow(drugs)){
      if (drugs[i,unit] == "Kg"){
            drugs[i,kilo := drugs[i,quantity]]
      }else if(drugs[i,unit] == "Litres"){
            drugs[i,kilo := drugs[i,quantity]]
      }else if(drugs[i,unit] == "Plants"){
            drugs[i,kilo := drugs[i,quantity] * .01]
      }else if(drugs[i,unit] == "None"){
            drugs[i,kilo := 0]
      }else if(drugs[i,unit] == "Grams"){
            drugs[i,kilo := drugs[i,quantity] * .001]
      }else{
            drugs[i,kilo := drugs[i,quantity]*(.001/3)]
      }
}

####QUANTILES --------------------------------------------------------------------

catlist <- unique(drugs$category)
catlist <- catlist[!is.na(catlist)]
catlist <- catlist[-8]
quantiles <- NULL
setkey(drugs, category)
for (j in 1:length(catlist)){
      drug <- catlist[j]
      quan <- as.data.frame(quantile(drugs[drug]$kilo, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99,1)))
      names(quan) <- drug
      quantiles <- bind_cols(quantiles, quan)
}
rownames(quantiles) <- c('10%', '20%','30%','40%','50%','60%','70%','80%','90%','95%','99%','100%')
quantiles <- select(quantiles, -`Other-Specify`)

####REGIONAL MATCHING ---------------------------------------------------------------

countrylist <- fread('countrycodeswithregion.csv')
case <- rename(case, iso2=REPORTING_COUNTRY_ISO2)
countrylist <- rename(countrylist, iso2 = `alpha-2`)
joined <- left_join(case, countrylist, by="iso2")

tabled <- as.data.table(table(joined$region))

pdf('regionalseizurecount')
ggplot(tabled, aes(x = V1, y = N, fill = V1)) + geom_bar(stat = 'identity') + 
      labs(x = "Region", y = "Seizure Count") +
      theme(legend.title = element_blank())
dev.off()


####Cannabis --------------------------------------------------------------------------

cannabis <- drugs["Cannabis"]

ggplot(cannabis, aes(x = direction,y = kilo)) + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim = c(0,225))
cannabis2 <- filter(cannabis, kilo < 250)
ggplot(cannabis2, aes(kilo)) + geom_histogram(stat = 'bin', binwidth = 10) + facet_grid(.~direction)
