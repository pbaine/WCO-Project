library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
drugs15 <- fread('Drugs/2015drugs.csv')
drugs16 <- fread('Drugs/2016drugs.csv')
names(drugs15) <- tolower(names(drugs15))
names(drugs16) <- tolower(names(drugs16))

####Seizure Count -------------------------------------------------------------------------

cats15 <- as.data.table(table(drugs15$category))
names(cats15) <- c('Name', '2015 Seizures')
cats16 <- as.data.table(table(drugs16$category))
names(cats16) <- c('Name', '2016 Seizures')

mrgd <- merge(cats15, cats16, by = 'Name')
mrgd <- filter(mrgd, Name != "Other-Specify")
mrgd$Name[mrgd$Name == "New Psychoactive Substances"] <- "New Psychoactive\nSubstances"
mltd <- melt(mrgd, id = 'Name')

gg <- ggplot(data = mltd, aes(x = Name, y = value, fill = Name)) + 
      geom_bar(stat = "identity") + facet_grid(variable~.) +
      scale_y_continuous(breaks = c(0,5000,10000,150000), labels = c('0','5,000','10,000','15,000')) +
      ylab("Seizure Count") +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(size = .5), 
            axis.title.x = element_blank(), 
            axis.title.y = element_text(family = "Gill Sans MT", size = 12, color = "black"),
            axis.text = element_text(family = "Gill Sans MT", size = 12, color = "black"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Gill Sans MT", size = 12, color = "black"))
ggsave("Seizurecountbar_facetyear.pdf", gg, device = cairo_pdf, width = 10, height = 8, units = "in")


gg <- ggplot(data = mltd, aes(x = variable, y = value, fill = Name)) + 
      geom_bar(stat = "identity", position = 'fill') + 
      scale_y_continuous(breaks = c(0,.5,1), labels = c('0%','50%','100%')) +
      ylab("Seizure Count") +
      theme_bw() +
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(size = .5), 
            axis.title.x = element_blank(), 
            axis.title.y = element_text(family = "Gill Sans MT", size = 12, color = "black"),
            axis.text = element_text(family = "Gill Sans MT", size = 12, color = "black"),
            legend.title = element_blank(),
            legend.text = element_text(family = "Gill Sans MT", size = 12, color = "black"))

ggsave("categoryshareoftotalbyyear.pdf", gg, device = cairo_pdf, width = 5, height = 4, units = "in")


####Case Analysis ---------------------------------------------------------------------------

case15 <- read.csv('2015_other/2015case.csv')
case16 <- read.csv('2016_other/2016case.csv')
drugcase15 <- filter(case15, case15$T_CASE_ID %in% drugs15$t_case_fk)
drugcase16 <- filter(case16, case16$T_CASE_ID %in% drugs16$t_case_fk)
month15 <- as.data.table(table(drugcase15$OFFENCE_MONTH))
month16 <- as.data.table(table(drugcase16$OFFENCE_MONTH))

mrgd <- merge(month15,month16, by = 'V1')
names(mrgd) <- c('Month','2015 Seizures', '2016 Seizures')
mrgd$Month <- factor(mrgd$Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))
mltd <- melt(mrgd, id = 'Month')

ggplot(mltd, aes(x = Month, y = value)) + geom_line(aes(group = variable, color = variable)) +
      theme_bw() + 
      scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c('January', 'February','March','April','May','June','July','August','September','October','November','December')) +
      
