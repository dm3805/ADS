library(GISTools); library(maptools); library(ggplot2); library(scales)
library(data.table); library(plyr); library(dplyr)

# load complete 311 data set previously saved as RData file
load("C:/Users/johndoe/Desktop/ADS_FP/311_orig.RData")

# get rid of the unnecessary stuff
names(data)
names(data)[c(1:3,5,6,9,11,12)] <- c("ID","CreatedDate","ClosedDate","AgencyName",
                                     "ComplaintType","ZIP","X","Y")

data$ComplaintType <- as.factor(data$ComplaintType)
data$Descriptor <- as.factor(data$Descriptor)

# object "data" contains our raw data 
###
###
# move on with object "plot_data"
plot_data <- dplyr::select(data, ID, CreatedDate, ComplaintType, Descriptor, X, Y)
plot_data <- dplyr::filter(plot_data, !is.na(X), !is.na(Y), !is.na(CreatedDate),
                           !is.na(ComplaintType), !is.na(Descriptor))
# all the noise-related complaints contain "Noise" in the ComplaintType, so...
plot_data <- dplyr::filter(plot_data, grepl("Noise", plot_data$ComplaintType))
# only 2010
plot_data <- dplyr::filter(plot_data, grepl("2010", plot_data$CreatedDate))
nrow(plot_data)

# check for missings
plot_data$ComplaintType <- droplevels(plot_data$ComplaintType)
plot_data$Descriptor <- droplevels(plot_data$Descriptor)
summary(plot_data$ComplaintType)
# this is the categorization of interest:
summary(plot_data$Descriptor)
# collect some levels
plot_data$Descriptor <- revalue(plot_data$Descriptor, 
                                c("Noise: Loud Music/Daytime (Mark Date And Time) (NN1)"="Loud Music/Party", 
                                  "Noise: Loud Music/Nighttime(Mark Date And Time) (NP1)"="Loud Music/Party",
                                  "Noise: Air Condition/Ventilation Equip, Residential (NJ1)"="Noise: air condition/ventilation equipment (NV1)",
                                  "Noise: Jack Hammering (NC2)"="Noise: Construction Equipment (NC1)"))

# some of the levels have small n(obs) -> collect all <1000 in 'Other'
tbl <- table(plot_data$Descriptor)
i <- names(tbl)[tbl < 5000]
levels(plot_data$Descriptor)[which(levels(plot_data$Descriptor)%in%i)] <- "Other"
summary(plot_data$Descriptor)

# rename some factor levels using plyr function
levels(plot_data$Descriptor)
plot_data$Descriptor <- revalue(plot_data$Descriptor, 
                                c("Noise, Barking Dog (NR5)"="Barking Dog", 
                                  "Noise, Ice Cream Truck (NR4)"="Ice Cream Truck",
                                  "Noise: air condition/ventilation equipment (NV1)"="Air Condition/Ventilation Equipment",
                                  "Noise: Alarms (NR3)"="Alarms",
                                  "Noise: Construction Before/After Hours (NM1)"="Construction Before/After Hours",
                                  "Noise: Construction Equipment (NC1)"="Construction Equipment",
                                  "Noise: Jack Hammering (NC2)"="Jack Hammering"))
levels(plot_data$Descriptor)

plot_data$Descriptor <- factor(plot_data$Descriptor, levels = c("Loud Music/Party", 
                                                                "Construction Before/After Hours",
                                                                "Loud Talking",
                                                                "Car/Truck Music",
                                                                "Construction Equipment",
                                                                "Barking Dog",
                                                                "Other"))

# generate time var
plot_data$dt_start <- strptime(plot_data$CreatedDate, "%m/%d/%Y %I:%M:%S %p")
sum(is.na(plot_data$dt_start)) # 0 missing values
head(plot_data$dt_start)
plot_data$day_start <- weekdays(plot_data$dt_start)
plot_data$dtt_cut <- cut(plot_data$dt_start, breaks = "hour")
plot_data$dt_cut <- as.ordered(format(as.POSIXct(plot_data$dtt_cut), "%m/%d/%Y"))
plot_data$hr_cut <- as.ordered(format(as.POSIXct(plot_data$dtt_cut), "%H:%M:%S"))
plot_data$hrmin <- substr(plot_data$hr_cut, 1, nchar(as.character(plot_data$hr_cut))-3)
plot_data$hr <- as.numeric(substring(as.character(plot_data$hr_cut),1,2))
plot_data$a10b6 <- as.logical(plot_data$hr < 6 | plot_data$hr > 21)

#####
# SAVE plot_data FOR LATER; now work on area plot
#####
# group by dates
plot_red <- dplyr::select(plot_data, -dt_start)
grouped_ddt <- dplyr::group_by(plot_red, dtt_cut) 
grouped_ddt_desc <- dplyr::group_by(plot_red, dtt_cut, Descriptor)

# 8601 groups (365*24=8760 -> 169 hrs w/o complaint in '10!)
total_ddt <- dplyr::summarise(grouped_ddt, n())
total_ddt_desc <- dplyr::summarise(grouped_ddt_desc, n())
total_ddt
total_ddt_desc
# merge by dtt_cut
joined_ddt_desc <- dplyr::left_join(total_ddt_desc, total_ddt, by = "dtt_cut")
# calculate proportion
prop <- joined_ddt_desc[,3]/joined_ddt_desc[,4]
joined_ddt_desc <- dplyr::tbl_df(cbind(joined_ddt_desc, prop))
# aggregate (mean) over days
dt_cut <- as.ordered(format(as.POSIXct(joined_ddt_desc$dtt_cut), "%m-%d-%Y"))
hr_cut <- as.ordered(format(as.POSIXct(joined_ddt_desc$dtt_cut), "%H:%M:%S"))

joined_ddt_desc <- dplyr::tbl_df(cbind(joined_ddt_desc, dt_cut, hr_cut))
joined_ddt_desc_grp <- group_by(joined_ddt_desc, hr_cut, Descriptor) 
names(joined_ddt_desc_grp)[5] <- "prop"
final <- as.data.frame(dplyr::summarise(joined_ddt_desc_grp, avg = mean(prop)))
final$perc <- paste(round(final$avg*100,0), "%", sep = "")
final$hrmin <- as.numeric(substring(as.character(final$hr_cut),1,2))

# make the plot!
head(plot_data)
head(final)

setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="AreaPlot.png",width=800,height=600, res=90)
plot <- ggplot(final, aes(x=hrmin,y=avg,group=Descriptor,fill=Descriptor)) + geom_area(position="fill")
plot <- plot + theme(axis.text.x = element_text(angle = 90))  
plot <- plot + scale_y_continuous(labels = percent) + labs(x="", y="", fill="Complaint Type") 
plot <- plot + ggtitle(expression(atop("Distribution of Complaint Types", 
                                       atop("by Hour of Day in 2010", "")))) 
plot <- plot + theme(plot.background = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA, size=1))
plot + scale_fill_brewer(palette = "Reds") +  guides(fill = guide_legend(reverse = TRUE))
dev.off()

# start from plot_red again!
plot_red$day_start <- factor(plot_red$day_start, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
plot_red <- dplyr::arrange(plot_red, day_start)
grouped_day <- dplyr::summarise(group_by(plot_red, day_start, hrmin),n()) 
names(grouped_day)[3] <- "count"

setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="LinePlot.png",width=800,height=600, res=90)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data=grouped_day,aes(x=hrmin,y=count, group=day_start)) + 
       geom_line(aes(color=day_start),size=1) +
       ggtitle("Noise Complaint Times by Day of Week") +
       labs(x="Time of Day",y="Number of Complaints") +
       theme(axis.text.x = element_text(angle = 90),
             legend.title=element_blank(),
             panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
         scale_colour_manual(values = cbbPalette)
dev.off()

############# other graphs ###############
# create a borough variable
# read in shape files as SpatialPolygonsDataFrame
setwd("C:/Users/johndoe/Documents/Uni/mappingDataUS")
shp3 <- readShapePoly("nybb_15c/nybb.shp")
shp4 <- readShapePoly("nynta_15c/nynta.shp")
# read in Population Data for Neighborhoods in 2010
NTAPop <- fread(input = "C:/Users/johndoe/Documents/Uni/mappingDataUS/NTA_Pop.csv",
                data.table = FALSE)
NTAPop$TotalPop2010 <- as.numeric(gsub(",([0-9])", "\\1", NTAPop$TotalPop2010))
NTAPop$NTACode <- as.factor(NTAPop$NTACode)
NTAPop <- dplyr::select(NTAPop, - NTAName)

# start again (some discarded observations can be included again)
plot_data <- dplyr::select(data, ID, CreatedDate, ComplaintType, Descriptor, X, Y)
plot_data <- dplyr::filter(plot_data, !is.na(X), !is.na(Y), !is.na(ComplaintType))
# all the noise-related complaints contain "Noise" in the ComplaintType, so...
plot_data <- dplyr::filter(plot_data, grepl("Noise", plot_data$ComplaintType))
# only 2010
plot_data <- dplyr::filter(plot_data, grepl("2010", plot_data$CreatedDate))
nrow(plot_data) # 84770

# drop old levels
plot_data$ComplaintType <- droplevels(plot_data$ComplaintType)
plot_data$Descriptor <- droplevels(plot_data$Descriptor)

data.spdf <- as.data.frame(plot_data)
coordinates(data.spdf) <- c("X", "Y")

# match spatial point data to neighborhoods
data.spdf$NTACode <- over(data.spdf, shp4)$NTACode
nrow(data.spdf[is.na(data.spdf$NTACode),]) # 6
data.spdf <- data.spdf[!is.na(data.spdf$NTACode),]
head(data.spdf)
nrow(data.spdf)

# create an object that contains: complp1K for each NTA
red <- data.spdf@data[,which(names(data.spdf@data) == "ID" | names(data.spdf@data) == "NTACode"|
                                names(data.spdf@data) == "Descriptor")]
grouped_NTA <- dplyr::group_by(red, NTACode)
total_NTA <- dplyr::summarise(grouped_NTA, n())
names(total_NTA)[2] <- "nCompl"
nrow(total_NTA) # 193 <- 2 less than Pop!
names(total_NTA)[2] <- "nCompl"
nrow(total_NTA) # 193 <- 2 less than Pop!

total_NTAPop <- dplyr::inner_join(total_NTA, NTAPop, by = "NTACode")
nrow(total_NTAPop) # 193, okay!
total_NTAPop$nComplp1K <- total_NTAPop$nCompl*1000/total_NTAPop$TotalPop2010
names(total_NTAPop)

# combine with shp4 data
shp4@data <- dplyr::left_join(shp4@data, total_NTAPop, by = "NTACode")
nrow(shp4@data)
names(shp4@data)

# Exclude parks, Rikers Island and the airport!
id <- which(shp4@data$NTACode == "MN99"|shp4@data$NTACode == "QN99"|
            shp4@data$NTACode == "SI99"|shp4@data$NTACode == "QN98"|
            shp4@data$NTACode == "BK99"|shp4@data$NTACode == "BX99"|
            shp4@data$NTACode == "BX98")
shp_new <- shp4[-id,]
nrow(shp_new) # 188

### PLOTTING ###
setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="ChorTotal.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
vacant.shades = auto.shading(shp_new$nComplp1K, n=7)
choropleth (shp_new, shp_new$nComplp1K, shading=vacant.shades, 
            main="Noise Complaints in NYC by Neighborhood (2010)", cex.main = 1)
choro.legend(title = c("Complaints/1000"), 902600 , 256068 ,vacant.shades, cex = 1)
dev.off()
# zoom in 
png(file="ChorZoom.png",width=800,height=600, res=90)
par(mfrow=c(1,2), mar=c(1,0,3,0))
# BK
shp_BK <- shp_new[shp_new@data$BoroName=="Brooklyn",]
par(mar=c(1,0,1.5,0))
vacant.shades = auto.shading(shp_BK$nComplp1K, n=4)
choropleth (shp_BK, shp_BK$nComplp1K, shading=vacant.shades, main = "Brooklyn", cex.main = 1)
choro.legend(title = c("Complaints/1000"), 1007415 , 216196 ,vacant.shades, cex = 1)
for(i in 1:nrow(shp_BK)) {
  if (shp_BK$nComplp1K[i] > 12) {
    text(shp_BK@polygons[[i]]@labpt[1],shp_BK@polygons[[i]]@labpt[2], round(shp_BK$nComplp1K[i],1), cex = .6)
  }
}
# MN
shp_MN <- shp_new[shp_new@data$BoroName=="Manhattan",]
par(mar=c(1,0,1.5,0))
vacant.shades = auto.shading(shp_MN$nComplp1K, n=4)
choropleth (shp_MN, shp_MN$nComplp1K, shading=vacant.shades,  main = "Manhattan", cex.main = 1)
choro.legend(title = c("Complaints/1000"), 969938.7 , 262022.9 ,vacant.shades, cex = 1)
#This makes the park areas white (already dealt with!)
#plot(shp_MN[shp_MN$NTACode=="MN99",], add = TRUE , col = "white")
# this would plot the labels (not run)
for(i in 1:nrow(shp_MN)) {
  if (shp_MN$nComplp1K[i] > 28) {
    text(shp_MN@polygons[[i]]@labpt[1],shp_MN@polygons[[i]]@labpt[2], round(shp_MN$nComplp1K[i],1), cex = .6)
  }
}  
dev.off()

# ranking
data_rank <- shp_new@data[,names(shp_new) == "NTAName" | names(shp_new) == "nComplp1K"]
data_rank <- dplyr::arrange(data_rank, desc(nComplp1K))
write.csv(data_rank,file = "rankTot.csv")

# create an object that contains: ice cream truck complaints for each NTA
#                                 party complaints for each NTA
parties_NTA <- dplyr::summarise(group_by(filter(red, Descriptor=="Loud Music/Party"), NTACode), n())
names(parties_NTA)[2] <- "parties_NTA"
iceCream_NTA <- dplyr::summarise(group_by(filter(red, Descriptor=="Noise, Ice Cream Truck (NR4)"), NTACode), n())
names(iceCream_NTA)[2] <- "iceCream_NTA"
nrow(parties_NTA)  # 192
nrow(iceCream_NTA) # 142

shp_other <- shp4
shp_other@data <- dplyr::left_join(shp_other@data, parties_NTA, by = "NTACode")
shp_other@data <- dplyr::left_join(shp_other@data, iceCream_NTA, by = "NTACode")
head(shp_other@data)
shp_other@data$parties_NTA[is.na(shp_other@data$parties_NTA)] <- 0
shp_other@data$iceCream_NTA[is.na(shp_other@data$iceCream_NTA)] <- 0

setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="PartiesTotal.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
vacant.shades = auto.shading(shp_other$parties_NTA, n=9)
choropleth (shp_other, shp_other$parties_NTA, shading=vacant.shades, 
            main="Party Complaints in NYC by Neighborhood (2010)", cex.main = 1)
choro.legend(title = c("No. of Complaints"), 902600 , 256068 ,vacant.shades, cex = 1)
dev.off()

# I really want to find out where the party happens!
shp_bigParty <- shp_other[shp_other$parties_NTA > 300,]
winner <- which(shp_bigParty$parties_NTA == max(shp_bigParty$parties_NTA))
legendStr <- paste("The winner is ", shp_bigParty$NTAName[winner], " with ", shp_bigParty$parties_NTA[winner], " parties!", sep="")
setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="BigPartiesTotalStar.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
vacant.shades = auto.shading(shp_bigParty$parties_NTA, n=5)
choropleth(shp_bigParty, shp_bigParty$parties_NTA, shading=vacant.shades, 
            main="Party Complaints in NYC by Neighborhood (2010)", cex.main = 1)
plot(shp4, add=TRUE)
choro.legend(title = c("No. of Complaints"), 942646.1, 260599.1,
             under = "300 to", vacant.shades, cex = 1)
for(i in 1:nrow(shp_bigParty)){
  text(shp_bigParty@polygons[[i]]@labpt[1],shp_bigParty@polygons[[i]]@labpt[2], 
       shp_bigParty$parties_NTA[i], cex = .6)
}
legend(x=942646.1, y= 235115.1, legendStr, bg = "gold", cex=.8)
dev.off()

# without extra box
png(file="BigPartiesTotal.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
vacant.shades = auto.shading(shp_bigParty$parties_NTA, n=5)
choropleth(shp_bigParty, shp_bigParty$parties_NTA, shading=vacant.shades, 
           main="Party Complaints in NYC by Neighborhood (2010)", cex.main = 1)
plot(shp4, add=TRUE)
choro.legend(title = c("No. of Complaints"), 942646.1, 260599.1,
             under = "300 to", vacant.shades, cex = 1)
for(i in 1:nrow(shp_bigParty)){
  text(shp_bigParty@polygons[[i]]@labpt[1],shp_bigParty@polygons[[i]]@labpt[2], 
       shp_bigParty$parties_NTA[i], cex = .6)
}
dev.off()


# ice cream truck
shp_other <- shp_other[shp_other@data$iceCream_NTA > 10,]
setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="IceCreamTotal.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
plot(shp4[shp4$BoroName != "Staten Island",], 
     main="Ice Cream Truck Complaints in NYC by Neighborhood (2010)")
vacant.shades = auto.shading(shp_other$iceCream_NTA, n=5)
choropleth(shp_other, shp_other$iceCream_NTA, shading=vacant.shades, add=TRUE, cex.main = 1)
choro.legend(title = c("No. of Complaints"), 932600 , 256068 , under = "10 to", vacant.shades, cex = 1)
dev.off()

# day & night (continue with data.spdf)
names(data.spdf)
plot_data2 <- as.data.frame(data.spdf)
plot_data2$dt_start <- strptime(plot_data2$CreatedDate, "%m/%d/%Y %I:%M:%S %p")
sum(is.na(plot_data2$dt_start)) # 0 missing values
head(plot_data2$dt_start)
plot_data2$day_start <- weekdays(plot_data2$dt_start)
plot_data2$dtt_cut <- cut(plot_data2$dt_start, breaks = "hour")
plot_data2$dt_cut <- as.ordered(format(as.POSIXct(plot_data2$dtt_cut), "%m/%d/%Y"))
plot_data2$hr_cut <- as.ordered(format(as.POSIXct(plot_data2$dtt_cut), "%H:%M:%S"))
plot_data2$test <- as.numeric(substring(as.character(plot_data2$hr_cut),1,2))
plot_data2$a10b6 <- as.logical(plot_data2$test < 6 | plot_data2$test > 21)

names(plot_data2)
night <- dplyr::filter(select(plot_data2, -dt_start), a10b6==TRUE)
total_night <- dplyr::summarise(group_by(night, NTACode), n())         
names(total_night)[2] <- "totalNight"
nrow(total_night) # 193

shp_night <- shp4
shp_night@data <- dplyr::left_join(shp_night@data, total_night, by = "NTACode")
shp_night <- shp_night[!is.na(shp_night$totalNight),]

setwd("C:/Users/johndoe/Desktop/Dropbox/Sozialwissenschaften/2_Master/3. Semester/Applied Data Science/Foundations Project/graphics")
png(file="NightTotal.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
vacant.shades = auto.shading(shp_night$totalNight, n=9)
choropleth(shp_night, shp_night$totalNight, shading=vacant.shades, 
           main="Complaints (22pm-6am) in NYC by Neighborhood (2010)", cex.main = 1)
choro.legend(title = c("No. of Complaints"), 932600 , 256068 ,vacant.shades, cex = 1)
dev.off()

# only over 365
shp_bigNight <- shp_night[shp_night$totalNight >= 365,]
png(file="BigNightTotal.png",width=800,height=600, res=90)
par(mfrow=c(1,1), mar=c(0,0,1,0))
plot(shp4[shp4$BoroName == "Manhattan"|shp4$BoroName == "Brooklyn",],
     main="Complaints (22pm-6am) in NYC by Neighborhood (2010)", cex.main = 1)
vacant.shades = auto.shading(shp_bigNight$totalNight, n=5)
choropleth(shp_bigNight, shp_bigNight$totalNight, shading=vacant.shades, add=TRUE)
choro.legend(title = c("No. of Complaints"), 932600, 256068, under = "365 to", vacant.shades, cex = 1)
dev.off()

#################
# end of R file #
#################