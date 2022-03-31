# Q1
# Read the dataset into a data frame called london_crime
# show the structure of data frame
london_crime <- read.csv("london-crime-data.csv", na = "")
str(london_crime)

# Using paste() function
# amalgamate month and year into Date
# sep = "/0/" to put the 1 for day and add the "/" in the date
london_crime$Date <- paste(london_crime$month, london_crime$year, sep = "/1/")

# drop the year and month columns
year_month_col <- names(london_crime) %in% c("year", "month")
london_crime <- london_crime[!year_month_col]


# Q2
# only the variables shown in this table
names(london_crime)

# Make the relevant changes
# convert the variable names Convert to in the table
names(london_crime)[names(london_crime) == "borough"] <- "Borough"
names(london_crime)[names(london_crime) == "major_category"] <- "MajorCategory"
names(london_crime)[names(london_crime) == "minor_category"] <- "SubCategory"
names(london_crime)[names(london_crime) == "value"] <- "Value"
names(london_crime)[names(london_crime) == "Date"] <- "CrimeDate"

# Q3
# Convert the CrimeDate as type Date
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, "%m/%d/%Y")

# show the structure to confirm changed
str(london_crime)

# Q4 
# Plot a chart to show the summary of the borough information
graph_range <- range(0, london_crime$Value)

plot(summary(as.factor(london_crime$Borough)),
     type = "o", 
     main = "Summary of Borough", 
     xlab = "Index", 
     ylab = "Borough")
summary(london_crime$Borough)

# Which borough has the highest level of crime.
# Croydon - 5226

# Which area has the lowest level of crime.
# City of London - 86

# Q5
# Display the MajorCategory variable data in a pie chart.
str(london_crime)
highest_MajorCategory <- summary(as.factor(max(london_crime$MajorCategory)))
lowest_MajorCategory <- min(summary(as.factor(london_crime$MajorCategory)))
pie(summary(as.factor(london_crime$MajorCategory)))
pie(summary(as.factor(max(london_crime$MajorCategory))))
pie(summary(as.factor(min(london_crime$MajorCategory))))

summary(as.factor(london_crime$MajorCategory))
as.factor(london_crime$MajorCategory)
min(london_crime$MajorCategory)
max(london_crime$MajorCategory)
min(london_crime$MajorCategory)

# Q6
# Categorise each borough in the London_crime datase
# create a new variable called Region
# store within it the correct region for each borough

# East Region
london_crime$Region[london_crime$Borough == "Barking and Dagenham" |
                    london_crime$Borough == "Bexley" |
                    london_crime$Borough == "Greenwich" |
                    london_crime$Borough == "Havering" |
                    london_crime$Borough == "Kingston upon Thames" |
                    london_crime$Borough == "Newham" | 
                    london_crime$Borough == "Redbridge" |
                    london_crime$Borough == "Wandsworth"] <- "East"

# North Region
london_crime$Region[london_crime$Borough == "Barnet" | 
                    london_crime$Borough == "Camden" | 
                    london_crime$Borough == "Enfield" | 
                    london_crime$Borough == "Hackney" | 
                    london_crime$Borough == "Haringey"] <- "North"

# West Region
london_crime$Region[london_crime$Borough == "Brent" | 
                      london_crime$Borough == "Ealing" | 
                      london_crime$Borough == "Hammersmith and Fulham" | 
                      london_crime$Borough == "Harrow" | 
                      london_crime$Borough == "Hillingdon" | 
                      london_crime$Borough == "Hounslow" | 
                      london_crime$Borough == "Richmond upon Thames"] <- "West"

# South Region
london_crime$Region[london_crime$Borough == "Bromley" | 
                      london_crime$Borough == "Croydon" | 
                      london_crime$Borough == "Merton" | 
                      london_crime$Borough == "Sutton"] <- "South"

# Central Region
london_crime$Region[london_crime$Borough == "Islington" | 
                      london_crime$Borough == "Kensington and Chelsea" | 
                      london_crime$Borough == "Lambeth" | 
                      london_crime$Borough == "Lewisham" | 
                      london_crime$Borough == "Southwark" | 
                      london_crime$Borough == "Tower Hamlets" | 
                      london_crime$Borough == "Waltham Forest" | 
                      london_crime$Borough == "Westminster"] <- "Central"

# check any missing value in Borough and Region
sum(is.na(london_crime$Borough))
sum(is.na(london_crime$Region))

# sort by Borough first for easy looking
london_crime <- london_crime[order(london_crime$Borough),]
# get the missing region by using !complete.cases
missing_region <- london_crime[!complete.cases(london_crime),]
missing_region

# City of London is the missing region
# In this case, I assign it as the Central
# Due to I really not sure what City of London is
london_crime$Region[london_crime$Borough == "City of London"] <- "Central"

# Check agian for the missing values of Region
sum(is.na(london_crime$Region))

# Q7
# get the range of summary Region
graph_range <- range(0, summary(as.factor(london_crime$Region)))
plot(summary(as.factor(london_crime$Region)), 
     main = "Reported Crimes by Region", 
     xlab = "Summary", 
     ylab = "Region", 
     labels = FALSE)
axis(1, at = 1:5, lab = c("Central", "East", "North", "South", "West"))
axis(2, at = 5 * 0:graph_range[2])

summary(as.factor(london_crime$Region))
# which region had the highest number of crimes. 
# How many crimes were committed?
# Central has the highest number of crimes.
# 28591

# which region had the lowest number of crimes. 
# How many crimes were committed?
# South
# 15487

# Q8
# subset of data that had the highest number of crimes
highest_Crimes <- subset(london_crime, london_crime$Region == "Central")

summary(as.factor(highest_Crimes$MajorCategory))
# The highest major category is Theft and Handling - 8297
# and the lowest major category is Sexual Offences - 293

# extract out a subset of data that had the lowest level of crimes.
lowest_Crimes <- subset(london_crime, london_crime$Region == "South")

summary(as.factor(lowest_Crimes$MajorCategory))
# The highest major category is Theft and Handling - 4609
# and the lowest major category is Sexual Offences - 110

# both get the same highest majot category and lowest major category

# Q9
# save the plot default setting
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(summary(as.factor(london_crime$Region)), 
     main = "Region", 
     xlab = "Region", 
     ylab = "Index")
plot(summary(as.factor(london_crime$Borough)), 
     main = "Borough", 
     xlab = "Index", 
     ylab = "Borough")
# resert to default setting
par(opar)

# Q10
write.csv(london_crime, file = "London-crime-modifies.csv")

