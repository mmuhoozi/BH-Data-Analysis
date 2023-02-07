
### eIDSR Vs Immigration data 2021
### generate first plot with bars for negative and postive values 
library(ggplot2)
BHdata <- data.frame(POE = c("Bunagana", "Busia", "Cyanika", "Entebbe", "Katuna", "Malaba", "Mirama Hills", "Mpondwe", "Mutukula"),
                     Immigration = c(2199, 107987, 2258, 47636, 914, 14567, 22823, 7791, 21887),
                     eIDSR = c(19835, 25011, 2026, 427174, 23628, 150674, 13899, 11448, 25474))
BHdata$difference <- BHdata$Immigration - BHdata$eIDSR
BHdata$pct_difference <- (BHdata$difference / pmax(BHdata$Immigration, BHdata$eIDSR)) * 100

ggplot(BHdata, aes(x=POE, y=pct_difference, fill=pct_difference > 0)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = POE, y = ifelse(pct_difference > 0, pct_difference + 5, pct_difference - 5)), 
            vjust = ifelse(BHdata$pct_difference > 0, 0, 1.5), 
            size = 3) +
  scale_fill_manual(values=c("red", "blue"),
                    labels = c("Negative", "Positive")) +
  labs(x="POE", y="Percentage Difference", fill="Difference") +
  ggtitle("Percentage Difference between Immigration and eIDSR by POE") +
  coord_flip() +
  scale_y_continuous(limits = c(min(BHdata$pct_difference) - 20, max(BHdata$pct_difference) + 20), expand = c(0,0), position = "right") +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

##### help generate relationships and some wrangling.
library(ggplot2)
BHdata <- data.frame(POE = c("Bunagana", "Busia", "Cyanika", "Entebbe", "Katuna", "Malaba", "Mirama Hills", "Mpondwe", "Mutukula"),
                     Immigration = c(2199, 107987, 2258, 47636, 914, 14567, 22823, 7791, 21887),
                     eIDSR = c(19835, 25011, 2026, 427174, 23628, 150674, 13899, 11448, 25474),
                     months_not_reported = c(0, 4, 2, 0, 6, 3, 4, 5, 4))
BHdata$difference <- BHdata$Immigration - BHdata$eIDSR
BHdata$pct_difference <- (BHdata$difference / pmax(BHdata$Immigration, BHdata$eIDSR)) * 100

ggplot(BHdata, aes(x=months_not_reported, y=pct_difference, color=months_not_reported)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_gradient(low = "red", high = "blue") +
  labs(x="Months not reported", y="Percentage Difference", color="Months not reported") +
  ggtitle("Percentage Difference between Immigration and eIDSR values by Months not reported") +
  theme(legend.position="bottom")
correlation <- cor(BHdata$months_not_reported, BHdata$pct_difference)
cor.test <- cor.test(BHdata$months_not_reported, BHdata$pct_difference)
p.value <- cor.test$p.value

##This will help create trendline for departures and arrivals over year 2021
# Load ggplot2 library
library(ggplot2)

# Define data
data <- data.frame(Month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                   Male = c(42412, 43673, 51001, 43858, 46883, 42943, 40639, 46530, 44613, 50210, 53140, 57184),
                   Female = c(10206, 9713, 11706, 9425, 12640, 10465, 7263, 11845, 12492, 16932, 18946, 27468))

# Convert Male and Female columns to numeric
data$Male <- as.numeric(gsub(",", "", data$Male))
data$Female <- as.numeric(gsub(",", "", data$Female))

# Convert the Month variable to a factor
data$Month <- factor(data$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Plot the data
ggplot(data, aes(x = Month)) +
  geom_line(aes(y = Male, color = "Male")) +
  geom_line(aes(y = Female, color = "Female")) +
  scale_x_discrete(limits = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) +
  labs(x = "Month", y = "Count", color = "Gender")

library(dplyr)

data1 <- data.frame(Months= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                    Travellers=c(24612, 25697, 30328, 30042, 34665, 33604, 56574, 74532, 74072, 94182, 108451, 119332))
data1 %>% 
  ggplot(aes(x=Months, y=Travellers))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  labs(title = "Travellers by months")


































install.packages("reshape2")
library(reshape2)
library(ggplot2)

# Create data frame from provided data
data <- data.frame(Purpose = c("Driver", "Business", "Leisure/Holiday", "Conference/Meeting", "Spiritual (Religious)", "Education", "Visiting friends", "Short contracts", "Other/ Missing", "Shopping", "Transit"),
                   Male = c(57876, 46231, 1670, 1261, 4517, 1415, 26797, 24807, 295, 994, 33699),
                   Female = c(1116, 4876, 1118, 544, 1978, 497, 10136, 537, 197, 509, 1335))

# Melt the data frame to long format
data_melted <- melt(data, id.vars = "Purpose")

# Generate bar plot
ggplot(data_melted, aes(x = Purpose, y = value, fill = variable)) +
  geom_col() +
  scale_fill_manual(values = c("#6D9EC1", "#A6D8A6")) +
  xlab("Purpose of Travel") +
  ylab("Count") +
  ggtitle("Distribution of Purpose of Travel by Gender") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm")) +
  scale_y_continuous(limits = c(0, max(data_melted$value)), expand = c(0, 0)) +
  scale_x_discrete(limits = data_melted$Purpose) +
  coord_flip() +
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(angle = 0, hjust = 1, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
  guides(fill = guide_legend(title = "Gender"))








































