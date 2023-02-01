
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






