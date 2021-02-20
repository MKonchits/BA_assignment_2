setwd("C:/Users/Мухаил/Documents/Trainings/MIM/Business_analytics/Demo/Data")

dir <- "C:/Users/Мухаил/Documents/Trainings/MIM/Business_analytics/Demo/"

dirData  <- paste0(dir,"Data/")
dirRslt  <- paste0(dir,"Results/")

#loading the file
dsFlights   <- read.csv2(file=paste0(dirData, "FlightTaxation.csv"),
                        stringsAsFactors=FALSE)
#checking the table
View(dsFlights)

#total count
length(dsFlights$cLiving)

#PART 1: Likert-scale variables
#calculation of Cronbach's alpha for the Guilt value
library(psych)

alpha_Guilt <- alpha(dsFlights[c("Guilt01","Guilt02","Guilt03","Guilt04","Guilt05")],
                     keys=c("Guilt03","Guilt04","Guilt05"), cumulative = FALSE)
print(alpha_Guilt)
summary(alpha_Guilt)

stargazer(alpha_Guilt$total, summary = FALSE, 
          align = TRUE, no.space = TRUE, type = "html", title="Cronbach alpha for Guilt", digits = 2,
          out=paste0(dirRslt,"Cronbach_guilt.doc"))

stargazer(alpha_Guilt$alpha.drop, summary = FALSE, 
          align = TRUE, no.space = TRUE, type = "html", title="Cronbach alpha for Guilt", digits = 2,
          out=paste0(dirRslt,"Cronbach_drop_guilt.doc"))

dsFlights$Guilt <- alpha_Guilt$scores

#calculation of Cronbach's alpha for the Environmental value
alpha_Env <- alpha(dsFlights[c("Nep01","Nep02","Nep03","Nep04","Nep05")],
                     keys=c("Nep02","Nep03","Nep04"), cumulative = FALSE)
print(alpha_Env)
summary(alpha_Env)

dsFlights$Env <- alpha_Env$scores
View(dsFlights)

View(alpha_Guilt$response.freq)

stargazer(alpha_Env$total, summary = FALSE, 
          align = TRUE, no.space = TRUE, type = "html", title="Cronbach alpha for Environmental beliefs", digits = 2,
          out=paste0(dirRslt,"Cronbach_Env.doc"))

stargazer(alpha_Env$alpha.drop, summary = FALSE, 
          align = TRUE, no.space = TRUE, type = "html", title="Scale analysis for Environmental beliefs", digits = 2,
          out=paste0(dirRslt,"Cronbach_drop_Env.doc"))

#PART2: descriptive analysis
library(ggplot2)
setwd(dirRslt)

#first analyzing Likert-scale quanitities
#Environmental behavior
ggplot(dsFlights, aes(x = Env)) +
  geom_histogram(bins = 7, fill = "cadetblue3", col = "black") +
  xlab("Avg environmental score") + ggtitle("Environmental consciousness") + theme(
    plot.title = element_text(hjust = 0.5),
  )
ggsave("Env_histogram.png")

#Feeling guilty
ggplot(dsFlights, aes(x = Guilt)) +
  geom_histogram(bins = 7, fill = "darkolivegreen1", col = "black") +
  xlab("Avg guilty score") + ggtitle("Guilty feelings") + theme(
    plot.title = element_text(hjust = 0.5),
  )
ggsave("Guilty_histogram.png")

#likelihood of travelling by plane
ggplot(dsFlights, aes(x = rateAirplane)) +
  geom_histogram(bins = 10, fill = "darkolivegreen1", col = "black") +
  xlab("rate Airplane") + ggtitle("Likelihood of travelling by plane") + theme(
    plot.title = element_text(hjust = 0.5),
  )
ggsave("plane_histogram.png")

#ManipTax
ggplot(dsFlights, aes(x = ManipTax)) +
  geom_histogram(bins = 3, fill = "darkolivegreen1", col = "black") +
  xlab("ManipTax") + ggtitle("Distribution of the ManipTax values") + theme(
    plot.title = element_text(hjust = 0.5),
  )
ggsave("maniptax_histogram.png")

#ManipDest
ggplot(dsFlights, aes(x = ManipDest)) +
  geom_histogram(bins = 3, fill = "darkkhaki", col = "black") +
  xlab("ManipDest") + ggtitle("Distribution of the ManipDest values") + theme(
    plot.title = element_text(hjust = 0.5),
  )
ggsave("manipDest_histogram.png")

#ManipInfo
ggplot(dsFlights, aes(x = ManipInfo)) +
  geom_histogram(bins = 2, fill = "dodgerblue", binwidth=1, col = "black") +
  xlab("ManipInfo") + ggtitle("Distribution of the ManipInfo values") + theme(
    plot.title = element_text(hjust = 0.5),
  )
ggsave("manipInfo_histogram.png")

#cliving
unique(dsFlights$cLiving)
dsFlights$cLiving <- factor(dsFlights$cLiving, 
                             labels = c("Student housing",
                             "Privately rented room",
                             "Living with parents", "House of one’s own", "Other"))
ggplot(dsFlights, aes(x = dsFlights$cLiving)) +
  geom_bar(fill = "azure3", col = "bisque4") +
  xlab("Living mode") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Living_mode.png")

#PART3: descriptive statistics

dftemp <- data.frame(dsFlights$rateAirplane, dsFlights$Guilt, dsFlights$Env,
                     dsFlights$ImportPrice, dsFlights$ImportTime)
View(dftemp)


#quantitative
library("stargazer")
stargazer(dftemp, summary = TRUE)
stargazer(dftemp, type = "html", title="Summary statistics Guilt scores", digits = 1,
          out=paste0(dirRslt,"Guilt_stats.doc"))

#qualitative
table(dsFlights$cLiving)
table(dsFlights$ManipDest)
table(dsFlights$ManipInfo)
table(dsFlights$ManipTax)

#PART4: 99% confidence intervals
tmpAir <- describe(dftemp, skew=FALSE, ranges = FALSE)

# Add critical values, error margins and CI
alpha <- 0.01

tmpAir$tcrit <- NA
tmpAir$tcrit <- qt(1-alpha/2, df = tmpAir$n-1) #find critical value

tmpAir$err.mrg <- tmpAir$tcrit*tmpAir$se
tmpAir$ciL     <- tmpAir$mean - tmpAir$err.mrg #confidence interval bounds lower
tmpAir$ciR     <- tmpAir$mean + tmpAir$err.mrg #and upper

stargazer(tmpAir, summary = FALSE, 
          align = TRUE, no.space = TRUE, type = "html", title="Confidence intervals 99%", digits = 1,
          out=paste0(dirRslt,"Conf_int.doc")) #important to make summary = false
#otherwise it will calculate averages of averages
