# Library Packages
library(dplyr) #%>%
library(tidyr) #could not find function "gather"
library(plyr)  #count
library(reshape2) # melt
library(ggplot2)
library(ggcorrplot)
# ==============================================================================
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# ==============================================================================
# Import Data
setwd("C:/Users/yihlu/Documents/YiRu/????????????/?????????/Bank Marketing")
df0 <- read.csv("bank-additional-full1.csv", header=T)

head(df0)
tail(df0)

# Data Information
names(df0)
dim(df0)
str(df0)
unique(df0$month)
unique(df0$duration)

# Convert variables type: as.factor
dt0 <- as.data.frame(unclass(df0), stringsAsFactors = TRUE)
str(dt0)

# Checking Missing Value: no issing value
sort(colSums(is.na(dt0)), decreasing = T)

# Desripitive Statistic
#*summary
dsf <- function(x){
  summary0 <- summary(x)
  n <- length(x)
  s <- sd(x)
  var0 <- var(x)
  iqr0 <- IQR(x)
  # skew <- skewness(x)
  # kurt <- kurtosis(x), skew=skew, kurt=kurt
  result <- c(n=n, summary0, IQR=iqr0, stdev=s, var=var0)
  return(round(result, 3))
}
head(dt0)
names(dt0)
v2 <- names(dt0)[c(1, 9, 12, 14:15, 17:21)];v2
sapply(dt0[v2], dsf)
class(dt0$y)

#*Correlation heatmap
v2
cor0 <- cor(dt0[v2]);cor0
ggcorrplot(
  cor0,
  hc.order = TRUE,
  type = "lower",
  # method = "circle",
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726"),
  lab = TRUE
)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

#*Y
table(dt0$y)
dy <- count(dt0$y);dy
dy$ratio0 <- round(dy$freq/sum(dy$freq), 4)
dy$ratio <- paste(round(100*dy$ratio0, 2), "%", sep="")
dy$fr <- paste(dy$freq, "\n", "(", dy$ratio, ")", sep="")
names(dy)[names(dy)=="x"] <- "y"
dy

p <- ggplot(data=dy, aes(x="", y=ratio0, fill=y)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  ggtitle("The ratio of client subscribed a term deposit") + 
  xlab(NULL) +
  ylab(NULL) +
  # scale_fill_manual(values=c("white","aquamarine1","aquamarine2","aquamarine3", "aquamarine4"))+
  geom_text(aes(label=fr), position=position_stack(vjust= 0.65), vjust=-0.3, size=3)+
  theme_minimal()
p

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#*Bank client data
#*Graph
# Y-age
p1 <- ggplot(dt0, aes(x=age, fill=y)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  # geom_histogram(colour="black")+
  geom_density(alpha=0.5) +
  # geom_vline(xintercept = c(30, 60),
             # col = "red",
             # linetype = "dashed") +
  ggtitle("Y - Age") + 
  # facet_grid(~ dt0$y) + 
  scale_x_continuous(breaks = seq(0, 100, 5))

p2 <- ggplot(data = dt0, mapping = aes(y = age, fill=y)) + 
  geom_boxplot() + 
  coord_flip()
p1
multiplot(p1, p2)

# age_g
dt0$age_g <- cut(dt0$age, 
                 breaks=c(15, 25, 35, 45, 55, 65, 100),
                 labels=c("15-24", "25-34", "35-44", "45-54", 
                          "55-64", "65+"),
                 right=T)

dage_g <- melt(table(dt0$age_g, dt0$y));dage_g
names(dage_g)[names(dage_g)=="Var1"] <- "age_g"
names(dage_g)[names(dage_g)=="Var2"] <- "y"
names(dage_g)[names(dage_g)=="value"] <- "freq"
dage_g1 <- dage_g[dage_g$y == "yes", ]
dage_g2 <- dage_g[dage_g$y == "no", ]
dage_g1$freq + dage_g2$freq
dage_g1$ratio <- round(dage_g1$freq/(dage_g1$freq + dage_g2$freq), 4)
dage_g2$ratio <- round(dage_g2$freq/(dage_g1$freq + dage_g2$freq), 4)
dage_g = rbind(dage_g1, dage_g2)
dage_g$ratio <- paste(round(100*dage_g$ratio, 2), "%", sep="")
dage_g$fr <- paste(dage_g$freq, " (", dage_g$ratio, ")", sep="")
dage_g

ggplot(data=dage_g, aes(x=age_g, y=freq, fill=y)) +
  geom_bar(stat="identity") + 
  ggtitle("The distribution of age group") + 
  xlab("age_g") +
  ylab(NULL) +
  facet_grid(y ~ .) +
  # coord_flip() +
  geom_text(aes(label=dage_g$fr), vjust=-0.2, size=3)

# ------------------------------------------------------------------------------
# Y-job
table(dt0$job)
dtg <- melt(table(dt0$job, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "job"
names(dtg)[names(dtg)=="Var2"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g1 <- dtg[dtg$y == "yes", ]
dtg_g2 <- dtg[dtg$y == "no", ]
dtg_g1$freq + dtg_g2$freq
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg

pjob <- ggplot(data=dtg[dtg$y == "yes", ], aes(x=reorder(job, -ratio), y=ratio)) +
  geom_bar(stat="identity",position = "dodge", fill="#56B4E9") + 
  ggtitle("Subscription Rates of job") + 
  # coord_flip() +
  ylim(0, 0.4) + 
  xlab("job") + 
  ylab("Subscription Rates") + 
  # facet_grid(~y) + 
  geom_text(aes(label=fr), vjust=-0.2, hjust=0.5, color="black", size=3)
pjob

# ------------------------------------------------------------------------------
# Y - eduaction
table(dt0$education)
dtg <- melt(table(dt0$education, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "education"
names(dtg)[names(dtg)=="Var2"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g1 <- dtg[dtg$y == "yes", ]
dtg_g2 <- dtg[dtg$y == "no", ]
dtg_g1$freq + dtg_g2$freq
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg
pe <- ggplot(data=dtg[dtg$y == "yes", ], aes(x=reorder(education, -ratio), y=ratio, fill = y)) +
  geom_bar(stat="identity", fill="#E69F00", ) + 
  ggtitle("Subscription Rates of education") + 
  # coord_flip() +
  ylim(0, 0.3) +
  xlab("education") +
  ylab("Subscription Rates") +
  # facet_grid(~y) +
  geom_text(aes(label=fr), vjust=-0.2, hjust=0.5, color="black", size=3)
pe

multiplot(pjob, pe)

# ------------------------------------------------------------------------------
# Y-house
table(dt0$housing)

dma <- count(dt0$housing);dma
dma$ratio <- round(dma$freq/sum(dma$freq), 4)
dma$ratio0 <- paste(round(100*dma$ratio, 2), "%", sep="")
dma$fr <- paste(dma$freq, "\n", "(", dma$ratio0, ")", sep="")
names(dma)[names(dma)=="x"] <- "housing"
dma
ggplot(data=dma, aes(x="", y=ratio, fill=housing)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  ggtitle("The pie chart of housing") + 
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values=c("aquamarine4",  "aquamarine1", "white"))+
  geom_text(aes(label=fr), position=position_stack(vjust= 0.35), hjust=0.5, vjust=0, size=3)+
  theme_minimal()

# ------------------------------------------------------------------------------
# loan
table(dt0$loan)

dma <- count(dt0$loan);dma
dma$ratio <- round(dma$freq/sum(dma$freq), 4)
dma$ratio0 <- paste(round(100*dma$ratio, 2), "%", sep="")
dma$fr <- paste(dma$freq, "\n", "(", dma$ratio0, ")", sep="")
names(dma)[names(dma)=="x"] <- "loan"
dma
ggplot(data=dma, aes(x="", y=ratio, fill=loan)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  ggtitle("The pie chart of loan") + 
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values=c("aquamarine4",  "aquamarine1", "white"))+
  geom_text(aes(label=fr), position=position_stack(vjust= 0.35), hjust=0.5, vjust=0, size=3)+
  theme_minimal()

dtg <- melt(table(dt0$loan, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "label2"
names(dtg)[names(dtg)=="Var2"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g1 <- dtg[dtg$y == "yes", ]
dtg_g2 <- dtg[dtg$y == "no", ]
dtg_g1$freq + dtg_g2$freq
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg_l = dtg
dtg_l$label = "loan"
dtg = rbind(dtg_h, dtg_l)
dtg
ggplot(data=dtg, aes(x=reorder(label2, freq), y=ratio, fill=y)) +
  geom_bar(stat="identity") + 
  ggtitle("Subscription Rates of housing and loan") +
  coord_flip() +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("Subscription Rates") + 
  facet_grid(label~.) +
  geom_text(aes(label=fr),   
            position = position_stack(vjust = 0.5),
            color="black", size=3)

# ------------------------------------------------------------------------------
# default
dma <- count(dt0$default);dma
dma$ratio <- round(dma$freq/sum(dma$freq), 4)
dma$ratio0 <- paste(round(100*dma$ratio, 2), "%", sep="")
dma$fr <- paste(dma$freq, "\n", "(", dma$ratio0, ")", sep="")
names(dma)[names(dma)=="x"] <- "default"
dma
ggplot(data=dma, aes(x="", y=ratio, fill=default)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  ggtitle("The pie chart of whether client has credit in default") + 
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values=c("aquamarine4",  "aquamarine1", "white"))+
  geom_text(aes(label=fr), position=position_stack(vjust= 0.5), hjust=0.5, vjust=-0.5, size=3)+
  theme_minimal()

table(dt0$default)
dtg <- melt(table(dt0$default, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "default"
names(dtg)[names(dtg)=="Var2"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g1 <- dtg[dtg$y == "yes", ]
dtg_g2 <- dtg[dtg$y == "no", ]
dtg_g1$freq + dtg_g2$freq
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg
ggplot(data=dtg, aes(x=reorder(default, freq), y=ratio, fill=y)) +
  geom_bar(stat="identity") + 
  ggtitle("Subscription Rates of default") + 
  coord_flip() +
  ylim(0, 1) +
  xlab("default") + 
  ylab("Subscription Rates") + 
  # facet_grid(~y) + 
  geom_text(aes(label=fr),   
            position = position_stack(vjust = 0.5),
            color="black", size=3)

# ------------------------------------------------------------------------------
# marital
dma <- count(dt0[dt0$y == "yes", ]$marital);dma
dma$ratio <- round(dma$freq/sum(dma$freq), 4)
dma$ratio0 <- paste(round(100*dma$ratio, 2), "%", sep="")
dma$fr <- paste(dma$freq, "\n", "(", dma$ratio, ")", sep="")
names(dma)[names(dma)=="x"] <- "marital"
dma
ggplot(data=dma, aes(x="", y=ratio, fill=marital)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  ggtitle("The ratio of kinds of marital when client subscribed a term deposit") + 
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values=c("aquamarine4", "aquamarine3", "aquamarine1", "white"))+
  geom_text(aes(label=ratio0), position=position_stack(vjust= 0.35), hjust=0.5, vjust=-1, size=3)+
  theme(plot.title = element_text(size=11))

dtg <- melt(table(dt0$marital, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "marital"
names(dtg)[names(dtg)=="Var2"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g1 <- dtg[dtg$y == "yes", ]
dtg_g2 <- dtg[dtg$y == "no", ]
dtg_g1$freq + dtg_g2$freq
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg
ggplot(data=dtg, aes(x=reorder(marital, -ratio), y=ratio, fill=y)) +
  geom_bar(stat="identity") + 
  ggtitle("Subscription Rates of marital") + 
  # coord_flip() +
  ylim(0, 1) +
  xlab("marital") + 
  ylab("Subscription Rates") + 
  # facet_grid(~y) + 
  geom_text(aes(label=fr),   
            position = position_stack(vjust = 0.5),
            color="black", size=3)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#*Hypothesis test
# Y-age
x1 = dt0[dt0$y == "yes", ]$age
x2 = dt0[dt0$y == "no", ]$age
# Normality test (when n >30 do not need do this part)
length(x1)
length(x2)
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)
#  Two sample t test
t.test(x1, x2, var.equal = FALSE)

# Y-age_g
table(dt0$y, dt0$age_g) 
chisq.test(table(dt0$y, dt0$age_g))$expected
chisq.test(table(dt0$y, dt0$age_g))

# Y-job
table(dt0$y, dt0$job) 
chisq.test(table(dt0$y, dt0$job))$expected
chisq.test(table(dt0$y, dt0$job))

# Y-eduation
table(dt0$y, dt0$education)  #use fisher
chisq.test(table(dt0$y, dt0$education))$expected
fisher.test(table(dt0$y, dt0$education), simulate.p.value=TRUE)

# Y-default
table(dt0$y, dt0$default)  #use fisher
chisq.test(table(dt0$y, dt0$default))$expected
fisher.test(table(dt0$y, dt0$default))

# Y-housing
table(dt0$y, dt0$housing) 
chisq.test(table(dt0$y, dt0$housing))$expected
chisq.test(table(dt0$y, dt0$housing))

# Y-loan
table(dt0$y, dt0$loan) 
chisq.test(table(dt0$y, dt0$loan))$expected
chisq.test(table(dt0$y, dt0$loan))

# Y-maritial
table(dt0$y, dt0$marital) 
chisq.test(table(dt0$y, dt0$marital))$expected
chisq.test(table(dt0$y, dt0$marital))

# ------------------------------------------------------------------------------
# ==============================================================================
#*Related with the last contact of the current campaign
# Y-duration
dt0$duration_m = dt0$duration/60
df1$duration_m = df1$duration/60

p5 <- ggplot(dt0, aes(x=duration_m, fill=y)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=0.5) + 
  ggtitle("Y - Duration_m") + 
  scale_x_continuous(breaks = seq(0, 100, 5))

p6 <- ggplot(data = dt0, mapping = aes(y = duration_m, fill=y)) + 
  geom_boxplot() + 
  coord_flip() + 
  scale_y_continuous(breaks = seq(0, 100, 5))
multiplot(p5, p6, cols = 1)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Hypothesis test
# Y-duration
x1 = dt0[dt0$y == "yes", ]$duration
x2 = dt0[dt0$y == "no", ]$duration
# Normality test (when n >30 do not need do this part)
length(x1)
length(x2)
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-contact
table(dt0$y, dt0$contact) 
chisq.test(table(dt0$y, dt0$contact))$expected
chisq.test(table(dt0$y, dt0$contact))

# Y-year
table(dt0$y, dt0$year) 
chisq.test(table(dt0$y, dt0$year))$expected
chisq.test(table(dt0$y, dt0$year))

# Y-month
table(dt0$y, dt0$month) 
chisq.test(table(dt0$y, dt0$month))$expected
chisq.test(table(dt0$y, dt0$month))

# Y-day_od_week
table(dt0$y, dt0$day_of_week)  
chisq.test(table(dt0$y, dt0$day_of_week))$expected
chisq.test(table(dt0$y, dt0$day_of_week))

# ------------------------------------------------------------------------------
# ==============================================================================
#*Other attributes
# pdays
table(dt0$pdays)
dpdays <- melt(table(dt0$pdays, dt0$y));dpdays
dpdays$ratio <- round(dpdays$value/sum(dpdays[dpdays$Var2 == "yes"]$value), 4)
dpdays$ratio <- paste(round(100*dpdays$ratio, 2), "%", sep="")
dpdays$fr <- paste(dpdays$value, " (", dpdays$ratio, ")", sep="")
dpdays = dpdays[-c(27, 54), ]
dpdays
names(dpdays)[names(dpdays) == 'Var1'] <- "pdays"
names(dpdays)[names(dpdays) == 'Var2'] <- "y"
names(dpdays)[names(dpdays) == 'value'] <- "freq"

ggplot(data=dpdays, aes(x=pdays, y=freq, fill=y)) +
  geom_bar(stat="identity") + 
  ggtitle("Y - pdays_g - ever_contact") + 
  xlab("pdays") +
  ylab("count")+
  facet_grid(y~.) + 
  scale_x_continuous(breaks = seq(0, 30, 2))
  # coord_flip()
  # geom_text(aes(label=dpdays$fr), vjust=0.5, color="black", size=3.5)

# pdays_g
table(dt0$pdays)
dt0$pdays_g <- ifelse(dt0$pdays == 999, "never_contact", "ever_contact")

t1 <- table(dt0$pdays_g, dt0$y)
t1 <- melt(t1);t1
t1$ratio_y <- c("36.17%", "90.74%", "63.83%", "9.26%")
t1$fr <- paste(t1$value, " (", t1$ratio_y, ")", sep="")
colnames(t1)[2] = c("y")
t1

ggplot(data=t1, aes(x=y, y=value, fill=y)) +
  geom_bar(stat="identity") + 
  ggtitle("Y - Pdays_g") + 
  xlab("pdays_g") +
  ylab("count") + 
  facet_grid(~Var1) + 
  # scale_fill_manual(" y", values = c("no" = "#E69F00", "yes" = "#56B4E9")) +
  geom_text(aes(label=fr), vjust=-0.5, color="black", size=3.5)

# ------------------------------------------------------------------------------
# Y-previous
table(dt0$previous, dt0$y)
dtg <- melt(table(dt0$previous, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "previous"
names(dtg)[names(dtg)=="Var2"] <- "y"
# names(dtg)[names(dtg)=="Var3"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g1 <- dtg[(dtg$y == "yes"), ]
dtg_g2 <- dtg[(dtg$y == "no"), ]
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg

ggplot(dtg[(dtg$y == "yes"), ], aes(y=ratio, x=previous)) + 
  scale_x_continuous(breaks=seq(0,7,1)) + 
  scale_y_continuous(breaks=seq(0,1.2,0.2)) + 
  ylim(0, 1) +
  xlab("previous") + 
  ylab(NULL) +
  geom_point(size=2, color="#13c19e") + 
  geom_line(color="#13c19e" ) + 
  # facet_grid(y~.) +
  ggtitle("Subscription Rates of previous") + 
  geom_text(aes(label=fr),vjust = -0.6, 
            color="black", size=3)


# ------------------------------------------------------------------------------
# Y-poutcome
dma <- count(dt0$poutcome);dma
dma$ratio <- round(dma$freq/sum(dma$freq), 4)
dma$ratio0 <- paste(round(100*dma$ratio, 2), "%", sep="")
dma$fr <- paste(dma$freq, "\n", "(", dma$ratio0, ")", sep="")
names(dma)[names(dma)=="x"] <- "poutcome"
dma
ggplot(data=dma, aes(x="", y=ratio, fill=poutcome)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0)+
  ggtitle("The ratio of poutcome") + 
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values=c("white", "aquamarine2", "aquamarine4"))+
  geom_text(aes(label=fr), position=position_stack(vjust= 0.2), hjust=0.5, vjust=0.5, size=2.8)


table(dt0$poutcome, dt0$pdays_g, dt0$y)
dtg <- melt(table(dt0$poutcome, dt0$y));dtg
names(dtg)[names(dtg)=="Var1"] <- "poutcome"
names(dtg)[names(dtg)=="Var2"] <- "y"
# names(dtg)[names(dtg)=="Var3"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"
dtg_g11 <- dtg[(dtg$y == "yes" & dtg$poutcome == "failure"), ]
dtg_g21 <- dtg[(dtg$y == "no" & dtg$poutcome == "failure"), ]
dtg_g12 <- dtg[(dtg$y == "yes" & dtg$poutcome == "nonexistent"), ]
dtg_g22 <- dtg[(dtg$y == "no" & dtg$poutcome == "nonexistent"), ]
dtg_g13 <- dtg[(dtg$y == "yes" & dtg$poutcome == "success"), ]
dtg_g23 <- dtg[(dtg$y == "no" & dtg$poutcome == "success"), ]
dtg
dtg_g1$freq + dtg_g2$freq
dtg_g11$ratio <- round(dtg_g11$freq/(dtg_g11$freq + dtg_g21$freq), 4)
dtg_g21$ratio <- round(dtg_g21$freq/(dtg_g11$freq + dtg_g21$freq), 4)
dtg_g12$ratio <- round(dtg_g12$freq/(dtg_g12$freq + dtg_g22$freq), 4)
dtg_g22$ratio <- round(dtg_g22$freq/(dtg_g12$freq + dtg_g22$freq), 4)
dtg_g13$ratio <- round(dtg_g13$freq/(dtg_g13$freq + dtg_g23$freq), 4)
dtg_g23$ratio <- round(dtg_g23$freq/(dtg_g13$freq + dtg_g23$freq), 4)
dtg = rbind(dtg_g11, dtg_g12, dtg_g13, dtg_g21, dtg_g22, dtg_g23)

dtg
dtg_g1 <- dtg[(dtg$y == "yes"), ]
dtg_g2 <- dtg[(dtg$y == "no"), ]
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg
ggplot(data=dtg, aes(x=reorder(poutcome, -ratio), y=ratio, fill=y)) +
  geom_bar(stat="identity", position = "stack") + 
  ggtitle("Subscription Rates of poutcome") + 
  coord_flip() +
  # ylim(0, 1) +
  xlab("poutcome") + 
  ylab(NULL) +
  # scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  # facet_grid(pdays_g~.) +
  geom_text(aes(label=fr),
            position = position_stack(vjust = 0.5),
            color="black", size=3)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Hypothesis test
# Y-campaign: discrete variable 
x1 = dt0[dt0$y == "yes", ]$campaign
x2 = dt0[dt0$y == "no", ]$campaign
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")


# Y-pdays: discrete variable 
x1 = dt0[dt0$y == "yes", ]$pdays
x2 = dt0[dt0$y == "no", ]$pdays
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-pdays_g
table(dt0$y, dt0$pdays_g) #
chisq.test(table(dt0$y, dt0$pdays_g))$expected
chisq.test(table(dt0$y, dt0$pdays_g))

# Y-previous
x1 = dt0[dt0$y == "yes", ]$previous
x2 = dt0[dt0$y == "no", ]$previous
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-poutcome
table(dt0$y, dt0$poutcome) 
chisq.test(table(dt0$y, dt0$poutcome))$expected
chisq.test(table(dt0$y, dt0$poutcome))

# ------------------------------------------------------------------------------
# ==============================================================================
#*Social and economic context
# create month_n
dt0$month_n = dt0$month
levels(dt0$month_n)
levels(dt0$month_n) = c(4, 8, 12, 7, 6, 3, 5, 11, 10, 9)
dt0$month_n = as.numeric(levels(dt0$month_n))[dt0$month_n]
dt0[,c("month", "month_n")]

# ------------------------------------------------------------------------------

# cons.price.idx
table(dt0$month_n, dt0$year, dt0$y)
table(dt0$month_n, dt0$y)
dtg <- melt(table(dt0$month_n, dt0$year, dt0$y));dtg
# dtg = rbind(dtg, c(1, "no", 0), c(2, "no", 0), c(1, "yes", 0), c(2, "yes", 0))
names(dtg)[names(dtg)=="Var1"] <- "month_n"
names(dtg)[names(dtg)=="Var2"] <- "year"
names(dtg)[names(dtg)=="Var3"] <- "y"
names(dtg)[names(dtg)=="value"] <- "freq"

dtg_g1 <- dtg[(dtg$y == "yes"), ]; dtg_g1
dtg_g2 <- dtg[(dtg$y == "no"), ]; dtg_g2
dtg_g1$ratio <- round(dtg_g1$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg_g2$ratio <- round(dtg_g2$freq/(dtg_g1$freq + dtg_g2$freq), 4)
dtg = rbind(dtg_g1, dtg_g2)
dtg[is.na(dtg)] <- 0
dtg$ratio0 <- paste(round(100*dtg$ratio, 2), "%", sep="")
dtg$fr <- paste(dtg$freq, "\n", "(", dtg$ratio0, ")", sep="")
dtg = dtg[c(1:29, 31:59), ]
dcast(dt0, year~month_n, value.var='cons.price.idx', mean)
g1 = ggplot(data=dtg[(dtg$y == "yes"), ], aes(y=ratio, x=month_n),) + 
  scale_x_continuous(breaks=seq(0,12,3)) +
  xlab("month") + 
  ylab(NULL) +
  geom_point(size=2, color="#13c19e") +
  geom_line(color="#13c19e") +
  facet_grid(~year) + 
  ggtitle("Subscription Rates")  
g1
g2 = ggplot(data=dt0, aes(y=cons.price.idx, x=month_n)) + 
  scale_x_continuous(breaks=seq(0,12,3)) +
  scale_y_continuous(breaks=seq(92,96,0.5)) +
  xlab("month") + 
  ylab(NULL) +
  geom_point(size=2, color="#56B4E9") +
  geom_line(color="#56B4E9") +
  facet_grid(~year) + 
  ggtitle("The trend of cons.price.idx")

multiplot(g1, g2)
# ------------------------------------------------------------------------------

# cons.conf.idx
dcast(dt0, year~month_n, value.var='cons.price.idx', mean)
g3 = ggplot(data=dt0, aes(y=cons.conf.idx, x=month_n)) + 
  scale_x_continuous(breaks=seq(0,12,3)) +
  # scale_y_continuous(breaks=seq(92,96,0.5)) +
  xlab("month") + 
  ylab(NULL) +
  geom_point(size=2, color="#E69F00") +
  geom_line(color="#E69F00") +
  facet_grid(~year) + 
  ggtitle("The trend of cons.conf.idx")
g3

multiplot(g2, g3)

# ------------------------------------------------------------------------------
# euribor3m
# table0 = aggregate(dt0$euribor3m, 
#                    list(dt0$year, dt0$month_n), 
#                    FUN=function(x) c(mn = mean(x),  
#                                      max = max(x),
#                                      min = min(x)))
# colnames(table0)[1:2] = c("year", "month_n")
# table0 = cbind(table0[, 1:2], data.frame(table0[, 3]))
# 
# g4 = ggplot(data=table0, aes(y=mn, x=month_n)) + 
#   scale_x_continuous(breaks=seq(0,12,3)) +
#   # scale_y_continuous(breaks=seq(0, 7, 1)) +
#   xlab("month") + 
#   ylab(NULL) +
#   geom_point(size=2, color="lightsalmon") +
#   geom_line(color="lightsalmon") +
#   facet_grid(~year) + 
#   ggtitle("The trend of the mean of euribor3m")
# g4

# ------------------------------------------------------------------------------
# nr.employed
# 3-5:1, 6-8:2, 9-11:3, 12-2:4
# dn = unique(dt0[, c("year", "month_n", "nr.employed")])
# dn$season = ifelse(dn$month_n %in% 3:5, 1,
#                    ifelse(dn$month_n %in% 6:8, 2,
#                           ifelse(dn$month_n %in% 9:11, 3, 4))) #>0.5: Level 2ifelse(MinePred > 0.5,"yes","no") #>0.5: Level 2
# dn
# 
# 
# dcast(dn, year~season, value.var='nr.employed', mean)
# g5 = ggplot(dn, aes(y=nr.employed, x=month_n)) +
#   scale_x_continuous(breaks=seq(0,12,3)) +
#   geom_point(size=2, color="palevioletred1") +
#   geom_line(color="palevioletred1") +
#   facet_grid(~year) +
#   ggtitle("nr.employed - season - year")
# g5
# multiplot(g2, g5)

# ------------------------------------------------------------------------------
# # # Y-emp.var.rate
# dem = unique(dt0[, c("year", "month_n", "emp.var.rate")])
# dem
# dem$season = ifelse(dem$month_n %in% 3:5, 1,
#                    ifelse(dn$month_n %in% 6:8, 2,
#                           ifelse(dn$month_n %in% 9:11, 3, 4))) #>0.5: Level 2ifelse(MinePred > 0.5,"yes","no") #>0.5: Level 2
# dem
# 
# 
# dcast(d1, year~season, value.var='nr.employed', mean)
# g6 = ggplot(dem, aes(y=emp.var.rate, x=month_n)) +
#   scale_x_continuous(breaks=seq(0,12,3)) +
#   geom_point(size=2, color="mediumpurple1") +
#   geom_line(color="mediumpurple1") +
#   facet_grid(~year) +
#   ggtitle("emp.var.rate - season - year")
# 
# multiplot(g5, g6)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Hypothesis Testing
# Y-emp.var.rate
x1 = dt0[dt0$y == "yes", ]$emp.var.rate
x2 = dt0[dt0$y == "no", ]$emp.var.rate
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-cons.price.idx
x1 = dt0[dt0$y == "yes", ]$cons.price.idx
x2 = dt0[dt0$y == "no", ]$cons.price.idx
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-cons.conf.idx
x1 = dt0[dt0$y == "yes", ]$cons.conf.idx
x2 = dt0[dt0$y == "no", ]$cons.conf.idx
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-euribor3m
x1 = dt0[dt0$y == "yes", ]$euribor3m
x2 = dt0[dt0$y == "no", ]$euribor3m
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# Y-nr.employed
x1 = dt0[dt0$y == "yes", ]$nr.employed
x2 = dt0[dt0$y == "no", ]$nr.employed
# Normality test (when n >30 do not need do this part)
count(is.na(x1))
count(is.na(x2))
shapiro.test(x1)
shapiro.test(x2)
# F test
var.test(x1, x2)   #var not equal
#  Two sample t test
t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")

# ==============================================================================
# ==============================================================================
# Import data again for imputing missing value
df1 <- read.csv("bank-additional-full1.csv", header=T, na.strings = "unknown")
df1$age_g <- cut(df1$age, 
                 breaks=c(15, 25, 35, 45, 55, 65, 100),
                 labels=c("15-24", "25-34", "35-44", "45-54", 
                          "55-64", "65+"),
                 right=T)
df1$pdays_g <- ifelse(df1$pdays == 999, "never_contact", "ever_contact")

dt1 <- as.data.frame(unclass(df1), stringsAsFactors = TRUE)
str(dt1)

# Imputing missing value
library(mice)
imputed_Data <- mice(dt1, m=5, maxit = 10, method = 'cart', seed = 500)
summary(imputed_Data)
cd1 <- complete(imputed_Data, 1)
cd2 <- complete(imputed_Data, 2)
cd3 <- complete(imputed_Data, 3)
cd4 <- complete(imputed_Data, 4)
cd5 <- complete(imputed_Data, 5)
sort(colSums(is.na(cd2)), decreasing = T)
cd2

# write.csv(cd1, file = "cd1_1113.csv")
# write.csv(cd2, file = "cd2_1113.csv")
# write.csv(cd3, file = "cd3_1113.csv")
# write.csv(cd4, file = "cd4_1113.csv")
# write.csv(cd5, file = "cd5_1113.csv")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Split training data & testing data
# cd2 AIC????????????
dim(dt0)
data0 = cd2
all_n = 1:nrow(data0)
set.seed(400)
train_n = sample(nrow(data0), 0.8*nrow(data0))
test_n = setdiff(all_n, train_n)
traindata = data0[train_n, ]
testdata = data0[test_n, ]

# ------------------------------------------------------------------------------
### Model
library(caret) 
## Feature Selection using Logistic Regression
model <- glm(y ~ age_g + job + marital + education + housing + loan + contact + 
               year + month + day_of_week + duration + campaign + 
               pdays_g + previous + poutcome + emp.var.rate + cons.price.idx + 
               cons.conf.idx + euribor3m + nr.employed,
             data = traindata, family = binomial(link = logit))
#*Stepwise
model_step <- step(model, direction = "both")
result_step <- summary(model_step);result_step

# ------------------------------------------------------------------------------
# Logistic Regression
Model_LR <- glm(y ~ age_g + contact + year + education + month + 
                  day_of_week + duration + campaign + pdays_g + 
                  poutcome + emp.var.rate + cons.price.idx + 
                  cons.conf.idx + euribor3m + nr.employed,
                data=traindata, 
                family = binomial(link = logit))

# Predict
Pred_LR <-predict(Model_LR, newdata = testdata)
Pred_LR
Ans_LR <- ifelse(Pred_LR > 0.5,"yes","no") #>0.5: Level 2
Ans_LR <- as.factor(Ans_LR);Ans_LR

indicators <- caret::confusionMatrix(Ans_LR, testdata$y, positive = "yes")
indicators

# ------------------------------------------------------------------------------
# Random Forest
library(randomForest)
Model_RF <- randomForest(y ~ age_g + contact + year + education + month + 
                           day_of_week + duration + campaign + pdays_g + 
                           poutcome + emp.var.rate + cons.price.idx + 
                           cons.conf.idx + euribor3m + nr.employed,
                         data=traindata) #
plot(Model_RF, lwd=2) #OOB Error Rate

#*Important variables
importance(Model_RF) # MeanDecreaseGini
varImpPlot(Model_RF, sort = TRUE)

## Perform on the testing data
# type: response(???????????????), prob(??????????????????????????????), vote(???????????????????????????????????????)
Pred_RF <- predict(Model_RF, newdata = testdata);Pred_RF #Type preset is "response"
indicators <- caret::confusionMatrix(Pred_RF, testdata$y, positive = "yes")
indicators

# For Roc Curve
Pred_RF_df <- as.data.frame(predict(Model_RF, 
                                    newdata = testdata, 
                                    type = "prob"))#Type preset is "response"
Pred_RF_df$predict <- names(Pred_RF_df)[1:2][apply(Pred_RF_df[, 1:2], 1, which.max)]
head(Pred_RF_df)

# ------------------------------------------------------------------------------
# Decision Tree
library(party)
Model_DT <- ctree(y ~ age_g + education + month + 
                   day_of_week + duration + campaign + pdays_g + 
                   poutcome + emp.var.rate + cons.price.idx + 
                   euribor3m + nr.employed, 
                 traindata) #cons.conf.idx +   contact +year +
plot(Model_DT)
# dev.off()
# Predict
Pred_DT <- predict(Model_DT, newdata = testdata); Pred_DT

indicators <- caret::confusionMatrix(Pred_DT, testdata$y, positive = "yes")
indicators

# For Roc Curve
Pred_DT_p <- predict(Model_DT, newdata = testdata, type = "prob")
Pred_DT_p_list = c()
for(i in 1:length(Pred_DT_p)){
  Pred_DT_p_list = rbind(Pred_DT_p_list, Pred_DT_p[[i]])
}
Pred_DT_df <- as.data.frame(Pred_DT_p_list); Pred_DT_df #Type preset is "response"
colnames(Pred_DT_df) = c("no", "yes"); head(Pred_DT_df)
Pred_DT_df$predict <- names(Pred_DT_df)[1:2][apply(Pred_DT_df[, 1:2], 1, which.max)]
head(Pred_DT_df)
count(Pred_DT_df$predict)

# ------------------------------------------------------------------------------
# SVM
require(e1071)
Model_SVM <- svm(y ~ age_g + education + contact + year + month +
                   day_of_week + duration + campaign + pdays_g + 
                   poutcome + emp.var.rate + cons.price.idx + 
                   cons.conf.idx + euribor3m + nr.employed,# ??????????????????????????????Factor
                 data = traindata,
                 probability=TRUE)#
summary(Model_SVM)

Pred_SVM <- predict(Model_SVM, newdata = testdata);Pred_SVM

indicators <- caret::confusionMatrix(Pred_SVM, testdata$y, positive = "yes")
indicators

# For Roc Curve
Pred_SVM_p <- predict(Model_SVM, newdata = testdata, probability=TRUE)
Pred_SVM_p
attr(Pred_SVM_p, "probabilities")
Pred_SVM_df <- as.data.frame(attr(Pred_SVM_p, "probabilities")); Pred_SVM_df
Pred_SVM_df$predict <- names(Pred_SVM_df)[1:2][apply(Pred_SVM_df[, 1:2], 1, which.max)]
head(Pred_SVM_df)
count(Pred_SVM_df$predict)

# precision = ppv
# recall = sensitivity

# ------------------------------------------------------------------------------
# ROC & AUC
library(ggplot2)
library(pROC)
#define object to plot
roc_LR <- roc(testdata$y, Pred_LR); roc_LR
roc_RF <- roc(ifelse(testdata$y=="yes", "yes", "no"), 
              as.numeric(Pred_RF_df$yes)); roc_RF
roc_DT <- roc(ifelse(testdata$y=="yes", "yes", "no"), 
              as.numeric(Pred_DT_df$yes)); roc_DT
roc_SVM <- roc(ifelse(testdata$y=="yes", "yes", "no"), 
               as.numeric(Pred_SVM_df$yes)); roc_SVM

#create ROC plot
ggroc(roc_SVM)
# Multiple curves:
g2 <- ggroc(list(LR=roc_LR, RF=roc_RF,DT=roc_DT, SVM=roc_SVM),
            size = 0.5,
            legacy.axes = T) + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="gray40", linetype="dashed") +
  ggtitle("ROC curve")
g2





