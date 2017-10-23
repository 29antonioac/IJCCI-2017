## ----setup, include=FALSE, cache=FALSE-----------------------------------
library(knitr)
library(ggplot2)
library(ggthemes)
library(reshape2)

knitr::opts_chunk$set(echo = F, fig.pos = "H")
#knit_hooks$set(document = function(x) {sub('\\usepackage[]{color}', '\\usepackage[pdftex,dvipsnames]{xcolor}', x, fixed = TRUE)})

readSparkData <- function(folder) {
  dfs <- lapply(list.files(folder, pattern = "*.csv", full.names = TRUE), read.csv)
  df <- do.call(rbind, dfs)
  df
}

# opts_chunk$set(cache=TRUE, autodep=TRUE)
# opts_chunk$set(fig.width=4, fig.height=3,fig.pos = '!hbt')
#opts_chunk$set(fig.pos = 'H', cache=T, autodep=T)
#options(scipen=999)
options(digits=3)

## ----measures------------------------------------------------------------
measures.full.path <- "data/measures_full.csv"
measures.full <- read.csv(measures.full.path)
frames <- sort(unique(measures.full$Frames))

max.duration <- max(measures.full$Frames)
avg.duration.frames <- 43968.9184653
frames.to.mins <- 15*60
avg.duration.mins <- avg.duration.frames / frames.to.mins

measures.mean <- aggregate(measures.full[,3:8], by = list(Frames = measures.full$Frames), mean)
measures.median <- aggregate(measures.full[,3:8], by = list(Frames = measures.full$Frames), median)
measures.sd <- aggregate(measures.full[,3:8], by = list(Frames = measures.full$Frames), sd)
measures.norm <- aggregate(measures.full[,3:8], by = list(Frames = measures.full$Frames), function(v) shapiro.test(v)$p.value > 0.05)

mean.melt <- melt(measures.mean, id.vars = "Frames", variable.name = "Classifier", value.name = "Accuracy")
median.melt <- melt(measures.median, id.vars = "Frames", variable.name = "Classifier", value.name = "Accuracy")
sd.melt <- melt(measures.sd, id.vars = "Frames", variable.name = "Classifier", value.name = "SD")

normality <- all(measures.norm[,2:7] == T)
mean_or_median.sd <- merge( if (normality) mean.melt else median.melt,
                            sd.melt,
                            by = c("Frames", "Classifier"))

## ----MeasuresData,results='asis'-----------------------------------------
results <- subset(mean_or_median.sd, Frames == max.duration)

params <- data.frame( Classifier = c("GBT","LR","NB","RF","MLP","KNN"),
                      Params = c("numTrees = 150, maxDepth = 10","maxIter = 150, regParam = 0.3",
                     "Smoothing = 1", "numTrees = 150, maxDepth = 10",
                     "Hidden layers = (10,10)", "K = 3"))
results <- merge(results, params, by = "Classifier")

rownames(results) <- NULL
# print(xtable::xtable(params),
#   floating.environment = "table")
print(xtable::xtable(results[,-2],
  caption = "Trained algorithms with their final parameters",
  table.placement="H",
  label = "tab:measures",
  digits = 6 ),
  floating.environment = "table*")

## ----MeasuresGraphics, fig.cap = "Measures obtained with all models", fig.height=8----
ggplot(subset(mean_or_median.sd, Frames == max.duration), aes(Classifier, Accuracy, fill = Classifier)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=Accuracy-SD, ymax=Accuracy+SD),width=.3) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))+ guides(fill=FALSE) +
  theme(legend.position="bottom")+theme_tufte()
ggsave("bars.png",width=32,height=18,units="cm")

## ----TimeComparisonAcc, fig.cap = "Comparing classifiers over time", fig.height = 8----
ggplot(subset(mean_or_median.sd, Frames != max.duration), aes(Frames, Accuracy, col = Classifier)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=Accuracy-SD, ymax=Accuracy+SD)) +
  scale_x_continuous(breaks = frames) +
  scale_y_continuous(breaks = seq(0.4,1.0,by=0.05)) +
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8), axis.text.y = element_text(size = 8))+theme_tufte()
ggsave("lines.png",width=32,height=18,units="cm")

## ----Friedman, results = "asis"------------------------------------------
d <- as.matrix(subset(measures.full, Frames == max.duration)[,-c(1,2)])
fr <- friedman.test(d)

rownames(d) <- NULL

print(xtable::xtable(d,
  caption = "Accuracy of classifiers in each validation set",
  table.placement = "H",
  label = "tab:acc",
  digits = 6),
  floating.environment = "table*")

## ----Pairwise, results = "asis"------------------------------------------
tam <- dim(d)
groups <- rep(1:tam[2], each=tam[1])

a <- pairwise.t.test(d, g = groups, paired = T)
out <- a$p.value
colnames(out) <- colnames(measures.full)[3:7]
rownames(out) <- colnames(measures.full)[4:8]
print(xtable::xtable(out,
  caption = "p-values from a pairwise t-test",
  table.placement="H",
  label = "tab:pvalues",
  digits = 6 ),
  floating.environment = "table*")

