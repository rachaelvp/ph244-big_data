library(here)
library(ggplot2)
library(dplyr)

dat <- read.csv(file=here::here("Project4.Imaging","data","train_summ.csv"))
cols <- seq(1:28)
dat[,cols] <- apply(dat[,cols], 2, function(x) as.numeric(as.character(x)))
dat$cancer <- as.factor(dat$cancer)

summ <- dat %>%
  group_by(cancer) %>%
  summarize_all(mean)

t_summ <- t(summ)
names <- row.names(t_summ)
names <- c(names)
t_summ <- data.frame(t_summ, names)
colnames(t_summ) <- c("No Cancer", "Cancer", "Var")
t_summ <- t_summ[-c(24:29),]
t_summ <- t_summ[-1,]
t_summ2 <- melt(t_summ, id = "Var")

ggplot(t_summ2, aes(Var,value,color=variable)) + geom_point()


melted <- melt(dat, id.vars = c("cancer", "id"))
melt <- melted[c(1:30712),]

melt1 <- train_summ %>%
  select(c(HU_A, HU_B, HU_C, HU_D, HU_J, HU_K, HU_L, HU_M, cancer))
melt1 <- melt(melt1, id.vars = "cancer")
ggplot(data = melt1, aes(x=variable, y=value)) +
  geom_violin(aes(fill=as.factor(cancer))) +
  coord_flip() + scale_fill_manual(values=rainbow(10),
                                   name="Lung Cancer",
                                   breaks=c(0, 1),
                                   labels=c("No", "Yes")) +
  ylab("Percentage of HU Frequency") +
  xlab("Various Hounsfield Bins") +
  ggtitle("Frequency Percentages of Hounsfield Unit") +
  theme(legend.position = c(0.9, 0.42),
  legend.text=element_text(size=10),
  legend.title=element_text(size=12))


melt2 <- train_summ %>%
  select(-c(HU_A, HU_B, HU_C, HU_D, HU_J, HU_K, HU_L, HU_M, id, Kurtosis, Median, Mean, Skew, Variance))
melt2 <- melt(melt2, id.vars = "cancer")

ggplot(data = melt2, aes(x=variable, y=value)) +
  geom_violin(aes(fill=as.factor(cancer))) +
  coord_flip(ylim = c(0,.075)) + scale_fill_manual(values=rainbow(10),
                                   name="Lung Cancer",
                                   breaks=c(0, 1),
                                   labels=c("No", "Yes")) +
  ylab("Frequency Percentage HU") +
  xlab("Various Hounsfield Bins") +
  ggtitle("Frequency Percentages of HU") +
  theme(legend.position = c(0.9, 0.15),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12))
