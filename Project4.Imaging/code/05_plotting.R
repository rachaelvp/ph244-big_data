# Code to create summary plots of raw data

# Read in the raw CSV
train_summary <- read.csv("/Users/Tommy/Desktop/ph244-big_data/Project4.Imaging/data/train_summ.csv")

# There's one patient with a whole row of NA's... we should remove them.
train_summary <- train_summary[-c(374), ]

# Subset the important columns
train_summary <- train_summary[, c( "HU_A", "HU_B", "HU_C", "HU_D", "HU_E", "HU_F", "HU_G", "HU_H", "HU_I", "HU_J", "HU_K", "HU_L", "HU_M", "HU_N", "HU_O", "HU_P", "HU_Q", "id", "cancer")]
train_summary_sub <- train_summary[, c( "HU_A", "HU_F", "HU_K", "HU_L", "HU_M", "HU_N", "HU_O", "HU_P", "HU_Q", "id", "cancer")]
train_summary_sum <- train_summary[, c("Kurtosis", "Variance", "Skew", "Mean", "Median", "id", "cancer")]


# Side by side boxplot
library(ggplot2)
library(reshape)

# Melt the dataframe for boxplot
melted <- melt(train_summary, id.vars = c("id", "cancer"))
melted_sub <- melt(train_summary_sub, id.vars = c("id", "cancer"))
melted_sub_sum <- melt(train_summary_sum, id.vars = c("id", "cancer"))

# Convert cancer to factor for boxplot
melted$cancer <- factor(melted$cancer)
melted_sub$cancer <- factor(melted_sub$cancer)
melted_sub_sum$cancer <- factor(melted_sub_sum$cancer)

summ_plot <- ggplot(data = melted, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cancer))

summ_plot + coord_flip() + scale_fill_manual(values=rainbow(10), 
                            name="Lung Cancer Status",
                            breaks=c(0, 1),
                            labels=c("No Cancer", "Cancer")) + ylab("Percent Hounsfield Unit Makeup") + xlab("Various Hounsfield Bins") + ggtitle("Frequency Percentages of HU")


summ_plot_sub <- ggplot(data = melted_sub, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cancer))

summ_plot_sub + scale_fill_manual(values=rainbow(10), 
                                             name="Lung Cancer Status",
                                             breaks=c(0, 1),
                                             labels=c("No Cancer", "Cancer")) + ylab("Percent Hounsfield Unit Makeup") + xlab("Various Hounsfield Bins") + ggtitle("Frequency Percentages of HU")

summ_plot_sub_sum <- ggplot(data = melted_sub_sum, aes(x=variable, y=value)) + geom_boxplot(aes(fill=cancer))

summ_plot_sub_sum + coord_flip() +  scale_fill_manual(values=rainbow(10), 
                                  name="Lung Cancer Status",
                                  breaks=c(0, 1),
                                  labels=c("No Cancer", "Cancer")) + ylab("Hounsfield Unit Makeup") + xlab("Various Hounsfield Summaries") + ggtitle("Frequency Percentages of HU")



