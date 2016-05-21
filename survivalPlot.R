if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}

survival <- data.frame(t(read.csv("./survival.csv")))
names(survival) <- c("Rotten", "Fresh", "Dog")
rownames(survival) <- NULL
survival$Week <- 0:10
survival = subset(survival, survival$Week <= 7)
m_survival <- melt(survival, id.vars = "Week")
m_survival$variable <- factor(m_survival$variable, levels = c("Fresh", "Rotten", "Dog"))

survival_plot <- ggplot(m_survival, aes(Week, value, group = variable, linetype = variable, shape = variable)) +
  labs(y = "Number of individuals alive\n") + geom_point(size = 3.5) + geom_line(size = 1.3) +
  scale_shape_discrete(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food")) +
  scale_linetype_manual(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food"), values = c("solid", "dotted", "dashed"), guide=FALSE) +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = seq(14, 30, 2)) +
  theme(legend.position = c(0.8, 0.9))
save_plot("./survival_feed_experiment_pb.png", survival_plot, base_height = 4.5, base_aspect_ratio = 1.1)
