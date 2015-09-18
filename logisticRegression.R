library(rms)
library(ggplot2)
library(reshape2)
library(cowplot)
library(magrittr)
data = read.csv("./exp_escolha.csv")

model = lrm(choice ~ treatment, data)
contrast(model, list(treatment = "rotten"), list(treatment = "fresh"))
contrast(model, list(treatment = "rotten"), list(treatment = "dog"))
contrast(model, list(treatment = "fresh"), list(treatment = "dog"))

x = Predict(model, treatment  = levels(data$treatment))
ggplot(x) + theme_classic()
pvalues = data.frame( x = c(2, 2, 3, 3),  y = c(2.8, 2.9, 2.9, 2.8))
odds_ratio = ggplot(data.frame(x), aes(treatment, yhat)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    labs(x = "\nTreatment", y = "Log odds ratio of choosing rotten\n") +
    scale_x_discrete(labels = c("Dog food", "Fresh cricket", "Rotten cricket"), expand = c(0.2, 0)) +
    scale_y_continuous(breaks = -2:2) +
    geom_hline(yintercept = 0, linetype =  "dotted") +
    geom_path(data = pvalues, aes(x, y)) +
    annotate("text", x = 2.5, y = 3.1, label = "p < 0.01") +
    annotate("text", x = 0.5, y = 2, label = "chooses\nrotten") +
    annotate("text", x = 0.5, y = 0, label = "no\npreference") +
    annotate("text", x = 0.5, y = -2, label = "chooses\nfresh")
save_plot("./log_odds_ratio_feed_experiment.png", odds_ratio, base_height = 7)


survival <- data.frame(t(read.csv("./survival.csv")))
names(survival) <- c("Rotten", "Fresh", "Dog")
rownames(survival) <- NULL
survival$Week <- 0:10
m_survival <- melt(survival, id.vars = "Week")
m_survival$variable <- factor(m_survival$variable, levels = c("Fresh", "Rotten", "Dog"))

survival_plot <- ggplot(m_survival, aes(Week, value, group = variable, color = variable, shape = variable)) +
    labs(y = "Number of individuals alive") + geom_point(size = 3) + geom_line() +
    scale_color_discrete(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food")) +
    scale_shape_discrete(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food")) +
    scale_x_continuous(breaks = 0:10) + scale_y_continuous(breaks = seq(15, 27, 2)) + theme(legend.position = c(0.8, 0.9), text = element_text(size = 20))
save_plot("./survival_feed_experiment.png", survival_plot, base_height = 7, base_aspect_ratio = 1.1)
