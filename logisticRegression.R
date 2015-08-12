library(rms)
library(ggplot2)
library(magrittr)
data = read.csv("./exp_escolha.csv")

model = lrm(choice ~ treatment, data)
contrast(model, list(treatment = "rotten"), list(treatment = "fresh"))
contrast(model, list(treatment = "rotten"), list(treatment = "dog"))
contrast(model, list(treatment = "fresh"), list(treatment = "dog"))

x = Predict(model, treatment  = levels(data$treatment))
ggplot(x) + theme_classic()
pvalues = data.frame( x = c(2, 2, 3, 3),  y = c(2.8, 2.9, 2.9, 2.8))
ggplot(data.frame(x), aes(treatment, yhat)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    theme_classic() + labs(x = "\nTreatment", y = "Log odds ratio of choosing rotten\n") +
    scale_x_discrete(labels = c("Dog food", "Fresh cricket", "Rotten cricket"), expand = c(0.2, 0)) +
    scale_y_continuous(breaks = -2:2) +
    geom_hline(yintercept = 0, linetype =  "dotted") +
    geom_path(data = pvalues, aes(x, y)) +
    annotate("text", x = 2.5, y = 3.1, label = "p < 0.01") +
    annotate("text", x = 0.5, y = 2, label = "chooses\nrotten") +
    annotate("text", x = 0.5, y = 0, label = "no\npreference") +
    annotate("text", x = 0.5, y = -2, label = "chooses\nfresh")
ggsave("./log_odds_ratio_feed_experiment.png", height = 7, width = 10)
