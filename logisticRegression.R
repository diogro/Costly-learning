if(!require(rms)){install.packages("rms"); library(rms)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
data = read.csv("./exp_escolha.csv")

model = lrm(choice ~ treatment, data)
contrast(model, list(treatment = "rotten"), list(treatment = "fresh"))
contrast(model, list(treatment = "rotten"), list(treatment = "dog"))
contrast(model, list(treatment = "fresh"), list(treatment = "dog"))

x = Predict(model, treatment  = levels(data$treatment))
expx = x
expx[-1] = exp(x[-1])
pvalues = data.frame( x = c(2, 2, 3, 3),  y = c(15, 16, 16, 15))
labels = c(seq(0.1, 0.9, 0.1), 1:20)
labels[!labels %in% c(0.1, 0.2, 0.3, 0.4, 0.5, 1, 2, 3, 4, 5, 10, 20)] = ""
labels[3] = "0.3"
odds_ratio = ggplot(data.frame(expx), aes(treatment, yhat)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    labs(x = "\n         Treatment", y = "Odds ratio of choosing\n rotten cricket") +
    scale_x_discrete(labels = c("Dog\nfood", "Fresh\ncricket", "Rotten\ncricket"), expand = c(0.2, 0)) +
    scale_y_log10(breaks = c(seq(0.1, 0.9, 0.1), 1:20), labels = labels, lim = c(0.1, 20)) +
    geom_hline(yintercept = 1, linetype =  "dotted") +
    geom_path(data = pvalues, aes(x, y)) +
    annotate("text", x = 2.5, y = 19, label = "p < 0.01") +
    annotate("text", x = 0.5, y = 4, label = "Chooses\nrotten") +
    annotate("text", x = 0.5, y = 1, label = "No\npreference") +
    annotate("text", x = 0.5, y = 0.2, label = "Chooses\nfresh") +
    theme(legend.position = c(0.8, 0.9))
save_plot("./odds_ratio_feed_experiment.png", odds_ratio, base_height = 4.5, base_aspect_ratio = 1.1)

count_plot = ggplot(data, aes(treatment, fill = choice)) + geom_bar() + 
  scale_fill_grey(name = "Choice", labels = c("Fresh\ncricket", "Rotten\ncricket")) + 
  labs(x = "Treatment", y = "Number of individuals") + 
  scale_x_discrete(labels = c("Dog\nfood", "Fresh\ncricket", "Rotten\ncricket")) + 
  coord_flip() +  theme(legend.position = c(1, 0.8), 
                        legend.key.height=unit(2, "line"),
                        plot.margin = unit(c(0.1, 1.1, 0.1, 0.1), "cm"))
save_plot("./count_plot_pb.png", count_plot, base_height = 4.5, base_aspect_ratio = 1.1)

