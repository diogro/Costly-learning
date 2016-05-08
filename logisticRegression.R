if(!require(rms)){install.packages("rms"); library(rms)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}
if(!require(cowplot)){install.packages("cowplot"); library(cowplot)}
if(!require(magrittr)){install.packages("magrittr"); library(magrittr)}
data = read.csv("./exp_escolha.csv")

model <- glm(choice ~ treatment, family=binomial("logit"), data = data)

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


survival <- data.frame(t(read.csv("./survival.csv")))
names(survival) <- c("Rotten", "Fresh", "Dog")
rownames(survival) <- NULL
survival$Week <- 0:10
survival = dplyr::filter(survival, Week <= 7)
m_survival <- melt(survival, id.vars = "Week")
m_survival$variable <- factor(m_survival$variable, levels = c("Fresh", "Rotten", "Dog"))

survival_plot <- ggplot(m_survival, aes(Week, value, group = variable, color = variable, linetype = variable, shape = variable)) +
    labs(y = "Number of individuals alive\n") + geom_point(size = 3.5) + geom_line(size = 1.5) +
    scale_color_discrete(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food")) +
    scale_shape_discrete(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food")) +
    scale_linetype_manual(name = "Treatment", labels = c("Fresh cricket", "Rotten cricket", "Dog food"), values = c("solid", "dotted", "dashed")) +
    scale_x_continuous(breaks = 0:10) + 
    scale_y_continuous(breaks = seq(14, 30, 2)) +
        theme(legend.position = c(0.8, 0.9))
save_plot("./survival_feed_experiment.png", survival_plot, base_height = 4.5, base_aspect_ratio = 1.1)
