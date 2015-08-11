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
ggplot(data.frame(x), aes(treatment, yhat)) + geom_pointrange(aes(ymin = lower, ymax = upper)) + theme_classic() + labs(y = "Log odds of choosing rotten")

