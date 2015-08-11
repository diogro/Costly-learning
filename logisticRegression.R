library(rms)
data = read.csv("./exp_escolha.csv")

model = lrm(choice ~ treatment, data)
contrast(model, list(treatment = "rotten"), list(treatment = "fresh"))

ggplot(Predict(model, treatment  = levels(data$treatment)))
