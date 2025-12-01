changes=read.csv("muscle change.csv")

head(changes)

changes$Gender=factor(changes$Gender, labels = c("Male", "Female"))
changes$Programme=factor(changes$Programme, labels=c("Weights", "Cardio"))

# Fit an ANOVA

model=lm(Change~Gender+Programme+Gender:Programme, data=changes)
summary(model)

interaction.plot(changes$Gender, changes$Programme, changes$Change, ylab="Muscle Mass Change", xlab="Gender")

