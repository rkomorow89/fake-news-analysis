library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(Hmisc)

data<-mtcars
m.mtcars <- lm(mpg ~ cyl + hp + wt, data = mtcars)

#Spalten Label hinzufügen
var.labels = c(mpg="Miles/(US) gallon", cyl="Number of cylinders", hp="Gross horsepower", wt="Weight (1000 lbs)")
label(data) = as.list(var.labels[match(names(data), names(var.labels))])
label(data)

m.mtcars <- lm(mpg ~ cyl + hp + wt, data = data)
tab_model(m.mtcars)


set.seed(2)
dat <- data.frame(
  y = runif(100, 0, 100),
  drug = as.factor(sample(c("nonsense", "useful", "placebo"), 100, TRUE)),
  group = as.factor(sample(c("control", "treatment"), 100, TRUE))
)

var.labels = c(y="y", drug="Droge", group="Gruppe")
label(dat) = as.list(var.labels[match(names(dat), names(var.labels))])
label(dat)

pretty_names <- lm(y ~ drug + group, data = dat)

tab_model(pretty_names)

#auf d_test anwenden, um aus art --> Artikelart und sammlung --> Textsammlung zu machen

d_test <- 
  daten_gesamt %>% 
  dplyr::mutate(art = case_when(grepl("FAKE|F_", ID_2) ~ "Fake News",
                                TRUE ~ "True News")) %>% 
  filter(ID_2 != "174_FAKE_1")

var.labels = c(LIX="LIX", art="Artikelart", sammlung="Textsammlung")
label(d_test) = as.list(var.labels[match(names(d_test), names(var.labels))])
label(d_test)

m <- lm(LIX ~ art + sammlung, d_test)

sjPlot::tab_model(m)







