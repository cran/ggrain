## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  #warning = FALSE, message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggrain)

## ----most basic raincloud possible--------------------------------------------
ggplot(iris, aes(1, Sepal.Width)) +
  geom_rain() +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

## ----rainclouds by species grouped--------------------------------------------
ggplot(iris, aes(1, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2')

## ----rainclouds by Species colored--------------------------------------------
ggplot(iris, aes(1, Sepal.Width, fill = Species, color = Species)) +
  geom_rain(alpha = .6,
            boxplot.args = list(color = "black", outlier.shape = NA)) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2')

## ----rainclouds by Species colored dogded-------------------------------------
ggplot(iris, aes(1, Sepal.Width, fill = Species, color = Species)) +
  geom_rain(alpha = .5, rain.side = 'l',
            boxplot.args = list(color = "black", outlier.shape = NA),
            boxplot.args.pos = list(
              position = ggpp::position_dodgenudge(x = .1, width = 0.1), width = 0.1
            )) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none')

## ----rainclouds by species----------------------------------------------------
ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none')

## ----rainclouds by time flip--------------------------------------------------
ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

## ----rainclouds by time flip nudged-------------------------------------------
ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, 
            boxplot.args.pos = list(
              width = 0.05, position = position_nudge(x = 0.13)),
            violin.args.pos = list(
              side = "r",
              width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

## ----rainclouds by time colored by group--------------------------------------
ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .6,
            cov = "Sepal.Length") +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  scale_color_viridis_c(option =  "A", direction = -1)

## ----making long iris data----------------------------------------------------
set.seed(42) # the magic number

iris_subset <- iris[iris$Species %in% c('versicolor', 'virginica'),]

iris.long <- cbind(rbind(iris_subset, iris_subset, iris_subset), 
                   data.frame(time = c(rep("t1", dim(iris_subset)[1]), rep("t2", dim(iris_subset)[1]), rep("t3", dim(iris_subset)[1])),
                              id = c(rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]))))

# adding .5 and some noise to the versicolor species in t2
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] <- iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] + .5 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"]), sd = .2)
# adding .8 and some noise to the versicolor species in t3
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] <- iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] + .8 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"]), sd = .2)

# now we subtract -.2 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] <- iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] - .2 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"]), sd = .2)

# now we subtract -.4 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] <- iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] - .4 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"]), sd = .2)

iris.long$Sepal.Width <- round(iris.long$Sepal.Width, 1) # rounding Sepal.Width so t2 data is on the same resolution
iris.long$time <- factor(iris.long$time, levels = c('t1', 't2', 't3'))

## ----rainclouds by time by group----------------------------------------------
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5) +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

## ----rainclouds by time by group flipped--------------------------------------
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f',
             boxplot.args.pos = list(width = .1,
                position = ggpp::position_dodgenudge(width = .1, #width needed now in ggpp version 0.5.g
                  x = c(-.13, -.13, # pre versicolor, pre virginica
                        .13, .13))), # post; post
            violin.args.pos = list(width = .7, quantiles = NULL,
             position = position_nudge(x = c(rep(-.2, 512), rep(-.2, 512),# pre; pre
                                             rep(.2, 512), rep(.2, 512))))) + #post; post
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

## ----rainclouds by time by group flipped long id------------------------------
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2', id.long.var = "id") +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

## ----rainclouds by time by group flipped long id colored----------------------
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species, color = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2', id.long.var = "id",
            violin.args = list(color = NA, alpha = .7)) +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  scale_color_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

## ----rainclouds long cov------------------------------------------------------
ggplot(iris.long, aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f', id.long.var = "id", cov = "Sepal.Length",
            boxplot.args = list(outlier.shape = NA, alpha = .8),
            violin.args = list(alpha = .8, color = NA),
            boxplot.args.pos = list(width = .1,
             position = ggpp::position_dodgenudge(width = .1,
                                                  x = c(-.13, -.13, # t1 old, t1 young
                                                        -.13, .13, 
                                                         .13, .13))),
            violin.args.pos = list(width = .7,
             position = position_nudge(x = c(rep(-.2, 512), rep(-.2, 512),# t1
                                             rep(-.2, 512), rep(.2, 512), # t2
                                             rep(.2, 512), rep(.2, 512))))) +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  scale_color_viridis_c(option =  "A", direction = -1) +
  guides(fill = 'none', color = 'none')

## ----rainclouds by time by group flipped long id colored mean-----------------
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2') +
  theme_classic() +
  stat_summary(fun = mean, geom = "line", aes(group = Species, color = Species)) +
  stat_summary(fun = mean, geom = "point",
               aes(group = Species, color = Species)) +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  scale_color_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

## ----rainclouds sig, eval = F, echo = T---------------------------------------
# ggplot(iris.long[iris.long$Species == 'versicolor' & iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
#   geom_rain(alpha = .5, rain.side = 'f1x1') +
#  ggsignif::geom_signif(
#   comparisons = list(c("t1", "t2")),
#   map_signif_level = TRUE) +
# scale_fill_manual(values=c("darkorange", "darkorange")) +
# theme_classic()

