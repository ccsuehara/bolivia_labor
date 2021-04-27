## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE, 
  fig.width = 8, fig.height = 6, out.width = "100%"
)

## ----libs----------------------------------------------------------------
library(waffle)
library(extrafont)
library(dplyr)

## ----ef------------------------------------------------------------------
extrafont::loadfonts(quiet = TRUE)

extrafont::fonttable() %>% 
  as_tibble() %>% 
  filter(grepl("Awesom", FamilyName)) %>% 
  select(afmfile, FullName, FamilyName, FontName)

## ----rocket--------------------------------------------------------------
fa_grep("rocket")

## ----food----------------------------------------------------------------
fa_grep("bread|pizza|apple|pear|peach|lemon|sandwich")

## ----pict-data-----------------------------------------------------------
tibble(
  food_group = factor(
    c("Fruit", "Sandwiches", "Pizza"),
    levels=c("Fruit", "Sandwiches", "Pizza")
  ),
  consumption = c(5, 20, 52)
) -> xdf

xdf

## ----waf1----------------------------------------------------------------
ggplot(xdf, aes(fill = food_group, values = consumption)) +
  geom_waffle() +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle()

## ----waf2----------------------------------------------------------------
ggplot(xdf, aes(fill = food_group, values = consumption)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle()

## ----pg1, fig.width=8, fig.height=6--------------------------------------
ggplot(xdf, aes(label = food_group, values = consumption)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE, color = "black") +
  scale_label_pictogram(
    name = NULL,
    values = c(
      Fruit = "apple-alt", 
      Sandwiches = "bread-slice", 
      Pizza = "pizza-slice"
    )
  ) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))

## ----pg2, fig.width=8, fig.height=6--------------------------------------
ggplot(xdf, aes(label = food_group, values = consumption, color = food_group)) +
  geom_pictogram(n_rows = 10, make_proportional = TRUE) +
  scale_color_manual(
    name = NULL,
    values = c(
      Fruit = "#a40000",
      Sandwiches = "#c68958", 
      Pizza = "#ae6056"
    )
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c(
      Fruit = "apple-alt", 
      Sandwiches = "bread-slice", 
      Pizza = "pizza-slice"
    )
  ) +
  coord_equal() +
  theme_minimal() +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75))

