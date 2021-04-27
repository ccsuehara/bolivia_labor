## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE, 
  fig.width = 8, fig.height = 6, out.width = "100%"
)

## ----libs----------------------------------------------------------------
library(waffle)
library(ggplot2)
library(dplyr)

## ----data----------------------------------------------------------------
three_states <- sample(state.name, 3)

data.frame(
  states = factor(rep(three_states, 3), levels = three_states),
  vals = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  col = rep(c("blue", "black", "red"), 3),
  fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
) -> xdf

xdf

## ----base----------------------------------------------------------------
xdf %>%
  count(states, wt = vals) %>%
  ggplot(aes(fill = states, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_minimal() +
  theme_enhance_waffle() -> waf

## ----plain---------------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 20, size = 0.33, colour = "white", flip = TRUE
  )

## ----prop----------------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 0.33, colour = "white", flip = TRUE,
    make_proportional = TRUE
  )

## ----bigger-lines--------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 3, colour = "white", 
    make_proportional = TRUE
  )

## ----color-change-1------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 0.33, colour = "black", 
    make_proportional = TRUE
  )

## ----color-change-2------------------------------------------------------
waf +
  geom_waffle(
    aes(colour = states),
    n_rows = 10, size = 0.33, make_proportional = TRUE
  )

## ----color-change-3------------------------------------------------------
waf +
  geom_waffle(
    aes(colour = states),
    n_rows = 10, size = 0.45, make_proportional = TRUE
  ) +
  scale_colour_manual(
    values = c(alpha("black", 1/3), "black", alpha("black", 1/3))
  )

## ----color-change-4------------------------------------------------------
waf +
  geom_waffle(
    aes(colour = states),
    n_rows = 10, size = 0.3, make_proportional = TRUE,
    height = 0.9, width = 0.9
  ) +
  scale_colour_manual(
    values = c("white", "black", "white")
  )

## ----color-change-5------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 0.3, make_proportional = TRUE,
    height = 0.9, width = 0.9
  ) +
  scale_fill_manual(
    values = c(alpha("#f8766d", 1/3), "#00ba38", alpha("#619cff", 1/3))
  ) 

## ----round-one-----------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 0.5, colour = "white", 
    make_proportional = TRUE,
    radius = unit(4, "pt")
  )

## ----round-two-----------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 0.5, colour = "white", 
    radius = unit(4, "pt")
  )

## ----round-three---------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 1, colour = "black", 
    make_proportional = TRUE,
    radius = unit(4, "pt"),
    height = 0.8, width = 0.8
  )

## ----round-four----------------------------------------------------------
waf +
  geom_waffle(
    aes(colour = states),
    n_rows = 10, size = 0.4, make_proportional = TRUE,
    radius = unit(4, "pt"),
    height = 0.9, width = 0.9
  ) +
  scale_colour_manual(
    values = c("black", "white", "white")
  )

## ----round-five----------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 10, size = 1, color = "white", make_proportional = TRUE,
    radius = unit(4, "pt"),
    height = 1, width = 1
  ) +
  scale_fill_manual(
    values = c("#f8766d", alpha("#00ba38", 1/3), alpha("#619cff", 1/3))
  )

## ----waffle-bar----------------------------------------------------------
waf +
  geom_waffle(
    n_rows = 5, color = "white", show.legend = FALSE, flip = TRUE
  ) +
  facet_wrap(~states) +
  theme_minimal() +
  theme(panel.spacing.x = unit(0, "npc")) +
  theme(strip.text.x = element_text(hjust = 0.5)) 

## ----waffle-buffet-setup-------------------------------------------------
xdf %>%
  ggplot(aes(fill = states, values = vals)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_minimal() +
  theme_enhance_waffle() -> buf

## ----waffle-buffet-------------------------------------------------------
buf +
  geom_waffle(
    color = "white", size = 0.33
  ) +
  facet_wrap(~fct) +
  theme(strip.text.x = element_text(hjust = 0.5))

## ----waffle-buffet-prop--------------------------------------------------
buf +
  geom_waffle(
    color = "white", size = 0.33, 
    make_proportional = TRUE, n_rows = 10
  ) +
  facet_wrap(~fct) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(hjust = 0.5))

## ----waffle-buffet-round-------------------------------------------------
buf +
  geom_waffle(
    color = "white", size = 0.33, 
    make_proportional = TRUE, n_rows = 10,
    radius = unit(2, "pt")
  ) +
  facet_wrap(~fct) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(hjust = 0.5))

## ----waffle-buffet-high--------------------------------------------------
buf +
  geom_waffle(
    color = "white", size = 0.33, 
    make_proportional = TRUE, n_rows = 10,
    radius = unit(2, "pt")
  ) +
  facet_wrap(~fct) +
  scale_fill_manual(
    values = c("#f8766d", alpha("#00ba38", 1/3), alpha("#619cff", 1/3))
  ) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(hjust = 0.5))

## ----waffle-buffet-bars--------------------------------------------------
buf +
  geom_waffle(
    color = "white", size = 0.33, n_rows = 4, flip = TRUE
  ) +
  facet_wrap(~fct) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(hjust = 0.5))

## ----over-the-top--------------------------------------------------------
storms %>%
  filter(year >= 2010) %>%
  count(year, status) -> storms_df

ggplot(storms_df, aes(fill = status, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))

