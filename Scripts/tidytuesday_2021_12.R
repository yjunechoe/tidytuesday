library(dplyr)
library(ggplot2)
library(ggforce)
library(cowplot)
library(ggfx)
library(ragg)

tuesdata <- tidytuesdayR::tt_load(2021, week = 12)$games

portal <- tuesdata %>% 
  filter(gamename == "Portal") %>% 
  mutate(month = match(month, month.name))

portal_year_data <- portal %>% 
  group_by(year) %>% 
  summarize(
    avg = mean(avg),
    month_min = min(month),
    month_max = max(month)
  )

portal %>% 
  ggplot(aes(x = month, y = avg)) +
  geom_ellipse(
    aes(x0 = month_max + 1, y0 = avg, a = .7, b = 75, angle = 0),
    color = "#27A7D8",
    fill = "grey90",
    inherit.aes = FALSE,
    data = portal_year_data %>% 
      filter(year != max(year))
  ) +
  geom_ellipse(
    aes(x0 = month_min - 1, y0 = avg, a = .7, b = 75, angle = 0),
    color = "#FF9A00",
    fill = "grey90",
    inherit.aes = FALSE,
    data = portal_year_data %>% 
      filter(year != min(year))
  ) +
  geom_segment(
    aes(x = month, xend = month, y = year_avg, yend = avg),
    data = portal %>% 
      inner_join(
        rename(portal_year_data, year_avg = avg),
        by = "year"
      )
  ) +
  with_blur(
    geom_linerange(
      aes(y = avg, xmin = month_min - 1, xmax = month_max + 1),
      size = 1.1,
      color = "grey50",
      inherit.aes = FALSE,
      data = portal_year_data
    )
  ) +
  geom_linerange(
    aes(y = avg, xmin = month_min - 1, xmax = month_max + 1),
    size = 1,
    color = "grey85",
    inherit.aes = FALSE,
    data = portal_year_data
  ) +
  with_blur(
    geom_point(
      size = .5,
      color = "white"
    ) ,
    sigma = 1
  ) +
  scale_x_continuous(expand = expansion(mult = .03)) +
  scale_y_continuous(expand = expansion(mult = .1)) +
  facet_row(
    ~ year,
    scales = "free_x",
    space = "free",
    strip.position = "bottom"
  ) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 8, family = "Supply", margin = margin(r = .5, unit = "npc")),
    plot.margin = margin(.05, 0.02, .05, 0.02, unit = "npc"),
    plot.background = element_rect(fill = "grey40"),
    strip.placement = "outside",
    strip.text = element_text(family = "Supply")
  )


junebug::raggsave_auto(height = 8, width = 30)
