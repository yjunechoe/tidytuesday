library(tidyverse)   # CRAN v1.3.0
library(extrafont)   # CRAN v0.17
library(ggtext)      # CRAN v0.1.1
library(ggfittext)   # CRAN v0.9.0
library(ggforce)     # CRAN v0.3.2
library(ggraph)      # CRAN v2.0.4
library(magick)      # CRAN v2.6.0
library(colorspace)  # CRAN v2.0-0

set.seed(202105)

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics

companies_2019 <- plastics %>% 
  filter(year == 2019, parent_company != "Grand Total") %>% 
  group_by(parent_company) %>%
  summarize(company_total = sum(grand_total)) %>% 
  arrange(-company_total) %>% 
  add_count(wt = company_total, name = "total") %>% 
  mutate(company_total_pct = company_total/total * 100)

totals_2019 <- plastics %>% 
  filter(year == 2019, parent_company == "Grand Total")

top_countries <- totals_2019 %>% 
  mutate(country = str_to_lower(country)) %>% 
  group_by(country) %>% 
  summarize(
    country_total = sum(grand_total),
    country_total_pct = country_total/sum(totals_2019$grand_total, na.rm = TRUE) * 100
  ) %>% 
  arrange(-country_total) %>% 
  filter(country != "empty") %>% 
  mutate(country = str_to_title(str_extract(country, "^([:alpha:]| )+"))) %>% 
  slice_max(country_total, n = 10) %>% 
  mutate(
    country = str_replace_all(country, " ", "\n"),
    country = fct_inorder(country)
  )

circles_pos <- pack_circles(top_countries$country_total_pct)
circles_shrinkage <- .78
circles_df <- tibble(
  x = circles_pos[,1] * .8,
  y = circles_pos[,2] * .9,
  r = sqrt(top_countries$country_total_pct/pi) * circles_shrinkage
)
total_area <- pi * attr(circles_pos, 'enclosing_radius')^2 * circles_shrinkage

# Scaled to enclosing circle area
bin_shape <- tibble(
  x = c(-5.5, 5.7, 3.2, -3),
  y = c(5, 5, -5.2, -5.2)
)

circle_palette <- qualitative_hcl(10)

description <- "\"The <strong>#breakfreefromplastic</strong> Movement is a global movement envisioning a future free from plastic pollution.<br>Since its launch in 2016, more than 11,000 organizations and individual supporters from across the world have<br>joined the movement to demand massive reductions in single-use plastics and to push for lasting solutions to the<br>plastic pollution crisis.\" - breakfreefromplastic.org"

white_text_style <- function(text) {
  paste0("<span style='color:white'>", text, "</span>")
}

annotation_top <- "In <strong>2019</strong>, nearly <strong style='color:#E10000'>430 thousand pieces of plastic waste</strong> <br> were documented and tracked by over <strong>70 thousand<br> volunteers</strong> from fifty-two countries."
annotation_mid <- "Just <strong>TEN countries</strong> accounted for <strong>71%</strong> of the waste. <br> <strong style='color:#DD7DB6'>Taiwan</strong> alone was responsible for <strong style='color:#DD7DB6'>over a quarter (28%)</strong> <br> of the global plastic pollution."
annotation_bot <- "Plastic waste was traced to various industrial and <br> individual activities. Among the <strong style='color:#E10000'>biggest polluters</strong> were <br> <em>The Coca-Cola Company</em>, <em>Nestlé</em>, and <em>PepsiCo<em>"
annotation_fin <- "<strong style='color:#30CD22'>Join the movement to stop plastic pollution</strong>"

final_plot <- top_countries %>% 
  ggplot() +
  geom_shape(
    aes(x = x, y = y),
    data = bin_shape,
    radius = unit(1, 'cm'),
    fill = '#3162B3',
    color = "black",
    size = 2
  ) +
  # white background layer for circles
  geom_circle(
    aes(x0 = x, y0 = y, r = r),
    fill = 'white',
    color = NA,
    inherit.aes = FALSE,
    data = circles_df,
    show.legend = FALSE,
  ) +
  geom_circle(
    aes(x0 = x, y0 = y, r = r, fill = circle_palette),
    inherit.aes = FALSE,
    color = lighten("#3162B3", .3),
    data = circles_df,
    show.legend = FALSE,
    alpha = .5
  ) +
  geom_text(
    aes(x = -4.7, y = 4.1, label = "2019"),
    size = 10,
    family = 'Rubik',
    color = "white",
    hjust = 0
  ) +
  geom_richtext(
    aes(x = 3.5, y = 3.3, label = "<span style='font-family: \"Font Awesome 5 Free Solid\"'>&#61880;</span>"),
    size = 25,
    color = "#910101",
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt')
  ) +
  geom_fit_text(
    aes(xmin = x - r, xmax = x + r, ymin = y - r, ymax = y + r, label = country, fill = circle_palette),
    size = 40,
    family = "Adelle",
    inherit.aes = FALSE,
    data = circles_df %>% 
      mutate(country = top_countries$country),
    show.legend = FALSE,
  ) +
  geom_richtext(
    aes(x = 6.5, y = 3.5, label = white_text_style(annotation_top)),
    size = 4,
    family = 'Roboto',
    hjust = 0,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt')
  ) +
  geom_richtext(
    aes(x = 6.5, y = 1, label = white_text_style(annotation_mid)),
    size = 4,
    family = 'Roboto',
    hjust = 0,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt')
  ) +
  geom_richtext(
    aes(x = 6.5, y = -1.5, label = white_text_style(annotation_bot)),
    size = 4,
    family = 'Roboto',
    hjust = 0,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt')
  ) +
  geom_richtext(
    aes(x = 4.5, y = -3.7, label = annotation_fin),
    size = 7,
    family = 'Roboto Condensed',
    hjust = 0,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt')
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, .9))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  coord_equal() +
  labs(
    title = "#break<strong style='color:#01A8DC'>free</strong>fromplastic",
    subtitle = description,
    caption = "Made by: @yjunechoe | Data: Break Free From Plastic"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#35363A", color = "white"),
    plot.title = element_markdown(
      size = 48,
      family = "Rubik",
      color = "#96D8EA"
    ),
    plot.subtitle = element_markdown(
      family = "Roboto",
      color = "white",
      lineheight = 1.2
    ),
    plot.caption = element_text(
      family = "IBM Plex Mono",
      color = "white",
      hjust = .97
    ),
    plot.margin = margin(t = 1, l = 1, b = .5, unit = 'cm')
  )

ggsave(filename = "tidytuesday_2021_05.png", plot = final_plot, width = 24, height = 18, type = 'cairo', unit = "cm")