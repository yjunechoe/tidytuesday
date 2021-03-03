# Tweet - https://twitter.com/yjunechoe/status/1366968405664874499

library(dplyr)     # CRAN v1.0.4
library(purrr)     # CRAN v0.3.4
library(tidyr)     # CRAN v1.1.2
library(ggplot2)   # CRAN v3.3.3
library(ggtext)    # CRAN v0.1.1
library(ragg)      # CRAN v1.1.0
library(extrafont) # CRAN v0.17
library(magick)    # CRAN v2.6.0
library(gganimate) # CRAN v1.0.7


##
## Data cleaning
##

tuesdata <- tidytuesdayR::tt_load(2021, week = 10)$youtube

tuesdata_types <- tuesdata %>% 
  select(brand, where(is.logical)) %>% 
  rowwise() %>% 
  filter(any(c_across(where(is.logical)))) %>% 
  ungroup() %>% 
  mutate(
    brand = forcats::fct_rev(forcats::fct_infreq(brand)),
    ad_id = row_number()
  )

brand_ad <- set_names(tuesdata_types$brand, tuesdata_types$ad_id)



##
## Tiles for brands
##

tuesdata_brand_tiles <- tuesdata_types %>% 
  select(brand, ad_id) %>% 
  # Waffle layout
  group_by(brand) %>% 
  mutate(
    row = rep(1:8, each = 8, length.out = n()),
    col = rep(1:8, length.out = n())
  ) %>% 
  group_split() %>% 
  imap_dfr(~ {
    .x %>% 
      mutate(
        pos_x = if (.y %% 2 == 0) { 0 } else { 300 },
        pos_y = (ceiling(.y/2) - 1) * 200 - 30,
        x = pos_x + (col - 1) * 30,
        y = pos_y + (row - 1) * 30
      )
  }) %>% 
  # Adjustments
  mutate(
    xend = x + 25,
    yend = y + 25,
    across(where(is.double), ~ .x * -1)
  )



##
## Tiles for ad types
##

tuesdata_long <- tuesdata_types %>% 
  pivot_longer(-c(brand, ad_id), names_to = "type") %>% 
  filter(value) %>% 
  select(-value) %>% 
  mutate(tile_id = row_number()) %>% 
  arrange(ad_id, tile_id)

tuesdata_type_tiles <- tuesdata_long %>% 
  select(-brand) %>% 
  mutate(type = forcats::fct_rev(forcats::fct_infreq(type))) %>% 
  # Waffle layout
  group_by(type) %>% 
  mutate(
    row = rep(1:14, each = 14, length.out = n()),
    col = rep(1:14, length.out = n())
  ) %>% 
  group_split() %>% 
  imap_dfr(~ {
    .x %>% 
      mutate(
        pos_x = if (.y %% 2 == 0 || .y == 7) { 350 } else { 0 },
        pos_y = (ceiling(.y/2) - 1) * 250 * rep(c(0, 0.75, 0.8, 1), each = 2)[.y],
        x = pos_x + (col - 1) * 20,
        y = pos_y + (row - 1) * 20
      )
  }) %>% 
  # Adjustments
  mutate(
    xend = x + 15,
    yend = y + 15,
    across(where(is.double), ~ .x * -1),
    across(starts_with("x"), ~.x - 900)
  )



##
## Check layout
##

layout_plot <- ggplot(NULL) +
  geom_rect(
    aes(xmin = x, ymin = y, xmax = xend, ymax = yend),
    color = 'black',
    fill = NA,
    data = tuesdata_brand_tiles
  ) +
  geom_rect(
    aes(xmin = x, ymin = y, xmax = xend, ymax = yend, fill = type),
    data = tuesdata_type_tiles
  ) +
  coord_equal()




##
## Animation setup
##

type_palette <- set_names(colorspace::qualitative_hcl(7), levels(tuesdata_type_tiles$type))


state_left <- tuesdata_long %>% 
  select(ad_id, tile_id) %>% 
  right_join(
    tuesdata_type_tiles %>% 
      select("tile_id", type, starts_with(c("x", "y"))),
    by = "tile_id"
  ) %>% 
  mutate(
    fill = type_palette[type],
    type = forcats::fct_relevel(type, c("patriotic", "celebrity", "animals")),
    state = as.integer(type)
  )

state_right <- tuesdata_long %>% 
  select(ad_id, tile_id) %>%
  nest(data = c(tile_id)) %>% 
  right_join(
    tuesdata_brand_tiles %>% 
      select("ad_id", starts_with(c("x", "y"))),
    by = "ad_id"
  ) %>% 
  unnest(data) %>% 
  mutate(
    fill = state_left$fill,
    across(starts_with("y"), ~ .x - 20),
    state = state_left$state + 1L
  ) %>% 
  arrange(state)

# Text labels for types
labels_types <- tuesdata_type_tiles %>%
  distinct(type, pos_x, pos_y) %>%
  # Define bounding area for `geon_fit_text()`
  mutate(
    pos_xend = pos_x - 14 * 20,
    pos_yend = pos_y + 60,
    across(starts_with("pos_x"), ~ .x - 900),
    across(starts_with("pos_y"), ~ .x + 10)
  ) %>% 
  # Reorder and recode
  mutate(
    text = case_when(
      type == "use_sex" ~ "sex",
      type == "show_product_quickly" ~ "product",
      TRUE ~ as.character(type)
    ),
    text = toupper(text)
  )

# Images for brands
labels_brands <- tuesdata_brand_tiles %>% 
  distinct(brand, pos_x, pos_y) %>%
  mutate(
    pos_x = pos_x - 4 * 30,
    pos_y = pos_y - 10,
    # Image paths - code by @geokaramanis from https://github.com/gkaramanis/tidytuesday
    image = paste0(here::here("img", "tidytuesday_2021_10"), "/", brand, ".png"),
    size = case_when(
      brand %in% c("E-Trade", "Hynudai", "Doritos") ~ .17,
      brand %in% c("Kia", "Coca-Cola", "Bud Light") ~ .14,
      TRUE ~ .15
    )
  )



##
## Events
##

anim_df <- bind_rows(
  
  # Start
  mutate(state_left, state = 0L),
  # Pre-movement
  state_left,
  # Post-movement
  state_right,
  # Turn to grey after moving
  mutate(state_right, state = state + 1L, fill = scales::alpha("grey", .6)),
  
  # Pause for one tick
  mutate(state_right, state = 10L, fill = scales::alpha("grey", .6)),
  
  # Reverse (Pre-movement)
  state_right %>% 
    group_by(brand = brand_ad[ad_id]) %>% 
    mutate(
      state = cur_group_id() + 10L,
      fill = scales::alpha("grey", .6)
    ) %>% 
    ungroup(),
  # Reverse (Post-movement)
  state_left %>% 
    group_by(brand = brand_ad[ad_id]) %>% 
    mutate(state = cur_group_id() + 11L) %>% 
    ungroup(),
  # Finish and hold for 1 tick
  mutate(state_left, state = 21L)
  
) %>% 
  arrange(ad_id, tile_id)



##
## Animation
##


anim <- anim_df %>% 
  ggplot() +
  
  # Static type labels
  ggfittext::geom_fit_text(
    aes(xmin = pos_x, ymin = pos_y, xmax = pos_xend, ymax = pos_yend, label = text),
    size = 50,
    color = "grey30",
    family = "Impact",
    data = labels_types
  ) +
  
  # Static brand images
  ggimage::geom_image(
    aes(x = pos_x, y = pos_y, image = image, size = size),
    data = labels_brands
  ) +
  scale_size_identity() +
  
  # Dynamic tiles (all animation handled here)
  geom_rect(aes(xmin = x, ymin = y, xmax = xend, ymax = yend, group = tile_id, fill = fill)) +
  scale_fill_identity() +
  coord_equal(expand = FALSE) +
  
  # Labels and styles
  labs(
    title = "Super Bowl Commercials",
    subtitle = "2000-2020",
    caption = "@yjunechoe"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 24,
      family = "Impact",
      hjust = 0.5,
      margin = margin(t = .2, b = .2, unit = "in")
    ),
    plot.subtitle = element_text(
      size = 16,
      family = "IBM Plex Mono",
      face = 'bold',
      hjust = 0.5,
      margin = margin(b = .2, unit = "in")
    ),
    plot.caption = element_text(
      size = 6,
      family = "Roboto Mono",
      margin = margin(t = .2, b = .2, unit = "in")
    )
  ) +
  
  # Engine
  transition_components(state) +
  ease_aes("sine-in-out")

animate(anim, device = agg_png(), width = 6, height = 5.5, units = "in", res = 300, nframes = 200, end_pause = 1)

anim_save("tidytuesday_2021_10.gif")
  
