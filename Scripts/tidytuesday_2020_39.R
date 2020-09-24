library(tidyverse)

# Data

tuesdata <- tidytuesdayR::tt_load("2020-09-22")

climb_data <- tuesdata$expeditions %>% 
  left_join(tuesdata$peaks, by = "peak_name") %>% 
  select(peak = peak_name, year, height = height_metres) %>% 
  arrange(-height) %>% 
  mutate(height_group = fct_inorder(case_when(peak == "Everest" ~ "Mt. Everest (8850m)",
                                              between(height, 8000, 8849) ~ "> 8000m",
                                              between(height, 7000, 7999) ~ "7999m ~ 7000m",
                                              between(height, 6000, 6999) ~ "6999m ~ 6000m",
                                              TRUE ~ "< 6000m"))
  ) %>% 
  count(five_years = round(year/5) * 5, height_group) %>% 
  filter(five_years >= 1920) %>% 
  complete(five_years, height_group, fill = list(n = 0)) %>% 
  group_by(five_years) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()

# Plot

mountain_palette <- c("#6E86A6", "#95A2B3", "#5C606A", "#44464E", "#3D3737")

climb_plot <- climb_data %>% 
  ggplot(aes(five_years, prop)) +
  geom_area(aes(fill = height_group, color = height_group))  +
  scale_fill_manual(values = mountain_palette) +
  scale_color_manual(values = mountain_palette) +
  coord_cartesian(xlim = c(1920, 2020), expand = FALSE) +
  scale_x_continuous(breaks = scales::pretty_breaks(11)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Himalayan Peaks Attempted Over Time",
    subtitle = "Over 1/4th of all expeditions were to Mount Everest",
    x = NULL, y = NULL, fill = NULL, color = NULL,
    caption = "By: @yjunechoe | Source: The Himalayan Database"
  ) +
  theme_classic(base_family = "Futura Hv BT", base_size = 16) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 28, color = "white", family = "Lora", face = "bold"),
    plot.subtitle = element_text(size = 14, color = "white", face = "italic"),
    plot.margin = margin(2, 2.5, 2, 2, 'cm'),
    plot.caption = element_text(color = "white", family = "Roboto Mono", hjust = 1.15, vjust = -13),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(color = "white"),
    legend.background = element_rect(fill = NA),
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(vjust = -.1),
    axis.text.x = element_text(vjust = -2),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#606F84", color = NA)
  )


# Save

pngfile <- fs::path(getwd(), "plot.png")
ragg::agg_png(
  pngfile,
  width = 60,
  height = 36,
  units = "cm",
  res = 300,
  scaling = 2
)
plot(climb_plot); invisible(dev.off())

tuesdata$expeditions %>% 
  left_join(tuesdata$peaks, by = "peak_name") %>% 
  select(peak = peak_name, year, height = height_metres) %>% 
  arrange(-height) %>% 
  mutate(height_group = fct_inorder(case_when(peak == "Everest" ~ "Mt. Everest (8850m)",
                                              between(height, 8000, 8849) ~ "8000m and up",
                                              between(height, 7000, 7999) ~ "7500m ~ 7999m",
                                              between(height, 6000, 6999) ~ "8500m ~ 8849m",
                                              TRUE ~ "Under 6000m"))
  ) %>% 
  count(decade = floor(year/10) * 10, height_group) %>% 
  filter(decade >= 1920) %>% 
  group_by(decade) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  complete(decade, height_group, fill = list(prop = 0)) %>% 
  ggplot(aes(decade, prop)) +
  geom_area(aes(fill = height_group, color = height_group))  +
  scale_fill_manual(values = mountain_palette) +
  scale_color_manual(values = mountain_palette) 




## Replace PLOT_NAME and PLOT_OBJECT
pngfile <- fs::path(
  getwd(), #knitr::fig_path(),
  "plot.png"
)
ragg::agg_png(
  pngfile,
  width = 60,
  height = 36,
  units = "cm",
  res = 300,
  scaling = 2
)
plot(p) ; invisible(dev.off())
magick::image_read(pngfile) #knitr::include_graphics(pngfile)
  


df %>% 
  ggplot(aes(year, cut))

first_successes <- tuesdata$expeditions %>% 
  filter(str_detect(termination_reason, "Success")) %>% 
  group_by(peak_name) %>% 
  slice_min(highpoint_date, n = 1)

tuesdata$expeditions %>% 
  filter(str_detect(termination_reason, "Success")) %>% 
  group_by(peak_name) %>% 
  slice_min(highpoint_date - basecamp_date, n = 1) %>% 
  ggplot(aes(highpoint_date, highpoint_date - basecamp_date)) +
  geom_point()


tuesdata$expeditions %>% 
  group_by(peak_name, heighpoint_metres) %>% 
  filter(n() > 1) %>% 
  summarize(accidents = sum(!str_detect(termination_reason, "Accident"))) %>% 
  arrange(-accidents)


tuesdata$expeditions %>% 
  filter(peak_name == "Everest") %>% 
  mutate(highpoint_group = cut(highpoint_metres, breaks = 5)) %>% 
  ggplot(aes(highpoint_date, group = highpoint_group)) +
  geom_freqpoly(bins = 20)

tuesdata$expeditions %>% 
  filter(peak_name == "Everest") %>% 
  ggplot(aes(highpoint_date, highpoint_metres)) +
  geom_area()

peaks_clean <- tuesdata$peaks %>% 
  filter(first_ascent_year > 1000, climbing_status == "Climbed") %>%
  filter(!str_detect(first_ascent_country, ",|Inida|W Germany")) %>%
  rename(year = contains("year"),
         country = contains("country"),
         height = contains("height"))

peaks_clean %>% 
  group_by(country) %>% 
  arrange(height) %>% 
  slice(1) %>% 
  ggplot(aes(year, height)) +
  geom_point(size = 2)






## Replace PLOT_NAME and PLOT_OBJECT
pngfile <- fs::path(
  getwd(), #knitr::fig_path(),
  "plot.png"
)
ragg::agg_png(
  pngfile,
  width = 80,
  height = 100,
  units = "cm",
  res = 300,
  scaling = 3
)
plot(p) ; invisible(dev.off())
magick::image_read(pngfile) #knitr::include_graphics(pngfile)
