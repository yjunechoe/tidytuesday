library(tidyverse)

make_mountain <- function(x_start, x_end, base = 0, peak_x, peak_y, n_peaks = 3, peaks_ratio = 0.3, side.first = "left") {
  
  midpoint_abs <- (peak_y - base)/2 + base
  midpoint_rel <- (peak_y - base)/2
  
  side_1_n_peaks <- floor(n_peaks/2)
  side_2_n_peaks <- n_peaks - side_1_n_peaks -1
  
  side_1_x <- seq(x_start, peak_x, length.out = side_1_n_peaks * 2 + 2)
  side_1_x <- side_1_x[-c(1, length(side_1_x))]
  
  side_2_x <- seq(peak_x, x_end, length.out = side_2_n_peaks * 2 + 2)
  side_2_x <- side_2_x[-c(1, length(side_2_x))]
  
  side_1_y <- numeric(length(side_1_x))
  side_1_y[c(TRUE, FALSE)] <- runif(length(side_1_y)/2, midpoint_abs, midpoint_abs + midpoint_rel * peaks_ratio)
  side_1_y[c(FALSE, TRUE)] <- runif(length(side_1_y)/2, midpoint_abs - midpoint_rel * peaks_ratio, midpoint_abs)
  
  side_2_y <- numeric(length(side_2_x))
  side_2_y[c(TRUE, FALSE)] <- runif(length(side_2_y)/2, midpoint_abs, midpoint_abs + midpoint_rel * peaks_ratio)
  side_2_y[c(FALSE, TRUE)] <- runif(length(side_2_y)/2, midpoint_abs - midpoint_rel * peaks_ratio, midpoint_abs)
  
  if (side.first == "left") {
    side_left <- data.frame(x = side_1_x, y = side_1_y)
    side_right <- data.frame(x = side_2_x, y = rev(side_2_y))
  } else if (side.first == "right") {
    side_left <- data.frame(x = side_2_x, y = side_2_y)
    side_right <- data.frame(x = side_1_x, y = rev(side_1_y))
  } else {
    error('Inavlid value for side.first - choose between "left" (default) or "right"')
  }
  
  polygon_points <- rbind(
    data.frame(x = c(x_start, peak_x, x_end), y = c(base, peak_y, base)),
    side_left,
    side_right
  )
  
  polygon_points[order(polygon_points$x),]

}

tuesdata <- tidytuesdayR::tt_load("2020-09-22")

peaks <- tuesdata$peaks
expeditions <- tuesdata$expeditions

top_peaks <- expeditions %>% 
  count(peak_name) %>% 
  slice_max(n, n = 10)

plot_df <- peaks %>% 
  filter(peak_name %in% top_peaks$peak_name) %>% 
  arrange(-height_metres) %>% 
  mutate(peak_name = fct_inorder(peak_name))

plot_df %>% 
  ggplot(aes(x = first_ascent_year, y = height_metres)) +
  pmap(list(plot_df$first_ascent_year, plot_df$height_metres, plot_df$peak_name),
       ~ geom_polygon(aes(x, y, fill = ..3), alpha = .6,
                      make_mountain(x_start = 1945, x_end = 1965, base = 5000,
                                    peak_x = ..1, peak_y = ..2, n_peaks = sample(3:5, 1)))
  ) +
  geom_point(color = "white") +
  ggrepel::geom_text_repel(aes(label = peak_name),
                           nudge_y = 100, segment.color = 'white',
                           family = "Montserrat", fontface = "bold", color = "white") +
  guides(fill = guide_none()) +
  scale_x_continuous(expand = expansion(0.01, 0)) +
  scale_y_continuous(limits = c(5000, 9000), expand = expansion(0.02, 0)) +
  theme_minimal(base_family = "Montserrat", base_size = 12) +
  labs(title = "TOP 10 Most Attempted Himalayan Peaks",
       x = "First Ascent Year", y = "Peak Height (m)") +
  palettetown::scale_fill_poke(pokemon = "articuno") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 24, vjust = 3, family = "Lora"),
    text = element_text(color = "white", face = "bold"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(size = 14),
    axis.title.x = element_text(vjust = -2),
    axis.title.y = element_text(vjust = 4),
    plot.margin = margin(1, .5, .7, .7, "cm"),
    plot.background = element_rect(fill = "#5C606A", color = NA),
    panel.grid = element_blank(),
  ) -> p

## Replace PLOT_NAME and PLOT_OBJECT
pngfile <- fs::path(
  getwd(), #knitr::fig_path(),
  "plotgen.png"
)
ragg::agg_png(
  pngfile,
  width = 72,
  height = 54,
  units = "cm",
  res = 300,
  scaling = 3
)
plot(p) ; invisible(dev.off())
magick::image_read(pngfile) #knitr::include_graphics(pngfile)
