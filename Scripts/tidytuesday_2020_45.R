library(tidyverse)
library(waffle)
library(extrafont)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2020, week = 45)

ikea_counts <- tuesdata$ikea %>% 
  count(category) %>% 
  mutate(n = round(n/5)) %>% 
  arrange(-n)

ikea_colors <- c(nord::nord_palettes$algoma_forest, dutchmasters::dutchmasters_pal()(13)[-c(1, 8, 12)])

ikea_waffles <- map(1:nrow(ikea_counts), ~ {
  df <- slice(ikea_counts, .x)
  ggplot(df) +
    geom_waffle(
      aes(fill = category, values = n),
      n_rows = 20,
      size = 1.5,
      flip = TRUE,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = ikea_colors[.x]) +
    ggfittext::geom_fit_text(
      aes(xmin = -15, xmax = -5, ymin = .5, ymax = .5 + ceiling(df$n/20), label = category),
      size = 54, grow = FALSE, fullheight = FALSE, place = "left" ,
      family = "Roboto Slab", fontface = "bold"
    ) +
    coord_equal(xlim = c(-16, 21)) +
    theme_void()
})

legend_key <- ggplot() +
  annotation_custom(rectGrob(0.5, 0.5, height = .02, width = .02, gp = gpar(fill = "grey50", color = "black", lwd = 1))) +
  annotation_custom(textGrob("=  5 units", gp = gpar(fontfamily = "Roboto Slab", fontface = "bold", fontsize = 12)), 3, 2.6) +
  coord_equal(xlim = c(0, 5), ylim = c(0, 5)) +
  theme_void()

patched <- wrap_plots(ikea_waffles, ncol = 1) +
  plot_annotation(
    title = "<span style='color:#997A00'>IKEA</span> <span style='color:#001F5C'>Furnitures in Stock</span>",
    caption = "@yjunechoe",
    theme = theme(
      plot.title = ggtext::element_markdown(
        size = 100,
        family = "Noto",
        face = "bold",
        hjust = .5,
        margin = margin(t = 1.5, b = 2, unit = "in")
      ),
      plot.caption = element_text(
        size = 32,
        family = "IBM Plex Mono",
        face = "bold",
        margin = margin(t = 1, b = 1, unit = "in")
      ),
      plot.margin = margin(2, 2, 2, 2, unit = "in")
    )
  ) &
  theme(plot.margin = margin(t = .5, b = .5, unit = "in")) 


ggsave("tidytuesday_2020_45.pdf", patched, device = cairo_pdf, scale = 2, width = 12, height = 26, limitsize = FALSE)

