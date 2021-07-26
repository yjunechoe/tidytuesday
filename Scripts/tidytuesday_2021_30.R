library(tidyverse)
library(grid)
library(ggtext)
library(gridtext)

tuesdata <- tidytuesdayR::tt_load(2021, week = 30)
drought <- tuesdata$drought

drought_agg <- drought %>% 
  filter(state_abb == "CA", drought_lvl != "None") %>% 
  group_by(
    state = state.name[match(state_abb, state.abb)],
    year = lubridate::year(valid_start),
    month = lubridate::month(valid_start),
    drought_lvl = fct_rev(fct_inorder(drought_lvl, ordered = TRUE))
  ) %>% 
  summarize(area_pct = first(area_pct), .groups = 'drop') %>% 
  mutate(drought_fill = scales::col_factor(
    c("#E2E6BD", "#E5D75E", "#EAAF2B", "#E6863C", "#DB595C"),
    levels(drought_lvl))(drought_lvl)
  )

cal_years <- map(
  unique(drought_agg$year),
  ~ {
    p <- drought_agg %>% 
      filter(year == .x) %>% 
      ggplot(aes(month, area_pct, group = drought_lvl, fill = drought_fill)) +
      geom_col(
        width = 1,
        show.legend = FALSE
      ) +
      scale_x_continuous(
        limits = c(0.5, 12.5),
        breaks = c(2, 5, 8, 11),
        labels = c("Feb", "May", "Aug", "Nov")
      ) +
      scale_fill_identity(guide = guide_legend()) +
      coord_cartesian(expand = FALSE) +
      labs(
        title = .x,
        x = NULL, y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey95", color = NA),
        panel.spacing = unit(10, "pt"),
      )
    # x-axis styling
    if (.x %in% 2016:2021) {
      p <- p +
        theme(axis.text.x = element_text(
          family = "Roboto",
          size = 8,
          color = "black",
          margin = margin(t = 8)
        ))
    } else {
      p <- p + theme(axis.text.x = element_blank())
    }
    # strip text styling for drought years
    if (.x %in% c(2007:2009, 2012:2016, 2018, 2020:2021)) {
      p <- p + 
        theme(plot.title = ggtext::element_markdown(
          family = "Roboto Mono",
          face = "bold",
          size = 12,
          color = "white",
          margin = margin(b = 12),
          fill = "#C24A4E",
          padding = margin(4, 4, 2, 4),
          r = unit(4, "pt"),
          hjust = 0.5
        ))
    } else {
      p <- p +
        theme(plot.title = element_text(
          family = "Roboto Mono",
          face = "bold",
          size = 12,
          color = "black",
          margin = margin(b = 12),
          hjust = 0.5
        ))
    }
  }
)

junebug::font_hoist("Roboto Slab")


description <- textbox_grob(
  "<strong style='font-size:24px;color:#662325;font-family:\"Roboto Slab Black\"'>California in drought</strong>
   <br><br>
   California has been in a drought
   <strong style='color:#E60000;'>more often than it has not</strong>
   over the last two decades — <i style='color:#252525'> U.S. drought monitor</i>",
  x = 0, y = 1,
  hjust = 0,
  vjust = 1,
  gp = gpar(col = "black", fontsize = 12, fontfamily = "Bitter", lineheight = 1.5),
  box_gp = gpar(col = "#DC9942", fill = "#E5D67D", lwd = 4),
  r = unit(5, "pt"),
  padding = unit(c(15, 15, 10, 15), "pt"),
  margin = unit(c(0, 3, 8, 3), "pt")
)

final <- wrap_elements(full = wrap_plots(c(rep(list(plot_spacer()), 3), cal_years), ncol = 6)) +
  inset_element(wrap_elements(full = description), clip = FALSE, top = 1, right = 0.5, bottom = 0.75, left = 0)

ggsave("tidytuesday_2021_30.png", final, width = 11, height = 6.5, dpi = 300)
