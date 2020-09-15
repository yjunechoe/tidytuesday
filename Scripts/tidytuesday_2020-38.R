library(tidyverse)
library(gt)

kids <- tidytuesdayR::tt_load("2020-09-15")$kids


# TABLE DATA

state_regions <- setNames(c(as.character(state.region), "Northeast"), c(state.name, "District of Columbia"))

kids_tbl_data <- kids %>% 
  filter(variable == "PK12ed") %>%
  mutate(region = state_regions[state]) %>% 
  select(region, state, year, inf_adj_perchild) %>% 
  pivot_wider(names_from = year, values_from = inf_adj_perchild) %>%
  mutate(Trend = NA) 


# SPARKLINE

plotter <- function(data){
  data %>% 
    tibble(
      year = 1997:2016,
      value = data
    ) %>% 
    ggplot(aes(year, value)) +
    geom_line(size = 10, show.legend = FALSE) +
    theme_void() +
    scale_y_continuous(expand = c(0, 0))
}

spark_plots <- kids_tbl_data %>% 
  group_split(state) %>% 
  map(~ flatten_dbl(select(.x, where(is.numeric)))) %>% 
  map(plotter)


# TABLE

kids_tbl <- kids_tbl_data %>% 
  gt(
    groupname_col = 'region',
    rowname_col = 'state'
  ) %>% 
  fmt_number(
    columns = 3:22
  ) %>% 
  summary_rows(
    groups = TRUE,
    columns = 3:22,
    fns = list(Average = ~mean(.))
  ) %>% 
  text_transform(
    locations = cells_body(vars(Trend)),
    fn = function(x){
      map(spark_plots, ggplot_image, height = px(15), aspect_ratio = 4)
    }
  ) %>%
  tab_header(
    title = md("**State-by-State Spending on Primary and Secondary Education over 20 years**"),
    subtitle = md("*$1000s per child adjusted for inflation*")
  ) %>% 
  tab_source_note(
    md("**By**: @yjunechoe<br>
        **Inspiration**: @thomas_mock<br>
        **Data**: Urban Institute | {tidykids} by Joshua Rosenberg")
  ) %>% 
  tab_style(
    style = list(
      cell_text(font = "Futura MdCn BT")
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>%
  tab_options(
    table.width = 50,
    heading.align = "left",
    heading.title.font.size = 72,
    heading.subtitle.font.size = 32,
    row_group.font.size = 42,
    row_group.font.weight = 'bold',
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table.border.top.color = "black",
    heading.border.bottom.color = "white",
    heading.border.bottom.width = px(10),
    table.font.names = "Roboto",
    column_labels.font.size = 20,
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
    summary_row.border.color = "black", 
    summary_row.background.color = "#c0c5ce",
    table.border.bottom.color = "black"
  )



