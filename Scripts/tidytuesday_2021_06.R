# Tweet: https://twitter.com/yjunechoe/status/1357769847980007425
# Making of: 

library(ggplot2)     # CRAN v3.3.3
library(dplyr)       # CRAN v1.0.4
library(tidyr)       # CRAN v1.1.2
library(purrr)       # CRAN v0.3.4
library(extrafont)   # CRAN v0.17
library(ggtext)      # CRAN v0.1.1
library(ggfittext)   # CRAN v0.9.0
library(ggrepel)     # CRAN v0.9.1
library(glue)        # CRAN v1.4.2
library(magick)      # CRAN v2.6.0
library(colorspace)  # CRAN v2.0-0
library(gganimate)   # CRAN v1.0.7 

set.seed(2021)

##
## Data
##

tuesdata <- tidytuesdayR::tt_load(2021, week = 6)

# Cleaning script by @alexcookson - https://twitter.com/alexcookson/status/1356628106790985728
source("https://gist.githubusercontent.com/tacookson/b51b707ab2956f143562ae09670db45c/raw/e3648519771341cc7d145fade9a3c9c3d1bac084/clean-hbcu-enrollment")

hbcu_totals <- hbcu_all %>% 
  select(year, total_enrollment) %>% 
  left_join(tibble(year = seq(!!!range(.$year))), ., by = "year") %>% 
  mutate(
    year = as.integer(year),
    fill = ifelse(is.na(total_enrollment), "grey50", sequential_hcl(1, palette = "Purple"))
  ) %>% 
  fill(total_enrollment, .direction = "down") 


hbcu_by_program <- hbcu_by_program %>% 
  unite("type", program_length:public_private) %>% 
  mutate(
    type = str_replace_all(type, "_", " "),
    type = str_replace_all(type, " years", "-year"),
    type = fct_reorder(type, students, mean)
  ) %>% 
  arrange(desc(type))

hbcu_by_gender <- hbcu_by_gender %>% 
  mutate(
    gender = str_to_title(gender),
    gender = factor(gender, levels = c("Female", "Male"))
  )


##
## Colors
##

program_palette <- setNames(c(qualitative_hcl(4, palette = "Dark 3")[-4], "skyblue"), levels(hbcu_by_program$type))
gender_palette <- setNames(diverging_hcl(2, "Green-Brown"), levels(hbcu_by_gender$gender))

hbcu_by_program <- hbcu_by_program %>% 
  mutate(fill = program_palette[type])

hbcu_by_gender <- hbcu_by_gender %>% 
  mutate(fill = gender_palette[gender])

strong_color <- function(text, color) {
  glue("<strong style='color:{color}'>{text}</strong>")
}

##
## Isolate important events to annotate
##

program_makeup_initial <- hbcu_by_program %>% 
  filter(year == 1976)

gender_makeup_initial <- hbcu_by_gender %>% 
  filter(year == 1976)

gender_ratio_1.5 <- hbcu_by_gender %>% 
  count(year, gender, wt = students, name = "students") %>% 
  group_by(year) %>% 
  mutate(ratio = students[gender == "Female"]/students) %>% 
  filter(any(ratio > 1.5)) %>% 
  ungroup() %>% 
  slice_min(year) %>% 
  mutate(fill = gender_palette[gender])

nonblack_students_20pct <- hbcu_totals %>% 
  inner_join(
    tuesdata$hbcu_black %>% 
      select(year = Year, black_students = `Total enrollment`),
    by = "year"
  ) %>% 
  mutate(pct = black_students/total_enrollment) %>% 
  filter(pct < 0.8) %>% 
  slice_min(year, n = 1) %>% 
  mutate(nonblack_students = total_enrollment - black_students) %>%
  select(-total_enrollment, -pct) %>% 
  pivot_longer(c(nonblack_students, black_students)) %>% 
  mutate(
    fill = ifelse(name == "nonblack_students", lighten(fill, .3), fill),
    name = factor(name, levels = c("nonblack_students", "black_students"))
  )

##
## Animation df
##

# Accumulate data by year
animation_df_nested <- tibble(frame_state = 1:nrow(hbcu_totals)) %>%
  mutate(
    year = hbcu_totals$year,
    base_data = slider::slide(frame_state, ~ hbcu_totals[1:.x,])
  )

# Manually specify where/how long to pause for annotations
pauses <- tribble(
  ~year, ~duration,
  1976, 40, # 1976 - introduction, program makeup, gender makeup
  1979, 8,  # 1979 - note about missing data
  1982, 10, # 1982 - 20% of students enrolled in HBCUs are non-Black
  1997, 20, # 1997 - 1.5 times more female students than male students
  2010, 25, # 2010 - peak enrollment
  2015, 10  # 2015 - final pause
)

animation_df <- animation_df_nested %>% 
  # Imputed years are half as long as years with reported data
  mutate(nframes = ifelse(hbcu_totals$fill == "grey50", 1, 2)) %>% 
  # Allocate space (inject pauses) for annotations
  mutate(nframes = replace(
    x = nframes,
    list = which(.$year %in% pauses$year),
    values = pauses$duration)
  ) %>% 
  # Pauses are translated to data that is repeated across time
  uncount(nframes) %>% 
  # `frame_time` is the reference variable, used in `transition_states`
  mutate(frame_time = as.integer(row_number())) %>%
  select(-year) %>% 
  unnest(base_data)

frame_year_ends <- animation_df %>% 
  group_by(frame_time) %>% 
  summarize(end_year = max(year))  



##
## Animation plot
##

animation_plot <- animation_df %>% 
  ggplot(aes(year, total_enrollment)) +
  geom_col(
    aes(fill = fill),
    color = 'grey50',
    size = 1
  ) +
  scale_fill_identity(guide = guide_none()) +
  scale_color_identity(guide = guide_none()) +
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0.05, .15))
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(5)
  ) +
  labs(
    title = "Enrollment in Historically Black Colleges and Universities <span style='font-size:24pt'>1976—2015</span>",
    y = NULL,
    x = NULL
  ) +
  theme(
    text = element_text(color = "white", family = "Barlow"),
    axis.ticks.length.y = unit(.2, 'cm'),
    axis.ticks.y = element_line(color = "white", size = 1),
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 20, margin = margin(r = .2, unit = "cm")),
    axis.text.x = element_text(size = 28, face = 'bold', margin = margin(t = .7, unit = "cm")),
    panel.background = element_rect(fill = "grey20"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "grey20"),
    plot.margin = margin(1, 1, 1, 1, unit = 'cm'),
    plot.title = element_markdown(
      size = 36,
      face = 'bold',
      family = "Trispace CondensedMedium",
      margin = margin(b = .5, unit = "cm")
    ),
    plot.title.position = 'plot'
  ) +
  
  ##########################
  ## Animation components ##
  ##########################

  # A dummy transparent geom so that `view_follow()` initializes with sensible defaults
  geom_point(aes(x = 1979.5, y = 3.1e5), color = NA, data = tibble(frame_time = 1:max(animation_df$frame_time))) +
  
  ##
  ## 1. Introduction
  ##
  
  # Note that you don't need `stat = 'identity'` because there's no overlap _within_ a `frame_time``
  geom_richtext(
    aes(
      x = 1976, y = 3.1e5,
      label = glue("In 1976, over {strong_color('220 thousand', '#8C81D6')} students were enrolled in historically<br>Black colleges and universities (HBCU)")
    ),
    size = 8,
    color = "white",
    family = "IBM Plex Mono",
    hjust = 0,
    position = position_nudge(x = -.5),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    data = tibble(frame_time = 1:50)
  ) +
  
  ##
  ## 2. Program makeup in 1976
  ##
  
  geom_col(
    aes(y = students, group = type, fill = fill),
    color = 'black',
    size = 1,
    data = program_makeup_initial %>% 
      uncount(20, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + 10L)
  ) +
  geom_richtext(
    aes(x = 1976, y = y, label = label),
    size = 7,
    color = 'white',
    family = "IBM Plex Mono",
    hjust = 0,
    position = position_nudge(x = .68),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    data = program_makeup_initial %>% 
      mutate(
        y = cumsum(students) - students/2,
        y = replace(y, which(type == "2-year private"), 2.4e5),
        label = glue("{type} (<strong style='color:{program_palette[type]}'>{scales::percent(students/sum(students))}</strong>)")
      ) %>% 
      uncount(20, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + 10L)
  ) +
  geom_segment(
    aes(x = 1976.5, xend = 1976.6, y = y, yend = yend),
    size = .2,
    color = 'white',
    data = program_makeup_initial %>% 
      mutate(
        y = cumsum(students) - students/2,
        yend = ifelse(type == "2-year private", 2.4e5, y)
      ) %>% 
      uncount(20, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + 10L)
  ) +
  
  
  ##
  ## 3. Gender makeup in 1976
  ##
  
  geom_col(
    aes(y = students, group = gender, fill = fill),
    color = 'black',
    size = 1,
    data = gender_makeup_initial %>% 
      uncount(10, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + 30L)
  ) + 
  geom_bar_text(
    aes(y = students, label = label),
    size = 30,
    family = "IBM Plex Mono",
    lineheight = 1.1,
    color = "white",
    position = "stack",
    place = "center",
    data = gender_makeup_initial %>% 
      mutate(label = glue("{gender}\n({scales::percent(students/sum(students))})")) %>% 
      uncount(10, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + 30L)
  ) +


  ##
  ## 3. Note on missing data - data on some years are missing. Enrollment numbers are carried over for the purposes of visualization
  ##

  geom_text(
    aes(
      x = 1977, y = 2.55e5,
      label = "Enrollment numbers are carried over for years with missing data"
    ),
    size = 5.5,
    family = "Roboto",
    hjust = 0,
    color = "white",
    data = tibble(frame_time = 41:50)
  ) +
  
  
  ##
  ## 4. In 1982, 20% HCBU students were non-Black
  ##
  
  geom_col(
    aes(y = value, group = name, fill = fill, color = color),
    size = 2,
    data = nonblack_students_20pct %>% 
      mutate(color = ifelse(name == "black_students", "grey50", heat_hcl(1))) %>% 
      arrange(desc(name)) %>% 
      uncount(10, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + min(filter(frame_year_ends, end_year == 1982)$frame_time) - 1L)
  ) +
  geom_richtext(
    aes(
      x = 1976, y = 3.1e5,
      label = glue("In 1982, {strong_color('20%', '#8C81D6')} of students enrolled in HBCUs were {strong_color('non-Black', '#8C81D6')}")
    ),
    size = 8,
    color = "white",
    family = "IBM Plex Mono",
    hjust = 0,
    position = position_nudge(x = -.5),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    data = tibble(frame_time = min(filter(frame_year_ends, end_year == 1982)$frame_time):max(filter(frame_year_ends, end_year == 1982)$frame_time))
  ) +
  geom_segment(
    aes(x = 1981.4, xend = 1982, y = 3e5, yend = 2e5),
    size = .2,
    color = 'white',
    data = tibble(frame_time = min(filter(frame_year_ends, end_year == 1982)$frame_time):max(filter(frame_year_ends, end_year == 1982)$frame_time))
  ) +
  
  ##
  ## 5. By 1997, there are 50% more female students than male students attending in HCBUs
  ##
  
  geom_point(aes(x = 2000.5, y = 3.25e5), color = NA, data = tibble(frame_time = 64:max(animation_df$frame_time))) +

  geom_col(
    aes(y = students, group = gender, fill = fill),
    color = 'black',
    size = 1,
    data =  gender_ratio_1.5 %>% 
      uncount(20 - 3L, .id = "frame_time") %>% # add 3-frame lag to fill
      mutate(frame_time = frame_time + min(filter(frame_year_ends, end_year == 1997)$frame_time) - 1L + 3L)
  ) +
  geom_richtext(
    aes(
      x = 1976, y = 3.25e5,
      label = glue("By 1997, there were {strong_color('50% more female students', lighten(gender_palette['Female'], .2))} than {strong_color('male students', lighten(gender_palette['Male'], .2))}.<br>This ratio stays relatively constant for the next two decades.")
    ),
    size = 7,
    color = "white",
    family = "IBM Plex Mono",
    hjust = 0,
    lineheight = 1.1,
    position = position_nudge(x = -.5),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    data = tibble(frame_time = (min(filter(frame_year_ends, end_year == 1997)$frame_time) + 3):max(filter(frame_year_ends, end_year == 1997)$frame_time))
  ) +
  
  ##
  ## 6. Peak enrollment in 2010
  ##
  
  geom_point(aes(x = 2016.5, y = 3.4e5), color = NA, data = tibble(frame_time = 108:(max(animation_df$frame_time) + 40L))) +
  
  geom_hline(
    aes(yintercept = max(hbcu_all$total_enrollment) + 1e3),
    size = 1,
    color = 'white',
    linetype = 2,
    data = tibble(frame_time = (min(filter(frame_year_ends, end_year == 2010)$frame_time) + 3):max(filter(frame_year_ends, end_year == 2010)$frame_time))
  ) +
  geom_richtext(
    aes(
      x = 1976, y = 3.4e5,
      label = glue("Peak enrollment of over {strong_color('320 thousand', '#8C81D6')} students in 2010")
    ),
    size = 8,
    color = "white",
    family = "IBM Plex Mono",
    hjust = 0,
    lineheight = 1.1,
    position = position_nudge(x = -.5),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), 'pt'),
    data = tibble(frame_time = (min(filter(frame_year_ends, end_year == 2010)$frame_time) + 3):max(filter(frame_year_ends, end_year == 2010)$frame_time))
  ) +
  
  ##
  ## 7. Final show of program and gender make up
  ##

  # missing
  geom_col(
    fill = "grey50",
    color = 'grey50',
    size = 1,
    data = animation_df %>% 
      select(year, total_enrollment) %>% 
      distinct() %>% 
      uncount(40, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + max(animation_df$frame_time))
  ) +
  # program
  geom_col(
    aes(y = students, group = type, fill = fill),
    color = 'black',
    size = .3,
    data = hbcu_by_program %>% 
      uncount(20, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + max(animation_df$frame_time))
  ) +
  # gender
  geom_col(
    aes(y = students, group = gender, fill = fill),
    color = 'black',
    data = hbcu_by_gender %>% 
      uncount(20, .id = "frame_time") %>% 
      mutate(frame_time = frame_time + max(animation_df$frame_time) + 20L)
  )

  # # Hacky legend tricks with `override.aes` (doesn't work but might come back to this)
  # scale_alpha_manual(
  #   values = rep(1, 4),
  #   guide = guide_legend(
  #     title = 'Program Type',
  #     title.theme = element_text(
  #       family = "Barlow",
  #       size = 24,
  #       face = 'bold',
  #       color = 'white'
  #     ),
  #     label.theme = element_text(
  #       family = "Barlow",
  #       size = 18, color = 'white'),
  #     override.aes = list(fill = program_palette)
  #   )
  # ) +
  # scale_size_manual(
  #   values = rep(.3, 2),
  #   guide = guide_legend(
  #     title = 'Gender',
  #     title.theme = element_text(
  #       family = "Barlow",
  #       size = 24,
  #       face = 'bold',
  #       color = 'white'
  #     ),
  #     label.theme = element_text(
  #       family = "Barlow",
  #       size = 18, color = 'white'
  #     ),
  #     override.aes = list(fill = gender_palette)
  #   )
  # ) +
  # theme(
  #   legend.background = element_rect(fill = NA),
  #   legend.position = c(.1, .85),
  #   legend.key = element_rect(fill = NA),
  #   legend.key.size = unit(1, 'cm')
  # )


##
## Render and save
##

animate(
  animation_plot +
    transition_states(frame_time) +
    view_follow(),
  device = 'png',
  fps = 12, nframes = 500, start_pause = 4, end_pause = 8,
  width = 16, height = 10, units = 'in', res = 100, type = 'cairo-png'
)

#save
anim_save("tidytuesday_2021_06.gif") 
 
