tuesdata <- tidytuesdayR::tt_load(2020, week = 46)

readRDS("tuesdata-2020-46.rds")

tuesdata$mobile %>% write_csv("mobile.csv")

tuesdata$landline %>% write_csv("landline.csv")
