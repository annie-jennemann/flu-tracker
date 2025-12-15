### libraries

library(tidyverse)
library(cdcfluview)
library(DatawRappr)

### Load in new data

recent_flu <- ili_weekly_activity_indicators() %>% 
  filter(season == "2025-26") %>% 
  filter(weekend == max(weekend)) %>% 
  filter(statename != "Commonwealth of the Northern Mariana Islands") %>% 
  filter(statename != "Puerto Rico") %>% 
  filter(statename != "Virgin Islands") %>% 
  filter(statename != "New York City") %>%
  select(statename, activity_level, activity_level_label, weekend, season)

### Pull prior week values

last_week_flu <- ili_weekly_activity_indicators() %>% 
  filter(season == "2025-26") %>% 
  filter(weekend != max(weekend)) %>% 
  filter(weekend == max(weekend)) %>%
  filter(statename != "Commonwealth of the Northern Mariana Islands") %>% 
  filter(statename != "Puerto Rico") %>% 
  filter(statename != "Virgin Islands") %>% 
  filter(statename != "New York City") %>%
  select(statename, activity_level, activity_level_label, weekend, season) %>%
  rename(activity_level_label_last_week = activity_level_label) %>%
  rename(activity_level_last_week = activity_level) %>%
  rename(last_weekend = weekend)

## Load in API key, chart keys and data keys

api_key <- Sys.getenv("API_KEY")
flu_map <- Sys.getenv("CHART_CODE1")
flu_table <- Sys.getenv("CHART_CODE2")

datawrapper_auth(api_key =  api_key, overwrite=TRUE)

### Getting dates

date <- recent_flu %>% filter(statename == "Alaska") %>% select(weekend) %>% mutate(weekend = gsub("(\\D)0", "\\1", format(weekend, format = "%B %d")))

last_week <- last_week_flu %>% filter(statename == "Alaska") %>% select(last_weekend) %>% mutate(last_weekend = gsub("(\\D)0", "\\1", format(last_weekend, format = "%B -%d")))

### data to map

dw_data_to_chart(recent_flu, flu_map)

### Put data in map

dw_edit_chart(
  flu_map,
  title = paste("Flu activity levels by state for the week ending ",date),
  intro = paste('
<b style="color:#469733; vertical-align:text-top;;">⬤</b> Minimal&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<b style="color:#fcd259; vertical-align:text-top;">⬤</b> Low&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<b style="color:#d18900; vertical-align:text-top;">⬤</b> Moderate&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<b style="color:#cf3d34; vertical-align:text-top;">⬤</b> High&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<b style="color:#580001; vertical-align:text-top;">⬤</b> Very high&nbsp;&nbsp;&nbsp;&nbsp;
</span>'),
  annotate = paste("Data for week ending ",date,"."),
  visualize = list(
    group = "cases_range",
    "map-color-group" = "cases_range",
    colorscale = list(
      enabled = TRUE,
      map = list(
        "Minimal" = "#469733",
        "Low" = "#fcd259",
        "Moderate" = "#d18900",
        "High" = "#cf3d34",
        "Very high" = "#580001"
      )
    )
  )
)

dw_publish_chart(flu_map)

### Table

### Join

table_flu <- recent_flu %>% inner_join(last_week_flu, by = c("statename", "season")) %>% select(statename, activity_level_label_last_week, activity_level_label, activity_level) 

### rename column

table_flu <- table_flu %>% rename(State = statename)  %>%
  rename("Week prior" = activity_level_label_last_week) %>%
  rename("Most recent" = activity_level_label)

### select just columns we want in the correct order

dw_data_to_chart(table_flu, flu_table)

### Format dates

this_date <- as.character(date[[1]])

last_date <- as.character(last_week[[1]])

### Summaries for chatter

count_minimal <- table_flu %>% filter(`Most recent` == "Minimal") %>% summarise(count_minimal = n())

count_low <- table_flu %>% filter(`Most recent` == "Low") %>% summarise(count_low = n())

count_moderate <- table_flu %>% filter(`Most recent` == "Moderate") %>% summarise(count_moderate = n())

count_high <- table_flu %>% filter(`Most recent` == "High") %>% summarise(count_high = n())

count_veryhigh <- table_flu %>% filter(`Most recent` == "Very high") %>% summarise(count_veryhigh = n())

### Push to chart

common_colors <- list(
  "Minimal"   = "#469733",
  "Low"       = "#fcd259",
  "Moderate"  = "#d18900",
  "High"      = "#cf3d34",
  "Very high" = "#580001"
)

dw_edit_chart(
  flu_table,
  title = paste0("Search flu activity levels by state as of ", this_date),
  intro = paste0("There were ",count_high," states with high and ",count_veryhigh, " states with very high flu activity."),
  annotate = paste("Data for week ending ",date,"."),
  visualize = list(
    columns = list(
      "Week prior" = list(
        customColor = TRUE,
        customColorBy = "Week prior",
        "custom-color-by" = "Week prior",
        customColorBackground = common_colors,
        "custom-color-background" = common_colors
      ),
      "Most recent" = list(
        customColor = TRUE,
        customColorBy = "Most recent",
        "custom-color-by" = "Most recent",
        customColorBackground = common_colors,
        "custom-color-background" = common_colors
      )
    )
  )
)

dw_publish_chart(flu_table)
