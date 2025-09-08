#' Headwaters Hydrology Project Streamflow Prediction API Example
#' 
#' This function sends a GET request to the **Montana Climate Office’s (University of Montana) Streamflow API**  
#' to retrieve modeled streamflow predictions for specified locations and date ranges.  
#' The API supports multiple statistical aggregations and returns results in either CSV or JSON format.
#' 
#' @details
#' - The request retrieves **streamflow predictions** for specified **HUC-10 watersheds**.
#' - Users can define a **date range**, **aggregations**, and **units**.
#' - Results can be returned in **CSV format (default) for easier data handling**, but JSON is also available.
#' - By default, predictions are in **cubic feet per second (cfs)**, but results can be converted to **millimeters (mm)**.
#' - Multiple locations (HUC-10s) or **latitude/longitude pairs** can be queried simultaneously.
#' - Latitude/longitude-based requests return simulations corresponding to the **outlet of the HUC-10 basin** in which the coordinates fall.
#' - The API provides access to both **aggregated k-fold predictions** and **raw k-fold model outputs**.
#'
#' @param locations A comma-separated list of HUC-10 watershed IDs (e.g., `"0302010502,0208020106"`).
#' @param date_start The start date of the requested predictions (`"YYYY-MM-DD"` format).
#' @param date_end The end date of the requested predictions (`"YYYY-MM-DD"` format).
#' @param aggregations A comma-separated list of statistical aggregations. 
#'   Options: `"min,max,median,mean,stddev,iqr"` (default: `"median"`).
#' @param as_csv Logical (`TRUE/FALSE`) to return data in CSV format (default: `TRUE`).
#' @param latitude A comma-separated list of latitudes for spatial queries.
#' @param longitude A comma-separated list of longitudes for spatial queries.
#' @param units The unit for streamflow predictions. Options: `"cfs"` (default) or `"mm"`.
#' 
#' @return A response object from the API containing streamflow predictions.
#' 
#' @example

library(tidyverse)

# this example is for mutiple sites (HUC10 names) as well as lat lon pairs
# this example also returns all aggregations (mean, median, iqr, etc)
# all sites/aggregations will be returned in long format

request = httr::GET(
  # can replace this with /predictions/raw, the only query parameter that isn't shared is aggregations.
  "https://data.climate.umt.edu/streamflow-api/predictions/",
  query = list(
    locations="0302010502,0208020106",
    date_start="2023-01-01",
    date_end="2024-01-01",
    aggregations="min,max,median,mean,stddev,iqr", #These are all the options. If you don't specify, median is the default.
    as_csv=TRUE,
    latitude="40,41",
    longitude="-113,-113",
    units="cfs" # cfs is the default. Can also do mm.
  )
)

# look at url
print(request$url)

# get the csv
httr::content(request)

# request for headwaters of the headwaters of the south fork flathead (HUC10: 1701020902)
# this request is by lat lon 

request = httr::GET(
  # can replace this with /predictions/raw, the only query parameter that isn't shared is aggregations.
  "https://data.climate.umt.edu/streamflow-api/predictions/",
  query = list(
    date_start="1982-01-01",
    date_end="2025-07-01",
    aggregations="mean", #These are all the options. If you don't specify, median is the default.
    as_csv=TRUE,
    latitude="46.983945",
    longitude="-114.657650",
    units="cfs" # cfs is the default. Can also do mm.
  )
)

request = httr::GET(
  # can replace this with /predictions/raw, the only query parameter that isn't shared is aggregations.
  "https://data.climate.umt.edu/streamflow-api/predictions/",
  query = list(
    date_start="1982-01-01",
    date_end="2025-07-01",
    aggregations="mean", #These are all the options. If you don't specify, median is the default.
    as_csv=TRUE,
    locations = '1701021305',
    units="cfs" # cfs is the default. Can also do mm.
  )
)

data = httr::content(request)
print(data)

#' Generate Multi-Year Streamflow Percentile Plot
#' 
#' This function computes daily streamflow percentiles from historical data and extends them 
#' across multiple years for visualization. The analysis is based on a hydrologic 
#' drought/wet classification scheme. Users can define a climatology period to calculate percentiles 
#' and specify a separate plotting range for observed data.
#' 
#' @param data A dataframe containing at least `date` and `value` columns, where `date` is in `YYYY-MM-DD` format 
#' and `value` represents streamflow measurements.
#' @param site_id A string specifying the HUC-10 watershed ID for display in the plot title.
#' @param climatology_start_date The start date (in "YYYY-MM-DD" format) for the historical period used in percentile calculations (default: "1991-01-01").
#' @param climatology_end_date The end date (in "YYYY-MM-DD" format) for the historical period used in percentile calculations (default: "2020-12-31").
#' @param plot_start_date The start date for plotting observed data (default: "2023-01-01").
#' @param plot_end_date The end date for plotting observed data (default: "2024-01-01").
#' 
#' @return A `ggplot` object showing streamflow percentiles over multiple years.
#' 
#' @import tidyverse
#' @import lubridate
#' @import ggplot2
#' @import glue
#' 
#' @export
generate_streamflow_plot = function(data, site_id, 
                                     climatology_start_date = "1991-01-01",
                                     climatology_end_date = "2020-12-31",
                                     plot_start_date = "2023-01-01",
                                     plot_end_date = "2024-01-01") {
  
  # Ensure date is in proper format
  data = data %>%
    mutate(date = as.Date(date))
  
  # Compute daily percentiles based on historical streamflow data within the climatology period
  stats = data %>%
    filter(date >= as.Date(climatology_start_date) & date <= as.Date(climatology_end_date)) %>%
    mutate(yday = lubridate::yday(date)) %>%
    group_by(yday) %>%
    summarise(
      Median = quantile(value, 0.5, na.rm = TRUE),
      `30th Percentile` = quantile(value, 0.30, na.rm = TRUE),
      `20th Percentile` = quantile(value, 0.20, na.rm = TRUE),
      `10th Percentile` = quantile(value, 0.10, na.rm = TRUE),
      `5th Percentile` = quantile(value, 0.05, na.rm = TRUE),
      `2nd Percentile` = quantile(value, 0.02, na.rm = TRUE),
      `Minimum` = min(value, na.rm = TRUE),
      `70th Percentile` = quantile(value, 0.70, na.rm = TRUE),
      `80th Percentile` = quantile(value, 0.80, na.rm = TRUE),
      `90th Percentile` = quantile(value, 0.90, na.rm = TRUE),
      `95th Percentile` = quantile(value, 0.95, na.rm = TRUE),
      `98th Percentile` = quantile(value, 0.98, na.rm = TRUE),
      `Maximum` = max(value, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Generate a sequence of years to repeat the percentiles across multiple years
  years_to_plot = seq(year(as.Date(plot_start_date)), year(as.Date(plot_end_date)), by = 1)
  stats_expanded = map_dfr(years_to_plot, function(year) {
    stats %>%
      mutate(date = as.Date(glue::glue("{year}-01-01")) + yday - 1)
  })
  
  # Define legend order explicitly
  legend_order = c(
    "98th-Max (W4)", "95th-98th (W3)", "90th-95th (W2)", "80th-90th (W1)", "70th-80th (W0)",
    "30th-70th (Normal)",
    "20th-30th (D0)", "10th-20th (D1)", "5th-10th (D2)", "2nd-5th (D3)", "Min-2nd (D4)"
  )
  
  # Define color mapping
  fill_colors = c(
    "Min-2nd (D4)" = '#730000',
    "2nd-5th (D3)" = "#E60000",
    "5th-10th (D2)" = "#FFAA00",
    "10th-20th (D1)" = "#FCD37F",
    "20th-30th (D0)" = "#FFFF00",
    "30th-70th (Normal)" = 'white',
    "70th-80th (W0)" = '#aaf542',
    "80th-90th (W1)" = "#32E1FA",
    "90th-95th (W2)" = "#325CFE",
    "95th-98th (W3)" = "#4030E3",
    "98th-Max (W4)" = '#303B83'
  )
  
  # Generate plot with percentile ribbons
  plot = ggplot(stats_expanded, aes(x = date)) +
    geom_ribbon(aes(ymin = `Minimum`, ymax = `2nd Percentile`, fill = "Min-2nd (D4)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `2nd Percentile`, ymax = `5th Percentile`, fill = "2nd-5th (D3)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `5th Percentile`, ymax = `10th Percentile`, fill = "5th-10th (D2)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `10th Percentile`, ymax = `20th Percentile`, fill = "10th-20th (D1)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `20th Percentile`, ymax = `30th Percentile`, fill = "20th-30th (D0)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `30th Percentile`, ymax = `70th Percentile`, fill = "30th-70th (Normal)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `70th Percentile`, ymax = `80th Percentile`, fill = "70th-80th (W0)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `80th Percentile`, ymax = `90th Percentile`, fill = "80th-90th (W1)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `90th Percentile`, ymax = `95th Percentile`, fill = "90th-95th (W2)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `95th Percentile`, ymax = `98th Percentile`, fill = "95th-98th (W3)"), alpha = 0.8) +
    geom_ribbon(aes(ymin = `98th Percentile`, ymax = `Maximum`, fill = "98th-Max (W4)"), alpha = 0.8) +
    scale_fill_manual(values = fill_colors, breaks = legend_order, name = 'Streamflow\nPercentiles\n(Climatology)') +
    labs(
      x = NULL, 
      y = "Estimated Streamflow (CFS)", 
      title = glue::glue("Streamflow Percentiles for HUC10 ID: {site_id}")
    ) +
    theme_bw() +
    scale_y_log10(labels = scales::label_comma()) +
    theme(
      legend.text = element_text(size = 6),
      plot.title = element_text(hjust = 0.5)
    ) +
    geom_line(
      data = data %>% filter(date >= as.Date(plot_start_date) & date < as.Date(plot_end_date)), 
      aes(x = date, y = value), linewidth = 0.8
    )
  
  return(plot)
}

#one year example
generate_streamflow_plot(
  data = data, 
  site_id = data$location[1],
  climatology_start_date = '1991-01-01',
  climatology_end_date = '2020-12-31',
  plot_start_date = '2022-01-01',
  plot_end_date = '2022-12-31'
  )

#2 year example 
generate_streamflow_plot(
  data = data, 
  site_id = data$location[1],
  climatology_start_date = '1991-01-01',
  climatology_end_date = '2020-12-31',
  plot_start_date = '2021-01-01',
  plot_end_date = '2022-12-31'
)
