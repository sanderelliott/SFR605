suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
})

# ---- paths to your downloaded USGS .txt files ----
f2024 <- '/Volumes/T7 Shield/ATS Piscat Data/2025/2025_WaterLevel/WENF/USGS_WENF_2024.txt'
f2025 <- '/Volumes/T7 Shield/ATS Piscat Data/2025/2025_WaterLevel/WENF/USGS_WENF_2025.txt'

# ---- helper to read USGS tab-delimited file with # comment header ----
read_usgs_txt <- function(path, year_label = NULL, tz_local = "America/New_York") {
  x <- read_tsv(
    path,
    comment = "#",        # skips all the header disclaimer lines
    show_col_types = FALSE,
    na = c("", "NA")
  )
  
  # Find the gage-height value column (usually like ######_00065)
  val_col <- names(x)[str_detect(names(x), "_00065$")]
  if (length(val_col) == 0) stop("Couldn't find a *_00065 gage-height column in: ", path)
  val_col <- val_col[1]
  
  out <- x %>%
    mutate(
      datetime = ymd_hm(datetime, tz = tz_local),
      gage_ft  = as.numeric(.data[[val_col]])
    ) %>%
    filter(!is.na(datetime), !is.na(gage_ft)) %>%
    mutate(
      year = if (!is.null(year_label)) year_label else year(datetime),
      doy  = yday(datetime),
      tod  = format(datetime, "%H:%M")
    ) %>%
    select(datetime, year, doy, gage_ft, everything())
  
  out
}

d24 <- read_usgs_txt(f2024, year_label = 2024)
d25 <- read_usgs_txt(f2025, year_label = 2025)

d_all <- d_all %>%
  mutate(
    frac_day = doy + (hour(datetime) +
                        minute(datetime)/60 +
                        second(datetime)/3600) / 24
  )

# ---- plot: overlay both years aligned by day-of-year ----
ggplot(d_all, aes(x = frac_day, y = gage_ft, color = factor(year))) +
  geom_line(linewidth = 0.4, alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 366, by = 30)) +
  labs(
    x = "Day of year",
    y = "Gage height (ft)",
    color = NULL,
    title = "USGS 01034500 Penobscot River at West Enfield — 2024 vs 2025"
  ) +
  theme_bw()
#########################################################
###########################
#overlay by calendar day
ggplot(d_all, aes(x = frac_day, y = gage_ft, color = factor(year))) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  scale_x_continuous(
    name = "Date",
    breaks = yday(seq.Date(as.Date("2024-04-01"),
                           as.Date("2024-11-01"),
                           by = "1 month")),
    labels = format(seq.Date(as.Date("2024-04-01"),
                             as.Date("2024-11-01"),
                             by = "1 month"), "%b %d")
  ) +
  labs(
    y = "Gage Height (ft)",
    color = NULL,
    title = "USGS 01034500 Penobscot River at West Enfield — 2024 vs 2025"
  ) +
  theme_bw()
############################################
#Zoom in on time window
ggplot(d_all, aes(x = frac_day, y = gage_ft, color = factor(year))) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  
  scale_x_continuous(
    name = "Date",
    breaks = yday(seq.Date(as.Date("2024-05-01"),
                           as.Date("2024-07-01"),
                           by = "2 weeks")),
    labels = format(seq.Date(as.Date("2024-05-01"),
                             as.Date("2024-07-01"),
                             by = "2 weeks"), "%b %d")
  ) +
  
  coord_cartesian(
    xlim = c(yday(as.Date("2024-05-01")),
             yday(as.Date("2024-07-01")))
  ) +
  
  labs(y = "Gage height (ft)", color = NULL,
       title = "PR at West Enfield gage height — May 1 to July 1 (2024 vs 2025)") +
  theme_bw()