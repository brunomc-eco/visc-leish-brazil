# Exploring visceral leishmaniasis cases from DATASUS FTP

library(read.dbc)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(ggplot2)
library(ISOweek)


# Download data from FTP --------------------------------------------------

# create vectors of URLs and file paths
lv_url <- sprintf(paste0("ftp://ftp.datasus.gov.br/dissemin/",
                          "publicos/SINAN/DADOS/FINAIS/",
                          "LEIVBR%02d.dbc"), 1:24)
lv_fp <- sprintf("./LEIVBR%02d.DBC", 1:24)

download.file(lv_url, destfile = lv_fp, mode = "wb", method = "libcurl")

filenames <- list.files(path = "./data/leivbr/", full.names = TRUE)


# Fix 2001-2006 data ----------------------------------------------------

vars1 <- c("CON_CLASSI", # confirmed leish case
          "ID_MUNICIP", # municipality of notification
          "ID_MN_RESI", # municipality of residence
          "DT_NOTIFIC") # date of notification

filenames1 <- list.files(path = "./data/leivbr/", full.names = TRUE)[1:6]

lv1 <- list()
for(i in 1:length(filenames1)){
  temp <- read.dbc(filenames1[i])
  lv1[[i]] <- dplyr::select(temp, all_of(vars1)) %>%  
    rename("CLASSI_FIN" = "CON_CLASSI") %>% 
    mutate(TPAUTOCTO = NA)
  rm(temp)
}


# Merge data --------------------------------------------------------------

vars2 <- c(
  "CLASSI_FIN", # classificacao final, 1 = confirmado, 0 = descartado
  "ID_MUNICIP", # municipality of notification
  "ID_MN_RESI", # municipality of residence
  "DT_NOTIFIC", # date of notification
  "TPAUTOCTO" # autoctone mun res? 1=sim, 2=nao, 3=indeterminado
)

filenames2 <- list.files(path = "./data/leivbr/", full.names = TRUE)[7:24]

lv2 <- list()
for(i in 1:length(filenames2)){
  temp <- read.dbc(filenames2[i])
  lv2[[i]] <- dplyr::select(temp, all_of(vars2))
  rm(temp)
}

lv2 <- rbindlist(lv2)

lv <- bind_rows(lv1, lv2)


# Select municipality for plotting ----------------------------------------

codmun = "170210"
mun = "Araguaína - TO"

codmun = "330630"
mun = "Volta Redonda - RJ"


# Plot timeseries by epiweek ----------------------------------------------

ts_muni <- lv %>%
  filter(CLASSI_FIN == 1,
         ID_MN_RESI == codmun,
         !is.na(DT_NOTIFIC)) %>%
  
  mutate(
    # epidemiological year + week from date
    iso_year = isoyear(DT_NOTIFIC),
    iso_week = isoweek(DT_NOTIFIC),
    
    # Monday of epidemiological week
    date = ISOweek2date(sprintf("%d-W%02d-1", iso_year, iso_week))
  ) %>%
  count(date, name = "cases") %>%
  complete(date = seq(min(date), max(date), by = "week"),
           fill = list(cases = 0)) %>%
  arrange(date)

ggplot(ts_muni, aes(x = date, y = cases)) +
  geom_line(linewidth = 0.8, color = "#0072B2") +
  geom_point(size = 1.2, color = "#0072B2") +
  labs(
    title = "VL cases by epidemiological week",
    subtitle = mun,
    x = "Year",
    y = "Number of cases"
  ) +
  scale_x_date(
    date_breaks = "1 week",
    labels = function(x) {
      ifelse(lubridate::isoweek(x) == 1,
             lubridate::isoyear(x),
             "")
    }
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 9),
    panel.grid.minor.x = element_blank()
  )


# Plot timeseries by month ------------------------------------------------

vr <- lv %>%
  filter(#CLASSI_FIN == 1,
    ID_MN_RESI == codmun)

ts_muni_month <- lv %>%
  filter(#CLASSI_FIN == 1,
         ID_MN_RESI == codmun,
         !is.na(DT_NOTIFIC)) %>%
  
  mutate(
    month = floor_date(DT_NOTIFIC, unit = "month")
  ) %>%
  count(month, name = "cases") %>%
  complete(month = seq(min(month), max(month), by = "month"),
           fill = list(cases = 0)) %>%
  arrange(month)

ggplot(ts_muni_month, aes(x = month, y = cases)) +
  geom_line(linewidth = 0.9, color = "#0072B2") +
  geom_point(size = 1.8, color = "#0072B2") +
  labs(
    title = "VL cases by month",
    subtitle = mun,
    x = "Year",
    y = "Number of cases"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 12)+ 
  geom_smooth(se = FALSE, span = 0.25, color = "red")

