# SETUP -------------------------------------------------------------------

# * Libraries -------------------------------------------------------------
library(shiny)
library(shinytitle) # for flashing tab title
# library(shinyjs)
# CRAN version only supports v4, while github version supports v5
# remotes::install_github("rstudio/bslib")
library(bslib)
library(ggplot2)
library(plotly)
library(gginnards)
library(zoo)
library(tidyverse)
library(magrittr)
library(reactlog)
reactlog::reactlog_enable() # ctrl + Fn3
library(scales)
library(openxlsx)
library(rsconnect)
library(glue)
library(rclipboard)
library(fontawesome)

# * Other Setup -----------------------------------------------------------

# manually set dates for all forecast sources (safe assumption is prior month)
SPG_date <- "Nov 2024"
moody_date <- "Nov 2024"
bc_date <- "Nov 2024"

# bring in UEC responses date
uec_date <- readRDS("uec_date.rds")

year <- year(Sys.Date())

last_updated <- "December 3, 2024"
# last_updated <- format(today(), "%B %e, %Y") # I think this changes daily when the app is re-run

# set years of forecast interest from current year to two years out
forecast_years <- c(as.character(year),
                    as.character(year + 1),
                    as.character(year + 2)
                    )

# for formatting individual tables for easy forecast table construction
format_table <- function(source, table)
{
  # determine which column contains the forecast
  forecast_column <- names(table)[grep("Forecast", names(table))]
  # if "Forecast" wasn't detected...
  if(length(forecast_column) == 0) {forecast_column <- names(table)[grep("Change", names(table))]}
  
  # RAWG tables don't have a "Forecast" or "Change" column
  if(grepl("RAWG", source)){forecast_column <- source}
  
  table %<>%
    filter(`Calendar Year` %in% forecast_years) %>% # filter to current and next year
    select(`Calendar Year`, forecast_column) %>% # select year and forecast columns
    rename({{source}} := forecast_column) # rename forecast column to identify source

  table <- as.data.frame(t(table)) # transpose data frame
  colnames(table) <- table[1, ] # rename columns
  table <- rownames_to_column(table, var = "Source") %>% as_tibble() # format source column
  table %<>% slice(-1)
  
  return(table)
  
}

# RECESSION ---------------------------------------------------------------
recessions <- readRDS("recession/recessions.rds")
rec_indicators <- readRDS("recession/rec_indicators.rds")

start_date <- readRDS("recession/start_date.rds")

recession_date_comparison <- recessions %>%
  filter(Recession == 1) %>%
  summarize(latest_date = max(Date)) %>%
  pull()

# *** Recession Plot ------------------------------------------------------
recession_plot <- ggplot(rec_indicators) +
  geom_line(data = rec_indicators,
            aes(x = Date,
                y = value,
                color = indicator,
                group = indicator,
                text = paste0(as.yearqtr(Date), "\n", indicator, ": ", round(value, 1))
            ),
            size = 0.75
  ) +
  labs(title = paste0("National Recession Indicators (", as.yearqtr(recession_date_comparison), " = 100)"),
       x = NULL,
       y = "Index Level",
       color = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45,
                                   face = "bold"),
        axis.line = element_line(color = "black"),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0)) +
  guides(color = guide_legend(# title = NULL, this is taken care of in labs
    title.position = "top",
    title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "gold",
                                "green3",
                                "red2",
                                "blue")
  )

initial_status = 0
add_rect = FALSE
num_rectangles = 0

for(index in 1:nrow(recessions))
{
  # start date of a recession
  if(initial_status == 0 & recessions$Recession[index] == 1)
  {
    beg = recessions$Date[index]
  }
  
  # end date of a recession
  if(initial_status == 1 & recessions$Recession[index] == 0)
  {
    end <- recessions$Date[index - 1]
    
    # this is very necessary to create a reference point within the loop
    # so that the plot will remember all of the additions to render once it exits
    recession_df <- data.frame(beg, end)
    
    add_rect = TRUE
  }
  
  # handle most recent date being in recession
  if(index == nrow(recessions) & recessions$Recession[index] == 1)
  {
    end <- recessions$Date[index]
    recession_df <- data.frame(beg, end)
    
    # finish up last recession block
    add_rect = TRUE
  }
  
  if(add_rect)
  {
    num_rectangles = num_rectangles + 1
    
    recession_plot <- recession_plot +
      geom_rect(data = recession_df,
                aes(xmin=as.Date(beg),
                    xmax = as.Date(end),
                    ymin = 0, # plotly doesn't support y-axis infinite recession bars
                    ymax = 1.05 * max(rec_indicators$value, na.rm = TRUE)), # Inf isn't supported by plotly
                fill = "gray",
                alpha = 0.375,
                text = NULL
      ) +
      scale_y_continuous(expand = c(0, 0))
    
    add_rect = FALSE
  }
  
  # update last date's status every loop
  initial_status = recessions$Recession[index]
  
}

# move recessions to back
recession_plot <- move_layers(recession_plot, "GeomLine", position = "top")

# print(recession_plot)



# manually adjusting the legend orientation
recession_plot <- ggplotly(recession_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = rel(-0.25)),
         yaxis = list(title = list(standoff = 25)))

# gets rid of the automatic "trace {i}" tooltip for recessions
for(i in 1:num_rectangles)
{
  recession_plot$x$data[[i]]$hoverinfo = "none"
}

# print(recession_plot)

# CPI ---------------------------------------------------------------------
all_cpi <- readRDS("cpi/all_cpi.rds")

# *** CPI Plot ------------------------------------------------------------
cpi_plot <- ggplot(data = all_cpi, mapping = aes(Date, change)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "CPI Quarterly YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0)) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "blue",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(cpi_plot)

cpi_plot <- ggplotly(cpi_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(cpi_plot)


# *** CPI Forecast Table --------------------------------------------------
spg_cpia_table <- readRDS("cpi/spg_cpia_table.rds")
spg_cpi_forecasts <- format_table(paste("S&P Global", SPG_date), spg_cpia_table)



moody_cpia_table <- readRDS("cpi/moody_cpia_table.rds")
moody_cpi_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_cpia_table)

cpi_forecasts <- rbind(spg_cpi_forecasts,
                       moody_cpi_forecasts)

# declutter
rm(spg_cpi_forecasts,
   moody_cpi_forecasts)

blue_chip_cpi <- readRDS("cpi/blue_chip_cpi.rds")
if(!is.null(blue_chip_cpi)) # is null if indicated at top of other script
{
    blue_chip_cpi_forecasts <- format_table(paste("Blue Chip", bc_date), blue_chip_cpi)
    cpi_forecasts <- rbind(cpi_forecasts, blue_chip_cpi_forecasts)
}
# declutter
rm(blue_chip_cpi,
   blue_chip_cpi_forecasts)

rawg_cpi_table <- readRDS("cpi/rawg_cpi_table.rds")
colname <- colnames(rawg_cpi_table)[2] # to send column identification through function as "source"
rawg_cpi_forecasts <- format_table(colname, rawg_cpi_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_cpi_forecasts)))
{
  rawg_cpi_forecasts %<>% mutate({{final_year}} := NA)
}

uec_cpi_table <- readRDS("cpi/uec_cpi_table.rds")
uec_cpi_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_cpi_table)

cpi_forecasts <- rbind(cpi_forecasts,
                       rawg_cpi_forecasts,
                       uec_cpi_forecasts)

# declutter
rm(rawg_cpi_forecasts,
   uec_cpi_forecasts)

# US UR -------------------------------------------------------------------
all_usur <- readRDS("usur/all_usur.rds")

# *** US UR Plot ----------------------------------------------------------------
usur_plot <- ggplot(data = all_usur, mapping = aes(Date, value)) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((value), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "US Unemployment Rate",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0)) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "blue",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(usur_plot)

usur_plot <- ggplotly(usur_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(usur_plot)

# *** US UR Forecast Table ------------------------------------------------
spg_usura_table <- readRDS("usur/spg_usura_table.rds")
spg_usur_forecasts <- format_table(paste("S&P Global", SPG_date), spg_usura_table)

moody_usura_table <- readRDS("usur/moody_usura_table.rds")
moody_usur_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_usura_table)

usur_forecasts <- rbind(spg_usur_forecasts,
                       moody_usur_forecasts)

# declutter
rm(spg_usur_forecasts,
   moody_usur_forecasts)

blue_chip_usur <- readRDS("usur/blue_chip_usur.rds")
if(!is.null(blue_chip_usur)) # is null if indicated at top of other script
{
  blue_chip_usur_forecasts <- format_table(paste("Blue Chip", bc_date), blue_chip_usur)
  usur_forecasts <- rbind(usur_forecasts, blue_chip_usur_forecasts)
}

# declutter
rm(blue_chip_usur,
   blue_chip_usur_forecasts)

rawg_usur_table <- readRDS("usur/rawg_usur_table.rds")
colname <- colnames(rawg_usur_table)[2] # to send column identification through function as "source"
rawg_usur_forecasts <- format_table(colname, rawg_usur_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_usur_forecasts)))
{
  rawg_usur_forecasts %<>% mutate({{final_year}} := NA)
}

uec_usur_table <- readRDS("usur/uec_usur_table.rds")
uec_usur_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                   uec_usur_table)

usur_forecasts <- rbind(usur_forecasts,
                        rawg_usur_forecasts,
                        uec_usur_forecasts)

# declutter
rm(rawg_usur_forecasts,
   uec_usur_forecasts)

# GDP ---------------------------------------------------------------------
all_gdp <- readRDS("gdp/all_gdp.rds")

# *** GDP History Plot ----------------------------------------------------
gdp_hist <- all_gdp %>% filter(source == "GDP Actual % Change") %>% select(-source)

gdp_hist_plot <- ggplot(data = gdp_hist, mapping = aes(Date, gdp)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(as.yearqtr(Date), "\n",
                                        "GDP: $", round((gdp / 1000000000000), 2), " Trillion")
            )
  ) +
  labs(title = "US Real GDP Actual",
       x = NULL,
       y = "USD (Trillions)",
       color = NULL) +
  ylim(5, NA) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  expand_limits(y = c(5000000000000, NA)) +
  scale_y_continuous(breaks = seq(5000000000000, max(gdp_hist$gdp), by = 5000000000000),
                     labels = label_number(prefix = "$", scale = 1e-12, suffix = " T")) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1975-01-01"), max(gdp_hist$Date)))

# print(gdp_hist_plot)

gdp_hist_plot <- ggplotly(gdp_hist_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(gdp_hist_plot)

# *** GDP Forecast Plot ---------------------------------------------------
gdp_forecast_data <- all_gdp %>% select(-gdp)

gdp_forecast_plot <- ggplot(data = gdp_forecast_data, mapping = aes(Date, change)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "US Real GDP Quarterly YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1975-01-01"), max(all_gdp$Date))) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "blue",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(gdp_forecast_plot)

gdp_forecast_plot <- ggplotly(gdp_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(gdp_forecast_plot)

# *** GDP Forecast Table --------------------------------------------------
spg_gdpa_table <- readRDS("gdp/spg_gdpa_table.rds")
spg_gdp_forecasts <- format_table(paste("S&P Global", SPG_date), spg_gdpa_table)

moody_gdpa_table <- readRDS("gdp/moody_gdpa_table.rds")
moody_gdp_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_gdpa_table)

gdp_forecasts <- rbind(spg_gdp_forecasts,
                        moody_gdp_forecasts)

# declutter
rm(spg_gdp_forecasts,
   moody_gdp_forecasts)

blue_chip_gdp <- readRDS("gdp/blue_chip_gdp.rds")
if(!is.null(blue_chip_gdp)) # is null if indicated at top of other script
{
  blue_chip_gdp_forecasts <- format_table(paste("Blue Chip", bc_date), blue_chip_gdp)
  gdp_forecasts <- rbind(gdp_forecasts, blue_chip_gdp_forecasts)
}
# declutter
rm(blue_chip_gdp,
   blue_chip_gdp_forecasts)

rawg_gdp_table <- readRDS("gdp/rawg_gdp_table.rds")
colname <- colnames(rawg_gdp_table)[2] # to send column identification through function as "source"
rawg_gdp_forecasts <- format_table(colname, rawg_gdp_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_gdp_forecasts)))
{
  rawg_gdp_forecasts %<>% mutate({{final_year}} := NA)
}

uec_gdp_table <- readRDS("gdp/uec_gdp_table.rds")
uec_gdp_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                   uec_gdp_table)

gdp_forecasts <- rbind(gdp_forecasts,
                       rawg_gdp_forecasts,
                       uec_gdp_forecasts)

# declutter
rm(rawg_gdp_forecasts,
   uec_gdp_forecasts)

# NPI ---------------------------------------------------------------------
all_npi <- readRDS("npi/all_npi.rds")

# *** NPI History Plot ----------------------------------------------------
npi_hist <- all_npi %>% filter(source == "Personal Income % Change") %>% select(-source)

npi_hist_plot <- ggplot(data = npi_hist, mapping = aes(Date, NPI)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(as.yearqtr(Date), "\n",
                                        "PI: $", round((NPI / 1000000000), 2), " Billion")
            )
  ) +
  labs(title = "Utah Nominal Personal Income",
       x = NULL,
       y = "USD (Billions)",
       color = NULL) +
  ylim(5, NA) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  scale_y_continuous(breaks = c(5000000000, seq(50000000000, max(npi_hist$NPI), by = 50000000000)),
                     labels = label_number(prefix = "$", scale = 1e-9, suffix = " B")) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(min(npi_hist$Date), max(npi_hist$Date)))

# print(npi_hist_plot)

npi_hist_plot <- ggplotly(npi_hist_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(npi_hist_plot)

# *** NPI Forecast Plot ---------------------------------------------------
npi_forecast_data <- all_npi %>% select(-NPI)

npi_forecast_plot <- ggplot(data = npi_forecast_data, mapping = aes(Date, change)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah Nominal Personal Income Quarterly YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(min(all_npi$Date), max(all_npi$Date))) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(npi_forecast_plot)

npi_forecast_plot <- ggplotly(npi_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(npi_forecast_plot)

# *** NPI Forecast Table --------------------------------------------------
moody_npia_table <- readRDS("npi/moody_npia_table.rds")
moody_npi_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_npia_table)

rawg_npi_table <- readRDS("npi/rawg_npi_table.rds")
colname <- colnames(rawg_npi_table)[2] # to send column identification through function as "source"
rawg_npi_forecasts <- format_table(colname, rawg_npi_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_npi_forecasts)))
{
  rawg_npi_forecasts %<>% mutate({{final_year}} := NA)
}

uec_npi_table <- readRDS("npi/uec_npi_table.rds")
uec_npi_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_npi_table)

npi_forecasts <- rbind(moody_npi_forecasts,
                       rawg_npi_forecasts,
                       uec_npi_forecasts)

# declutter
rm(moody_npi_forecasts,
   rawg_npi_forecasts,
   uec_npi_forecasts)

# UT UR -------------------------------------------------------------------
all_utur <- readRDS("utur/all_utur.rds")

# *** UT UR Plot ----------------------------------------------------------------
utur_plot <- ggplot(data = all_utur, mapping = aes(Date, value)) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((value), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah Unemployment Rate",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1975-01-01"), max(all_utur$Date))) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(utur_plot)

utur_plot <- ggplotly(utur_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(utur_plot)

# *** UT UR Forecast Table ------------------------------------------------
moody_utura_table <- readRDS("utur/moody_utura_table.rds")
moody_utur_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_utura_table)

rawg_utur_table <- readRDS("utur/rawg_utur_table.rds")
colname <- colnames(rawg_utur_table)[2] # to send column identification through function as "source"
rawg_utur_forecasts <- format_table(colname, rawg_utur_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_utur_forecasts)))
{
  rawg_utur_forecasts %<>% mutate({{final_year}} := NA)
}

uec_utur_table <- readRDS("utur/uec_utur_table.rds")
uec_utur_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_utur_table)

utur_forecasts <- rbind(moody_utur_forecasts,
                        rawg_utur_forecasts,
                        uec_utur_forecasts)

# declutter
rm(moody_utur_forecasts,
   rawg_utur_forecasts,
   uec_utur_forecasts)

# EMPLOYMENT --------------------------------------------------------------
all_emp <- readRDS("emp/all_emp.rds")

# *** Emp History Plot ----------------------------------------------------
emp_hist <- all_emp %>% filter(source == "Employment Actual % Change") %>% select(-source)

emp_hist_plot <- ggplot(data = emp_hist, mapping = aes(Date, emp)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(as.yearqtr(Date), "\n",
                                        "Employed: ", round((emp / 1000000), 2), " Million")
            )
  ) +
  labs(title = "Utah Non-Farm Employment",
       x = NULL,
       y = "Employed (Millions)",
       color = NULL) +
  ylim(5, NA) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  scale_y_continuous(breaks = seq(500000, max(emp_hist$emp), by = 250000),
                     labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1970-01-01"), max(emp_hist$Date)))

# print(emp_hist_plot)

emp_hist_plot <- ggplotly(emp_hist_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(emp_hist_plot)

# *** Emp Forecast Plot ---------------------------------------------------
emp_forecast_data <- all_emp %>% select(-emp)

emp_forecast_plot <- ggplot(data = emp_forecast_data, mapping = aes(Date, change)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah Non-Farm Employment Quarterly YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1970-01-01"), max(all_emp$Date))) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(emp_forecast_plot)

emp_forecast_plot <- ggplotly(emp_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(emp_forecast_plot)

# *** Emp Forecast Table --------------------------------------------------
moody_empa_table <- readRDS("emp/moody_empa_table.rds")
moody_emp_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_empa_table)

rawg_emp_table <- readRDS("emp/rawg_emp_table.rds")
colname <- colnames(rawg_emp_table)[2] # to send column identification through function as "source"
rawg_emp_forecasts <- format_table(colname, rawg_emp_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_emp_forecasts)))
{
  rawg_emp_forecasts %<>% mutate({{final_year}} := NA)
}

uec_emp_table <- readRDS("emp/uec_emp_table.rds")
uec_emp_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_emp_table)

emp_forecasts <- rbind(moody_emp_forecasts,
                       rawg_emp_forecasts,
                       uec_emp_forecasts)

# declutter
rm(moody_emp_forecasts,
   rawg_emp_forecasts,
   uec_emp_forecasts)

# ANNUAL PAY --------------------------------------------------------------
all_pay <- readRDS("pay/all_pay.rds")

# *** Annual Pay History Plot ---------------------------------------------
pay_hist <- all_pay %>% filter(source == "Average Annual Pay % Change") %>% select(-source)

pay_hist_plot <- ggplot(data = pay_hist, mapping = aes(Year, pay)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(Year, "\n",
                                        "Pay: ", dollar(round(pay)))
            )
  ) +
  labs(title = "Utah Average Annual Pay",
       x = NULL,
       y = "Pay (USD)",
       color = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  expand_limits(y = c(10000, NA)) +
  scale_y_continuous(breaks = seq(10000, max(pay_hist$pay), by = 10000),
                     labels = label_number(prefix = "$", suffix = NULL, big.mark = ",")) +
  scale_x_continuous(breaks = seq(1980, max(pay_hist$Year), by = 5),
                     expand = c(0, 0),
                     limits = c(min(pay_hist$Year), max(pay_hist$Year))
  )

# print(pay_hist_plot)

pay_hist_plot <- ggplotly(pay_hist_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(pay_hist_plot)

# *** Annual Pay Forecast Plot --------------------------------------------
pay_forecast_data <- all_pay %>% select(-pay)

pay_forecast_plot <- ggplot(data = pay_forecast_data, mapping = aes(Year, change)) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(Year, "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah Average Annual Pay % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(1975,
                                  max(pay_forecast_data$Year),
                                  by = 5),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "darkorange",
                                "green3")
  )

# print(pay_forecast_plot)

pay_forecast_plot <- ggplotly(pay_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(pay_forecast_plot)

# *** Annual Pay Forecast Table -------------------------------------------
rawg_pay_table <- readRDS("pay/rawg_pay_table.rds")
colname <- colnames(rawg_pay_table)[2] # to send column identification through function as "source"
rawg_pay_forecasts <- format_table(colname, rawg_pay_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_pay_forecasts)))
{
  rawg_pay_forecasts %<>% mutate({{final_year}} := NA)
}

uec_pay_table <- readRDS("pay/uec_pay_table.rds")
uec_pay_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_pay_table)

pay_forecasts <- rbind(rawg_pay_forecasts,
                       uec_pay_forecasts)

# declutter
rm(rawg_pay_forecasts,
   uec_pay_forecasts)

# TAXABLE SALES -----------------------------------------------------------
all_taxable_sales <- readRDS("sales/all_taxable_sales.rds")

# *** UT Taxable Sales History Plot ---------------------------------------
tax_sales_hist <- all_taxable_sales %>% filter(source == "Taxable Sales Actual % Change") %>% select(-source)

tax_sales_hist_plot <- ggplot(data = tax_sales_hist, mapping = aes(Year, sales)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(Year, "\n",
                                        "Sales: ", round((sales / 1000000000), 2), " Billion")
            )
  ) +
  labs(title = "Utah Taxable Sales",
       x = NULL,
       y = "Sales (Billions)",
       color = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  scale_y_continuous(breaks = seq(20000000000, max(tax_sales_hist$sales), by = 20000000000),
                     labels = label_number(scale = 1e-9, suffix = " B")) +
  scale_x_continuous(breaks = seq(1980, max(tax_sales_hist$Year),
                                  by = 5),
                     expand = c(0, 0),
                     limits = c(min(tax_sales_hist$Year), max(tax_sales_hist$Year))
  )

# print(tax_sales_hist_plot)

tax_sales_hist_plot <- ggplotly(tax_sales_hist_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(tax_sales_hist_plot)

# *** UT Taxable Sales Forecast Plot --------------------------------------
tax_sales_forecast_data <- all_taxable_sales %>% select(-sales)

tax_sales_forecast_plot <- ggplot(data = tax_sales_forecast_data, mapping = aes(Year, change)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(Year, "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah Taxable Sales YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(1975,
                                  max(all_taxable_sales$Year),
                                  by = 5),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "darkorange",
                                "green3")
  )

# print(tax_sales_forecast_plot)

tax_sales_forecast_plot <- ggplotly(tax_sales_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(tax_sales_forecast_plot)

# *** UT Monthly Taxable Sales Plot -------------------------------
monthly_sales_hist <- readRDS("sales/monthly_sales_hist.rds")

monthly_sales_plot <- ggplot(data = monthly_sales_hist, mapping = aes(Date, sales)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(as.yearmon(Date), "\n",
                                        "Sales: ", round((sales / 1000000000), 2), " Billion",
                                        "\n", "Change: ", percent(change, accuracy = 0.1))
            )
  ) +
  labs(title = "Utah Monthly Taxable Sales",
       x = NULL,
       y = "Monthly Sales (Billions)",
       color = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  scale_y_continuous(breaks = seq(2000000000, max(monthly_sales_hist$sales), by = 1000000000),
                     labels = label_number(scale = 1e-9, suffix = " B")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(min(monthly_sales_hist$Date), max(monthly_sales_hist$Date)))

# print(monthly_sales_plot)

monthly_sales_plot <- ggplotly(monthly_sales_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(monthly_sales_plot)

# *** UT Taxable Sales Forecast Table -------------------------------------
rawg_tax_sales_table <- readRDS("sales/rawg_tax_sales_table.rds")
colname <- colnames(rawg_tax_sales_table)[2] # to send column identification through function as "source"
rawg_tax_sales_forecasts <- format_table(colname, rawg_tax_sales_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_tax_sales_forecasts)))
{
  rawg_tax_sales_forecasts %<>% mutate({{final_year}} := NA)
}

uec_tax_sales_table <- readRDS("sales/uec_tax_sales_table.rds")
uec_tax_sales_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_tax_sales_table)

tax_sales_forecasts <- rbind(rawg_tax_sales_forecasts,
                             uec_tax_sales_forecasts)

# declutter
rm(rawg_tax_sales_forecasts,
   uec_tax_sales_forecasts)

# HPI ---------------------------------------------------------------------
all_hpi <- readRDS("hpi/all_hpi.rds")

# *** UT HPI Levels Plot --------------------------------------------------
hpi_levels <- all_hpi %>% filter(source == "HPI Actual % Change") %>% select(-source)

hpi_levels_plot <- ggplot(data = hpi_levels, mapping = aes(Date, hpi)) +
  geom_line(color = "black", size = 0.75,
                mapping = aes(group = 1,
                              text = paste0(as.yearqtr(Date), "\n",
                                            "HPI Level: ", round((hpi), 1))
                              )
            ) +
  labs(title = "Utah House Price Index Levels (1980 Q1 = 100)",
       x = NULL,
       y = "Index Level",
       color = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, max(hpi_levels$hpi), by = 100)) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1975-01-01"), max(hpi_levels$Date)))

# print(hpi_levels_plot)

hpi_levels_plot <- ggplotly(hpi_levels_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(hpi_levels_plot)

# *** UT HPI Forecast Plot ------------------------------------------------
hpi_forecast_data <- all_hpi %>% select(-hpi)

hpi_forecast_plot <- ggplot(data = hpi_forecast_data, mapping = aes(Date, change)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.25) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(as.yearqtr(Date), "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah House Price Index Quarterly YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0),
               limits = c(as.Date("1975-01-01"), max(all_hpi$Date))) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "red2",
                                "darkorange",
                                "green3")
  )

# print(hpi_forecast_plot)

hpi_forecast_plot <- ggplotly(hpi_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(hpi_forecast_plot)

# *** UT HPI Forecast Table -----------------------------------------------
moody_hpia_table <- readRDS("hpi/moody_hpia_table.rds")
moody_hpi_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_hpia_table)

rawg_hpi_table <- readRDS("hpi/rawg_hpi_table.rds")
colname <- colnames(rawg_hpi_table)[2] # to send column identification through function as "source"
rawg_hpi_forecasts <- format_table(colname, rawg_hpi_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_hpi_forecasts)))
{
  rawg_hpi_forecasts %<>% mutate({{final_year}} := NA)
}

uec_hpi_table <- readRDS("hpi/uec_hpi_table.rds")
uec_hpi_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")),
                                  uec_hpi_table)

hpi_forecasts <- rbind(moody_hpi_forecasts,
                       rawg_hpi_forecasts,
                       uec_hpi_forecasts)

# declutter
rm(moody_hpi_forecasts,
   rawg_hpi_forecasts,
   uec_hpi_forecasts)

# POPULATION --------------------------------------------------------------
all_pop <- readRDS("pop/all_pop.rds")

# *** UT Pop History Plot -------------------------------------------------
pop_hist <- all_pop %>% filter(source == "Population Actual % Change") %>% select(-source)

pop_hist_plot <- ggplot(data = pop_hist, mapping = aes(Year, pop)) +
  geom_line(color = "black", size = 0.75,
            mapping = aes(group = 1,
                          text = paste0(Year, "\n",
                                        "Population: ", round((pop / 1000000), 2), " Million")
            )
  ) +
  labs(title = "Utah Resident Population",
       x = NULL,
       y = "Population (Millions)",
       color = NULL) +
  ylim(5, NA) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y.left = element_text(margin = margin(r = 15))
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_continuous(breaks = seq(min(pop_hist$Year),
                                  max(pop_hist$Year),
                                  by = 5),
                     expand = c(0, 0)
                     )

# print(pop_hist_plot)

pop_hist_plot <- ggplotly(pop_hist_plot, tooltip = "text") %>%
  layout(yaxis = list(title = list(standoff = 25)))

# print(pop_hist_plot)

# *** UT Pop Forecast Plot ------------------------------------------------
pop_forecast_data <- all_pop %>% select(-pop)

pop_forecast_plot <- ggplot(data = pop_forecast_data, mapping = aes(Year, change)) +
  geom_line(aes(color = source,
                group = source,
                text = paste0(Year, "\n", source, ": ", round((change * 100), 1), "%"),
  ),
  size = 0.75) +
  labs(title = "Utah Resident Population YoY % Change",
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(min(all_pop$Year),
                                  max(all_pop$Year),
                                  by = 5),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5)),
        plot.margin = margin(15, 30, 30, 30),
        legend.position = "bottom",
        axis.text.x = element_text(face = "bold",
                                   angle = 45),
        axis.line = element_line(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)
  ) +
  scale_color_manual(values = c("black",
                                "red2",
                                "darkorange",
                                "green3",
                                "blue")
  )

# print(pop_forecast_plot)

pop_forecast_plot <- ggplotly(pop_forecast_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "h", xanchor = "center", x = 0.5,
                       yanchor = "top", y = -0.25))

# print(pop_forecast_plot)

# *** UT Pop Forecast Table -----------------------------------------------


if(file.exists("pop/gardner_pop_table.rds")){ # include_gardner == T // unofficial gardner estimate exists
  gardner_pop_table <- readRDS("pop/gardner_pop_table.rds")
  
  # this is to pull the date of the Gardner numbers
  gardner_forecast_year <- grep("Gardner", unique(pop_forecast_data$source))[[1]]
  gardner_forecast_year <- unique(pop_forecast_data$source)[gardner_forecast_year]
  gardner_forecast_year <- as.character(gardner_forecast_year)
  gardner_forecast_year <- strsplit(as.character(gardner_forecast_year), " ")[[1]]
  gardner_forecast_year <- paste(tail(gardner_forecast_year, 2), collapse = " ")
  
  gardner_pop_forecasts <- format_table(paste("Gardner Institute Unofficial Estimate", gardner_forecast_year), gardner_pop_table)
}

moody_pop_table <- readRDS("pop/moody_pop_table.rds")
moody_pop_forecasts <- format_table(paste("Moody's Analytics", moody_date), moody_pop_table)

rawg_pop_table <- readRDS("pop/rawg_pop_table.rds")
colname <- colnames(rawg_pop_table)[2] # to send column identification through function as "source"
rawg_pop_forecasts <- format_table(colname, rawg_pop_table)
# if it's missing the last column, create it for rbinding
final_year <- forecast_years[length(forecast_years)]
if(!(final_year %in% colnames(rawg_pop_forecasts)))
{
  rawg_pop_forecasts %<>% mutate({{final_year}} := NA)
}

uec_pop_table <- readRDS("pop/uec_pop_table.rds")
uec_pop_forecasts <- format_table(paste("Economic Council", format(uec_date, "%b %Y")), uec_pop_table)

if(file.exists("pop/gardner_pop_table.rds")){
  pop_forecasts <- rbind(gardner_pop_forecasts,
                         moody_pop_forecasts,
                         rawg_pop_forecasts,
                         uec_pop_forecasts)
  
  # declutter
  rm(gardner_pop_forecasts,
     moody_pop_forecasts,
     rawg_pop_forecasts,
     uec_pop_forecasts)

} else {
  pop_forecasts <- rbind(moody_pop_forecasts,
                         rawg_pop_forecasts,
                         uec_pop_forecasts)
  
  # declutter
  rm(moody_pop_forecasts,
     rawg_pop_forecasts,
     uec_pop_forecasts)
}


# UI ----------------------------------------------------------------------

# *** Summary Tab ---------------------------------------------------------
ui <- fluidPage(
  rclipboardSetup(),
  title = "UEC Forecast Info",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel(h1("Utah Economic Council Forecast Information",
                  style = "padding: 5px 0px 10px 15px; font-size: 2.5vw;")),
  navlistPanel(
    tags$head( # this adjusts the font size of the navlistPanel, "Survey Summary" list items, and p texts
      tags$style(type = "text/css", ".nav-item {font-size: 1.125vw;}
                 li {font-size: 1.375vw;}
                 h3 {font-size: 2vw;}
                 p {font-size: 1.5vw;}"),
      
      # tags$style(HTML(".fa{font-size: 0.75vw;}")), # this was an attempt to adjust the icon sizing (only does icon, not entire border with)
      
      tags$style(HTML(".adjusted{font-size: 1.5vw;"))
      ),
        

    
    id = "tabName",
    widths = c(3, 9),
    tabPanel("App Summary",
             value = "welcome_tab",
             
             fluidRow(
               
               column(6,
                      tags$br(),
                      tags$p("This information packet provides Utah Economic Council members
                             with definitions, sources, and context for their
                             quarterly economic indicator forecasts.", br(), br(),
                             "Each indicator page includes interactive tools for
                             visualizing indicator historical actuals,
                             up-to-date quarterly year-over-year changes (where applicable),
                             quarterly percent change forecasts from various sources,
                             and a forecast summary on the yearly level.", br(), br(),
                             "Both historical and forecast data are available for
                             download within each page. Tab title hyperlinks provide redirection to public
                             historical data sources for convenient citation.
                             Series IDs are provided when necessary for retrieving data.",
                             style="font-size: 1.375vw;"),
                      
                      tags$br(),
                      
                      tags$p(tags$b("Note on Included Historic and Proprietary Forecasts: "),
                             "This packet includes proprietary and
                             historical forecasts for informational purposes,
                             not as a suggestion for Council adoption.
                             Specifically, the packet includes forecasts from S&P Global
                             (formerly IHS Markit, and formerly Global Insights)
                             for national level indicators, Moody’s Analytics for state level indicators,
                             the most recent Utah Revenue Assumptions Working Group (RAWG),
                             and the Economic Council from last quarter.",
                             style="font-size: 1.5vw;")
               ),
               
               column(6,
                      tags$br(),
                      tags$h2("Survey Summary", style = "font-size: 2vw;"),
                      tags$p("Council members complete quarterly surveys on their expectations for:",
                             style="font-size: 1.375vw;"),
                      
                      tags$ul(
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'recession_tab');", tags$u("US and Utah recession probabilities"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'inflation_tab');", tags$u("US CPI inflation rate"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'us_unemployment_tab');", tags$u("US unemployment rate"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'gdp_tab');", tags$u("US real GDP year-over change"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'income_tab');", tags$u("Utah nominal personal income year-over change"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'ut_unemployment_tab');", tags$u("Utah unemployment rate"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'nonfarm_employment_tab');", tags$u("Utah nonfarm employment year-over change"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'ave_annual_pay_tab');", tags$u("Utah nominal average pay year-over change"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'taxable_sales_tab');", tags$u("Utah nominal taxable sales year-over change"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'home_prices_tab');", tags$u("Utah home nominal home price year-over change"))),
                        tags$li(tags$a(href="#", onclick="Shiny.setInputValue('switch_tab', 'population_tab');", tags$u("Utah population year-over change"))),
                        ),
                      
                      #tags$p(paste0("Navigate to the most recent Utah Economic Council forecast sheet here."), style="font-size: 1.5vw;"),
                      
                      # tags$p(
                      #   "Dashboard developed by: ",
                      #   tags$a(href = "https://www.linkedin.com/in/jackson-bolos/", "Jackson Bolos", target = "_blank"),
                      #   style = "font-size: 1.5vw;"
                      # ),
                      
                      tags$p(paste0("Data last updated: ", last_updated), style="font-size: 1.5vw;"),
                      
                      #tags$p(paste0("Developed by: Jackson Bolos"), style="font-size: 1.5vw;"),
                      
               )
               
             )
             
    ),
    
    

# *** Recession Tab -------------------------------------------------------
    tabPanel("US Economic Recession",
             value = "recession_tab",
             
             tags$h3(tags$a(href="https://fred.stlouisfed.org/",
                            target="_blank",
                            tags$u("US Economic Recession", .noWS = "outside")
                            )
                     ),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": A decrease in economic activity across the national
                    economy that meets a degree of significance based on depth, diffusion, and duration. Indicators
                    include GDP components, employment, unemployment, and real personal income transfers."),
             
             tags$br(),
             
             # had to use straight html to embed dynamic date into paste function
             tags$p(
               HTML(paste0("The below graphics show 1) the frequency and duration of U.S. recessions since ",
                           year(start_date), ", and 2) how different",
                           "<a href='https://www.nber.org/research/business-cycle-dating/business-cycle-dating-procedure-frequently-asked-questions' target='_blank'>
                           
                              <u>factors taken into account by the NBER in defining a recession</u>
                           
                           </a>
                           have changed in reference to the last recession for the US."
                           )
                    )
               ),
             
             tags$br(),
             
             div(plotlyOutput("recession_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("recession_plot_data_download", "Recession Indicators Data"), align = "center")
                      
                      )
               
               ),
             
             # tags$p(tags$a(href="https://www.nber.org/research/business-cycle-dating/business-cycle-dating-procedure-frequently-asked-questions#:~:text=A%3A%20The%20NBER%27s%20traditional%20definition,more%20than%20a%20few%20months.",
             #               target="_blank",
             #               tags$u("National Bureau of Economic Research")), style="font-size: 1.125vw;"),
             
             tags$br()
    ),
    
    

# *** CPI Tab -------------------------------------------------------------
tabPanel("US CPI Inflation Rate",
             value = "inflation_tab",
             
             tags$h3(tags$a(href="https://data.bls.gov/cgi-bin/srgate",
                            target="_blank",
                            tags$u("US CPI Inflation Rate", .noWS = "outside")
                            ),
                     span("(Adjusted)", class = "adjusted")
                     ),
         
         tags$p("Series ID: CUSR0000SA0",
                rclipButton(inputId = "clipValue",
                            label = "",
                            clipText = "CUSR0000SA0",
                            icon = icon("copy"),
                            tooltip = "Copy ID"),
                style="font-size: 1.3125vw;"),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": Consumer Price Index (CPI)
                    measures the average change in the prices paid by urban consumers for
                    a representative basket of consumer goods and services over time."),
             
             tags$br(),

             div(plotlyOutput("cpi_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("cpi_plot_data_download", "CPI Plot Data"), style = "display: inline-block;"),
                      
                      div(downloadButton("cpi_history_download", "Monthly CPI History"), style = "display: inline-block;")
                      
                      )
               ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("CPI Forecasts")),
                      tableOutput("cpi_forecasts"),
                      # downloadButton("cpi_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
             
    ),
    
    

# *** US UR Tab -----------------------------------------------------------
tabPanel("US Unemployment Rate",
             value = "us_unemployment_tab",
             
             tags$h3(tags$a(href="https://data.bls.gov/cgi-bin/srgate",
                            target="_blank",
                            tags$u("US Unemployment Rate", .noWS = "outside")
                            ),
                     span("(Adjusted)", class = "adjusted")
         ),
         
         tags$p("Series ID: LNS14000000",
                rclipButton(inputId = "clipValue",
                            label = "",
                            clipText = "LNS14000000",
                            icon = icon("copy"),
                            tooltip = "Copy ID"),
                style="font-size: 1.3125vw;"),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": Represents the number of unemployed people as a
                    percentage of the labor force. People are classified as unemployed if they do not have a job,
                    have actively looked for work in the prior 4 weeks, and are currently available for work."),
             
             tags$br(),
             
             div(plotlyOutput("usur_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("usur_plot_data_download", "US UR Plot Data"), style = "display: inline-block;"),
                      
                      div(downloadButton("usur_history_download", "Monthly US UR History"), style = "display: inline-block;")
                      
               )
             ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("US Unemployment Forecasts")),
                      tableOutput("usur_forecasts"),
                      # downloadButton("usur_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
    ),
    
    

# *** GDP Tab -------------------------------------------------------------
tabPanel("US Real GDP",
             value = "gdp_tab",
             
             tags$h3(tags$a(href="https://apps.bea.gov/iTable/?reqid=19&step=2&isuri=1&categories=survey&_gl=1*1z4vx3*_ga*Njg0NzQ1NTIuMTcwNTUzMTE0NA..*_ga_J4698JNNFT*MTcxMTQ5Njk0Mi4yNC4xLjE3MTE0OTc1OTcuNDcuMC4w#eyJhcHBpZCI6MTksInN0ZXBzIjpbMSwyLDMsM10sImRhdGEiOltbImNhdGVnb3JpZXMiLCJTdXJ2ZXkiXSxbIk5JUEFfVGFibGVfTGlzdCIsIjYiXSxbIkZpcnN0X1llYXIiLCIxOTc1Il0sWyJMYXN0X1llYXIiLCIyMDIzIl0sWyJTY2FsZSIsIi05Il0sWyJTZXJpZXMiLCJRIl0sWyJTZWxlY3RfYWxsX3llYXJzIiwiMSJdXX0=",
                            target="_blank",
                            tags$u("US Real GDP", .noWS = "outside")
                            ),
                     span("(Adjusted)", class = "adjusted")
                     ),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": Measures the inflation-adjusted
                    (chained 2017 dollars) value of the final goods and services produced in the United States
                    (without double counting the intermediate goods and services used up to produce them)."),
             
             tags$br(),
             
             div(plotlyOutput("gdp_hist_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("gdp_history_download", "Quarterly GDP History"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             
             div(plotlyOutput("gdp_forecast_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("gdp_plot_data_download", "GDP Forecast Plot Data"), align = "center"),
                      
                      )
               ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("GDP Forecasts")),
                      tableOutput("gdp_forecasts"),
                      # downloadButton("gdp_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
             
    ),
    
    

# *** NPI Tab -------------------------------------------------------------
tabPanel("Utah Nominal Personal Income",
             value = "income_tab",
             
             tags$h3(tags$a(href="https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*16n621j*_ga*Njg0NzQ1NTIuMTcwNTUzMTE0NA..*_ga_J4698JNNFT*MTcxMTQ5Njk0Mi4yNC4xLjE3MTE0OTY5NTYuNDYuMC4w#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCIyMSJdLFsiTWFqb3JfQXJlYSIsIjAiXSxbIlN0YXRlIixbIjAiXV0sWyJBcmVhIixbIjQ5MDAwIl1dLFsiU3RhdGlzdGljIixbIjEiXV0sWyJVbml0X29mX21lYXN1cmUiLCJMZXZlbHMiXSxbIlllYXIiLFsiLTEiXV0sWyJZZWFyQmVnaW4iLCItMSJdLFsiWWVhcl9FbmQiLCItMSJdXX0=",
                            target="_blank",
                            tags$u("Utah Nominal Personal Income", .noWS = "outside")
                            ),
                     span("(Unadjusted)", class = "adjusted")
         ),
             
             tags$p(tags$u("Definition", .noWS = "outside"),
                    ": Income received by ",
                    tags$a(href="https://www.bea.gov/help/glossary/persons", target="_blank", "persons", .noWS = "outside"),
                    " from all sources. It includes income received from participation in production as well as from government and business transfer payments. It is the sum of ",
                    tags$a(href="https://www.bea.gov/help/glossary/compensation-employees-received", target="_blank", "compensation of employees (received)", .noWS = "outside"),
                    ", ",
                    tags$a(href="https://www.bea.gov/help/glossary/supplements-wages-and-salaries", target="_blank", "supplements to wages and salaries", .noWS = "outside"),
                    ", ",
                    tags$a(href="https://www.bea.gov/help/glossary/proprietors-income", target="_blank", "proprietors' income", .noWS = "outside"),
                    " with ",
                    tags$a(href="https://www.bea.gov/help/glossary/inventory-valuation-adjustment-iva", target="_blank", "inventory valuation adjustment (IVA)", .noWS = "outside"),
                    " and ",
                    tags$a(href="https://www.bea.gov/help/glossary/capital-consumption-adjustment-ccadj-private", target="_blank", "capital consumption adjustment (CCAdj)", .noWS = "outside"),
                    ", ",
                    tags$a(href="https://www.bea.gov/help/glossary/rental-income-persons-capital-consumption-adjustment", target="_blank", "rental income of persons", .noWS = "outside"),
                    " with ",
                    tags$a(href="https://www.bea.gov/help/glossary/capital-consumption-adjustment-ccadj-private", target="_blank", "CCAdj", .noWS = "outside"),
                    ", ",
                    tags$a(href="https://www.bea.gov/help/glossary/personal-income-receipts-assets", target="_blank", "personal income receipts on assets", .noWS = "outside"),
                    ", and ",
                    tags$a(href="https://www.bea.gov/help/glossary/personal-current-transfer-receipts", target="_blank", "personal current transfer receipts", .noWS = "outside"),
                    ", less ",
                    tags$a(href="https://www.bea.gov/help/glossary/contribution-government-social-insurance", target="_blank", "contributions for government social insurance", .noWS = "outside"),
                    "."),
             
             tags$br(),
             
             div(plotlyOutput("npi_hist_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("npi_history_download", "Quarterly Income History"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             
             div(plotlyOutput("npi_forecast_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("npi_plot_data_download", "PI Forecast Plot Data"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("Personal Income Forecasts")),
                      tableOutput("npi_forecasts"),
                      # downloadButton("npi_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
             
    ),
    
    

# *** UT UR Tab -----------------------------------------------------------
tabPanel("Utah Unemployment Rate",
             value = "ut_unemployment_tab",
             
             tags$h3(tags$a(href="https://data.bls.gov/cgi-bin/srgate",
                            target="_blank",
                            tags$u("Utah Unemployment Rate", .noWS = "outside")
                            ),
                     span("(Adjusted)", class = "adjusted")
                     ),
         
         tags$p("Series ID: LASST490000000000003",
                rclipButton(inputId = "clipValue",
                            label = "",
                            clipText = "LASST490000000000003",
                            icon = icon("copy"),
                            tooltip = "Copy ID"),
                style="font-size: 1.3125vw;"),
             
         tags$p(tags$u("Definition", .noWS = "outside"), ": Measures people who reside in Utah,
                are jobless, and are available to take a job and have actively sought work
                in the past four weeks, in terms of percentage of the Utah labor force."),
         
         column(width = 6,
                tags$p(tags$a(href="https://jobs.utah.gov/wi/data/library/employment/countyunemployment.html#:~:text=The%20unemployment%20rate%20is%20a,for%20the%20availability%20of%20labor.",
                              target="_blank",
                              tags$u("Utah Department of Workforce Services", .noWS = "outside")),
                       style="font-size: 1.125vw;")
                ),
         
         tags$br(),
             
         div(plotlyOutput("utur_plot", height = "500px", width ="75%"), align = "center"),
             
         fluidRow(
               
           column(offset = 6, width = 6,
                      
                  div(downloadButton("utur_plot_data_download", "UT UR Plot Data"), style = "display: inline-block;"),
                      
                  div(downloadButton("utur_history_download", "Monthly UT UR History"), style = "display: inline-block;")
                      
                  )
           ),
             
         tags$br(),
         tags$br(),
             
         fluidRow(
               
           column(offset = 2, width = 7,
                      
                  tags$p(tags$u("Utah Unemployment Forecasts")),
                  tableOutput("utur_forecasts"),
                  # downloadButton("utur_forecasts_download", "Detailed Forecasts")
                      
                  )
           ),
             
         tags$br()
             
    ),
    
    

# *** Emp Tab -------------------------------------------------------------
tabPanel("Utah Non-Farm Employment",
             value = "nonfarm_employment_tab",
             
             tags$h3(tags$a(href="https://data.bls.gov/cgi-bin/srgate",
                            target="_blank",
                            tags$u("Utah Non-Farm Employment", .noWS = "outside")
                            ),
                     span("(Adjusted)", class = "adjusted")
         ),
         
         tags$p("Series ID: SMS49000000000000001",
                rclipButton(inputId = "clipValue",
                            label = "",
                            clipText = "SMS49000000000000001",
                            icon = icon("copy"),
                            tooltip = "Copy ID"),
                style="font-size: 1.3125vw;"),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": Number of workers  excluding those in farming,
                    private households, proprietors, non-profit employees, and active military."),
             
             tags$br(),
             
             div(plotlyOutput("emp_hist_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("emp_history_download", "Quarterly Employment History"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             
             div(plotlyOutput("emp_forecast_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("emp_plot_data_download", "Employment Forecast Plot Data"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("Employment Forecasts")),
                      tableOutput("emp_forecasts"),
                      # downloadButton("emp_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
             
    ),
    
    

# *** Annual Pay Tab ------------------------------------------------------
tabPanel("Utah Average Annual Pay",
         value = "ave_annual_pay_tab",
             
         tags$h3(tags$a(href="https://data.bls.gov/cgi-bin/srgate",
                        target="_blank",
                        tags$u("Utah Nominal Average Annual Pay", .noWS = "outside")
                        )
         ),
         
         tags$p("Series ID: ENU4900050010",
                rclipButton(inputId = "clipValue",
                            label = "",
                            clipText = "ENU4900050010",
                            icon = icon("copy"),
                            tooltip = "Copy ID"),
                tags$br(),
                "(Revised series from RAWG historical file used)",
                style="font-size: 1.3125vw;"),
             
         tags$p(tags$u("Definition", .noWS = "outside"), ": Total non-farm wages divided by total non-farm employment."),
             
         tags$br(),
             
         div(plotlyOutput("pay_hist_plot", height = "500px", width ="75%"), align = "center"),
         
         fluidRow(
           
           column(offset = 6, width = 6,
                  
                  div(downloadButton("pay_history_download", "Annual Pay History"), align = "center")
                  
                  )
           ),
         
         tags$br(),
         
         div(plotlyOutput("pay_forecast_plot", height = "500px", width ="75%"), align = "center"),
         
         fluidRow(
           
           column(offset = 6, width = 6,
                  
                  div(downloadButton("pay_plot_data_download", "Annual Pay Forecast Plot Data"), align = "center"),
                  
                  )
           ),
         
         tags$br(),
         tags$br(),
         
         fluidRow(
           
           column(offset = 2, width = 7,
                  
                  tags$p(tags$u("Annual Pay Forecasts")),
                  tableOutput("pay_forecasts"),
                  # downloadButton("pay_forecasts_download", "Detailed Forecasts")
                  
                  )
           ),
             
         tags$br()
             
         ),
    
    

# *** Taxable Sales Tab ---------------------------------------------------
tabPanel("Utah Nominal Taxable Sales",
             value = "taxable_sales_tab",
             
             tags$h3(tags$a(href="https://tax.utah.gov/econstats/sales",
                            target="_blank",
                            tags$u("Utah Nominal Taxable Sales", .noWS = "outside")
                            )
         ),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": All sales subject to Utah sales and
                    use tax, including taxable services, retail sales, and business investment."),
             
             # tags$br(),
             
             tags$p("Three graphics are included below. The first shows actual annual Utah taxable sales since 1980.
                    The second shows annual Utah taxable sales year-over percent change for the same time period.
                    The third shows actual monthly Utah taxable sales since 2000 (see tooltip for year-over growth)
                    Together, the graphics show long term annual trends as well as movements on the monthly level."),
         
         tags$br(),
         
         div(plotlyOutput("tax_sales_hist_plot", height = "500px", width ="75%"), align = "center"),
         
         fluidRow(
           
           column(offset = 6, width = 6,
                  
                  div(downloadButton("tax_sales_history_download", "Annual Taxable Sales History"), align = "center")
                  
                  )
           ),
             
             tags$br(),
             
             div(plotlyOutput("tax_sales_forecast_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("tax_sales_plot_data_download", "Taxable Sales Forecast Plot Data"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             
             div(plotlyOutput("monthly_sales_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("monthly_sales_history_download", "Monthly Taxable Sales History"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("Taxable Sales Forecasts")),
                      tableOutput("tax_sales_forecasts"),
                      # downloadButton("tax_sales_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
             
    ),
    
    

# *** HPI Tab -------------------------------------------------------------
tabPanel("Utah Nominal Home Prices",
             value = "home_prices_tab",
             
             tags$h3(tags$a(href="https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx#qat",
                            target="_blank",
                            tags$u("Utah Nominal Home Prices", .noWS = "outside")
                            ),
                     span("(Unadjusted)", class = "adjusted")
         ),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": Utah home prices are measured by the all-transactions Home Price Index (HPI),
                        which is a housing price indicator that measures the movement of single-family house prices in
                        the U.S., states and metropolitan areas, with 1980 (Q1) as its base year."),
             
             # tags$br(),
             # 
             # # don't think this is necessary, as it follows the pattern of most the other indicators
             # tags$p("Two graphics are included below. The first shows annual HPI year-over percent change from
             #            1975 to 2022. The second shows quarterly HPI levels from the first quarter of 2018 to the most
             #            recent quarter. Together, the graphics show long term annual trends as well as movements in
             #            the most recent quarterly data."),
         
             column(width = 6,
                    tags$p(tags$a(href="https://jobs.utah.gov/wi/data/library/macro/housingprices.html",
                                  target="_blank",
                                  tags$u("Utah Department of Workforce Services"),
                                  .noWS = "outside"),
                           style="font-size: 1.125vw;")
                    ),
             
             tags$br(),
             
             div(plotlyOutput("hpi_levels_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("hpi_levels_download", "Quarterly HPI Levels"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             
             div(plotlyOutput("hpi_forecast_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("hpi_plot_data_download", "HPI Change Forecast Plot Data"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             tags$br(),         
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("HPI Forecasts")),
                      tableOutput("hpi_forecasts"),
                      # downloadButton("hpi_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br()
             
    ),
    
    
    
# *** Population Tab ------------------------------------------------------
tabPanel("Utah Population",
             value = "population_tab",
             
             tags$h3(tags$a(# href="https://d36oiwf74r1rap.cloudfront.net/wp-content/uploads/UPC-Estimates-Dec2023.pdf?x71849",
                            href="https://gardner.utah.edu/demographics/population-estimates/state-and-county/",
                            target="_blank",
                            tags$u("Utah Population", .noWS = "outside")
                            )
                     ),
         
         tags$p("(Population series from Kem C. Gardner Policy Institute)",
                style="font-size: 1.3125vw;"),
             
             tags$p(tags$u("Definition", .noWS = "outside"), ": An estimate of the number of
                    people who are usually residents in Utah as of July 1 of each year."),
             
             # tags$p("This population series is from the ",
             #        tags$a(# href="https://d36oiwf74r1rap.cloudfront.net/wp-content/uploads/UPC-Estimates-Dec2023.pdf?x71849",
             #               href="https://gardner.utah.edu/demographics/population-estimates/state-and-county/",
             #               target="_blank",
             #               tags$u("Kem C. Gardner Policy Institute", .noWS = "outside"), .noWS = "outside"), ".",
             #        style="font-size: 1.125vw;"),
             
             tags$br(),
             
             div(plotlyOutput("pop_hist_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("pop_history_download", "Population History"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             
             div(plotlyOutput("pop_forecast_plot", height = "500px", width ="75%"), align = "center"),
             
             fluidRow(
               
               column(offset = 6, width = 6,
                      
                      div(downloadButton("pop_plot_download", "Population Forecast Plot Data"), align = "center")
                      
                      )
               ),
             
             tags$br(),
             tags$br(),
             
             fluidRow(
               
               column(offset = 2, width = 7,
                      
                      tags$p(tags$u("Population Forecasts")),
                      tableOutput("pop_forecasts"),
                      # downloadButton("pop_forecasts_download", "Detailed Forecasts")
                      
                      )
               ),
             
             tags$br(),
             
             )
    
    )
  
  )



# Server ------------------------------------------------------------------
server <- function(input, output, session) {

# Nav Tab
observeEvent(input$switch_tab, {
    updateTabsetPanel(session, "tabName", selected = input$switch_tab)
  })

# Recession Page ----------------------------------------------------------
output$recession_plot <- renderPlotly(recession_plot)

output$recession_plot_data_download <- downloadHandler(
  filename = function(){"recession_plot_data.csv"},
  content = function(file){write.csv(rec_indicators %>%
                                       mutate(value = round(value, 2)) %>%
                                       # pivot wider on indicator
                                       pivot_wider(names_from = indicator,
                                                   values_from = value) %>%
                                       # recessions back into data frame
                                       left_join(recessions, by = "Date") %>%
                                       relocate(Recession, .after = "Date"),
                                     file,
                                     row.names = FALSE)}
  )


# CPI Page ----------------------------------------------------------------

# plot
output$cpi_plot <- renderPlotly(cpi_plot)

# download buttons
output$cpi_plot_data_download <- downloadHandler(
  filename = function(){"cpi_plot_data.csv"},
  content = function(file){write.csv(all_cpi %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )
output$cpi_history_download <- downloadHandler(
  filename = function(){"monthly_cpi_history.csv"},
  content = function(file){write.csv(readRDS("cpi/cpi_hist_table.rds"),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$cpi_forecasts <- renderTable(cpi_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# US UR Page --------------------------------------------------------------

# plot
output$usur_plot <- renderPlotly(usur_plot)

# download buttons
output$usur_plot_data_download <- downloadHandler(
  filename = function(){"us_unemployment_plot_data.csv"},
  content = function(file){write.csv(all_usur %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = value),
                                     file,
                                     row.names = FALSE)}
  )
output$usur_history_download <- downloadHandler(
  filename = function(){"monthly_us_unemployment_history.csv"},
  content = function(file){write.csv(readRDS("usur/usur_hist_table.rds"),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$usur_forecasts <- renderTable(usur_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# GDP Page ----------------------------------------------------------------

# history plot
output$gdp_hist_plot <- renderPlotly(gdp_hist_plot)

# history download button
output$gdp_history_download <- downloadHandler(
  filename = function(){"quarterly_gdp_history.csv"},
  content = function(file){write.csv(gdp_hist %>%
                                       transmute(Date,
                                                 `Real GDP Actual` = dollar(round(gdp)),
                                                 `Real GDP Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# forecast plot
output$gdp_forecast_plot <- renderPlotly(gdp_forecast_plot)

# plot data download button
output$gdp_plot_data_download <- downloadHandler(
  filename = function(){"gdp_plot_data.csv"},
  content = function(file){write.csv(gdp_forecast_data %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$gdp_forecasts <- renderTable(gdp_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# NPI Page ----------------------------------------------------------------

# history plot
output$npi_hist_plot <- renderPlotly(npi_hist_plot)

# history download button
output$npi_history_download <- downloadHandler(
  filename = function(){"quarterly_income_history.csv"},
  content = function(file){write.csv(npi_hist %>%
                                       as_tibble() %>%
                                       transmute(Date,
                                                 `PI Actuals` = dollar(round(NPI)),
                                                 `PI Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
)

# forecast plot
output$npi_forecast_plot <- renderPlotly(npi_forecast_plot)

# plot data download button
output$npi_plot_data_download <- downloadHandler(
  filename = function(){"personal_income_plot_data.csv"},
  content = function(file){write.csv(npi_forecast_data %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$npi_forecasts <- renderTable(npi_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# UT UR Page --------------------------------------------------------------

# plot
output$utur_plot <- renderPlotly(utur_plot)

# download buttons
output$utur_plot_data_download <- downloadHandler(
  filename = function(){"ut_unemployment_plot_data.csv"},
  content = function(file){write.csv(all_utur %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = value),
                                     file,
                                     row.names = FALSE)}
  )
output$utur_history_download <- downloadHandler(
  filename = function(){"monthly_ut_unemployment_history.csv"},
  content = function(file){write.csv(readRDS("utur/utur_hist_table.rds"),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$utur_forecasts <- renderTable(utur_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# Emp Page ----------------------------------------------------------------

# history plot
output$emp_hist_plot <- renderPlotly(emp_hist_plot)

# history download button
output$emp_history_download <- downloadHandler(
  filename = function(){"quarterly_employment_history.csv"},
  content = function(file){write.csv(emp_hist %>%
                                       as_tibble() %>%
                                       transmute(Date,
                                                 `Employment Actual` = emp,
                                                 `Employment Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# forecast plot
output$emp_forecast_plot <- renderPlotly(emp_forecast_plot)

# plot data download button
output$emp_plot_data_download <- downloadHandler(
  filename = function(){"employment_plot_data.csv"},
  content = function(file){write.csv(emp_forecast_data %>%
                                       as_tibble() %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$emp_forecasts <- renderTable(emp_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# Annual Pay Page ---------------------------------------------------------

# history plot
output$pay_hist_plot <- renderPlotly(pay_hist_plot)

# history download button
output$pay_history_download <- downloadHandler(
  filename = function(){"average_annual_pay_history.csv"},
  content = function(file){write.csv(pay_hist %>%
                                       transmute(Year,
                                                 `Annual Pay Actual` = dollar(round(pay)),
                                                 `Annual Pay Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# forecast plot
output$pay_forecast_plot <- renderPlotly(pay_forecast_plot)

# plot data download
output$pay_plot_data_download <- downloadHandler(
  filename = function(){"annual_pay_plot_data.csv"},
  content = function(file){write.csv(pay_forecast_data %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$pay_forecasts <- renderTable(pay_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")


# Taxable Sales Page ------------------------------------------------------

# history plot
output$tax_sales_hist_plot <- renderPlotly(tax_sales_hist_plot)

# history download button
output$tax_sales_history_download <- downloadHandler(
  filename = function(){"annual_taxable_sales_history.csv"},
  content = function(file){write.csv(tax_sales_hist %>%
                                       as_tibble() %>%
                                       transmute(Year,
                                                 `Taxable Sales Actual` = dollar(round(sales)),
                                                 `Taxable Sales Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# forecast plot
output$tax_sales_forecast_plot <- renderPlotly(tax_sales_forecast_plot)

# plot data download button
output$tax_sales_plot_data_download <- downloadHandler(
  filename = function(){"taxable_sales_plot_data.csv"},
  content = function(file){write.csv(tax_sales_forecast_data %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )


# monthly plot
output$monthly_sales_plot <- renderPlotly(monthly_sales_plot)

# download download button
output$monthly_sales_history_download <- downloadHandler(
  filename = function(){"monthly_taxable_sales_history.csv"},
  content = function(file){write.csv(monthly_sales_hist %>%
                                       transmute(Date,
                                                 `Taxable Sales Actual` = dollar(round(sales)),
                                                 `Taxable Sales Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$tax_sales_forecasts <- renderTable(tax_sales_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# HPI Page ----------------------------------------------------------------

# HPI Levels plot
output$hpi_levels_plot <- renderPlotly(hpi_levels_plot)

# HPI levels download button
output$hpi_levels_download <- downloadHandler(
  filename = function(){"quarterly_hpi_levels.csv"},
  content = function(file){write.csv(hpi_levels %>%
                                       transmute(Date,
                                                 `HPI Level` = round(hpi, 2),
                                                 `HPI Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# change forecast plot
output$hpi_forecast_plot <- renderPlotly(hpi_forecast_plot)

# plot data download button
output$hpi_plot_data_download <- downloadHandler(
  filename = function(){"hpi_change_plot_data.csv"},
  content = function(file){write.csv(hpi_forecast_data %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )


# forecast table
output$hpi_forecasts <- renderTable(hpi_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  

# Population Page ---------------------------------------------------------

# history plot
output$pop_hist_plot <- renderPlotly(pop_hist_plot)

# history download button
output$pop_history_download <- downloadHandler(
  filename = function(){"population_history_data.csv"},
  content = function(file){write.csv(all_pop %>%
                                       filter(source == "Population Actual % Change") %>%
                                       transmute(Date = Year,
                                                 `Population Actual` = pop,
                                                 `Population Change` = percent(change, accuracy = 0.1)),
                                     file,
                                     row.names = FALSE)}
  )

# forecast plot
output$pop_forecast_plot <- renderPlotly(pop_forecast_plot)

# plot data download button
output$pop_plot_download <- downloadHandler(
  filename = function(){"population_change_plot_data.csv"},
  content = function(file){write.csv(all_pop %>% select(-pop) %>%
                                       # pivot wider on source
                                       pivot_wider(names_from = source,
                                                   values_from = change),
                                     file,
                                     row.names = FALSE)}
  )

# forecast table
output$pop_forecasts <- renderTable(pop_forecasts, striped = TRUE, bordered = TRUE, width = "100%", na = "")
  
}

shinyApp(ui = ui, server = server)
