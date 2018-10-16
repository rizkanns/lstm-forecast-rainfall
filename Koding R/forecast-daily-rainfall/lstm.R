## load dataset
perak <- read.csv("perak.csv", header = TRUE, col.names= c("NA", "ID", "DY", "TN","TM","TA","HA","RF","LG","VA","VX"))
head(perak)
str(perak)
dim(perak)
summary(perak)


## change date format
#perak$DY <- strptime(as.character(perak$DY), "%d/%m/%Y")
#format(perak$DY, "%Y-%m-%d")


## remove column, bcs its name are the same
perak <- data.frame(perak[,-1])


## check missing value
row.has.na <- apply(perak, 1, function(x){any(is.na(x))})
sum(row.has.na)
#perak2 <- perak[!row.has.na]
#perak2 <- 0


## draw plot perak - masi gabisa
require(stats)
plot(perak,xlab="DY",ylab="RF",col="darkblue")


####### LSTM #######
## prepare libraries
# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)


## add index as date tk_tbl format
perak2 <- perak %>%
  tk_tbl() %>%
  mutate(index = as_date(DY)) %>%
  as_tbl_time(index = index)

#perak <- NULL
#perak2 <- NULL


## draw plot perak2
p1 <- perak2 %>%
  ggplot(aes(index, RF)) + geom_point(color = palette_light()[[1]], alpha = 0.5) + theme_tq() + labs(title = "From 1987 to 2013 (Full Data Set)")
p2 <- perak2 %>%
  filter_time("start" ~ "1988") %>%
  ggplot(aes(index, RF)) + geom_line(color = palette_light()[[1]], alpha = 0.5) + geom_point(color = palette_light()[[1]]) + geom_smooth(method = "loess", span = 0.2, se = FALSE) + theme_tq() + labs(title = "1749 to 1800 (Zoomed In To Show Cycle)")
p_title <- ggdraw() + 
  draw_label("Perak", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))


## evaluating the ACF
tidy_acf <- function(perak2, RF, lags = 0:20)
{
  value_expr <- enquo(RF)
  
  acf_values <- perak2 %>%
    pull(RF) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}

max_lag <- 12 * 50

perak2 %>%
  tidy_acf(RF, lags = 0:max_lag)

#tidy_acf <- NULL


######## error ########
periods_train <- 12 * 50
periods_test  <- 12 * 10
skip_span     <- 12 * 20

rolling_origin_resamples <- rolling_origin(
  perak2,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

## plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14)
  {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = RF, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    perak2_time_summary <- perak2 %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(perak2_time_summary$start, 
                              perak2_time_summary$end))
  }
  
  return(g)
}

rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) + theme(legend.position = "bottom")



## Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, title = "Backtesting Strategy: Rolling Origin Sampling Plan")

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, title = "Backtesting Strategy: Zoomed In")
