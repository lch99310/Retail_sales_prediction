Retail sales prediction
================
Chung-Hao Lee
8/22/2021

``` r
### Setting up environment
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("tidymodels")
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 0.1.2 ──

    ## ✓ broom     0.7.8      ✓ recipes   0.1.15
    ## ✓ dials     0.0.9      ✓ rsample   0.1.1 
    ## ✓ infer     0.5.4      ✓ tune      0.1.6 
    ## ✓ modeldata 0.1.0      ✓ workflows 0.2.4 
    ## ✓ parsnip   0.1.5      ✓ yardstick 0.0.8

    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## x scales::discard() masks purrr::discard()
    ## x dplyr::filter()   masks stats::filter()
    ## x recipes::fixed()  masks stringr::fixed()
    ## x dplyr::lag()      masks stats::lag()
    ## x yardstick::spec() masks readr::spec()
    ## x recipes::step()   masks stats::step()

``` r
library("timetk")
library("modeltime")
library("patchwork")
```

``` r
knitr::opts_chunk$set(
  collapse = TRUE, 
  comment = "#>",
  fig.path = "fig/figures/README-",
  out.width = "100%",
  message=FALSE, 
  warning=FALSE
)
```

``` r
### Loading dataset

####Total Business Sales, Not Seasonally Adjusted
df_sales <- 
  read_csv("/.../TOTBUSSMNSA.csv") %>% 
  select(-c(realtime_start, realtime_end)) %>% 
  rename(sales = value) %>% 
  mutate(trend = row_number(),
         quarter = as.factor(lubridate::quarter(date)),
         month = as.factor(lubridate::month(date)))

df_inv <- 
  read_csv("/.../TOTBUSIMNSA.csv") %>% 
  select(-c(realtime_start, realtime_end))%>% 
  rename(inv = value) %>% 
  mutate(trend = row_number(),
         quarter = as.factor(lubridate::quarter(date)),
         month = as.factor(lubridate::month(date)))

str(df_sales)
#> tibble [333 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ sales  : num [1:333] 478951 496844 542833 533768 537400 ...
#>  $ date   : Date[1:333], format: "1992-01-01" "1992-02-01" ...
#>  $ trend  : int [1:333] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ quarter: Factor w/ 4 levels "1","2","3","4": 1 1 1 2 2 2 3 3 3 4 ...
#>  $ month  : Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
str(df_inv)
#> tibble [333 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ inv    : num [1:333] 802948 809329 813301 819247 815688 ...
#>  $ date   : Date[1:333], format: "1992-01-01" "1992-02-01" ...
#>  $ trend  : int [1:333] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ quarter: Factor w/ 4 levels "1","2","3","4": 1 1 1 2 2 2 3 3 3 4 ...
#>  $ month  : Factor w/ 12 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
```

``` r
### Plot sales and inv
df_sales %>% 
  plot_time_series(date, sales, .smooth=FALSE, .plotly_slider = T, .title = "Sales")
```

<div id="htmlwidget-1a86ea92a87a6df31433" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-1a86ea92a87a6df31433">{"x":{"data":[{"x":["1992-01-01","1992-02-01","1992-03-01","1992-04-01","1992-05-01","1992-06-01","1992-07-01","1992-08-01","1992-09-01","1992-10-01","1992-11-01","1992-12-01","1993-01-01","1993-02-01","1993-03-01","1993-04-01","1993-05-01","1993-06-01","1993-07-01","1993-08-01","1993-09-01","1993-10-01","1993-11-01","1993-12-01","1994-01-01","1994-02-01","1994-03-01","1994-04-01",...
df_inv %>% 
  plot_time_series(date, inv, .smooth=FALSE, .plotly_slider = T, .title = "Inventory")
<div id="htmlwidget-5abf360a3ca9acbfbc81" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-5abf360a3ca9acbfbc81">{"x":{"data":[{"x":["1992-01-01","1992-02-01","1992-03-01","1992-04-01","1992-05-01","1992-06-01","1992-07-01","1992-08-01","1992-09-01","1992-10-01","1992-11-01","1992-12-01","1993-01-01","1993-02-01","1993-03-01","1993-04-01","1993-05-01","1993-06-01","1993-07-01","1993-08-01","1993-09-01","1993-10-01","1993-11-01","1993-12-01","1994-01-01","1994-02-01","1994-03-01","1994-04-01",...

-   We can see that both sales and inventory have similar patterns and
    have drops after 2008.

``` r
###Seasonal plot for US retail sales from 2000
df_sales %>% 
  filter(date >= '2000-01-01') %>% 
  group_by(lubridate::year(date)) %>% 
  mutate(sales = scale(sales) - scale(sales)[1]) %>% 
  ungroup() %>% 
  plot_time_series(month, sales, 
                   .smooth= FALSE, 
                   .color_var = lubridate::year(date), 
                   .interactive = FALSE, 
                   .color_lab = "Year", 
                   .title='Seasonal plot for US retail sales from 2000',
                   .y_lab='Sales', 
                   .x_lab='Month')
```

<img src="fig/figures/README-unnamed-chunk-5-1.png" width="100%" /> \*
Excpt 2008, every sales have upward trends in a year since 2000.

``` r
# Create a seasonal subseries plot for the subset data starting from 2000

df_sales %>%
  filter(date >= '2000-01-01') %>%
  plot_time_series(
        .date = date,
        .value = sales,
        .facet_vars = month,
        .facet_ncol = 12, 
        .facet_scales = "fixed",
        .interactive = FALSE,
        .legend_show = FALSE,
        .title = "Seasonal subseries plot for US retail sales from 2000",
        .x_lab = "Year",
        .y_lab = "Sales ($ million)")  + 
        theme(axis.text.x=element_text(angle=60, hjust=1)
    )
```

<img src="fig/figures/README-unnamed-chunk-6-1.png" width="100%" /> \*
We can see that in every months, sales increases since 2000.

``` r
# Create a STL decomposition plot for the full data
df_sales %>%
  plot_stl_diagnostics(
    date, sales,
    # Set features to return, desired frequency and trend
    .feature_set = c("observed", "season", "trend", "remainder"), 
    .interactive = FALSE)
```

<img src="fig/figures/README-unnamed-chunk-7-1.png" width="100%" /> \*
We can see in STL plot, sales has clear seasonal pattern and upward
trend.

``` r
### ACF and PACF
df_sales%>%
  plot_acf_diagnostics(date, sales, 
                       .lags = 48,
                       .show_white_noise_bars =TRUE)
```

<div id="htmlwidget-80615ee277f8a612f716" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-80615ee277f8a612f716">{"x":{"data":[{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"visible":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","mode":"","frame":null},...

-   ACF shows that sales are self-related and has seasonal pattern every
    12 lags.

``` r
# Build an ARIMA model

arima_auto <- 
  arima_reg() %>% 
  set_engine("auto_arima") %>%
  fit(sales ~ date, data = df_sales)

arima_auto
#> parsnip model object
#> 
#> Fit time:  1.7s 
#> Series: outcome 
#> ARIMA(2,0,3)(2,1,1)[12] with drift 
#> 
#> Coefficients:
#>          ar1     ar2     ma1      ma2     ma3    sar1     sar2     sma1
#>       0.1924  0.6373  0.4777  -0.0639  0.4272  0.4033  -0.3812  -0.7251
#> s.e.  0.0598  0.0580  0.0764   0.0620  0.0550  0.0656   0.0587   0.0437
#>           drift
#>       2767.6500
#> s.e.   331.9943
#> 
#> sigma^2 estimated as 473230856:  log likelihood=-3665.16
#> AIC=7350.31   AICc=7351.02   BIC=7388.03
```

``` r
# Plot the residuals

models_tbl <- modeltime_table(
  arima_auto
)

models_tbl %>%
    modeltime_calibrate(new_data = df_sales) %>%
    modeltime_residuals() %>%
    plot_modeltime_residuals(.interactive = FALSE)
```

<img src="fig/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
# Unit root test: ADF test

df_sales %>%
  select(sales) %>% 
  ts(start = c(1992, 1), end = c(2019, 9), frequency = 12) %>% 
  tseries::adf.test()
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  .
#> Dickey-Fuller = -3.5173, Lag order = 6, p-value = 0.04129
#> alternative hypothesis: stationary
```

-   From ADF test, p-value &lt; 0.05. We can say it’s stationary.

``` r
# Run ADF test again after first order differencing

df_sales %>%
  mutate(diffsales = diff_vec(sales)) %>% 
  select(diffsales) %>% 
  drop_na() %>% 
  ts(start = c(1992, 1), end = c(2019, 9), frequency = 12) %>% 
  tseries::adf.test()
#> 
#>  Augmented Dickey-Fuller Test
#> 
#> data:  .
#> Dickey-Fuller = -7.625, Lag order = 6, p-value = 0.01
#> alternative hypothesis: stationary
```

-   After first order differencing, p-value is smaller than before. We
    can say first order differencing is closer to stationary.

``` r
# ACF and PACT after 1st order ordinary differencing

df_sales%>%
  plot_acf_diagnostics(date, diff_vec(df_sales$sales, difference = 1), 
                       .lags = 48,
                       .show_white_noise_bars =TRUE)
```

<div id="htmlwidget-19c191ff69b782c7e0ea" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-19c191ff69b782c7e0ea">{"x":{"data":[{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"visible":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","mode":"","frame":null},...

-   In ACFand PACF plots, even 12, 24, 36, 48 have peaks over threshold,
    it is closer to stationary compared to before first order
    differencing.

``` r
# After 1st order seasonal differencing

df_sales%>%
  plot_acf_diagnostics(date, diff_vec(df_sales$sales, lag = 12, difference = 1), 
                       .lags = 48,
                       .show_white_noise_bars =TRUE)
```

<div id="htmlwidget-e744e8aa311bc8705690" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-e744e8aa311bc8705690">{"x":{"data":[{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"visible":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","mode":"","frame":null},...

-   Because from previous ACF, we know that sales has seasonal patterns
    (lag = 12). We apply seasonal differencing and do ACF again. This
    time, no periodical peaks appear. But most lags are out of therhold
    in ACF. So I do 1st order ordinary differencing and 1st order
    seasonal differencing next.

``` r
# After 1st order ordinary differencing and 1st order seasonal differencing

df_sales%>%
  mutate(difford1 = diff_vec(sales, difference = 1), difford1seas1 = diff_vec(difford1, lag = 12, difference = 1)) %>% 
  plot_acf_diagnostics(date, difford1seas1, 
                       .lags = 48,
                       .show_white_noise_bars =TRUE)
```

<div id="htmlwidget-4089686aeb7680593bfe" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-4089686aeb7680593bfe">{"x":{"data":[{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"visible":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","mode":"","frame":null},...

``` r
# After 1st order ordinary differencing and 2nd order seasonal differencing

df_sales%>%
  mutate(difford1 = diff_vec(sales, difference = 1), difford1seas1 = diff_vec(difford1, lag = 12, difference = 2)) %>% 
  plot_acf_diagnostics(date, difford1seas1, 
                       .lags = 48,
                       .show_white_noise_bars =TRUE)
```

<div id="htmlwidget-c9823a1c07b737bda232" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-c9823a1c07b737bda232">{"x":{"data":[{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-2.4,50.4],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"visible":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","mode":"","frame":null},...

-   In 1st order ordinary differencing and 1st order seasonal
    differencing, we see that the effect is not quite good, so I add one
    more on seasonal differencing and do 1st order ordinary differencing
    and 2nd order seasonal differencing. This time, we have a better
    result and more lags are fall inside therhold.

# Prediction

``` r
# Split the data

df_train <- df_sales %>% filter(date < '2010-01-01')
df_test<- df_sales %>% filter(date >= '2010-01-01')
```

``` r
# Auto ARIMA: gird search q, d, p
auto_arima <- 
  arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(sales ~ date, data = df_train)

auto_arima
#> parsnip model object
#> 
#> Fit time:  1.8s 
#> Series: outcome 
#> ARIMA(4,0,2)(0,1,2)[12] with drift 
#> 
#> Coefficients:
#>          ar1     ar2     ar3      ar4      ma1      ma2     sma1     sma2
#>       0.7929  0.4564  0.2543  -0.6065  -0.1124  -0.1886  -0.3987  -0.2931
#> s.e.  0.1070  0.1694  0.1187   0.0620   0.1339   0.1684   0.0868   0.0931
#>           drift
#>       2868.5875
#> s.e.   301.2213
#> 
#> sigma^2 estimated as 389397010:  log likelihood=-2308.12
#> AIC=4636.24   AICc=4637.38   BIC=4669.42
```

``` r
# Model table and calibration

models_tbl <- modeltime_table(auto_arima)

calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = df_test)

calibration_tbl %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(.interactive = FALSE)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#tkaqozothf .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tkaqozothf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tkaqozothf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tkaqozothf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tkaqozothf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tkaqozothf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tkaqozothf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tkaqozothf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tkaqozothf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tkaqozothf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tkaqozothf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tkaqozothf .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#tkaqozothf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tkaqozothf .gt_from_md > :first-child {
  margin-top: 0;
}

#tkaqozothf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tkaqozothf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tkaqozothf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#tkaqozothf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tkaqozothf .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#tkaqozothf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tkaqozothf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tkaqozothf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tkaqozothf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tkaqozothf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tkaqozothf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#tkaqozothf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tkaqozothf .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#tkaqozothf .gt_left {
  text-align: left;
}

#tkaqozothf .gt_center {
  text-align: center;
}

#tkaqozothf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tkaqozothf .gt_font_normal {
  font-weight: normal;
}

#tkaqozothf .gt_font_bold {
  font-weight: bold;
}

#tkaqozothf .gt_font_italic {
  font-style: italic;
}

#tkaqozothf .gt_super {
  font-size: 65%;
}

#tkaqozothf .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="tkaqozothf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="9" class="gt_heading gt_title gt_font_normal" style>Accuracy Table</th>
    </tr>
    <tr>
      <th colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">.model_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.model_desc</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mae</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mape</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mase</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">smape</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rmse</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rsq</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_left">ARIMA(4,0,2)(0,1,2)[12] WITH DRIFT</td>
      <td class="gt_row gt_left">Test</td>
      <td class="gt_row gt_right">52914.2</td>
      <td class="gt_row gt_right">4.26</td>
      <td class="gt_row gt_right">0.82</td>
      <td class="gt_row gt_right">4.18</td>
      <td class="gt_row gt_right">64320.02</td>
      <td class="gt_row gt_right">0.73</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
### Check forcast with actual data
calibration_tbl %>%
    modeltime_forecast(
        new_data    = df_test,
        actual_data = df_sales
    ) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25,
      .interactive      = FALSE
    )
```

<img src="fig/figures/README-unnamed-chunk-20-1.png" width="100%" /> \*
Well, actually we forsee the trend and seasonal patterns are there, but
it just not quite fit. Maybe because 2008 was once in a lifetime crisis
and our split time 2010 was not far away from 2008, so it may have some
deviation.

``` r
# Split the data on 2012

df_train_12 <- df_sales %>% filter(date < '2012-01-01')
df_test_12 <- df_sales %>% filter(date >= '2012-01-01')
```

``` r
# Auto ARIMA: gird search q, d, p
auto_arima_12 <- 
  arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(sales ~ date, data = df_train_12)

auto_arima_12
#> parsnip model object
#> 
#> Fit time:  5.3s 
#> Series: outcome 
#> ARIMA(2,0,3)(0,1,2)[12] with drift 
#> 
#> Coefficients:
#>          ar1     ar2     ma1      ma2     ma3     sma1     sma2      drift
#>       0.5776  0.2969  0.1192  -0.0371  0.5759  -0.4383  -0.3160  2826.3247
#> s.e.  0.1377  0.1375  0.1255   0.0626  0.0666   0.0718   0.0684   448.7548
#> 
#> sigma^2 estimated as 419675549:  log likelihood=-2589.03
#> AIC=5196.06   AICc=5196.88   BIC=5226.92
```

``` r
# Model table and calibration

models_tbl_12 <- modeltime_table(auto_arima_12)

calibration_tbl_12 <- models_tbl_12 %>%
    modeltime_calibrate(new_data = df_test_12)

calibration_tbl_12 %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(.interactive = FALSE)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vgzqsaotfi .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vgzqsaotfi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vgzqsaotfi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vgzqsaotfi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vgzqsaotfi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vgzqsaotfi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vgzqsaotfi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vgzqsaotfi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vgzqsaotfi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vgzqsaotfi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vgzqsaotfi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vgzqsaotfi .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#vgzqsaotfi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vgzqsaotfi .gt_from_md > :first-child {
  margin-top: 0;
}

#vgzqsaotfi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vgzqsaotfi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vgzqsaotfi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#vgzqsaotfi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vgzqsaotfi .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#vgzqsaotfi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vgzqsaotfi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vgzqsaotfi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vgzqsaotfi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vgzqsaotfi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vgzqsaotfi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#vgzqsaotfi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vgzqsaotfi .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#vgzqsaotfi .gt_left {
  text-align: left;
}

#vgzqsaotfi .gt_center {
  text-align: center;
}

#vgzqsaotfi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vgzqsaotfi .gt_font_normal {
  font-weight: normal;
}

#vgzqsaotfi .gt_font_bold {
  font-weight: bold;
}

#vgzqsaotfi .gt_font_italic {
  font-style: italic;
}

#vgzqsaotfi .gt_super {
  font-size: 65%;
}

#vgzqsaotfi .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="vgzqsaotfi" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="9" class="gt_heading gt_title gt_font_normal" style>Accuracy Table</th>
    </tr>
    <tr>
      <th colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">.model_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.model_desc</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mae</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mape</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mase</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">smape</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rmse</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rsq</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_left">ARIMA(2,0,3)(0,1,2)[12] WITH DRIFT</td>
      <td class="gt_row gt_left">Test</td>
      <td class="gt_row gt_right">38777.74</td>
      <td class="gt_row gt_right">2.95</td>
      <td class="gt_row gt_right">0.57</td>
      <td class="gt_row gt_right">2.92</td>
      <td class="gt_row gt_right">47345.09</td>
      <td class="gt_row gt_right">0.74</td>
    </tr>
  </tbody>
  
  
</table></div>

-   We have smaller mae, rmse. Good sign of it.

``` r
### Check forcast with actual data
calibration_tbl_12 %>%
    modeltime_forecast(
        new_data    = df_test_12,
        actual_data = df_sales
    ) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25,
      .interactive      = FALSE
    )
```

<img src="fig/figures/README-unnamed-chunk-24-1.png" width="100%" /> \*
Bang!!! The trend and patterns look quite well and we have a better
prediction that fit the actual data.

Next, I’d like to add another variable to see if they can improve
prediction model.

``` r
# importing another dataset
df_econ <- 
  read_csv("/.../us-econ.csv") %>% 
  mutate(date = lubridate::mdy(date),
  trend = row_number()) %>% 
  filter(date >= '1992-01-01', date <= '2019-09-01') 

skimr::skim(df_econ)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | df\_econ |
| Number of rows                                   | 333      |
| Number of columns                                | 13       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| Date                                             | 1        |
| numeric                                          | 12       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: Date**

| skim\_variable | n\_missing | complete\_rate | min        | max        | median     | n\_unique |
|:---------------|-----------:|---------------:|:-----------|:-----------|:-----------|----------:|
| date           |          0 |              1 | 1992-01-01 | 2019-09-01 | 2005-11-01 |       333 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |      mean |       sd |        p0 |       p25 |       p50 |       p75 |      p100 | hist  |
|:---------------|-----------:|---------------:|----------:|---------:|----------:|----------:|----------:|----------:|----------:|:------|
| income         |          0 |              1 |  11067.87 |  3798.60 |   5264.20 |   7793.50 |  10890.40 |  13996.60 |  18645.10 | ▇▆▆▅▃ |
| saving         |          0 |              1 |      6.32 |     1.60 |      2.20 |      5.30 |      6.60 |      7.30 |     12.00 | ▂▅▇▁▁ |
| unemployment   |          0 |              1 |      5.83 |     1.63 |      3.50 |      4.60 |      5.50 |      6.70 |     10.00 | ▇▇▂▂▂ |
| labor          |          0 |              1 |     65.35 |     1.62 |     62.40 |     63.70 |     66.10 |     66.60 |     67.30 | ▅▂▁▆▇ |
| tenYearTeasury |          0 |              1 |      1.20 |     0.89 |     -0.41 |      0.36 |      1.22 |      1.98 |      2.83 | ▆▇▆▇▆ |
| CPI            |          0 |              1 |    197.31 |    35.13 |    138.10 |    164.00 |    198.30 |    230.28 |    256.76 | ▇▇▅▇▇ |
| inflation      |          0 |              1 |      2.28 |     1.11 |     -2.10 |      1.70 |      2.30 |      3.00 |      5.60 | ▁▁▇▇▁ |
| veichleSales   |          0 |              1 |     15.87 |     2.10 |      9.22 |     14.83 |     16.44 |     17.41 |     22.06 | ▁▂▇▇▁ |
| houseSales     |          0 |              1 |    707.40 |   265.22 |    270.00 |    502.00 |    665.00 |    882.00 |   1389.00 | ▆▇▇▃▂ |
| HPI            |          0 |              1 |    136.53 |    41.20 |     75.66 |     92.45 |    143.12 |    171.87 |    211.99 | ▇▃▆▇▂ |
| population     |          0 |              1 | 295735.00 | 21624.65 | 255331.00 | 277658.00 | 297089.00 | 314905.00 | 328897.00 | ▅▅▆▆▇ |
| trend          |          0 |              1 |    167.00 |    96.27 |      1.00 |     84.00 |    167.00 |    250.00 |    333.00 | ▇▇▇▇▇ |

``` r
# merge all tables
df_merge <- 
  df_sales %>% 
  bind_cols(df_inv)

df_merge_1 <- 
  inner_join(df_merge, df_econ, by = c("trend...3" = "trend")) %>% 
  select(sales, date...2, trend...3, quarter...4, month...5, inv, income, saving, unemployment, CPI, inflation, population, HPI) %>% 
  rename(date = date...2, trend = trend...3, quarter = quarter...4, month = month...5)
```

``` r
linear_model <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(sales ~ date + CPI + income + saving + HPI + population + inflation + trend + inv, data = df_merge_1)

summary(linear_model$fit)
#> 
#> Call:
#> stats::lm(formula = sales ~ date + CPI + income + saving + HPI + 
#>     population + inflation + trend + inv, data = data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -154367  -28824    1977   27047  124123 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.167e+08  2.868e+07  11.042  < 2e-16 ***
#> date        -3.988e+04  3.584e+03 -11.127  < 2e-16 ***
#> CPI          7.078e+03  1.172e+03   6.039 4.27e-09 ***
#> income      -2.816e+01  1.699e+01  -1.657   0.0984 .  
#> saving       3.013e+03  3.364e+03   0.896   0.3712    
#> HPI          4.255e+02  2.540e+02   1.675   0.0948 .  
#> population   6.757e+00  3.527e+00   1.916   0.0563 .  
#> inflation    1.811e+04  3.120e+03   5.803 1.56e-08 ***
#> trend        1.212e+06  1.092e+05  11.102  < 2e-16 ***
#> inv          5.138e-01  7.013e-02   7.326 1.91e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 44470 on 323 degrees of freedom
#> Multiple R-squared:  0.9754, Adjusted R-squared:  0.9747 
#> F-statistic:  1424 on 9 and 323 DF,  p-value: < 2.2e-16
```

``` r
# Build an ARIMA model with another variables

arima_auto_var <- 
  arima_reg() %>% 
  set_engine("auto_arima") %>%
  fit(sales ~ date + CPI + income + saving + HPI + population + inflation + trend + inv, data = df_merge_1)

arima_auto_var_imp <- 
  arima_reg() %>% 
  set_engine("auto_arima") %>%
  fit(sales ~ date + CPI + income + saving + HPI + population , data = df_merge_1)

arima_auto_var_imp_2 <- 
  arima_reg() %>% 
  set_engine("auto_arima") %>%
  fit(sales ~ date + income, data = df_merge_1)

arima_auto
#> parsnip model object
#> 
#> Fit time:  1.7s 
#> Series: outcome 
#> ARIMA(2,0,3)(2,1,1)[12] with drift 
#> 
#> Coefficients:
#>          ar1     ar2     ma1      ma2     ma3    sar1     sar2     sma1
#>       0.1924  0.6373  0.4777  -0.0639  0.4272  0.4033  -0.3812  -0.7251
#> s.e.  0.0598  0.0580  0.0764   0.0620  0.0550  0.0656   0.0587   0.0437
#>           drift
#>       2767.6500
#> s.e.   331.9943
#> 
#> sigma^2 estimated as 473230856:  log likelihood=-3665.16
#> AIC=7350.31   AICc=7351.02   BIC=7388.03
arima_auto_var
#> parsnip model object
#> 
#> Fit time:  406ms 
#> Series: outcome 
#> Regression with ARIMA(0,0,0) errors 
#> 
#> Coefficients:
#>            cpi    income     saving       hpi  population  inflation     trend
#>       9378.223  -29.5260  -4190.980  274.8591     -3.7539  15436.742  -37.6184
#> s.e.  1345.312   17.0678   3584.852  295.7047      0.6747   3578.792  584.2893
#>          inv
#>       0.4008
#> s.e.  0.0809
#> 
#> sigma^2 estimated as 2.755e+09:  log likelihood=-4087.64
#> AIC=8193.28   AICc=8193.84   BIC=8227.55
arima_auto_var_imp
#> parsnip model object
#> 
#> Fit time:  13.9s 
#> Series: outcome 
#> Regression with ARIMA(4,0,2)(2,1,1)[12] errors 
#> 
#> Coefficients:
#>          ar1     ar2     ar3      ar4      ma1      ma2    sar1     sar2
#>       0.9186  0.2382  0.3245  -0.5324  -0.6029  -0.0935  0.4031  -0.3408
#> s.e.  0.1089  0.1419  0.0735   0.0584   0.1328   0.1193  0.0817   0.0635
#>          sma1      drift        cpi   income     saving       hpi  population
#>       -0.6559  -6788.860  10931.593  77.4100  -7502.092  -57.8182     11.6423
#> s.e.   0.0535   1824.815   1002.803  13.5789   1502.696  517.6480      6.8118
#> 
#> sigma^2 estimated as 305844055:  log likelihood=-3588.64
#> AIC=7209.29   AICc=7211.07   BIC=7269.63
arima_auto_var_imp_2
#> parsnip model object
#> 
#> Fit time:  2.8s 
#> Series: outcome 
#> Regression with ARIMA(2,0,3)(2,1,2)[12] errors 
#> 
#> Coefficients:
#>          ar1     ar2     ma1      ma2     ma3    sar1     sar2     sma1   sma2
#>       0.4334  0.4192  0.2857  -0.0107  0.3602  0.8735  -0.7087  -1.3094  0.621
#> s.e.  0.1398  0.1298  0.1375   0.0626  0.0514  0.0659   0.0507   0.0861  0.070
#>           drift   income
#>       2246.8939  11.6691
#> s.e.   623.2696  12.0815
#> 
#> sigma^2 estimated as 414310833:  log likelihood=-3645.5
#> AIC=7315.01   AICc=7316.02   BIC=7360.27
```

-   After adding variables, we got better AIC and BIC.

``` r
# Plot the residuals

models_tbl_var <- modeltime_table(
  arima_auto,
  arima_auto_var,
  arima_auto_var_imp,
  arima_auto_var_imp_2
)

models_tbl_var %>%
    modeltime_calibrate(new_data = df_merge_1) %>%
    modeltime_residuals() %>%
    plot_modeltime_residuals(.interactive = FALSE)
```

<img src="fig/figures/README-unnamed-chunk-28-1.png" width="100%" /> \*
From residue plot, we can see a better outcome of arima with variables.

# Prediction with variables

``` r
# Split the data on 2012

df_train_12_var <- df_merge_1 %>% filter(date < '2012-01-01')
df_test_12_var <- df_merge_1 %>% filter(date >= '2012-01-01')
```

``` r
# Auto ARIMA: gird search q, d, p
auto_arima_12_var <- 
  arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(sales ~ date + CPI + income + saving + HPI + population + inflation + trend + inv, data = df_train_12_var)

auto_arima_12_var_imp <- 
  arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(sales ~ date + CPI + income + saving + HPI + population, data = df_train_12_var)

auto_arima_12_var_imp_2 <- 
  arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(sales ~ date + income, data = df_train_12_var)

auto_arima_12_var
#> parsnip model object
#> 
#> Fit time:  376ms 
#> Series: outcome 
#> Regression with ARIMA(0,0,0) errors 
#> 
#> Coefficients:
#>            cpi   income     saving       hpi  population  inflation      trend
#>       3686.915  53.3704  -8242.148  -49.3660     -1.5889  17380.281  -807.2711
#> s.e.  1639.476  26.2597   3830.151  281.0902      0.6874   3442.307   661.1804
#>          inv
#>       0.2283
#> s.e.  0.1006
#> 
#> sigma^2 estimated as 2.049e+09:  log likelihood=-2909.33
#> AIC=5836.66   AICc=5837.44   BIC=5867.98
auto_arima_12_var_imp
#> parsnip model object
#> 
#> Fit time:  3.6s 
#> Series: outcome 
#> Regression with ARIMA(2,0,3)(2,1,1)[12] errors 
#> 
#> Coefficients:
#>          ar1     ar2     ma1      ma2     ma3    sar1     sar2     sma1
#>       0.4438  0.4846  0.0145  -0.2744  0.4872  0.3842  -0.3414  -0.7059
#> s.e.  0.1030  0.1054  0.1032   0.0778  0.0643  0.0849   0.0742   0.0627
#>             cpi   income     saving       hpi  population
#>       10988.518  80.3603  -6792.740  1256.396    -18.5603
#> s.e.   1016.063  18.4676   1542.031  1011.863      3.4404
#> 
#> sigma^2 estimated as 251485161:  log likelihood=-2527.84
#> AIC=5083.67   AICc=5085.65   BIC=5131.68
auto_arima_12_var_imp_2
#> parsnip model object
#> 
#> Fit time:  1.7s 
#> Series: outcome 
#> Regression with ARIMA(2,0,3)(2,1,1)[12] errors 
#> 
#> Coefficients:
#>          ar1     ar2     ma1      ma2     ma3    sar1     sar2     sma1
#>       0.5852  0.2750  0.1182  -0.0378  0.5173  0.2744  -0.3130  -0.7275
#> s.e.  0.1492  0.1463  0.1349   0.0607  0.0789  0.0805   0.0714   0.0566
#>           drift   income
#>       1685.5305  31.7074
#> s.e.   805.7125  20.3012
#> 
#> sigma^2 estimated as 401392696:  log likelihood=-2583.81
#> AIC=5189.62   AICc=5190.84   BIC=5227.34
```

``` r
# Model table and calibration

models_tbl_12_var <- modeltime_table(
  auto_arima_12, 
  auto_arima_12_var, 
  auto_arima_12_var_imp,
  auto_arima_12_var_imp_2)

calibration_tbl_12_var <- models_tbl_12_var %>%
    modeltime_calibrate(new_data = df_test_12_var)

calibration_tbl_12_var %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(.interactive = FALSE)
```

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gdgpvpgfxo .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gdgpvpgfxo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gdgpvpgfxo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gdgpvpgfxo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gdgpvpgfxo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gdgpvpgfxo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gdgpvpgfxo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gdgpvpgfxo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gdgpvpgfxo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gdgpvpgfxo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gdgpvpgfxo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gdgpvpgfxo .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#gdgpvpgfxo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gdgpvpgfxo .gt_from_md > :first-child {
  margin-top: 0;
}

#gdgpvpgfxo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gdgpvpgfxo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gdgpvpgfxo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#gdgpvpgfxo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gdgpvpgfxo .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gdgpvpgfxo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gdgpvpgfxo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gdgpvpgfxo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gdgpvpgfxo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gdgpvpgfxo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gdgpvpgfxo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gdgpvpgfxo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gdgpvpgfxo .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gdgpvpgfxo .gt_left {
  text-align: left;
}

#gdgpvpgfxo .gt_center {
  text-align: center;
}

#gdgpvpgfxo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gdgpvpgfxo .gt_font_normal {
  font-weight: normal;
}

#gdgpvpgfxo .gt_font_bold {
  font-weight: bold;
}

#gdgpvpgfxo .gt_font_italic {
  font-style: italic;
}

#gdgpvpgfxo .gt_super {
  font-size: 65%;
}

#gdgpvpgfxo .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="gdgpvpgfxo" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="9" class="gt_heading gt_title gt_font_normal" style>Accuracy Table</th>
    </tr>
    <tr>
      <th colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style></th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">.model_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.model_desc</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">.type</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mae</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mape</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">mase</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">smape</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rmse</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">rsq</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_center">1</td>
      <td class="gt_row gt_left">ARIMA(2,0,3)(0,1,2)[12] WITH DRIFT</td>
      <td class="gt_row gt_left">Test</td>
      <td class="gt_row gt_right">38777.74</td>
      <td class="gt_row gt_right">2.95</td>
      <td class="gt_row gt_right">0.57</td>
      <td class="gt_row gt_right">2.92</td>
      <td class="gt_row gt_right">47345.09</td>
      <td class="gt_row gt_right">0.74</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">2</td>
      <td class="gt_row gt_left">REGRESSION WITH ARIMA(0,0,0) ERRORS</td>
      <td class="gt_row gt_left">Test</td>
      <td class="gt_row gt_right">72235.92</td>
      <td class="gt_row gt_right">5.51</td>
      <td class="gt_row gt_right">1.07</td>
      <td class="gt_row gt_right">5.37</td>
      <td class="gt_row gt_right">89796.32</td>
      <td class="gt_row gt_right">0.44</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">3</td>
      <td class="gt_row gt_left">REGRESSION WITH ARIMA(2,0,3)(2,1,1)[12] ERRORS</td>
      <td class="gt_row gt_left">Test</td>
      <td class="gt_row gt_right">110028.13</td>
      <td class="gt_row gt_right">8.09</td>
      <td class="gt_row gt_right">1.63</td>
      <td class="gt_row gt_right">7.61</td>
      <td class="gt_row gt_right">139619.09</td>
      <td class="gt_row gt_right">0.69</td>
    </tr>
    <tr>
      <td class="gt_row gt_center">4</td>
      <td class="gt_row gt_left">REGRESSION WITH ARIMA(2,0,3)(2,1,1)[12] ERRORS</td>
      <td class="gt_row gt_left">Test</td>
      <td class="gt_row gt_right">53328.63</td>
      <td class="gt_row gt_right">4.04</td>
      <td class="gt_row gt_right">0.79</td>
      <td class="gt_row gt_right">3.97</td>
      <td class="gt_row gt_right">62090.76</td>
      <td class="gt_row gt_right">0.70</td>
    </tr>
  </tbody>
  
  
</table></div>

``` r
### Check forcast with actual data
calibration_tbl_12_var %>%
    modeltime_forecast(
        
        actual_data = df_merge_1,
        h = 60
    ) %>%
    plot_modeltime_forecast(
      .legend_max_width = 25,
      .interactive      = FALSE
    )
```

<img src="fig/figures/README-unnamed-chunk-32-1.png" width="100%" />

``` r
models_tbl <- modeltime_table(
  arima_auto)


fcast <- 
  modeltime_forecast(models_tbl, actual_data = df_merge_1, , h = 100)

plot_modeltime_forecast(fcast)
```

<div id="htmlwidget-55e178572faa055ace0f" style="width:100%;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-55e178572faa055ace0f">{"x":{"data":[{"x":["1992-01-01","1992-02-01","1992-03-01","1992-04-01","1992-05-01","1992-06-01","1992-07-01","1992-08-01","1992-09-01","1992-10-01","1992-11-01","1992-12-01","1993-01-01","1993-02-01","1993-03-01","1993-04-01","1993-05-01","1993-06-01","1993-07-01","1993-08-01","1993-09-01","1993-10-01","1993-11-01","1993-12-01","1994-01-01","1994-02-01","1994-03-01","1994-04-01","1994-05-01","1994-06-01","1994-07-01","1994-08-01","1994-09-01","1994-10-01","1994-11-01","1994-12-01","1995-01-01","1995-02-01","1995-03-01","1995-04-01","1995-05-01","1995-06-01","1995-07-01","1995-08-01","1995-09-01","1995-10-01","1995-11-01","1995-12-01","1996-01-01","1996-02-01","1996-03-01","1996-04-01","1996-05-01","1996-06-01","1996-07-01","1996-08-01","1996-09-01","1996-10-01","1996-11-01","1996-12-01","1997-01-01","1997-02-01","1997-03-01","1997-04-01","1997-05-01","1997-06-01","1997-07-01","1997-08-01","1997-09-01","1997-10-01","1997-11-01","1997-12-01","1998-01-01","1998-02-01","1998-03-01","1998-04-01","1998-05-01","1998-06-01","1998-07-01","1998-08-01","1998-09-01","1998-10-01","1998-11-01","1998-12-01","1999-01-01","1999-02-01","1999-03-01","1999-04-01","1999-05-01","1999-06-01","1999-07-01","1999-08-01","1999-09-01","1999-10-01","1999-11-01","1999-12-01","2000-01-01","2000-02-01","2000-03-01","2000-04-01","2000-05-01","2000-06-01","2000-07-01","2000-08-01","2000-09-01","2000-10-01","2000-11-01","2000-12-01","2001-01-01","2001-02-01","2001-03-01","2001-04-01","2001-05-01","2001-06-01","2001-07-01","2001-08-01","2001-09-01","2001-10-01","2001-11-01","2001-12-01","2002-01-01","2002-02-01","2002-03-01","2002-04-01","2002-05-01","2002-06-01","2002-07-01","2002-08-01","2002-09-01","2002-10-01","2002-11-01","2002-12-01","2003-01-01","2003-02-01","2003-03-01","2003-04-01","2003-05-01","2003-06-01","2003-07-01","2003-08-01","2003-09-01","2003-10-01","2003-11-01","2003-12-01","2004-01-01","2004-02-01","2004-03-01","2004-04-01","2004-05-01","2004-06-01","2004-07-01","2004-08-01","2004-09-01","2004-10-01","2004-11-01","2004-12-01","2005-01-01","2005-02-01","2005-03-01","2005-04-01","2005-05-01","2005-06-01","2005-07-01","2005-08-01","2005-09-01","2005-10-01","2005-11-01","2005-12-01","2006-01-01","2006-02-01","2006-03-01","2006-04-01","2006-05-01","2006-06-01","2006-07-01","2006-08-01","2006-09-01","2006-10-01","2006-11-01","2006-12-01","2007-01-01","2007-02-01","2007-03-01","2007-04-01","2007-05-01","2007-06-01","2007-07-01","2007-08-01","2007-09-01","2007-10-01","2007-11-01","2007-12-01","2008-01-01","2008-02-01","2008-03-01","2008-04-01","2008-05-01","2008-06-01","2008-07-01","2008-08-01","2008-09-01","2008-10-01","2008-11-01","2008-12-01","2009-01-01","2009-02-01","2009-03-01","2009-04-01","2009-05-01","2009-06-01","2009-07-01","2009-08-01","2009-09-01","2009-10-01","2009-11-01","2009-12-01","2010-01-01","2010-02-01","2010-03-01","2010-04-01","2010-05-01","2010-06-01","2010-07-01","2010-08-01","2010-09-01","2010-10-01","2010-11-01","2010-12-01","2011-01-01","2011-02-01","2011-03-01","2011-04-01","2011-05-01","2011-06-01","2011-07-01","2011-08-01","2011-09-01","2011-10-01","2011-11-01","2011-12-01","2012-01-01","2012-02-01","2012-03-01","2012-04-01","2012-05-01","2012-06-01","2012-07-01","2012-08-01","2012-09-01","2012-10-01","2012-11-01","2012-12-01","2013-01-01","2013-02-01","2013-03-01","2013-04-01","2013-05-01","2013-06-01","2013-07-01","2013-08-01","2013-09-01","2013-10-01","2013-11-01","2013-12-01","2014-01-01","2014-02-01","2014-03-01","2014-04-01","2014-05-01","2014-06-01","2014-07-01","2014-08-01","2014-09-01","2014-10-01","2014-11-01","2014-12-01","2015-01-01","2015-02-01","2015-03-01","2015-04-01","2015-05-01","2015-06-01","2015-07-01","2015-08-01","2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01","2016-04-01","2016-05-01","2016-06-01","2016-07-01","2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01","2017-06-01","2017-07-01","2017-08-01","2017-09-01","2017-10-01","2017-11-01","2017-12-01","2018-01-01","2018-02-01","2018-03-01","2018-04-01","2018-05-01","2018-06-01","2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01","2018-12-01","2019-01-01","2019-02-01","2019-03-01","2019-04-01","2019-05-01","2019-06-01","2019-07-01","2019-08-01","2019-09-01"],"y":[478951,496844,542833,533768,537400,565572,526186,539854,560404,566566,545761,588252,501102,519670,577660,562473,568082,592054,541465,571966,587293,587643,582479,614453,523199,548442,622218,589743,606190,634299,570383,637290,638551,637472,636597,673862,583576,602657,671889,629789,665344,688374,610456,678051,677370,678823,671241,698703,608890,640408,687749,676459,705129,701473,661061,704489,703818,724199,700035,729370,648926,670542,733003,714436,728181,746888,699834,728842,755266,760523,718354,776516,665199,691649,766470,739466,743025,776742,710372,738979,765522,769652,740557,801062,674466,713828,808436,767844,782025,823830,752260,807053,819724,808320,808880,867470,736418,788463,877235,805781,857443,885803,785590,859519,857579,850389,832885,869309,770101,774650,861015,802263,859272,847650,778233,849942,794715,840896,806627,832555,744727,745345,829099,820175,854262,837932,809140,860703,829824,859516,823197,864883,782576,774935,868823,836394,860695,870860,843766,868323,878500,900758,841965,928806,806952,829190,960775,912194,921875,956686,900073,951366,950275,951949,948465,1022229,877196,893159,1031375,984751,1008702,1046772,969379,1060651,1044233,1037682,1024483,1091473,965506,959851,1115019,1031250,1118661,1130766,1038883,1138855,1069221,1080005,1060801,1119564,1004913,992297,1147403,1097282,1180260,1168094,1104920,1189734,1118575,1188692,1162536,1183406,1102069,1101336,1190154,1200214,1248440,1261888,1230917,1217705,1177945,1151293,995875,1050830,904169,888602,981431,958933,973259,1022271,991381,1012041,1009141,1029979,1001333,1093088,951062,949431,1123147,1091719,1088264,1123062,1077804,1118983,1109883,1116388,1117840,1199101,1064238,1058756,1262255,1200658,1228496,1255922,1180494,1270590,1231223,1226096,1217572,1283622,1151724,1188357,1324537,1259670,1322352,1277521,1226635,1316191,1245759,1309499,1277636,1307099,1210648,1187803,1321704,1295984,1361135,1306192,1299223,1345790,1291515,1355695,1297390,1365666,1225353,1201463,1362721,1356229,1395490,1376786,1361480,1371342,1362996,1390493,1289691,1397135,1188135,1163047,1337078,1308449,1320025,1361341,1311726,1308315,1310928,1319501,1256477,1352427,1132062,1178195,1328136,1258874,1303627,1345375,1244691,1339544,1311068,1300221,1299036,1394124,1212072,1199428,1395824,1295177,1388818,1395219,1295418,1408702,1366121,1400605,1402076,1450248,1301540,1268131,1465708,1392168,1510073,1481626,1418556,1513695,1425043,1509212,1457737,1476316,1339535,1295651,1481095,1461158,1527171,1469759,1461851,1508278,1446291],"text":[".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-01-01<br />.value:  478951",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-02-01<br />.value:  496844",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-03-01<br />.value:  542833",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-04-01<br />.value:  533768",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-05-01<br />.value:  537400",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-06-01<br />.value:  565572",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-07-01<br />.value:  526186",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-08-01<br />.value:  539854",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-09-01<br />.value:  560404",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-10-01<br />.value:  566566",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-11-01<br />.value:  545761",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1992-12-01<br />.value:  588252",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-01-01<br />.value:  501102",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-02-01<br />.value:  519670",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-03-01<br />.value:  577660",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-04-01<br />.value:  562473",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-05-01<br />.value:  568082",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-06-01<br />.value:  592054",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-07-01<br />.value:  541465",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-08-01<br />.value:  571966",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-09-01<br />.value:  587293",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-10-01<br />.value:  587643",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-11-01<br />.value:  582479",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1993-12-01<br />.value:  614453",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-01-01<br />.value:  523199",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-02-01<br />.value:  548442",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-03-01<br />.value:  622218",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-04-01<br />.value:  589743",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-05-01<br />.value:  606190",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-06-01<br />.value:  634299",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-07-01<br />.value:  570383",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-08-01<br />.value:  637290",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-09-01<br />.value:  638551",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-10-01<br />.value:  637472",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-11-01<br />.value:  636597",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1994-12-01<br />.value:  673862",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-01-01<br />.value:  583576",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-02-01<br />.value:  602657",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-03-01<br />.value:  671889",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-04-01<br />.value:  629789",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-05-01<br />.value:  665344",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-06-01<br />.value:  688374",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-07-01<br />.value:  610456",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-08-01<br />.value:  678051",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-09-01<br />.value:  677370",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-10-01<br />.value:  678823",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-11-01<br />.value:  671241",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1995-12-01<br />.value:  698703",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-01-01<br />.value:  608890",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-02-01<br />.value:  640408",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-03-01<br />.value:  687749",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-04-01<br />.value:  676459",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-05-01<br />.value:  705129",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-06-01<br />.value:  701473",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-07-01<br />.value:  661061",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-08-01<br />.value:  704489",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-09-01<br />.value:  703818",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-10-01<br />.value:  724199",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-11-01<br />.value:  700035",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1996-12-01<br />.value:  729370",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-01-01<br />.value:  648926",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-02-01<br />.value:  670542",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-03-01<br />.value:  733003",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-04-01<br />.value:  714436",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-05-01<br />.value:  728181",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-06-01<br />.value:  746888",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-07-01<br />.value:  699834",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-08-01<br />.value:  728842",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-09-01<br />.value:  755266",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-10-01<br />.value:  760523",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-11-01<br />.value:  718354",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1997-12-01<br />.value:  776516",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-01-01<br />.value:  665199",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-02-01<br />.value:  691649",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-03-01<br />.value:  766470",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-04-01<br />.value:  739466",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-05-01<br />.value:  743025",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-06-01<br />.value:  776742",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-07-01<br />.value:  710372",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-08-01<br />.value:  738979",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-09-01<br />.value:  765522",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-10-01<br />.value:  769652",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-11-01<br />.value:  740557",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1998-12-01<br />.value:  801062",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-01-01<br />.value:  674466",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-02-01<br />.value:  713828",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-03-01<br />.value:  808436",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-04-01<br />.value:  767844",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-05-01<br />.value:  782025",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-06-01<br />.value:  823830",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-07-01<br />.value:  752260",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-08-01<br />.value:  807053",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-09-01<br />.value:  819724",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-10-01<br />.value:  808320",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-11-01<br />.value:  808880",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 1999-12-01<br />.value:  867470",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-01-01<br />.value:  736418",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-02-01<br />.value:  788463",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-03-01<br />.value:  877235",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-04-01<br />.value:  805781",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-05-01<br />.value:  857443",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-06-01<br />.value:  885803",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-07-01<br />.value:  785590",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-08-01<br />.value:  859519",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-09-01<br />.value:  857579",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-10-01<br />.value:  850389",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-11-01<br />.value:  832885",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2000-12-01<br />.value:  869309",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-01-01<br />.value:  770101",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-02-01<br />.value:  774650",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-03-01<br />.value:  861015",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-04-01<br />.value:  802263",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-05-01<br />.value:  859272",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-06-01<br />.value:  847650",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-07-01<br />.value:  778233",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-08-01<br />.value:  849942",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-09-01<br />.value:  794715",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-10-01<br />.value:  840896",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-11-01<br />.value:  806627",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2001-12-01<br />.value:  832555",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-01-01<br />.value:  744727",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-02-01<br />.value:  745345",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-03-01<br />.value:  829099",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-04-01<br />.value:  820175",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-05-01<br />.value:  854262",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-06-01<br />.value:  837932",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-07-01<br />.value:  809140",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-08-01<br />.value:  860703",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-09-01<br />.value:  829824",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-10-01<br />.value:  859516",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-11-01<br />.value:  823197",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2002-12-01<br />.value:  864883",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-01-01<br />.value:  782576",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-02-01<br />.value:  774935",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-03-01<br />.value:  868823",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-04-01<br />.value:  836394",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-05-01<br />.value:  860695",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-06-01<br />.value:  870860",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-07-01<br />.value:  843766",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-08-01<br />.value:  868323",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-09-01<br />.value:  878500",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-10-01<br />.value:  900758",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-11-01<br />.value:  841965",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2003-12-01<br />.value:  928806",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-01-01<br />.value:  806952",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-02-01<br />.value:  829190",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-03-01<br />.value:  960775",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-04-01<br />.value:  912194",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-05-01<br />.value:  921875",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-06-01<br />.value:  956686",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-07-01<br />.value:  900073",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-08-01<br />.value:  951366",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-09-01<br />.value:  950275",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-10-01<br />.value:  951949",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-11-01<br />.value:  948465",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2004-12-01<br />.value: 1022229",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-01-01<br />.value:  877196",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-02-01<br />.value:  893159",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-03-01<br />.value: 1031375",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-04-01<br />.value:  984751",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-05-01<br />.value: 1008702",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-06-01<br />.value: 1046772",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-07-01<br />.value:  969379",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-08-01<br />.value: 1060651",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-09-01<br />.value: 1044233",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-10-01<br />.value: 1037682",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-11-01<br />.value: 1024483",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2005-12-01<br />.value: 1091473",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-01-01<br />.value:  965506",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-02-01<br />.value:  959851",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-03-01<br />.value: 1115019",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-04-01<br />.value: 1031250",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-05-01<br />.value: 1118661",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-06-01<br />.value: 1130766",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-07-01<br />.value: 1038883",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-08-01<br />.value: 1138855",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-09-01<br />.value: 1069221",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-10-01<br />.value: 1080005",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-11-01<br />.value: 1060801",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2006-12-01<br />.value: 1119564",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-01-01<br />.value: 1004913",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-02-01<br />.value:  992297",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-03-01<br />.value: 1147403",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-04-01<br />.value: 1097282",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-05-01<br />.value: 1180260",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-06-01<br />.value: 1168094",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-07-01<br />.value: 1104920",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-08-01<br />.value: 1189734",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-09-01<br />.value: 1118575",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-10-01<br />.value: 1188692",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-11-01<br />.value: 1162536",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2007-12-01<br />.value: 1183406",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-01-01<br />.value: 1102069",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-02-01<br />.value: 1101336",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-03-01<br />.value: 1190154",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-04-01<br />.value: 1200214",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-05-01<br />.value: 1248440",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-06-01<br />.value: 1261888",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-07-01<br />.value: 1230917",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-08-01<br />.value: 1217705",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-09-01<br />.value: 1177945",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-10-01<br />.value: 1151293",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-11-01<br />.value:  995875",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2008-12-01<br />.value: 1050830",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-01-01<br />.value:  904169",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-02-01<br />.value:  888602",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-03-01<br />.value:  981431",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-04-01<br />.value:  958933",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-05-01<br />.value:  973259",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-06-01<br />.value: 1022271",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-07-01<br />.value:  991381",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-08-01<br />.value: 1012041",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-09-01<br />.value: 1009141",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-10-01<br />.value: 1029979",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-11-01<br />.value: 1001333",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2009-12-01<br />.value: 1093088",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-01-01<br />.value:  951062",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-02-01<br />.value:  949431",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-03-01<br />.value: 1123147",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-04-01<br />.value: 1091719",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-05-01<br />.value: 1088264",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-06-01<br />.value: 1123062",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-07-01<br />.value: 1077804",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-08-01<br />.value: 1118983",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-09-01<br />.value: 1109883",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-10-01<br />.value: 1116388",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-11-01<br />.value: 1117840",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2010-12-01<br />.value: 1199101",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-01-01<br />.value: 1064238",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-02-01<br />.value: 1058756",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-03-01<br />.value: 1262255",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-04-01<br />.value: 1200658",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-05-01<br />.value: 1228496",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-06-01<br />.value: 1255922",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-07-01<br />.value: 1180494",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-08-01<br />.value: 1270590",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-09-01<br />.value: 1231223",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-10-01<br />.value: 1226096",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-11-01<br />.value: 1217572",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2011-12-01<br />.value: 1283622",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-01-01<br />.value: 1151724",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-02-01<br />.value: 1188357",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-03-01<br />.value: 1324537",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-04-01<br />.value: 1259670",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-05-01<br />.value: 1322352",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-06-01<br />.value: 1277521",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-07-01<br />.value: 1226635",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-08-01<br />.value: 1316191",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-09-01<br />.value: 1245759",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-10-01<br />.value: 1309499",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-11-01<br />.value: 1277636",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2012-12-01<br />.value: 1307099",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-01-01<br />.value: 1210648",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-02-01<br />.value: 1187803",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-03-01<br />.value: 1321704",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-04-01<br />.value: 1295984",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-05-01<br />.value: 1361135",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-06-01<br />.value: 1306192",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-07-01<br />.value: 1299223",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-08-01<br />.value: 1345790",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-09-01<br />.value: 1291515",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-10-01<br />.value: 1355695",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-11-01<br />.value: 1297390",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2013-12-01<br />.value: 1365666",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-01-01<br />.value: 1225353",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-02-01<br />.value: 1201463",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-03-01<br />.value: 1362721",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-04-01<br />.value: 1356229",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-05-01<br />.value: 1395490",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-06-01<br />.value: 1376786",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-07-01<br />.value: 1361480",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-08-01<br />.value: 1371342",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-09-01<br />.value: 1362996",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-10-01<br />.value: 1390493",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-11-01<br />.value: 1289691",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2014-12-01<br />.value: 1397135",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-01-01<br />.value: 1188135",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-02-01<br />.value: 1163047",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-03-01<br />.value: 1337078",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-04-01<br />.value: 1308449",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-05-01<br />.value: 1320025",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-06-01<br />.value: 1361341",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-07-01<br />.value: 1311726",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-08-01<br />.value: 1308315",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-09-01<br />.value: 1310928",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-10-01<br />.value: 1319501",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-11-01<br />.value: 1256477",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2015-12-01<br />.value: 1352427",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-01-01<br />.value: 1132062",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-02-01<br />.value: 1178195",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-03-01<br />.value: 1328136",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-04-01<br />.value: 1258874",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-05-01<br />.value: 1303627",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-06-01<br />.value: 1345375",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-07-01<br />.value: 1244691",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-08-01<br />.value: 1339544",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-09-01<br />.value: 1311068",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-10-01<br />.value: 1300221",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-11-01<br />.value: 1299036",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2016-12-01<br />.value: 1394124",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-01-01<br />.value: 1212072",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-02-01<br />.value: 1199428",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-03-01<br />.value: 1395824",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-04-01<br />.value: 1295177",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-05-01<br />.value: 1388818",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-06-01<br />.value: 1395219",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-07-01<br />.value: 1295418",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-08-01<br />.value: 1408702",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-09-01<br />.value: 1366121",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-10-01<br />.value: 1400605",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-11-01<br />.value: 1402076",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2017-12-01<br />.value: 1450248",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-01-01<br />.value: 1301540",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-02-01<br />.value: 1268131",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-03-01<br />.value: 1465708",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-04-01<br />.value: 1392168",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-05-01<br />.value: 1510073",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-06-01<br />.value: 1481626",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-07-01<br />.value: 1418556",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-08-01<br />.value: 1513695",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-09-01<br />.value: 1425043",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-10-01<br />.value: 1509212",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-11-01<br />.value: 1457737",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2018-12-01<br />.value: 1476316",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-01-01<br />.value: 1339535",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-02-01<br />.value: 1295651",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-03-01<br />.value: 1481095",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-04-01<br />.value: 1461158",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-05-01<br />.value: 1527171",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-06-01<br />.value: 1469759",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-07-01<br />.value: 1461851",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-08-01<br />.value: 1508278",".color_mod: ACTUAL<br />.color_mod: ACTUAL<br />.index: 2019-09-01<br />.value: 1446291"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(44,62,80,1)","dash":"solid"},"hoveron":"points","name":"ACTUAL","legendgroup":"ACTUAL","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":["2019-10-01","2019-11-01","2019-12-01","2020-01-01","2020-02-01","2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01","2020-08-01","2020-09-01","2020-10-01","2020-11-01","2020-12-01","2021-01-01","2021-02-01","2021-03-01","2021-04-01","2021-05-01","2021-06-01","2021-07-01","2021-08-01","2021-09-01","2021-10-01","2021-11-01","2021-12-01","2022-01-01","2022-02-01","2022-03-01","2022-04-01","2022-05-01","2022-06-01","2022-07-01","2022-08-01","2022-09-01","2022-10-01","2022-11-01","2022-12-01","2023-01-01","2023-02-01","2023-03-01","2023-04-01","2023-05-01","2023-06-01","2023-07-01","2023-08-01","2023-09-01","2023-10-01","2023-11-01","2023-12-01","2024-01-01","2024-02-01","2024-03-01","2024-04-01","2024-05-01","2024-06-01","2024-07-01","2024-08-01","2024-09-01","2024-10-01","2024-11-01","2024-12-01","2025-01-01","2025-02-01","2025-03-01","2025-04-01","2025-05-01","2025-06-01","2025-07-01","2025-08-01","2025-09-01","2025-10-01","2025-11-01","2025-12-01","2026-01-01","2026-02-01","2026-03-01","2026-04-01","2026-05-01","2026-06-01","2026-07-01","2026-08-01","2026-09-01","2026-10-01","2026-11-01","2026-12-01","2027-01-01","2027-02-01","2027-03-01","2027-04-01","2027-05-01","2027-06-01","2027-07-01","2027-08-01","2027-09-01","2027-10-01","2027-11-01","2027-12-01","2028-01-01"],"y":[1523082.6933707,1444978.79829895,1515814.02643059,1346666.02257522,1336926.47746917,1499441.13517853,1477465.16228484,1516892.27947638,1495960.13551829,1465781.18543315,1512111.17767827,1476867.72601386,1517922.51570065,1461317.85818724,1549757.11968923,1373180.85648936,1373760.8939175,1536702.77431692,1489701.81683511,1540678.3647146,1543573.97341942,1484606.60501441,1548472.18183819,1514436.28538521,1543371.13089698,1505859.73322583,1581202.50400671,1414087.74323165,1405661.06712813,1577561.90331907,1521156.51201547,1586936.91014904,1585483.25068152,1523392.12244817,1594331.46805746,1550581.0245717,1588225.20858534,1550209.62319293,1613543.75272546,1453066.42502133,1437061.33057276,1612403.81785291,1561734.21374721,1629075.42994992,1616778.12890259,1564393.79724701,1631496.15160931,1583362.58173354,1629133.83408742,1583633.32577943,1647112.34280817,1485702.15226401,1470070.5409249,1643383.51912587,1598609.07330702,1660934.99297245,1645920.24240649,1598639.35164232,1661496.45536863,1615296.43696704,1661024.33598271,1612696.2011413,1680809.66575161,1516492.4193856,1503899.06888434,1675080.98550829,1630497.08059826,1690204.83298475,1678226.64764886,1629303.64600882,1691910.68739596,1648160.6600992,1690773.05524567,1644157.11046716,1714084.06712766,1548949.71916417,1537439.06420615,1708534.86993434,1661780.66067277,1722343.93568684,1712625.95955568,1661095.3827129,1725219.52445161,1681720.45833222,1723092.81937409,1678245.03823512,1747136.84605808,1582780.91765478,1570548.79579961,1742422.06606179,1694719.9132885,1756626.07671494,1746662.18331122,1694705.84856486,1759537.09481136,1715205.38842159,1757265.10305772,1712477.77359514,1780260.87772123,1616530.2944898],"text":[".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2019-10-01<br />.value: 1523083",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2019-11-01<br />.value: 1444979",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2019-12-01<br />.value: 1515814",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-01-01<br />.value: 1346666",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-02-01<br />.value: 1336926",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-03-01<br />.value: 1499441",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-04-01<br />.value: 1477465",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-05-01<br />.value: 1516892",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-06-01<br />.value: 1495960",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-07-01<br />.value: 1465781",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-08-01<br />.value: 1512111",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-09-01<br />.value: 1476868",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-10-01<br />.value: 1517923",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-11-01<br />.value: 1461318",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2020-12-01<br />.value: 1549757",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-01-01<br />.value: 1373181",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-02-01<br />.value: 1373761",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-03-01<br />.value: 1536703",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-04-01<br />.value: 1489702",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-05-01<br />.value: 1540678",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-06-01<br />.value: 1543574",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-07-01<br />.value: 1484607",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-08-01<br />.value: 1548472",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-09-01<br />.value: 1514436",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-10-01<br />.value: 1543371",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-11-01<br />.value: 1505860",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2021-12-01<br />.value: 1581203",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-01-01<br />.value: 1414088",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-02-01<br />.value: 1405661",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-03-01<br />.value: 1577562",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-04-01<br />.value: 1521157",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-05-01<br />.value: 1586937",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-06-01<br />.value: 1585483",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-07-01<br />.value: 1523392",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-08-01<br />.value: 1594331",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-09-01<br />.value: 1550581",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-10-01<br />.value: 1588225",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-11-01<br />.value: 1550210",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2022-12-01<br />.value: 1613544",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-01-01<br />.value: 1453066",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-02-01<br />.value: 1437061",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-03-01<br />.value: 1612404",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-04-01<br />.value: 1561734",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-05-01<br />.value: 1629075",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-06-01<br />.value: 1616778",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-07-01<br />.value: 1564394",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-08-01<br />.value: 1631496",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-09-01<br />.value: 1583363",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-10-01<br />.value: 1629134",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-11-01<br />.value: 1583633",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2023-12-01<br />.value: 1647112",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-01-01<br />.value: 1485702",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-02-01<br />.value: 1470071",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-03-01<br />.value: 1643384",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-04-01<br />.value: 1598609",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-05-01<br />.value: 1660935",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-06-01<br />.value: 1645920",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-07-01<br />.value: 1598639",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-08-01<br />.value: 1661496",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-09-01<br />.value: 1615296",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-10-01<br />.value: 1661024",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-11-01<br />.value: 1612696",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2024-12-01<br />.value: 1680810",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-01-01<br />.value: 1516492",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-02-01<br />.value: 1503899",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-03-01<br />.value: 1675081",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-04-01<br />.value: 1630497",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-05-01<br />.value: 1690205",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-06-01<br />.value: 1678227",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-07-01<br />.value: 1629304",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-08-01<br />.value: 1691911",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-09-01<br />.value: 1648161",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-10-01<br />.value: 1690773",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-11-01<br />.value: 1644157",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2025-12-01<br />.value: 1714084",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-01-01<br />.value: 1548950",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-02-01<br />.value: 1537439",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-03-01<br />.value: 1708535",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-04-01<br />.value: 1661781",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-05-01<br />.value: 1722344",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-06-01<br />.value: 1712626",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-07-01<br />.value: 1661095",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-08-01<br />.value: 1725220",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-09-01<br />.value: 1681720",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-10-01<br />.value: 1723093",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-11-01<br />.value: 1678245",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2026-12-01<br />.value: 1747137",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-01-01<br />.value: 1582781",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-02-01<br />.value: 1570549",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-03-01<br />.value: 1742422",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-04-01<br />.value: 1694720",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-05-01<br />.value: 1756626",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-06-01<br />.value: 1746662",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-07-01<br />.value: 1694706",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-08-01<br />.value: 1759537",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-09-01<br />.value: 1715205",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-10-01<br />.value: 1757265",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-11-01<br />.value: 1712478",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2027-12-01<br />.value: 1780261",".color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.color_mod: 1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT<br />.index: 2028-01-01<br />.value: 1616530"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(227,26,28,1)","dash":"solid"},"hoveron":"points","name":"1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT","legendgroup":"1_ARIMA(2,0,3)(2,1,1)[12] WITH DRIFT","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":25.5707762557078,"l":51.8721461187215},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(44,62,80,1)","family":"","size":14.6118721461187},"title":{"text":"Forecast Plot","font":{"color":"rgba(44,62,80,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"date","autorange":true,"range":["1990-03-14","2029-10-19"],"tickmode":"auto","ticktext":["2000","2010","2020"],"tickvals":[10957,14610,18262],"categoryorder":"array","categoryarray":["2000","2010","2020"],"nticks":null,"ticks":"outside","tickcolor":"rgba(204,204,204,1)","ticklen":3.65296803652968,"tickwidth":0.22139200221392,"showticklabels":true,"tickfont":{"color":"rgba(44,62,80,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(204,204,204,1)","gridwidth":0.22139200221392,"zeroline":false,"anchor":"y","title":{"text":"","font":{"color":"rgba(44,62,80,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":true,"range":[413885.506113939,1845326.37160729],"tickmode":"auto","ticktext":["800000","1200000","1600000"],"tickvals":[800000,1200000,1600000],"categoryorder":"array","categoryarray":["800000","1200000","1600000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(204,204,204,1)","ticklen":3.65296803652968,"tickwidth":0.22139200221392,"showticklabels":true,"tickfont":{"color":"rgba(44,62,80,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(204,204,204,1)","gridwidth":0.22139200221392,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(44,62,80,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(44,62,80,1)","width":0.33208800332088,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(44,62,80,1)","family":"","size":11.689497716895},"y":0.913385826771654},"annotations":[{"text":"Legend","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(44,62,80,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"93a111a0188":{"colour":{},"x":{},"y":{},"type":"scatter"}},"cur_data":"93a111a0188","visdat":{"93a111a0188":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
