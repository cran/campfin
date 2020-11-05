## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE, error=FALSE-------------------------
library(campfin)
library(dplyr)
library(stringr)
library(readr)
library(scales)

## ----view_messy---------------------------------------------------------------
read_csv(
  file = system.file("extdata", "vt_contribs.csv", package = "campfin")
)

## ----read_messy---------------------------------------------------------------
vt <- read_csv(
  file = system.file("extdata", "vt_contribs.csv", package = "campfin"),
  trim_ws = FALSE,
  col_types = cols(
    amount = col_number(),
    date = col_date_usa()
  )
)

## ----date_math----------------------------------------------------------------
transmute(vt, date, next_week = date + 7)

## ----prop_valid_before--------------------------------------------------------
percent(prop_in(vt$city, str_to_lower(valid_city)))
percent(prop_in(vt$state, valid_state))
percent(prop_in(vt$zip, valid_zip))

## ----glimpse_fun--------------------------------------------------------------
col_stats(vt, n_distinct)
col_stats(vt, count_na)

## ----normal_address-----------------------------------------------------------
vt <- vt %>% 
  mutate(
    address = normal_address(
      address = address,
      abbs = usps_street,
      na = invalid_city,
      na_rep = TRUE
    ),
    city = normal_city(
      city = city,
      abbs = usps_city,
      states = "VT",
      na = invalid_city,
      na_rep = TRUE
    ),
    state = normal_state(
      state = state,
      abbreviate = TRUE,
      na_rep = TRUE,
      valid = valid_state
    ),
    zip = normal_zip(
      zip = zip,
      na_rep = TRUE
    )
  )

## ----showal, echo=FALSE-------------------------------------------------------
select(vt, 7:10)

## ----length_city--------------------------------------------------------------
length(valid_city)
sample(valid_city, 6)
sample(extra_city, 6)
# combine both vectors
many_city <- c(valid_city, extra_city)

## ----bad_city, echo=FALSE-----------------------------------------------------
(bad <- vt %>%
  select(1, 8:10) %>% 
  filter(!is.na(city)) %>% 
  mutate(valid = city %in% many_city) %>%
  filter(!valid))

## ----match_city, echo=FALSE---------------------------------------------------
(bad <- bad %>% 
  left_join(zipcodes, by = c("zip", "state"), suffix = c("_raw", "_match")))

## ----check_city---------------------------------------------------------------
(bad <- bad %>% 
  mutate(
    match_dist = str_dist(city_raw, city_match),
    match_abb = is_abbrev(city_raw, city_match)
  ))

## ----swap_city----------------------------------------------------------------
vt <- vt %>%
  rename(city_raw = city) %>% 
  left_join(zipcodes) %>% 
  rename(city_match = city) %>% 
  mutate(
    match_dist = str_dist(city_raw, city_match),
    match_abb = is_abbrev(city_raw, city_match),
    city = if_else(match_abb | match_dist == 1, city_match, city_raw)
  ) %>% 
  select(-city_raw, -city_match, -match_dist, -match_abb)

## ----show_swap, echo=FALSE----------------------------------------------------
vt %>%
  select(1, 8:10) %>% 
  filter(!is.na(city)) %>% 
  mutate(
    all_valid = city %in% valid_city & state %in% valid_state & zip %in% valid_zip
    )

## ----flag_na------------------------------------------------------------------
(vt <- flag_na(vt, last))

## ----flag_dupes---------------------------------------------------------------
(vt <- flag_dupes(vt, -id))

