## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(knitr)

## ----campfin------------------------------------------------------------------
library(campfin)
packageVersion("campfin")

## ----setup, warning=FALSE, message=FALSE, error=FALSE-------------------------
library(dplyr)
library(readr)
library(stringr)

## ----view_messy---------------------------------------------------------------
ex_file <- system.file("extdata", "vt_contribs.csv", package = "campfin")

## ----echo=FALSE---------------------------------------------------------------
kable(read_csv(ex_file, col_types = cols(.default = col_character())))

## ----read_messy---------------------------------------------------------------
vt <- read_csv(
  file = ex_file,
  trim_ws = FALSE,
  na = c("", "NA", "N/A"),
  col_types = cols(
    amount = col_number(),
    date = col_date_mdy()
  )
)

## ----date_math----------------------------------------------------------------
min(vt$date)

## ----prop_valid_before--------------------------------------------------------
prop_in(vt$city, str_to_lower(valid_city))
prop_in(vt$state, valid_state)
prop_in(vt$zip, valid_zip)

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
      na = invalid_city
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
vt %>% select(address, city, state, zip)

## ----length_city--------------------------------------------------------------
length(valid_city)
sample(valid_city, 6)
sample(extra_city, 6)
# combine both vectors
many_city <- c(valid_city, extra_city)

## ----bad_city, echo=FALSE-----------------------------------------------------
(bad <- vt %>%
  select(1, 7:9) %>% 
  filter(!is.na(city)) %>% 
  mutate(valid = city %in% many_city) %>%
  filter(!valid))

## ----bad_join-----------------------------------------------------------------
bad <- left_join(
  x = bad,
  y = zipcodes,
  by = c("zip", "state"), 
  suffix = c("_raw", "_match")
)

## ----echo=FALSE---------------------------------------------------------------
kable(select(bad, -valid))

## ----str_dist-----------------------------------------------------------------
str_dist("example", "xampel")

## ----is_abbrev----------------------------------------------------------------
is_abbrev(abb = "NYC", full = "New York City")
is_abbrev(abb = "DC", full = "Washington")

## ----check_city---------------------------------------------------------------
bad <- bad %>% 
  mutate(
    match_dist = str_dist(city_raw, city_match),
    match_abb = is_abbrev(city_raw, city_match)
  )

## ----echo=FALSE---------------------------------------------------------------
kable(select(bad, -valid))

## ----swap_city----------------------------------------------------------------
vt <- vt %>%
  rename(city_raw = city) %>% 
  # match city by ZIP
  left_join(zipcodes) %>% 
  rename(city_match = city) %>%
  mutate(
    # check against match
    match_dist = str_dist(city_raw, city_match),
    match_abb = is_abbrev(city_raw, city_match),
    city = ifelse(match_abb | match_dist == 1, city_match, city_raw)
  ) %>% 
  # remove intermediary columns
  select(-city_raw, -city_match, -match_dist, -match_abb)

## ----show_swap, echo=FALSE----------------------------------------------------
vt %>%
  select(1, 7:9) %>% 
  filter(!is.na(city)) %>% 
  mutate(
    all_valid = all(
      city %in% valid_city,
      state %in% valid_state,
      zip %in% valid_zip
    )
  ) %>%  
  kable()

## ----flag_na------------------------------------------------------------------
(vt <- flag_na(vt, name))

## ----flag_dupes---------------------------------------------------------------
(vt <- flag_dupes(vt, -id, .both = TRUE))

## ----echo=FALSE---------------------------------------------------------------
kable(vt)

