library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(rvest)

statuarstaedte = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSwtIKTFAd8-CJa2E45_d7tNtlJDVbQExNIp5oHI7v5WZBn4bW8L0kAevTWZ_7sjlF5uW-DcMkvP5FY/pub?output=csv")

usethis::use_data(statuarstaedte)
