library(tidyverse)
library(aws.s3)
library(qs2)

on_ec2 <- file.exists("/sys/hypervisor/uuid") ||
  file.exists("/sys/devices/virtual/dmi/id/product_uuid")

if (!on_ec2) {
  # Running locally (RStudio laptop)
  Sys.setenv(
    AWS_PROFILE = "brian-hurler",
    AWS_DEFAULT_REGION = "us-west-1"
  )
} else {
  # Running on EC2
  # DO NOT set AWS_PROFILE
  Sys.setenv(
    AWS_DEFAULT_REGION = "us-west-1"
  )
}

save_object(
  object = "v25_structure/v25_matches.rda",
  bucket = "usavbeach",
  file = "v25_matches.rda"
)
load("v25_matches.rda")
matches <- data


save_object(
  object = "rallies_with_off_def_elo.rda",
  bucket = "usavbeach",
  file = "rallies_with_off_def_elo.rda"
)
load("rallies_with_off_def_elo.rda")

  
save_object(
  object = "elo/long_matches_k_factor_30.rda",
  bucket = "usavbeach",
  file = "long_matches_k_factor_30.rda"
)
load("long_matches_k_factor_30.rda")


  