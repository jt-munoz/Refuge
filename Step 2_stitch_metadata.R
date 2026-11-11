
# STEP 2: MATCH PIT IDS TO FISH METADATA ----
# Mongolia Thermal Refuge Project | November 2026

# 1. SETUP ----

library(tidyverse)
library(lubridate)

indir <- "processed_pit"
outdir <- "processed_pit"

# 2. IMPORT DATA ----

pit <- readRDS(file.path(indir, "pit_tag_data.Rds"))

fish <- read_csv("pit_metadata.csv", show_col_types = FALSE) %>%
  filter(PIT_ID != "EEC") %>%
  transmute(PIT_ID, tag_id = Converted_ID,
            species = recode(str_to_lower(species), cyprinid = "dace"),
            length_cm, weight_g,
            catch_date = mdy(Catch_date),
            catch_location = Catch_location)

stopifnot(!anyNA(fish$tag_id), !anyDuplicated(fish$tag_id))

pit <- pit %>% filter(tag_id != "900_209000353516")

# 3. MATCH DETECTIONS TO FISH ----

detections <- pit %>%
  left_join(fish, by = "tag_id", relationship = "many-to-one") %>%
  arrange(datetime1) %>%
  select(datetime1, datetime2, antenna_id, PIT_ID, tag_id,
         species, weight_g, length_cm, catch_date,
         catch_location, everything())

# 4. SUMMARIZE FISH ----

fish_summary <- detections %>%
  group_by(tag_id) %>%
  summarise(
    total_detections = n(),
    days_detected = n_distinct(date),
    A1_detections = sum(antenna_id == "A1"),
    A2_detections = sum(antenna_id == "A2"),
    first_detection = min(datetime1),
    last_detection = max(datetime1),
    .groups = "drop"
  ) %>%
  right_join(fish, by = "tag_id") %>%
  mutate(
    across(c(total_detections, days_detected,
             A1_detections, A2_detections), ~ replace_na(.x, 0L)),
    detected = total_detections > 0
  ) %>%
  select(PIT_ID, tag_id, species, weight_g, length_cm,
         catch_date, catch_location, detected, everything())

# 5. VALIDATE MATCHES ----

tag_check <- full_join(
  fish %>% select(PIT_ID, tag_id, species),
  pit %>% count(tag_id, name = "detections"),
  by = "tag_id"
) %>%
  mutate(
    detections = replace_na(detections, 0L),
    match_status = case_when(
      is.na(PIT_ID) ~ "Unknown reader ID",
      detections == 0 ~ "Not detected",
      TRUE ~ "Matched"
    )
  ) %>%
  arrange(match_status, PIT_ID)

# 6. EXPORT ----

write_csv(detections, file.path(outdir, "fish_detections.csv"))
write_csv(fish_summary, file.path(outdir, "fish_detection_summary.csv"))
write_csv(tag_check, file.path(outdir, "tag_match_check.csv"))

# 7. VALIDATION SUMMARY ----

cat("\nTAG MATCH RESULTS\n")
print(count(tag_check, match_status))

cat("\nFISH BY SPECIES\n")
print(count(fish_summary, species, detected))

cat("\nEFA CHECK\n")
print(filter(tag_check, PIT_ID == "EFA"))
