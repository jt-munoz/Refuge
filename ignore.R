
# 8. EXPLORATORY VISUALIZATIONS ----

# Exclude unknown IDs and observations before tagging
plot_data <- detections %>%
  filter(!is.na(PIT_ID), !is.na(species), species != "na",
         as.Date(datetime1) >= catch_date) %>%
  mutate(day = as.Date(datetime1),
         hour = hour(datetime1))

# DAILY DETECTIONS BY SPECIES ----

plot_data %>%
  count(day, species) %>%
  ggplot(aes(day, n, color = species)) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  labs(x = "Date", y = "PIT detections", color = "Species") +
  theme_bw()

# UNIQUE FISH DETECTED PER DAY ----

plot_data %>%
  group_by(day, species) %>%
  summarise(n = n_distinct(PIT_ID), .groups = "drop") %>%
  ggplot(aes(day, n, color = species)) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  labs(x = "Date", y = "Unique fish detected", color = "Species") +
  theme_bw()

# DIEL DETECTION PATTERNS ----

plot_data %>%
  count(species, hour) %>%
  ggplot(aes(hour, n, fill = species)) +
  geom_col() +
  facet_wrap(~ species, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(x = "Hour of day", y = "PIT detections") +
  theme_bw() +
  theme(legend.position = "none")

# INDIVIDUAL DETECTION HISTORIES ----

ggplot(plot_data, aes(datetime1, reorder(PIT_ID, datetime1),
                      color = antenna_id)) +
  geom_point(alpha = 0.25, size = 0.6) +
  facet_wrap(~ species, scales = "free_y") +
  labs(x = "Date", y = "Individual fish", color = "Antenna") +
  theme_bw()

# ANTENNA USE BY SPECIES ----

plot_data %>%
  group_by(species, antenna_id) %>%
  summarise(n = n_distinct(PIT_ID), .groups = "drop") %>%
  ggplot(aes(antenna_id, n, fill = species)) +
  geom_col(position = "dodge") +
  labs(x = "Antenna", y = "Unique fish detected", fill = "Species") +
  theme_bw()

# GRAYLING: BODY WEIGHT VS. DETECTIONS ----

fish_summary %>%
  filter(species == "grayling", detected, !is.na(weight_g)) %>%
  ggplot(aes(x = weight_g, y = total_detections)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Body weight (g)", y = "Number of detections") +
  theme_bw()




# 24-HOUR CIRCULAR DETECTIONS BY SPECIES ----

detections %>%
  filter(species %in% c("grayling", "dace")) %>%
  mutate(hour = hour(datetime1)) %>%
  count(species, hour) %>%
  complete(species, hour = 0:23, fill = list(n = 0)) %>%
  group_by(species) %>%
  mutate(height = log1p(n) / max(log1p(n))) %>%
  ungroup() %>%
  ggplot(aes(x = hour, y = height)) +
  geom_col(width = 0.85, fill = "steelblue4") +
  coord_polar(start = 0, direction = 1) +
  scale_x_continuous(
    limits = c(-0.5, 23.5),
    breaks = seq(0, 21, 3),
    labels = sprintf("%02d:00", seq(0, 21, 3)),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(-0.3, 1.1), breaks = NULL) +
  facet_wrap(~ species) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 13, face = "bold")
  )



# DETECTIONS AND A1 TEMPERATURE THROUGH TIME ----

a1_temp <- temp %>%
  filter(antenna_id == "A1") %>%
  transmute(datetime = datetime2, temperature = temp_c) %>%
  filter(!is.na(datetime), !is.na(temperature))

for (sp in c("grayling", "dace")) {



