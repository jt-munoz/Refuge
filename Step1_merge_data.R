



# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

# Install package
# devtools::install_github("InStreamFisheries/PITR")
# devtools::install_github("KevinSee/PITcleanr")
# library(PITR)
# library(PITcleanr)

# Directory
indir <- "/Users/cfree/Dropbox/Chris/UCSB/workshops/mongolia_2026/thermal_refuge/data/raw"
outdir <- "/Users/cfree/Dropbox/Chris/UCSB/workshops/mongolia_2026/thermal_refuge/data/processed"

# Files
files2merge <- list.files(indir)


# Merge data
################################################################################

# Column names
column_names <- c("time1", "status", "date", "time2", "flag", "duration",
                  "antenna_group", "antenna_id", "tag_id", "read_type", "read_strength", "signal_strength")

# Loop through files
x <- files2merge[6]
data_orig <- purrr::map_df(files2merge, function(x){

  # Filename
  filename <- x

  # Read data
  df <- read.table(file.path(indir, filename), sep = "\n", header = FALSE)

  # Format data
  df1 <- df %>%
    # Rename
    rename(text=V1) %>%
    # Reduce to data rows (with underscore in tag id)
    filter(grepl("_", text)) %>%
    # Squish text
    mutate(text=stringr::str_squish(text)) %>%
    # Count number of items in text
    mutate(nitems=str_count(text, " ")+1) %>%
    # Reduce to only ones with 12 items
    filter(nitems==12) %>%
    # Seprate
    separate(col=text, sep=" ", into=column_names, remove=F) %>%
    # Add filename and arrange
    mutate(filename=filename) %>%
    select(filename, text, nitems, everything())


})


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Build datetimes
  mutate(datetime1=paste(date, time1) %>% ymd_hms(.),
         datetime2=paste(date, time2) %>% ymd_hms(.)) %>%
  # Format data
  mutate(date=lubridate::ymd(date),
         year=year(date),
         month=month(date)) %>%
  # Remove ones that failed (more misfires)
  filter(!is.na(date)) %>%
  # Filter to more good ones
  filter(antenna_group=="A" & antenna_id %in% c("A1", "A2") & flag=="U" & read_type=="REF" & status=="S") %>%
  filter(!grepl(":|-|\\.", tag_id)) %>%
  filter(!grepl(":|\\.", read_strength)) %>%
  filter(!grepl(":|_", signal_strength)) %>%
  # Format read and signal strength
  mutate(read_strength=as.numeric(read_strength),
         signal_strength=as.numeric(signal_strength)) %>%
  # Format time1
  mutate(time1=lubridate::hms(time1)) %>%
  mutate(hour1=hour(time1),
         minute1=minute(time1),
         second1=second(time1)) %>%
  mutate(hour_of_day1=hour1+minute1/60+second1/3600) %>%
  # Format time2
  mutate(time2=lubridate::hms(time2)) %>%
  mutate(hour2=hour(time2),
         minute2=minute(time2),
         second2=second(time2)) %>%
  mutate(hour_of_day2=hour2+minute2/60+second2/3600) %>%
  # Simplify
  select(-c(year, month, hour1, minute1, second1, hour2, minute2, second2)) %>%
  select(filename, text, nitems, date,
         datetime1, time1, hour_of_day1, datetime2, time2, hour_of_day2,
         status, flag, duration, antenna_group, antenna_id, tag_id,
         read_type, read_strength, signal_strength, everything())

# Inspect
freeR::complete(data)
str(data)

# Date
table(data$date)

# Status
# S seems good, T and 00 seem bad
table(data$status)

# Flag:
# U seems good, others seem to mark problems
table(data$flag)

# Antenna group
# A is good, others are problems
table(data$antenna_group)

# Antenna id
# A1 or A2 are good, others are problems
table(data$antenna_id)

# Tag id
# Ones with colons, hyphens, periods seem like problems
table(data$tag_id)

# Read type
# REF seems good, others seem like problems
table(data$read_type)

# Read strength
# Ones with colons or periods seem like problems
table(data$read_strength)

# Signal strength
# Ones with colons or underscores seems like problems
table(data$signal_strength)

# Export
saveRDS(data, file=file.path(outdir, "pit_tag_data.Rds"))


# Temp data
################################################################################

# Temp column names
temp_names <- c("time1", "status", "date", "time2", "flag",
                "antenna_id", "temp_text", "temp_c", "ms")

# Merge data
x <- files2merge[6]
temp_orig <- purrr::map_df(files2merge, function(x){

  # Filename
  filename <- x

  # Read data
  df <- read.table(file.path(indir, filename), sep = "\n", header = FALSE)

  # Format data
  df1 <- df %>%
    # Rename
    rename(text=V1) %>%
    # Reduce to data rows (with underscore in tag id)
    filter(grepl("Temp", text)) %>%
    # Squish text
    mutate(text=stringr::str_squish(text)) %>%
    # Count number of items in text
    mutate(nitems=str_count(text, " ")+1) %>%
    # Reduce to only ones with 9 items
    filter(nitems==9) %>%
    # Seprate
    separate(col=text, sep=" ", into=temp_names, remove=F) %>%
    # Add filename and arrange
    mutate(filename=filename) %>%
    select(filename, text, nitems, everything())

})

# Format temp data
temp <- temp_orig %>%
  # Remove useless
  select(-temp_text, nitems) %>%
  # Status = 1 and S seem useless
  filter(status=="T") %>%
  # Format datetime
  mutate(datetime1=paste(date, time1) %>% ymd_hms(.),
         datetime2=paste(date, time2) %>% ymd_hms(.)) %>%
  # Format date
  mutate(date=ymd(date)) %>%
  # Format time
  mutate(time1=hms(time1),
         time2=hms(time2)) %>%
  # Format temperature
  mutate(temp_c=gsub("C", "", temp_c) %>% as.numeric(.)) %>%
  # Format the MS thing
  mutate(ms=case_when(ms=="0.3" ~ NA,
                      ms!="0.3" ~ gsub("ms", "", ms) %>% as.numeric(.),
                      T ~ NA)) %>%
  # Arrange
  select(filename, text, date, datetime1, datetime2, time1, time2,
         antenna_id, temp_c, ms, status, flag, everything())

# Inspect
str(temp)
freeR::complete(temp)

# Inspect individual
table(temp$status) # T, S, 1
table(temp$flag) # U
table(temp$antenna_id) # A1 and A2
table(temp$ms)
table(temp$temp_c)

# Export
saveRDS(temp, file=file.path(outdir, "temp_data.Rds"))


# Quick plot
ggplot(temp, aes(x=datetime2, y=temp_c, color=antenna_id)) +
  geom_point() +
  # Labels
  labs(x="Date", y="Temperature (°C)", color="Antenna") +
  scale_x_datetime(name="Date", date_breaks="day", date_labels="%b %d") +
  # Theme
  theme_bw()


# Plot data
################################################################################

# Subset to real data
data1 <- data %>%
  # Reduce to field data
  filter(date>=ymd("2026-07-20"))

# Count by fish
nfish <- data1 %>%
  group_by(tag_id) %>%
  summarize(n=n(),
            ndays=n_distinct(date)) %>%
  ungroup()

# Count by date
ndates <- data1 %>%
  count(date)

# Count by antenna
nantenna <- data1 %>%
  count(antenna_id)

# Count by fish
g1 <- ggplot(nfish, aes(y=reorder(tag_id, desc(n)), x=n, fill=ndays)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Number of detections", y="Tag id") +
  # scale_x_continuous(trans="log10") +
  # Legend
  scale_fill_gradientn(name="# of days", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw()
g1

# Count by date
g2 <- ggplot(ndates, aes(x=date, y=n)) +
  geom_col() +
  # Labels
  labs(x="Date", y="Number of detections") +
  scale_x_datetime(name="Date", date_breaks="day", date_labels="%b %d") +
  # Theme
  theme_bw()
g2

# Count by antenna
g3 <- ggplot(nantenna, aes(x=antenna_id, y=n)) +
  geom_col() +
  # Labels
  labs(x="Antenna", y="Number of detections") +
  # Theme
  theme_bw()
g3

# Count by time of day
g4 <- ggplot(data, aes(x=hour_of_day1)) +
  geom_histogram(binwidth = 0.25) +
  theme_bw()
g4

# Count by time of day
g5 <- ggplot(data, aes(x=hour_of_day2)) +
  geom_histogram(binwidth = 0.1) +
  # Labels
  labs(x="Hour day", y="Number of detections") +
  scale_x_continuous(breaks=seq(0,24, 1)) +
  # Theme
  theme_bw()
g5






