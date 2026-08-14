# ===============================================================================================================
#                                           BREEDING SUCCESS FOR SGPE & AYNA
# ===============================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(dplyr); library(tidyr); library(lubridate); library(ggplot2); library(patchwork); library(forcats)

# ============================
# Uploading required datasets 
# ============================

SGPE_final <- read.csv("SGPE_final.csv")
SGPE_all_clean <- read.csv("SGPE_all_clean.csv")
AYNA_final <- read.csv("AYNA_final.csv")
AYNA_sum <- read.csv("AYNA_sum_clean.csv")
AYNA_all_clean <- read.csv("AYNA_all_clean.csv")

# ------------
# Removing 2023 data from AYNA because it was not included in analysis
AYNA_all_clean <- AYNA_all_clean %>%
  filter(Year != "2023")
AYNA_final <- AYNA_final %>%
  filter(Year != "2023")
# ------------


# ================================================================
# Hatching success, Fledging success and Breeding success -> SGPE
# ================================================================

# Nest counts per year
SGPE_nest_count <- SGPE_final %>%
  group_by(Year) %>%
  summarise(nests = n_distinct(Nest_label))

# Breeding success = fledged/nests, fledging_success = fledged/hatched, hatching success = hatched/nests
SGPE_breeding_success <- SGPE_final %>%
  group_by(Year) %>%
  summarise(breeding_success = mean(fledged == "Yes"),
            fledging_success = sum(fledged == "Yes" & hatched == "Yes")/ sum(hatched == "Yes"),
            hatching_success = mean(hatched == "Yes")) %>%
  right_join(SGPE_nest_count, by = "Year")

# Exporting table
write.csv(SGPE_breeding_success, file = "SGPE_breeding_success.csv", row.names = FALSE)

# ================================================================
# Hatching success, Fledging success and Breeding success -> AYNA
# ================================================================

# Nest counts per year 
AYNA_nest_count <- AYNA_final %>%
  group_by(Year) %>%
  summarise(nests = n_distinct(Nest_label))

# Breeding success = fledged/nests, fledging_success = fledged/hatched, hatching success = hatched/nests
AYNA_breeding_success <- AYNA_final %>%
  group_by(Year) %>%
  summarise(breeding_success = mean(fledged == "Yes"),
            fledging_success = sum(fledged == "Yes" & hatched == "Yes")/ sum(hatched == "Yes"),
            hatching_success = mean(hatched == "Yes")) %>%
  right_join(AYNA_nest_count, by = "Year")

# Exporting table
write.csv(AYNA_breeding_success, file = "AYNA_breeding_success.csv", row.names = FALSE)

# =====================
# Egg Lay Date -> AYNA
# =====================

# ==========================================================
# Cleaning, identification and plotting of lay date windows
# ==========================================================

# Removing AYNA_sum$StageFound values that include CHIC or FAIL - cannot determine their date laid since CHIC will
# have been laid far before recorded date (inaccurate) and FAIL will not have produced a laid egg at all.
table(AYNA_sum$Year, AYNA_sum$StageFound)

AYNA_sum <- AYNA_sum %>%
  filter(!StageFound %in% c("FAIL", "CHIC"))

# Finding the lay date windows for each year - identifying the earliest and latest date egg was found 
AYNA_sum$DateFound <- as.Date(AYNA_sum$DateFound)

AYNA_laydate <- AYNA_sum %>%
  group_by(Year) %>%
  summarise(EarliestDate = min(DateFound, na.rm = TRUE),
            LatestDate = max(DateFound, na.rm = TRUE),
            WindowDays = as.numeric(LatestDate - EarliestDate))


# Creating new column with yearless dates for easier laydate visualisation in the plot
AYNA_laydate <- AYNA_sum %>%
  mutate(Date_no_year = make_date(year = 2000,
                                  month = month(DateFound),
                                  day   = day(DateFound)))

# Creating yearless earliest & latest dates for easier laydate visualisation in the plot
AYNA_laydate <- AYNA_laydate %>%
  group_by(Year) %>%
  mutate(EarliestDate = min(Date_no_year, na.rm = TRUE),
         LatestDate   = max(Date_no_year, na.rm = TRUE))

# Plot to show lay date window each year
ggplot(AYNA_laydate) +
  geom_segment(aes(x = EarliestDate,
                   xend = LatestDate,
                   y = Year,
                   yend = Year),
               linewidth = 4,
               colour = "tomato3") +
  scale_x_date(date_labels = "%d-%b",
               limits = c(min(AYNA_laydate$EarliestDate),
                          max(AYNA_laydate$LatestDate)),
               date_breaks = "1 week") +
  labs(x = "AYNA Lay Date", y = "Year") +
  theme_minimal() 

# ===============================================================================================================
#                                           BREEDING SEASON TIMELINES
# ===============================================================================================================

# =======================================
# SGPE - Total breeding season durations
# =======================================

# Checking entire breeding season duration per year
SGPE_breeding_seasons <- SGPE_final %>%
  group_by(Year) %>%
  summarise(Season_start = min(Date, na.rm = TRUE),
            Season_end = max(Date, na.rm = TRUE))

# Formatting yearless season start and end dates + setting season start in austral winter (Jul) for visualisation
SGPE_breeding_seasons <- SGPE_final %>%
  mutate(Date_no_year = if_else(month(Date) >= 8,
                                make_date(2000, month(Date), day(Date)),
                                make_date(2001, month(Date), day(Date)))) %>%
  group_by(Year) %>%
  summarise(Season_start = min(Date_no_year, na.rm = TRUE),
            Season_end   = max(Date_no_year, na.rm = TRUE))

# Plot to show breeding season window each year
SGPE_breeding_plot <- ggplot(SGPE_breeding_seasons) + 
  geom_segment(aes(x = Season_start,
                   xend = Season_end,
                   y = Year,
                   yend = Year),
               linewidth = 4,
               colour = "cyan4") +
  scale_x_date(date_labels = "%d-%b",
               limits = c(min(SGPE_breeding_seasons$Season_start),
                          max(SGPE_breeding_seasons$Season_end)),
               date_breaks = "2 week") +
  scale_y_continuous(breaks = seq(min(SGPE_breeding_seasons$Year), max(SGPE_breeding_seasons$Year), by = 1)) +
  labs(x="SGPE Breeding Season Duration", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===================================================
# SGPE - Breeding season breakdown into chick stages 
# ===================================================

# Identifying the start and end dates for incubation, chick and fledging stages. Since there is a notation 
# discrepancy, I will use new column nest_outcome. 
# Incubation stage -> min date = earliest date for Egg Laid, max date = latest date Egg Laid
# Chick stage -> min date = earliest date Chick Hatched, max date = latest date Chick Fledged 
# Fledging period -> min date = earliest date Chick Fledged, max date = latest date Chick Fledged

SGPE_breakdown <- SGPE_all_clean%>%
  group_by(Year) %>%
  summarise(incubation_start = min(Date[nest_outcome == "Egg Laid"], na.rm = TRUE),
            incubation_end = max(Date[nest_outcome == "Egg Laid"], na.rm = TRUE),
            chick_start = min(Date[nest_outcome == "Chick Hatched"], na.rm = TRUE),
            chick_end = max(Date[nest_outcome == "Chick Fledged"], na.rm = TRUE),
            fledging_start = min(Date[nest_outcome == "Chick Fledged"], na.rm = TRUE),
            fledging_end= max(Date[nest_outcome == "Chick Fledged"], na.rm = TRUE),
            .groups = "drop")

# Converting columns for Date format
SGPE_breakdown <- SGPE_breakdown %>%
  mutate(across(c(incubation_start, incubation_end, 
                  chick_start, chick_end, 
                  fledging_start, fledging_end), as.Date))

# Exporting table
write.csv(SGPE_breakdown, file = "SGPE_breakdown.csv", row.names = FALSE)

# Some years record fledging on only a single day - adding one more day so fledging shows up as window in plot
SGPE_breakdown_plot <- SGPE_breakdown %>%
  mutate(new_fledging_end = if_else(fledging_end == fledging_start,
                                    fledging_end + days(1),
                                    fledging_end))

# Converting dates to yearless for plot visualisation 
SGPE_breakdown_plot <- SGPE_breakdown_plot %>%
  mutate(across(c(incubation_start, incubation_end, chick_start, chick_end, fledging_start, new_fledging_end),
                ~ if_else(month(.) >= 8,
                          make_date(2000, month(.), day(.)),
                          make_date(2001, month(.), day(.))),
                .names = "{.col}_no_year"))

# Reformatting table to long format for plotting 
SGPE_breakdown_plot <- SGPE_breakdown_plot %>%
  transmute(Year, Incubation_start = incubation_start_no_year, Incubation_end = incubation_end_no_year,
            Chick_start = chick_start_no_year, Chick_end = chick_end_no_year,
            Fledging_start = fledging_start_no_year, Fledging_end = new_fledging_end_no_year) %>%
  pivot_longer(cols = -Year,
               names_to = c("Stage", ".value"),
               names_sep = "_")

# Formatting stage and year order 
SGPE_breakdown_plot <- SGPE_breakdown_plot %>%
  mutate(Stage = factor(Stage, levels = c("Incubation", "Chick", "Fledging")),
         YearStage = factor(paste(Year, Stage),
                            levels = unlist(lapply(sort(unique(Year)),
                                                   function(y) paste(y, c("Incubation", "Chick", "Fledging")))))) %>%
  mutate(YearStage = fct_rev(YearStage))

# Plotting chick stage 
SGPE_breeding_stages_plot <- ggplot(SGPE_breakdown_plot) +
  geom_segment(aes(x = start,
                   xend = end,
                   y = YearStage,
                   yend = YearStage,
                   colour = Stage),
               linewidth = 4) +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_discrete(labels = function(x) sapply(seq_along(x), function(i) if ((i %% 3) == 1)
    sub("\\s.+$", "", x[i]) else "")) +
  scale_colour_manual(values = c("Incubation" = "skyblue3", "Chick" = "seagreen3", "Fledging" = "cyan4")) + 
  labs(x = "Date",
       y = "Year",
       colour = "SGPE Nest Stage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8, vjust = -1),
        legend.position = "bottom")

print(SGPE_breeding_stages_plot)

ggsave(filename = "figures/SGPE_breeding_stages_plot.png", 
       plot = SGPE_breeding_stages_plot, width = 14, height = 8, dpi = 300)



# =======================================
# AYNA - Total breeding season durations
# =======================================

# Checking entire breeding season duration per year
AYNA_breeding_seasons <- AYNA_final %>%
  group_by(Year) %>%
  summarise(Season_start = min(Date, na.rm = TRUE),
            Season_end = max(Date, na.rm = TRUE))

# Formatting yearless season start and end dates + setting season start in austral winter (Jul) for visualisation
AYNA_breeding_seasons <- AYNA_final %>%
  mutate(Date_no_year = if_else(month(Date) >= 8,
                                make_date(2000, month(Date), day(Date)),
                                make_date(2001, month(Date), day(Date)))) %>%
  group_by(Year) %>%
  summarise(Season_start = min(Date_no_year, na.rm = TRUE),
            Season_end   = max(Date_no_year, na.rm = TRUE))

# Plot to show breeding season window each year
AYNA_breeding_plot <- ggplot(AYNA_breeding_seasons) + 
  geom_segment(aes(x = Season_start,
                   xend = Season_end,
                   y = Year,
                   yend = Year),
               linewidth = 4,
               colour = "tomato3") +
  scale_x_date(date_labels = "%d-%b",
               limits = c(min(AYNA_breeding_seasons$Season_start),
                          max(AYNA_breeding_seasons$Season_end)),
               date_breaks = "2 week") +
  scale_y_continuous(breaks = seq(min(AYNA_breeding_seasons$Year), max(AYNA_breeding_seasons$Year), by = 1)) +
  labs(x="AYNA Breeding Season Duration", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==================================================
# AYNA- Breeding season breakdown into chick stages 
# ==================================================

# Identifying the start and end dates for incubation, chick and fledging stages. Since there is a notation 
# discrepancy, I will use new column nest_outcome. 
# Incubation stage -> min date = earliest date for Egg Laid, max date = latest date Egg Laid 
# Chick stage -> min date = earliest date Chick Hatched, max date = latest date Chick Fledged 
# Fledging period -> min date = earliest date Chick Fledged, max date = latest date Chick Fledged

AYNA_breakdown <- AYNA_all_clean %>%
  group_by(Year) %>%
  summarise(incubation_start = min(Date[nest_outcome == "Egg Laid"], na.rm = TRUE),
            incubation_end = max(Date[nest_outcome == "Egg Laid"], na.rm = TRUE),
            chick_start = min(Date[nest_outcome == "Chick Hatched"], na.rm = TRUE),
            chick_end = max(Date[nest_outcome == "Chick Fledged"], na.rm = TRUE),
            fledging_start = min(Date[nest_outcome == "Chick Fledged"], na.rm = TRUE),
            fledging_end= max(Date[nest_outcome == "Chick Fledged"], na.rm = TRUE),
            .groups = "drop")

# Formatting as Date
AYNA_breakdown <- AYNA_breakdown %>%
  mutate(across(c(incubation_start, incubation_end, 
                  chick_start, chick_end, 
                  fledging_start, fledging_end), as.Date))

# Some years record fledging on only a single day - adding one more day so fledging shows up as window in plot
AYNA_breakdown_plot <- AYNA_breakdown %>%
  mutate(new_fledging_end = if_else(fledging_end == fledging_start,
                                    fledging_end + days(1),
                                    fledging_end))

# Exporting table
write.csv(AYNA_breakdown, file = "AYNA_breakdown.csv", row.names = FALSE)

# Converting dates to yearless for plot visualisation 
AYNA_breakdown_plot <- AYNA_breakdown_plot %>%
  mutate(across(c(incubation_start, incubation_end, chick_start, chick_end, fledging_start, new_fledging_end),
                ~ if_else(month(.) >= 8,
                          make_date(2000, month(.), day(.)),
                          make_date(2001, month(.), day(.))),
                .names = "{.col}_no_year"))

# Reformatting table to long format for plotting 
AYNA_breakdown_plot <- AYNA_breakdown_plot %>%
  transmute(Year, Incubation_start = incubation_start_no_year, Incubation_end = incubation_end_no_year,
            Chick_start = chick_start_no_year, Chick_end = chick_end_no_year,
            Fledging_start = fledging_start_no_year, Fledging_end = new_fledging_end_no_year) %>%
  pivot_longer(cols = -Year,
               names_to = c("Stage", ".value"),
               names_sep = "_")

# Formatting stage and year order 
AYNA_breakdown_plot <- AYNA_breakdown_plot %>%
  mutate(Stage = factor(Stage, levels = c("Incubation", "Chick", "Fledging")),
         YearStage = factor(paste(Year, Stage),
                            levels = unlist(lapply(sort(unique(Year)),
                                                   function(y) paste(y, c("Incubation", "Chick", "Fledging")))))) %>%
  mutate(YearStage = fct_rev(YearStage))

# Plotting chick stage 
AYNA_breeding_stages_plot <- ggplot(AYNA_breakdown_plot) +
  geom_segment(aes(x = start,
                   xend = end,
                   y = YearStage,
                   yend = YearStage,
                   colour = Stage),
               linewidth = 4) +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_discrete(labels = function(x) sapply(seq_along(x), function(i) if ((i %% 3) == 1)
    sub("\\s.+$", "", x[i]) else "")) +
  scale_colour_manual(values = c("Incubation" = "peru", "Chick" = "sienna1", "Fledging" = "tomato3")) + 
  labs(x = "Date",
       y = "Year",
       colour = "AYNA Nest Stage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8, vjust = -1),
        legend.position = "bottom")

print(AYNA_breeding_stages_plot)


ggsave(filename = "figures/AYNA_breeding_stages_plot.png", 
       plot = AYNA_breeding_stages_plot, width = 14, height = 8, dpi = 300)



# =============================================================
# Combined breeding season plots for both species side-by-side 
# =============================================================

# Total breeding season durations
breeding_seasons <- SGPE_breeding_plot | AYNA_breeding_plot
breeding_seasons

# Breeding season breakdown into chick stages 
breeding_stages <- SGPE_breeding_stages_plot | AYNA_breeding_stages_plot
breeding_stages




# FINAL FIGURES FOR PRESENTATION AND DISSERTATION



# ========================================================================
# NEW PLOT: SGPE early and late incubation candidate windows across years
# ========================================================================

SGPE_incubation_windows_plot_data <- SGPE_breakdown_plot %>%
  filter(Stage == "Incubation") %>%
  mutate(incubation_days = as.numeric(end - start),
         incubation_midpoint = start + floor(incubation_days / 2),
         early_start = start,
         early_end = incubation_midpoint,
         late_start = incubation_midpoint + days(1),
         late_end = end,
         Year = factor(Year, levels = sort(unique(Year))))

SGPE_incubation_windows_plot <- ggplot(SGPE_incubation_windows_plot_data) +
  geom_segment(aes(x = start,
                   xend = end,
                   y = Year,
                   yend = Year),
               linewidth = 7,
               colour = "grey80") +
  geom_segment(aes(x = early_start,
                   xend = early_end,
                   y = Year,
                   yend = Year,
                   colour = "Early incubation"),
               linewidth = 4) +
  geom_segment(aes(x = late_start,
                   xend = late_end,
                   y = Year,
                   yend = Year,
                   colour = "Late incubation"),
               linewidth = 4) +
  scale_colour_manual(values = c("Early incubation" = "skyblue3",
                                 "Late incubation" = "cyan4"),
                      name = "Candidate window") +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_discrete(limits = rev) +
  labs(x = "Date",
       y = "Breeding year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom")

print(SGPE_incubation_windows_plot)

ggsave(filename = "figures/SGPE_incubation_candidate_windows.png", plot = SGPE_incubation_windows_plot,
       width = 10, height = 6, dpi = 300)


# ==================================================================================
# NEW PLOT: SGPE incubation and chick-rearing stages with early and late sub-windows
# ==================================================================================

SGPE_candidate_stage_plot_data <- SGPE_breakdown_plot %>%
  filter(Stage %in% c("Incubation", "Chick")) %>%
  mutate(stage_days = as.numeric(end - start),
         stage_midpoint = start + floor(stage_days / 2),
         early_start = start,
         early_end = stage_midpoint,
         late_start = stage_midpoint + days(1),
         late_end = end,
         Stage = recode(Stage, "Chick" = "Chick-rearing"))

# Forcing y axis spacing to make plot visuals clearer 
year_levels <- sort(unique(SGPE_candidate_stage_plot_data$Year))

n_years <- length(year_levels)

SGPE_candidate_stage_plot_data <- SGPE_candidate_stage_plot_data %>%
  mutate(year_index = match(Year, year_levels),
         y_position = (year_index - 1) * 3 + if_else(Stage == "Incubation", 1,2))

SGPE_candidate_stage_windows <- SGPE_candidate_stage_plot_data %>%
  select(Year,
         Stage,
         y_position,
         early_start,
         early_end,
         late_start,
         late_end) %>%
  pivot_longer(cols = c(early_start, early_end, late_start, late_end),
               names_to = c("Window", ".value"),
               names_sep = "_") %>%
  mutate(Window = recode(Window,
                         "early" = "Early",
                         "late" = "Late"),
         Stage_window = factor(paste(Window, Stage),
                               levels = c("Early Incubation",
                                          "Late Incubation",
                                          "Early Chick-rearing",
                                          "Late Chick-rearing")))

stage_axis_labels <- SGPE_candidate_stage_plot_data %>%
  distinct(Year, Stage, y_position) %>%
  arrange(y_position) %>%
  mutate(axis_label = if_else(Stage == "Incubation", as.character(Year),""))


# Y axis seperators
year_separator_positions <- seq(from = 3,
                                to = 3 * (n_years - 1),
                                by = 3)


SGPE_candidate_stages_plot <- ggplot() +
  geom_hline(yintercept = year_separator_positions,
             colour = "grey82",
             linewidth = 0.5) +
  geom_segment(data = SGPE_candidate_stage_plot_data,
               aes(x = start,
                   xend = end,
                   y = y_position,
                   yend = y_position),
               linewidth = 5.5,
               colour = "white") +
  geom_segment(data = SGPE_candidate_stage_windows,
               aes(x = start,
                   xend = end,
                   y = y_position,
                   yend = y_position,
                   colour = Stage_window),
               linewidth = 4) +
  scale_colour_manual(values = c("Early Incubation" = "skyblue3",
                                 "Late Incubation" = "dodgerblue3",
                                 "Early Chick-rearing" = "sandybrown",
                                 "Late Chick-rearing" = "sienna1"),
                      breaks = c("Early Incubation",
                                 "Late Incubation",
                                 "Early Chick-rearing",
                                 "Late Chick-rearing"),
                      name = "Candidate window") +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_continuous(trans = "reverse",
                     breaks = stage_axis_labels$y_position,
                     labels = stage_axis_labels$axis_label,
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Date",
       y = "Breeding Year") +
guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 28, hjust = 1),
        axis.text.y = element_text(size = 28, hjust = 1.5, vjust = 1),
        axis.title.x = element_text(size = 35),
        axis.title.y = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(SGPE_candidate_stages_plot)

ggsave(filename = "figures/SGPE_all_candidate_windows.png",
       plot = SGPE_candidate_stages_plot,
       width = 12, height = 6, dpi = 300)


# ========================================================================
# NEW PLOT: SGPE early and late incubation candidate windows across years
# ========================================================================

SGPE_incubation_windows_plot_data <- SGPE_breakdown_plot %>%
  filter(Stage == "Incubation") %>%
  mutate(incubation_days = as.numeric(end - start),
         incubation_midpoint = start + floor(incubation_days / 2),
         early_start = start,
         early_end = incubation_midpoint,
         late_start = incubation_midpoint + days(1),
         late_end = end,
         Year = factor(Year, levels = sort(unique(Year))))

SGPE_incubation_windows_plot <- ggplot(SGPE_incubation_windows_plot_data) +
  geom_segment(aes(x = start,
                   xend = end,
                   y = Year,
                   yend = Year),
               linewidth = 7,
               colour = "grey80") +
  geom_segment(aes(x = early_start,
                   xend = early_end,
                   y = Year,
                   yend = Year,
                   colour = "Early incubation"),
               linewidth = 4) +
  geom_segment(aes(x = late_start,
                   xend = late_end,
                   y = Year,
                   yend = Year,
                   colour = "Late incubation"),
               linewidth = 4) +
  scale_colour_manual(values = c("Early incubation" = "skyblue3",
                                 "Late incubation" = "cyan4"),
                      name = "Candidate window") +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_discrete(limits = rev) +
  labs(x = "Date",
       y = "Breeding year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom")

print(SGPE_incubation_windows_plot)

ggsave(filename = "figures/SGPE_incubation_candidate_windows.png", plot = SGPE_incubation_windows_plot,
       width = 10, height = 6, dpi = 300)


# ==================================================================================
# NEW PLOT: SGPE incubation and chick-rearing stages with early and late sub-windows
# ==================================================================================

SGPE_candidate_stage_plot_data <- SGPE_breakdown_plot %>%
  filter(Stage %in% c("Incubation", "Chick")) %>%
  mutate(stage_days = as.numeric(end - start),
         stage_midpoint = start + floor(stage_days / 2),
         early_start = start,
         early_end = stage_midpoint,
         late_start = stage_midpoint + days(1),
         late_end = end,
         Stage = recode(Stage, "Chick" = "Chick-rearing"))

# Forcing y axis spacing to make plot visuals clearer 
year_levels <- sort(unique(SGPE_candidate_stage_plot_data$Year))

n_years <- length(year_levels)

SGPE_candidate_stage_plot_data <- SGPE_candidate_stage_plot_data %>%
  mutate(year_index = match(Year, year_levels),
         y_position = (year_index - 1) * 3 + if_else(Stage == "Incubation", 1,2))

SGPE_candidate_stage_windows <- SGPE_candidate_stage_plot_data %>%
  select(Year,
         Stage,
         y_position,
         early_start,
         early_end,
         late_start,
         late_end) %>%
  pivot_longer(cols = c(early_start, early_end, late_start, late_end),
               names_to = c("Window", ".value"),
               names_sep = "_") %>%
  mutate(Window = recode(Window,
                         "early" = "Early",
                         "late" = "Late"),
         Stage_window = factor(paste(Window, Stage),
                               levels = c("Early Incubation",
                                          "Late Incubation",
                                          "Early Chick-rearing",
                                          "Late Chick-rearing")))

stage_axis_labels <- SGPE_candidate_stage_plot_data %>%
  distinct(Year, Stage, y_position) %>%
  arrange(y_position) %>%
  mutate(axis_label = if_else(
    Stage == "Incubation",
    if_else(Year == 2011, paste0("* ", Year),
    if_else(Year == 2023, paste0("** ", Year), 
            as.character(Year))), ""))

# Y axis seperators
year_separator_positions <- seq(from = 3,
                                to = 3 * (n_years - 1),
                                by = 3)


SGPE_candidate_stages_plot <- ggplot() +
  geom_hline(yintercept = year_separator_positions,
             colour = "grey82",
             linewidth = 0.5) +
  geom_segment(data = SGPE_candidate_stage_plot_data,
               aes(x = start,
                   xend = end,
                   y = y_position,
                   yend = y_position),
               linewidth = 5.5,
               colour = "white") +
  geom_segment(data = SGPE_candidate_stage_windows,
               aes(x = start,
                   xend = end,
                   y = y_position,
                   yend = y_position,
                   colour = Stage_window),
               linewidth = 4) +
  scale_colour_manual(values = c("Early Incubation" = "skyblue3",
                                 "Late Incubation" = "dodgerblue3",
                                 "Early Chick-rearing" = "sandybrown",
                                 "Late Chick-rearing" = "sienna1"),
                      breaks = c("Early Incubation",
                                 "Late Incubation",
                                 "Early Chick-rearing",
                                 "Late Chick-rearing"),
                      name = "Candidate window") +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_continuous(trans = "reverse",
                     breaks = stage_axis_labels$y_position,
                     labels = stage_axis_labels$axis_label, 
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Date",
       y = "Breeding Year") +
guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13, hjust = 1),
        axis.text.y = element_text(size = 13, hjust = 1.5, vjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(SGPE_candidate_stages_plot)

ggsave(filename = "figures/SGPE_all_candidate_windows.png", plot = SGPE_candidate_stages_plot,
       width = 12, height = 6, dpi = 300)







# ========================================================================
# NEW PLOT: AYNA early and late incubation candidate windows across years
# ========================================================================

AYNA_incubation_windows_plot_data <- AYNA_breakdown_plot %>%
  filter(Stage == "Incubation") %>%
  mutate(incubation_days = as.numeric(end - start),
         incubation_midpoint = start + floor(incubation_days / 2),
         early_start = start,
         early_end = incubation_midpoint,
         late_start = incubation_midpoint + days(1),
         late_end = end,
         Year = factor(Year, levels = sort(unique(Year))))

AYNA_incubation_windows_plot <- ggplot(AYNA_incubation_windows_plot_data) +
  geom_segment(aes(x = start,
                   xend = end,
                   y = Year,
                   yend = Year),
               linewidth = 7,
               colour = "grey80") +
  geom_segment(aes(x = early_start,
                   xend = early_end,
                   y = Year,
                   yend = Year,
                   colour = "Early incubation"),
               linewidth = 4) +
  geom_segment(aes(x = late_start,
                   xend = late_end,
                   y = Year,
                   yend = Year,
                   colour = "Late incubation"),
               linewidth = 4) +
  scale_colour_manual(values = c("Early incubation" = "skyblue3",
                                 "Late incubation" = "cyan4"),
                      name = "Candidate window") +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_discrete(limits = rev) +
  labs(x = "Date",
       y = "Breeding year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom")

print(AYNA_incubation_windows_plot)

ggsave(filename = "figures/AYNA_incubation_candidate_windows.png", plot = AYNA_incubation_windows_plot,
       width = 10, height = 6, dpi = 300)


# ==================================================================================
# NEW PLOT: AYNA incubation and chick-rearing stages with early and late sub-windows
# ==================================================================================

AYNA_candidate_stage_plot_data <- AYNA_breakdown_plot %>%
  filter(Stage %in% c("Incubation", "Chick")) %>%
  mutate(stage_days = as.numeric(end - start),
         stage_midpoint = start + floor(stage_days / 2),
         early_start = start,
         early_end = stage_midpoint,
         late_start = stage_midpoint + days(1),
         late_end = end,
         Stage = recode(Stage, "Chick" = "Chick-rearing"))

# Forcing y axis spacing to make plot visuals clearer 
year_levels <- sort(unique(AYNA_candidate_stage_plot_data$Year))

n_years <- length(year_levels)

AYNA_candidate_stage_plot_data <- AYNA_candidate_stage_plot_data %>%
  mutate(year_index = match(Year, year_levels),
         y_position = (year_index - 1) * 3 + if_else(Stage == "Incubation", 1,2))

AYNA_candidate_stage_windows <- AYNA_candidate_stage_plot_data %>%
  select(Year,
         Stage,
         y_position,
         early_start,
         early_end,
         late_start,
         late_end) %>%
  pivot_longer(cols = c(early_start, early_end, late_start, late_end),
               names_to = c("Window", ".value"),
               names_sep = "_") %>%
  mutate(Window = recode(Window,
                         "early" = "Early",
                         "late" = "Late"),
         Stage_window = factor(paste(Window, Stage),
                               levels = c("Early Incubation",
                                          "Late Incubation",
                                          "Early Chick-rearing",
                                          "Late Chick-rearing")))

stage_axis_labels <- AYNA_candidate_stage_plot_data %>%
  distinct(Year, Stage, y_position) %>%
  arrange(y_position) %>%
  mutate(axis_label = if_else(Stage == "Incubation", as.character(Year),""))


# Y axis seperators
year_separator_positions <- seq(from = 3,
                                to = 3 * (n_years - 1),
                                by = 3)


AYNA_candidate_stages_plot <- ggplot() +
  geom_hline(yintercept = year_separator_positions,
             colour = "grey82",
             linewidth = 0.5) +
  geom_segment(data = AYNA_candidate_stage_plot_data,
               aes(x = start,
                   xend = end,
                   y = y_position,
                   yend = y_position),
               linewidth = 5.5,
               colour = "white") +
  geom_segment(data = AYNA_candidate_stage_windows,
               aes(x = start,
                   xend = end,
                   y = y_position,
                   yend = y_position,
                   colour = Stage_window),
               linewidth = 4) +
  scale_colour_manual(values = c("Early Incubation" = "skyblue3",
                                 "Late Incubation" = "dodgerblue3",
                                 "Early Chick-rearing" = "sandybrown",
                                 "Late Chick-rearing" = "sienna1"),
                      breaks = c("Early Incubation",
                                 "Late Incubation",
                                 "Early Chick-rearing",
                                 "Late Chick-rearing"),
                      name = "Candidate window") +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month") +
  scale_y_continuous(trans = "reverse",
                     breaks = stage_axis_labels$y_position,
                     labels = stage_axis_labels$axis_label,
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Date",
       y = "Breeding Year") +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15, hjust = 1),
        axis.text.y = element_text(size = 15, hjust = 1.5, vjust = 1),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 16),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(AYNA_candidate_stages_plot)

ggsave(filename = "figures/AYNA_all_candidate_windows.png", plot = AYNA_candidate_stages_plot,
       width = 15, height = 8, dpi = 300)
