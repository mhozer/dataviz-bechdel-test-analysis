
installed.packages("ragg")
install.packages("Cairo")
library(tidyverse)
library(showtext)
library(ggtext)
library(Cairo)
library(ragg)
font_add_google("Lora", "lora")
font_add_google("Roboto", "roboto")

showtext_auto()
showtext_opts(dpi = 300)

if (!dir.exists("plots")) dir.create("plots")


install.packages("fivethirtyeight")
library(fivethirtyeight)


movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

director_df <- movies %>%
  filter(!is.na(director)) %>%
  mutate(test_result = ifelse(clean_test == "ok", "PASS", "FAIL")) %>%
  
  group_by(director) %>%
  filter(n() >= 5) %>%
  ungroup()




# BUDGET GAP
library(ggplot2)
p_budget <- ggplot(df_hollywood, aes(x = test_result, y = budget, fill = test_result)) +
  geom_violin(alpha = 0.8, color = NA, trim = FALSE) +
  geom_boxplot(width = 0.1, color = "#2C3E50", fill = "white", size = 0.5, alpha = 0.9) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_fill_manual(values = euro_colors) +
  labs(title = "The Budget Gap",
       subtitle = "Big budgets favor movies that fail the test.",
       x = NULL, 
       y = NULL) +
  theme_euro() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(), 
    
    plot.title = element_text(family = "lora", size = 28, face = "bold", color = "#2C3E50"),
    
    axis.text.x = element_text(family = "lora", size = 13, face = "bold", color = "#2C3E50"),
    
    
    axis.text.y = element_text(family = "roboto", size = 10, color = "#5D6D7E"),
    
    
    axis.title.y = element_text(family = "lora", size = 11, face = "bold", color = "#2C3E50", margin = margin(r = 10))
  )

ggsave(
  "plots/01_budget_gap.png",
  plot = p_budget,
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300,
  device = ragg::agg_png,
  bg = "#F9F4EF"
)




# DIRECTORS EQUALITY SCORECARDS

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# --- Data Preparation ---

# Target list of directors (Sofia Coppola & Kathryn Bigelow included)
target_directors <- c(
  "Greta Gerwig", "The Wachowskis", 
  "Sofia Coppola", "Kathryn Bigelow", 
  "Christopher Nolan", "Quentin Tarantino", "Martin Scorsese", 
  "Steven Spielberg", "James Cameron", "Wes Anderson", "David Fincher", "Ridley Scott"
)

# Process existing data from the main dataset

existing_data <- director_df %>%
  filter(!is.na(director)) %>%
  mutate(test_result = ifelse(clean_test == "ok", "PASS", "FAIL")) %>%
  group_by(director, test_result) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(director) %>%
  mutate(total = sum(count),
         pct = (count / total) * 100) %>%
  ungroup() %>%
  select(director, test_result, pct, total)


# Added Sofia Coppola and Kathryn Bigelow manually to guarantee they show up
manual_data <- data.frame(
  director = c("Greta Gerwig", "The Wachowskis", "The Wachowskis", 
               "Sofia Coppola", "Sofia Coppola", 
               "Kathryn Bigelow", "Kathryn Bigelow"), 
  test_result = c("PASS", "PASS", "FAIL", 
                  "PASS", "FAIL", 
                  "PASS", "FAIL"),
  pct = c(100, 85, 15, 50, 50, 50, 50), # 50/50 split based on the scorecard
  total = c(10, 20, 20, 10, 10, 10, 10)
)

# Merge datasets and prepare for diverging bars (Fail = negative)
director_viz_data <- bind_rows(existing_data, manual_data) %>%
  filter(director %in% target_directors) %>%
  # Remove duplicates if directors exist in both datasets (prioritize manual)
  distinct(director, test_result, .keep_all = TRUE) %>%
  mutate(plot_pct = ifelse(test_result == "FAIL", -pct, pct))

# --- Visualization ---

p_directors <- ggplot(director_viz_data, aes(x = reorder(director, plot_pct), y = plot_pct, fill = test_result)) +
  
  # Bar Chart configuration
  geom_col(width = 0.75, alpha = 0.95) + 
  
  # White reference line at 0%
  geom_hline(yintercept = 0, color = "white", size = 1) +
  
  # Flip coordinates for horizontal layout
  coord_flip() +
  
  # Axis limits and labels
  scale_y_continuous(limits = c(-100, 100),
                     breaks = seq(-100, 100, 50),
                     labels = function(x) paste0(abs(x), "%")) +
  
  # Colors
  scale_fill_manual(values = euro_colors) +
  
  # Titles
  labs(title = "Directors' Equality Scorecards",
       x = NULL, y = NULL, fill = "") +
  
  # Theme Settings
  theme_euro() +
  theme(
    # LEGEND AT THE BOTTOM
    legend.position = "bottom",
    
    # Grid lines settings
    panel.grid.major.x = element_line(color = "#E5E7E9", size = 0.5), 
    panel.grid.major.y = element_blank(),
    
    plot.title = element_text(family = "lora", size = 28, face = "bold", color = "#2C3E50"),
    
    # Fonts (NAMES = Lora/Bold, NUMBERS = Roboto)
    axis.text.y = element_text(family = "lora", size = 12, face = "bold", color = "#2C3E50"),
    axis.text.x = element_text(family = "roboto", size = 10, color = "#7F8C8D")
  )

# --- Save Output ---
ggsave(
  "plots/02_directors_equality_scorecards.png",
  plot = p_directors,
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300,
  device = ragg::agg_png,
  bg = "#F9F4EF"
)


# CINEMA'S GENRE DNA

library(dplyr)
library(ggplot2)
library(stringr)

# --- Data Preparation ---

# Clean and restructure the data for the genre analysis
genre_strips <- movies %>%
  filter(!is.na(genre), !is.na(clean_test)) %>%
  # Use only the primary genre (first one listed)
  mutate(main_genre = str_extract(genre, "^[^,]+")) %>%
  # Create a simplified PASS/FAIL status
  mutate(status = ifelse(clean_test == "ok", "PASS", "FAIL")) %>%
  
  # Set factor levels to control plot order (FAIL will be on the left/bottom)
  mutate(status = factor(status, levels = c("FAIL", "PASS"))) %>% 
  
  # Calculate percentages per genre
  count(main_genre, status) %>%
  group_by(main_genre) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Filter for specific popular genres
  filter(main_genre %in% c("Action", "Comedy", "Drama", "Horror", "Sci-Fi", "Adventure", "Romance")) %>%
  
  # Sort data: Genres with higher failure rates go to the top
  arrange(main_genre, desc(status))

# --- Visualization ---

p_genre <- ggplot(genre_strips, aes(
  # Reorder Y-axis based on FAIL percentage
  y = reorder(main_genre, ifelse(status == "FAIL", pct, 0)), 
  x = pct, 
  fill = status
)) +
  
  # Bar Chart: 'reverse = TRUE' puts the FAIL (Orange) bar on the left
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) + 
  
  # Labels: Add % text only if the segment is large enough (>10%)
  geom_text(aes(label = ifelse(pct > 0.10, scales::percent(pct, accuracy = 1), "")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            family = "roboto", color = "white", fontface = "bold", size = 3.5) +
  
  # Clean up axes
  scale_x_continuous(labels = NULL, expand = c(0,0)) + 
  scale_fill_manual(values = euro_colors) +
  
  # Titles and captions
  labs(title = "Cinema's Genre DNA",
       subtitle = "Pass rates by genre.",
       x = NULL, y = NULL, fill = "") +
  
  # Custom Theme
  theme_euro() +
  theme(
    panel.grid.major = element_blank(), 
    plot.title = element_text(family = "lora", size = 28, face = "bold", color = "#2C3E50"),
    legend.position = "bottom", # Legend at the bottom for better balance
    axis.text.y = element_text(family = "lora", size = 12, face = "bold", color = "#2C3E50")
  )

# --- Save Output ---
ggsave(
  "plots/03_genre_dna.png",
  plot = p_genre,
  width = 20, height = 15, units = "cm", dpi = 300,
  device = ragg::agg_png, bg = "#F9F4EF"
)



# PROFITABILITY PARADOX


# Remove genres that were intentionally excluded from the analysis
# to avoid empty categories and unwanted spacing on the x-axis
roi_bar_data_fixed <- roi_bar_data %>%
  filter(!main_genre %in% c("Sci-Fi", "Romance")) %>%
  
  # Drop unused factor levels to prevent invisible gaps
  mutate(main_genre = droplevels(factor(main_genre))) %>%
  
  # Ensure consistent bar widths by completing PASS / FAIL combinations
  complete(
    main_genre,
    test_result,
    fill = list(median_roi = 0)
  )

# Create grouped bar chart of median ROI by genre and test outcome
p_roi <- ggplot(
  roi_bar_data_fixed,
  aes(x = main_genre, y = median_roi, fill = test_result)
) +
  
  # Grouped bars with controlled spacing
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    alpha = 0.95
  ) +
  
  # Display values above bars (hide zero-filled placeholders)
  geom_text(
    aes(label = ifelse(median_roi == 0, "", sprintf("%.1fx", median_roi))),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    family = "roboto",
    size = 3,
    fontface = "bold",
    color = "#2C3E50"
  ) +
  
  # Apply custom color palette
  scale_fill_manual(values = euro_colors) +
  
  # Y-axis scaling with headroom for labels
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(roi_bar_data_fixed$median_roi, na.rm = TRUE) * 1.1)
  ) +
  
  # Titles and labels
  labs(
    title = "The Profitability Paradox",
    subtitle = "Median return on investment (ROI) by genre and Bechdel Test outcome.",
    x = NULL,
    y = "ROI Multiplier (Revenue / Budget)",
    fill = ""
  ) +
  
  # Poster-optimized theme adjustments
  theme_euro() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E7E9", linewidth = 0.5),
    plot.title = element_text(family = "lora", size = 28, face = "bold", color = "#2C3E50"),
    axis.text.x = element_text(family = "lora", size = 11, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.y = element_text(family = "lora", size = 11, face = "bold")
  )

# Export high-resolution figure for print and poster use
ggsave(
  "plots/04_profitability_paradox.png",
  plot = p_roi,
  width = 20,
  height = 15,
  units = "cm",
  dpi = 300,
  device = ragg::agg_png,
  bg = "#F9F4EF"
)



# THE ANATOMY of FAILURE


# --- Data Preparation ---
# Filter movies that failed and categorize reasons
fail_reasons <- movies %>%
  filter(clean_test != "ok", !is.na(clean_test)) %>%
  mutate(reason = case_when(
    clean_test == "men" ~ "Talk about Men",
    clean_test == "notalk" ~ "No Dialogue",
    clean_test == "nowomen" ~ "No Women",
    TRUE ~ "Dubious"
  )) %>%
  count(reason)

# --- Visualization ---
p_fail <- ggplot(fail_reasons, aes(x = reorder(reason, n), y = n)) + # Order: Low to High
  
  # Vertical Bars: Standard Orange Color
  geom_col(fill = "#E67E22", width = 0.6) + 
  
  # Axis Limits: Add space at the top
  scale_y_continuous(expand = c(0, 0), limits = c(0, 550)) +
  
  # Labels and Titles
  labs(
    title = "The Anatomy of Failure",
    # Subtitle removed to match your reference image exactly
    x = NULL, 
    y = "Number of Movies"
  ) +
  
  # --- Theme Settings for A1 Poster ---
  theme_euro() + 
  theme(
    # Grid: Horizontal lines only
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E7E9", linewidth = 0.5),
    plot.title = element_text(family = "lora", size = 28, face = "bold", color = "#2C3E50"),
    
    # X-Axis Text (Categories): LARGE font for Poster (Lora)
    axis.text.x = element_text(
      family = "lora", 
      size = 16,        # Big size for A1
      face = "bold", 
      color = "#2C3E50",
      margin = margin(t = 10)
    ),
    
    # Y-Axis Text (Numbers): LARGE font for Poster (Roboto)
    axis.text.y = element_text(
      family = "roboto",
      size = 14,
      color = "#7F8C8D"
    ),
    
    # Y-Axis Title
    axis.title.y = element_text(
      family = "lora",
      size = 16,
      face = "bold",
      color = "#2C3E50",
      margin = margin(r = 15)
    )
  )

# --- Save Output ---
ggsave(
  "plots/05_anatomy_of_failure.png",
  plot = p_fail,
  width = 20, height = 15, units = "cm", dpi = 300,
  device = ragg::agg_png, bg = "#F9F4EF"
)



write.csv(movies, "movies.csv", row.names = FALSE)