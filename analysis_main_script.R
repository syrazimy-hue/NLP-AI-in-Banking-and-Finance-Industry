###############################################################################
#  SENTIMENT ANALYSIS: Artificial Intelligence in Banking and Finance Industry
#  Method  : Lexicon-based sentiment analysis (Bing, NRC, AFINN)
#  Input   : dataset_clean.xlsx  (PaperID, Abstract)
###############################################################################

# ============================================================================
# STEP 0 — CLEAR ENVIRONMENT & SET WORKING DIRECTORY
# ============================================================================

# ============================================================================
# STEP 1 — INSTALL & LOAD REQUIRED PACKAGES
# ============================================================================
# List of required packages
required_packages <- c(
  "readxl",        # Read Excel files
  
  "tidyverse",     # Data manipulation & ggplot2
  "tidytext",      # Text mining in tidy format
  "textdata",      # Sentiment lexicons (AFINN, Bing, NRC)
  "wordcloud",     # Word cloud visualisation
  "RColorBrewer",  # Colour palettes
  "reshape2",      # Data reshaping (for comparison cloud)
  "ggwordcloud",   # ggplot-based word clouds
  "scales",        # Formatting axes in plots
  "writexl",       # Export results to Excel
  "syuzhet"        # Additional sentiment scoring
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
  cat("Installed:", paste(new_packages, collapse = ", "), "\n")
} else {
  cat("All required packages are already installed.\n")
}

# Load libraries
lapply(required_packages, library, character.only = TRUE)
cat("All packages loaded successfully.\n")

# ============================================================================
# STEP 2 — LOAD DATASET
# ============================================================================
dataset_clean <- read_excel("dataset_clean.xlsx")

# Quick inspection
cat("\n--- Dataset Overview ---\n")
cat("Dimensions:", nrow(dataset_clean), "rows x", ncol(dataset_clean), "columns\n")
cat("Column names:", paste(colnames(dataset_clean), collapse = ", "), "\n")
str(dataset_clean)
head(dataset_clean, 5)

# Check for missing abstracts
missing_count <- sum(is.na(dataset_clean$Abstract) | dataset_clean$Abstract == "")
cat("Missing/empty abstracts:", missing_count, "\n")

# Remove rows with missing abstracts (if any)
dataset_clean <- dataset_clean %>%
  filter(!is.na(Abstract) & Abstract != "")

cat("Final dataset size:", nrow(dataset_clean), "papers\n")

# ============================================================================
# STEP 3 — TEXT TOKENISATION (UNNEST TOKENS)
# ============================================================================
# Tokenise abstracts into individual words
tidy_abstracts <- dataset_clean %>%
  unnest_tokens(word, Abstract)

cat("\nTotal tokens (words):", nrow(tidy_abstracts), "\n")

# Remove stop words
data("stop_words")
tidy_abstracts <- tidy_abstracts %>%
  anti_join(stop_words, by = "word")

cat("Tokens after removing stop words:", nrow(tidy_abstracts), "\n")

# Remove purely numeric tokens
tidy_abstracts <- tidy_abstracts %>%
  filter(!str_detect(word, "^[0-9]+$"))

cat("Tokens after removing numbers:", nrow(tidy_abstracts), "\n")

# ============================================================================
# STEP 4 — WORD FREQUENCY ANALYSIS
# ============================================================================
word_freq <- tidy_abstracts %>%
  count(word, sort = TRUE)

cat("\n--- Top 20 Most Frequent Words ---\n")
print(head(word_freq, 20))

# Save word frequency table
write_xlsx(word_freq, "Table_Word_Frequency.xlsx")
cat("Saved: Table_Word_Frequency.xlsx\n")

# --- Figure 1: Top 20 Most Frequent Words (Bar Chart) ---
fig1 <- word_freq %>%
  slice_max(n, n = 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word, fill = n)) +
  geom_col(show.legend = FALSE) +
  scale_fill_gradient(low = "#2C7BB6", high = "#D7191C") +
  labs(
    title = "Top 20 Most Frequent Words in Abstracts",
    subtitle = "AI in Banking and Finance Industry",
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_01_Top20_Word_Frequency.png", fig1, width = 10, height = 7, dpi = 300)
cat("Saved: Figure_01_Top20_Word_Frequency.png\n")

# --- Figure 2: Word Cloud ---
png("Figure_02_WordCloud.png", width = 2000, height = 1600, res = 300)
wordcloud(
  words = word_freq$word,
  freq  = word_freq$n,
  min.freq = 50,
  max.words = 150,
  random.order = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(8, "Dark2"),
  scale = c(4, 0.5)
)
title(main = "Word Cloud — AI in Banking and Finance Abstracts", 
      cex.main = 1.2, font.main = 2)
dev.off()
cat("Saved: Figure_02_WordCloud.png\n")

# ============================================================================
# STEP 5 — SENTIMENT ANALYSIS USING BING LEXICON
# ============================================================================
bing_lexicon <- get_sentiments("bing")

bing_sentiment <- tidy_abstracts %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(word, sentiment, sort = TRUE)

cat("\n--- Bing Sentiment: Top Words ---\n")
print(head(bing_sentiment, 20))

# Save Bing sentiment word table
write_xlsx(bing_sentiment, "Table_Bing_Sentiment_Words.xlsx")
cat("Saved: Table_Bing_Sentiment_Words.xlsx\n")

# --- Bing: Aggregate counts ---
bing_summary <- bing_sentiment %>%
  group_by(sentiment) %>%
  summarise(total_count = sum(n), .groups = "drop")

cat("\n--- Bing Sentiment Summary ---\n")
print(bing_summary)

write_xlsx(bing_summary, "Table_Bing_Sentiment_Summary.xlsx")
cat("Saved: Table_Bing_Sentiment_Summary.xlsx\n")

# --- Figure 3: Bing Sentiment — Top 15 Positive vs Negative Words ---
fig3 <- bing_sentiment %>%
  group_by(sentiment) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(
    n = ifelse(sentiment == "negative", -n, n),
    word = reorder(word, n)
  ) %>%
  ggplot(aes(x = n, y = word, fill = sentiment)) +
  geom_col(show.legend = TRUE) +
  scale_fill_manual(values = c("negative" = "#D7191C", "positive" = "#2C7BB6")) +
  labs(
    title = "Bing Sentiment: Top 15 Positive & Negative Words",
    subtitle = "AI in Banking and Finance Industry",
    x = "Frequency (negative shown as negative values)",
    y = NULL,
    fill = "Sentiment"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_03_Bing_Top15_PosNeg.png", fig3, width = 10, height = 8, dpi = 300)
cat("Saved: Figure_03_Bing_Top15_PosNeg.png\n")

# --- Figure 4: Bing Sentiment — Overall Proportion Pie Chart ---
fig4 <- bing_summary %>%
  mutate(
    percentage = round(total_count / sum(total_count) * 100, 1),
    label = paste0(sentiment, "\n", total_count, " (", percentage, "%)")
  ) %>%
  ggplot(aes(x = "", y = total_count, fill = sentiment)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("negative" = "#D7191C", "positive" = "#2C7BB6")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  labs(
    title = "Overall Sentiment Distribution (Bing Lexicon)",
    subtitle = "AI in Banking and Finance Industry",
    fill = "Sentiment"
  ) +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Figure_04_Bing_Sentiment_Pie.png", fig4, width = 8, height = 7, dpi = 300)
cat("Saved: Figure_04_Bing_Sentiment_Pie.png\n")

# --- Figure 5: Bing Comparison Word Cloud ---
png("Figure_05_Bing_Comparison_Cloud.png", width = 2200, height = 1600, res = 300)
bing_sentiment %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("#D7191C", "#2C7BB6"),
    max.words = 100,
    title.size = 1.5,
    scale = c(3.5, 0.5)
  )
title(main = "Comparison Cloud — Positive vs Negative (Bing)", 
      cex.main = 1.0, font.main = 2)
dev.off()
cat("Saved: Figure_05_Bing_Comparison_Cloud.png\n")

# ============================================================================
# STEP 6 — SENTIMENT ANALYSIS USING NRC LEXICON
# ============================================================================
nrc_lexicon <- get_sentiments("nrc")

nrc_sentiment <- tidy_abstracts %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(sentiment, sort = TRUE)

cat("\n--- NRC Emotion Counts ---\n")
print(nrc_sentiment)

write_xlsx(nrc_sentiment, "Table_NRC_Emotion_Counts.xlsx")
cat("Saved: Table_NRC_Emotion_Counts.xlsx\n")

# --- Figure 6: NRC Emotion Bar Chart ---
emotion_colors <- c(
  "anger"        = "#E41A1C",
  "anticipation" = "#FF7F00",
  "disgust"      = "#984EA3",
  "fear"         = "#A65628",
  "joy"          = "#4DAF4A",
  "negative"     = "#D7191C",
  "positive"     = "#2C7BB6",
  "sadness"      = "#377EB8",
  "surprise"     = "#FFFF33",
  "trust"        = "#66C2A5"
)

fig6 <- nrc_sentiment %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(x = n, y = sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = emotion_colors) +
  labs(
    title = "NRC Emotion Lexicon — Sentiment Distribution",
    subtitle = "AI in Banking and Finance Industry",
    x = "Word Count",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_06_NRC_Emotion_BarChart.png", fig6, width = 10, height = 7, dpi = 300)
cat("Saved: Figure_06_NRC_Emotion_BarChart.png\n")

# --- NRC: Top words per emotion ---
nrc_top_words <- tidy_abstracts %>%
  inner_join(nrc_lexicon, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup()

write_xlsx(nrc_top_words, "Table_NRC_Top_Words_Per_Emotion.xlsx")
cat("Saved: Table_NRC_Top_Words_Per_Emotion.xlsx\n")

# --- Figure 7: NRC Top 10 Words per Emotion (Faceted) ---
fig7 <- nrc_top_words %>%
  mutate(word = reorder_within(word, n, sentiment)) %>%
  ggplot(aes(x = n, y = word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ sentiment, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = emotion_colors) +
  labs(
    title = "NRC Lexicon — Top 10 Words per Emotion Category",
    subtitle = "AI in Banking and Finance Industry",
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave("Figure_07_NRC_Top10_Per_Emotion.png", fig7, width = 14, height = 16, dpi = 300)
cat("Saved: Figure_07_NRC_Top10_Per_Emotion.png\n")

# ============================================================================
# STEP 7 — SENTIMENT ANALYSIS USING AFINN LEXICON
# ============================================================================
afinn_lexicon <- get_sentiments("afinn")

afinn_sentiment <- tidy_abstracts %>%
  inner_join(afinn_lexicon, by = "word")

# --- AFINN: Paper-level sentiment score ---
afinn_by_paper <- afinn_sentiment %>%
  group_by(PaperID) %>%
  summarise(
    sentiment_score = sum(value),
    word_count      = n(),
    avg_sentiment   = mean(value),
    .groups = "drop"
  ) %>%
  arrange(desc(sentiment_score))

cat("\n--- AFINN Sentiment: Paper-Level Summary (Top 10) ---\n")
print(head(afinn_by_paper, 10))

write_xlsx(afinn_by_paper, "Table_AFINN_Paper_Level_Sentiment.xlsx")
cat("Saved: Table_AFINN_Paper_Level_Sentiment.xlsx\n")

# Classify papers
afinn_by_paper <- afinn_by_paper %>%
  mutate(sentiment_class = case_when(
    sentiment_score > 0  ~ "Positive",
    sentiment_score < 0  ~ "Negative",
    TRUE                 ~ "Neutral"
  ))

# Summary of paper-level classification
afinn_class_summary <- afinn_by_paper %>%
  count(sentiment_class, name = "paper_count") %>%
  mutate(percentage = round(paper_count / sum(paper_count) * 100, 1))

cat("\n--- AFINN Paper-Level Classification ---\n")
print(afinn_class_summary)

write_xlsx(afinn_class_summary, "Table_AFINN_Classification_Summary.xlsx")
cat("Saved: Table_AFINN_Classification_Summary.xlsx\n")

# --- Figure 8: AFINN Sentiment Score Distribution (Histogram) ---
fig8 <- ggplot(afinn_by_paper, aes(x = sentiment_score, fill = sentiment_class)) +
  geom_histogram(binwidth = 2, color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("Negative" = "#D7191C", "Neutral" = "#FDAE61", "Positive" = "#2C7BB6")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  labs(
    title = "Distribution of AFINN Sentiment Scores per Paper",
    subtitle = "AI in Banking and Finance Industry",
    x = "Sentiment Score (AFINN)",
    y = "Number of Papers",
    fill = "Classification"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_08_AFINN_Score_Distribution.png", fig8, width = 10, height = 7, dpi = 300)
cat("Saved: Figure_08_AFINN_Score_Distribution.png\n")

# --- Figure 9: AFINN Paper Classification — Bar Chart ---
fig9 <- afinn_class_summary %>%
  mutate(
    sentiment_class = reorder(sentiment_class, paper_count),
    label = paste0(paper_count, " (", percentage, "%)")
  ) %>%
  ggplot(aes(x = paper_count, y = sentiment_class, fill = sentiment_class)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = label), hjust = -0.1, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Negative" = "#D7191C", "Neutral" = "#FDAE61", "Positive" = "#2C7BB6")) +
  labs(
    title = "Paper-Level Sentiment Classification (AFINN)",
    subtitle = "AI in Banking and Finance Industry",
    x = "Number of Papers",
    y = NULL
  ) +
  xlim(0, max(afinn_class_summary$paper_count) * 1.15) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_09_AFINN_Classification_Bar.png", fig9, width = 10, height = 5, dpi = 300)
cat("Saved: Figure_09_AFINN_Classification_Bar.png\n")

# --- Figure 10: AFINN Top Contributing Words ---
afinn_word_contrib <- afinn_sentiment %>%
  group_by(word) %>%
  summarise(
    contribution = sum(value),
    occurrences  = n(),
    avg_value    = mean(value),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(contribution)))

write_xlsx(afinn_word_contrib, "Table_AFINN_Word_Contributions.xlsx")
cat("Saved: Table_AFINN_Word_Contributions.xlsx\n")

fig10 <- afinn_word_contrib %>%
  slice_max(abs(contribution), n = 20) %>%
  mutate(
    word = reorder(word, contribution),
    direction = ifelse(contribution >= 0, "Positive", "Negative")
  ) %>%
  ggplot(aes(x = contribution, y = word, fill = direction)) +
  geom_col(show.legend = TRUE) +
  scale_fill_manual(values = c("Negative" = "#D7191C", "Positive" = "#2C7BB6")) +
  labs(
    title = "AFINN: Top 20 Words by Total Sentiment Contribution",
    subtitle = "AI in Banking and Finance Industry",
    x = "Total Sentiment Contribution",
    y = NULL,
    fill = "Direction"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_10_AFINN_Word_Contribution.png", fig10, width = 10, height = 8, dpi = 300)
cat("Saved: Figure_10_AFINN_Word_Contribution.png\n")

# ============================================================================
# STEP 8 — SENTIMENT COMPARISON ACROSS LEXICONS (Syuzhet)
# ============================================================================
# Compute sentiment using 4 methods via syuzhet
cat("\nComputing sentiment via Syuzhet (this may take a few minutes)...\n")

abstracts_text <- dataset_clean$Abstract

syuzhet_scores <- get_sentiment(abstracts_text, method = "syuzhet")
bing_scores    <- get_sentiment(abstracts_text, method = "bing")
afinn_scores   <- get_sentiment(abstracts_text, method = "afinn")
nrc_scores     <- get_sentiment(abstracts_text, method = "nrc")

# Combine into data frame
multi_method <- data.frame(
  PaperID  = dataset_clean$PaperID,
  Syuzhet  = syuzhet_scores,
  Bing     = bing_scores,
  AFINN    = afinn_scores,
  NRC      = nrc_scores
)

write_xlsx(multi_method, "Table_MultiMethod_Sentiment_Scores.xlsx")
cat("Saved: Table_MultiMethod_Sentiment_Scores.xlsx\n")

# Descriptive statistics
multi_stats <- multi_method %>%
  select(-PaperID) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "Score") %>%
  group_by(Method) %>%
  summarise(
    Mean   = round(mean(Score), 3),
    Median = round(median(Score), 3),
    SD     = round(sd(Score), 3),
    Min    = min(Score),
    Max    = max(Score),
    .groups = "drop"
  )

cat("\n--- Multi-Method Descriptive Statistics ---\n")
print(multi_stats)

write_xlsx(multi_stats, "Table_MultiMethod_Descriptive_Stats.xlsx")
cat("Saved: Table_MultiMethod_Descriptive_Stats.xlsx\n")

# --- Figure 11: Boxplot Comparison of Sentiment Methods ---
fig11 <- multi_method %>%
  select(-PaperID) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "Score") %>%
  ggplot(aes(x = Method, y = Score, fill = Method)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Sentiment Score Comparison Across Lexicons",
    subtitle = "AI in Banking and Finance Industry",
    x = "Sentiment Method",
    y = "Sentiment Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

ggsave("Figure_11_MultiMethod_Boxplot.png", fig11, width = 10, height = 7, dpi = 300)
cat("Saved: Figure_11_MultiMethod_Boxplot.png\n")

# --- Figure 12: Density Plot Comparison ---
fig12 <- multi_method %>%
  select(-PaperID) %>%
  pivot_longer(everything(), names_to = "Method", values_to = "Score") %>%
  ggplot(aes(x = Score, fill = Method, color = Method)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Density Distribution of Sentiment Scores",
    subtitle = "AI in Banking and Finance Industry",
    x = "Sentiment Score",
    y = "Density",
    fill = "Method",
    color = "Method"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_12_MultiMethod_Density.png", fig12, width = 10, height = 7, dpi = 300)
cat("Saved: Figure_12_MultiMethod_Density.png\n")

# ============================================================================
# STEP 9 — NRC EMOTION BREAKDOWN PER PAPER (using syuzhet)
# ============================================================================
cat("\nComputing NRC emotion breakdown per paper...\n")

nrc_emotion_matrix <- get_nrc_sentiment(abstracts_text)

nrc_paper_emotions <- cbind(
  PaperID = dataset_clean$PaperID,
  nrc_emotion_matrix
)

write_xlsx(as.data.frame(nrc_paper_emotions), "Table_NRC_Paper_Emotions.xlsx")
cat("Saved: Table_NRC_Paper_Emotions.xlsx\n")

# Aggregate emotion totals
nrc_totals <- colSums(nrc_emotion_matrix)
nrc_totals_df <- data.frame(
  Emotion = names(nrc_totals),
  Total   = as.numeric(nrc_totals)
) %>%
  arrange(desc(Total))

cat("\n--- NRC Emotion Totals (Syuzhet) ---\n")
print(nrc_totals_df)

write_xlsx(nrc_totals_df, "Table_NRC_Emotion_Totals_Syuzhet.xlsx")
cat("Saved: Table_NRC_Emotion_Totals_Syuzhet.xlsx\n")

# --- Figure 13: NRC Emotion Totals (Syuzhet package) ---
fig13 <- nrc_totals_df %>%
  mutate(Emotion = reorder(Emotion, Total)) %>%
  ggplot(aes(x = Total, y = Emotion, fill = Emotion)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "anger" = "#E41A1C", "anticipation" = "#FF7F00", "disgust" = "#984EA3",
    "fear" = "#A65628", "joy" = "#4DAF4A", "negative" = "#D7191C",
    "positive" = "#2C7BB6", "sadness" = "#377EB8", "surprise" = "#FFFF33",
    "trust" = "#66C2A5"
  )) +
  labs(
    title = "NRC Emotion Totals Across All Abstracts (Syuzhet)",
    subtitle = "AI in Banking and Finance Industry",
    x = "Total Score",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figure_13_NRC_Emotion_Totals_Syuzhet.png", fig13, width = 10, height = 7, dpi = 300)
cat("Saved: Figure_13_NRC_Emotion_Totals_Syuzhet.png\n")

# ============================================================================
# STEP 10 — SUMMARY REPORT
# ============================================================================
cat("\n")
cat("================================================================\n")
cat("     SENTIMENT ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("================================================================\n")
cat("Research: AI in Banking and Finance Industry\n")
cat("Dataset : dataset_clean.xlsx\n")
cat("Papers  :", nrow(dataset_clean), "\n")
cat("================================================================\n")
cat("\n--- OUTPUT FILES GENERATED ---\n")
cat("\n[TABLES]\n")
cat("  1. Table_Word_Frequency.xlsx\n")
cat("  2. Table_Bing_Sentiment_Words.xlsx\n")
cat("  3. Table_Bing_Sentiment_Summary.xlsx\n")
cat("  4. Table_NRC_Emotion_Counts.xlsx\n")
cat("  5. Table_NRC_Top_Words_Per_Emotion.xlsx\n")
cat("  6. Table_AFINN_Paper_Level_Sentiment.xlsx\n")
cat("  7. Table_AFINN_Classification_Summary.xlsx\n")
cat("  8. Table_AFINN_Word_Contributions.xlsx\n")
cat("  9. Table_MultiMethod_Sentiment_Scores.xlsx\n")
cat(" 10. Table_MultiMethod_Descriptive_Stats.xlsx\n")
cat(" 11. Table_NRC_Paper_Emotions.xlsx\n")
cat(" 12. Table_NRC_Emotion_Totals_Syuzhet.xlsx\n")
cat("\n[FIGURES]\n")
cat("  1.  Figure_01_Top20_Word_Frequency.png\n")
cat("  2.  Figure_02_WordCloud.png\n")
cat("  3.  Figure_03_Bing_Top15_PosNeg.png\n")
cat("  4.  Figure_04_Bing_Sentiment_Pie.png\n")
cat("  5.  Figure_05_Bing_Comparison_Cloud.png\n")
cat("  6.  Figure_06_NRC_Emotion_BarChart.png\n")
cat("  7.  Figure_07_NRC_Top10_Per_Emotion.png\n")
cat("  8.  Figure_08_AFINN_Score_Distribution.png\n")
cat("  9.  Figure_09_AFINN_Classification_Bar.png\n")
cat(" 10.  Figure_10_AFINN_Word_Contribution.png\n")
cat(" 11.  Figure_11_MultiMethod_Boxplot.png\n")
cat(" 12.  Figure_12_MultiMethod_Density.png\n")
cat(" 13.  Figure_13_NRC_Emotion_Totals_Syuzhet.png\n")
cat("\n================================================================\n")
cat("   All outputs saved to working directory.\n")
cat("================================================================\n")

###############################################################################
#                           END OF SCRIPT
###############################################################################

