# ============================================================
#  LING 460 — Words Beyond the Podium
#  Full Analysis Script
#  Sections:
#    1. Setup & Libraries
#    2. Data Collection (Tyler's + Dante's original scraping)
#    3. Additional Article Scraping (Alex Ferreira + missing articles)
#    4. Corpus Assembly
#    5. Word Dictionaries (Appearance, Emotion, Personal Life, Performance)
#    6. Tokenization & Cleaning
#    7. Proportional Frequency Counts (per 1,000 words)
#    8. Mann-Whitney U Tests + Bonferroni Correction + Effect Sizes
#    9. Visualizations
# ============================================================


# ── 1. SETUP & LIBRARIES ────────────────────────────────────

library(rvest)
library(stringr)
library(tibble)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(coin)        # for wilcox_test with exact p-values
library(rstatix)     # for wilcox_effsize (rank-biserial r)
library(textdata)    # for NRC lexicon via get_sentiments()
library(patchwork)   # for combining plots


# ── 2. ORIGINAL DATA COLLECTION (Tyler's & Dante's work) ────
#    (Reproduced exactly as submitted — do not modify)

# ---- Jack Hughes ----
jack1 <- read_html("https://www.espn.com/nhl/story/_/id/48280406/nhl-2025-26-olympics-team-usa-jack-hughes-gold-medal-goal-puck-controversy") %>%
  html_elements("div.article-body p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

jack2 <- read_html("https://www.cbssports.com/olympics/news/jack-hughes-2026-olympics-golden-goal-hockey-hall-of-fame/") %>%
  html_elements("div.Article-bodyContent p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

jack3 <- read_html("https://www.nbcnews.com/sports/olympics/jack-hughes-devils-panthers-rcna261563") %>%
  html_elements("div.article-body__content p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

jack4 <- read_html("https://www.foxnews.com/sports/team-usas-jack-hughes-shares-patriotic-message-after-olympic-thriller") %>%
  html_elements("div.article-body p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Hilary Knight ----
hillary1 <- read_html("https://www.cbssports.com/olympics/news/usa-hockey-hilary-knight-president-donald-trump-joke-2026-olympics/") %>%
  html_elements("div#Article-body.article.article-main-body.Article-body p") %>%
  html_text2() %>% paste(collapse = " ") %>% str_squish()

hillary2 <- read_html("https://www.nbcchicago.com/olympics/2026-milan-cortina/hilary-knight-addresses-trumps-comment-about-womens-hockey-team-mens-response/3900508/") %>%
  html_elements("div.article-content.rich-text p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

hillary3 <- read_html("https://sports.yahoo.com/olympics/article/usa-womens-ice-hockey-captain-hilary-knight-reveals-she-played-2026-winter-olympics-with-torn-mcl-in-knee-212254458.html") %>%
  html_elements("div.body-wrapper p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Henrik Kristoffersen ----
henrik1 <- read_html("https://www.nbcolympics.com/news/henrik-kristoffersen-wins-difficult-schladming-slalom-ahead-2026-winter-olympics") %>%
  html_elements("div p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

henrik2 <- read_html("https://www.espn.in/olympics/skiing/story/_/id/47758338/henrik-kristoffersen-ends-drought-wins-last-world-cup-slalom-olympics") %>%
  html_elements("div.article-body p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

henrik3 <- read_html("https://sports.yahoo.com/articles/kristoffersen-wants-continue-skiing-until-124335987.html") %>%
  html_elements("div.body-wrapper p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Mikaela Shiffrin ----
mikaela1 <- read_html("https://www.nbcolympics.com/news/mikaela-shiffrin-won-more-ways-one-2026-milan-cortina-winter-olympics") %>%
  html_elements("div p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

mikaela2 <- read_html("https://www.cnn.com/2026/02/18/sport/mikaela-shiffrin-olympics-golden-peace") %>%
  html_elements("div.article__content p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Jake Canter ----
jake1 <- read_html("https://www.nbcchicago.com/olympics/2026-milan-cortina/for-snowboarder-jake-canter-an-olympic-medal-is-the-prize-after-near-death-journey/3896226/") %>%
  html_elements("div.article-content.rich-text p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

jake2 <- read_html("https://www.themanual.com/outdoors/jake-canter-interview/") %>%
  html_elements("div#h-maincontent.b-page p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

jake3 <- read_html("https://www.summitdaily.com/news/olympic-snowboarding-slopestyle-bronze-colorado-canter-gerard-silverthorne-2026/") %>%
  html_elements("div.container p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Ilia Malinin ----
paras1_malinin <- read_html("https://www.espn.com/olympics/figureskating/story/_/id/47922785/quad-god-ilia-malinin-falls-twice-olympic-men-figure-skating-disaster-allowing-mikhail-shaidorov-claim-gold") %>%
  html_elements("article p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras2_malinin <- read_html("https://www.espn.com/olympics/figureskating/story/_/id/47890597/us-star-ilia-malinin-leads-men-figure-skating-olympics") %>%
  html_elements("article p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras3_malinin <- read_html("https://www.nbcnews.com/sports/olympics/ilia-malinin-admits-not-ready-handle-olympic-pressure-rcna259316") %>%
  html_elements("article p") %>% html_text2()
paras3_malinin <- paras3_malinin[-13]
paras3_malinin <- paste(paras3_malinin, collapse = " ") %>% str_squish()

# ---- Alysa Liu ----
paras1_liu <- read_html("https://www.si.com/winter-olympics/alysa-liu-radiates-joy-as-figure-skating-champion-gold") %>%
  html_elements("article p") %>% html_text2()
paras1_liu <- paras1_liu[-26]
paras1_liu <- paste(paras1_liu, collapse = " ") %>% str_squish()

paras2_liu <- read_html("https://www.cbssports.com/olympics/news/2026-winter-olympics-american-figure-skater-alysa-liu/") %>%
  html_elements("article p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras3_liu <- read_html("https://www.elitedaily.com/wellness/alysa-liu-2026-olympics-figure-skating-interview") %>%
  html_elements("article p") %>% html_text2()
paras3_liu <- paras3_liu[-c(21, 22)]
paras3_liu <- paste(paras3_liu, collapse = " ") %>% str_squish()

# ---- Chloe Kim ----
paras1_kim <- read_html("https://www.cbsnews.com/news/chloe-kim-injury-snowboarding-2026-winter-olympics-italy/") %>%
  html_elements("article p") %>% html_text2()
paras1_kim <- paras1_kim[-c(1, 19, 20)]
paras1_kim <- paste(paras1_kim, collapse = " ") %>% str_squish()

paras2_kim <- read_html("https://www.nbcolympics.com/news/chloe-kim-unfazed-injury-crushes-halfpipe-qualifier-roar-final-mastro-3rd") %>%
  html_elements("p") %>% html_text2()
paras2_kim <- paras2_kim[-c(17:21)]
paras2_kim <- paste(paras2_kim, collapse = " ") %>% str_squish()

# ---- Jordan Stolz ----
paras1_stolz <- read_html("https://www.nbcolympics.com/news/amid-dramatic-olympics-jordan-stolz-quietly-becomes-us-star-and-hes-just-getting-started") %>%
  html_elements("p") %>% html_text2()
paras1_stolz <- paras1_stolz[-c(14:18)]
paras1_stolz <- paste(paras1_stolz, collapse = " ") %>% str_squish()

paras2_stolz <- read_html("https://www.nytimes.com/athletic/7060476/2026/02/21/jordan-stolz-milan-olympics-speedskating-fame-three-medals/") %>%
  html_elements("p") %>% html_text2()
paras2_stolz <- paras2_stolz[-c(31:34)]
paras2_stolz <- paste(paras2_stolz, collapse = " ") %>% str_squish()

paras3_stolz <- read_html("https://www.espn.com/olympics/speedskating/story/_/id/47928388/american-jordan-stolz-wins-2nd-speedskating-gold-olympics") %>%
  html_elements("article p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Jutta Leerdam ----
paras1_leerdam <- read_html("https://edition.cnn.com/2026/02/10/sport/jutta-leerdam-dutch-speed-skater-olympics",
                             encoding = "UTF-8") %>%
  html_elements("p") %>% html_text2()
paras1_leerdam <- paras1_leerdam[-c(22:24)]
paras1_leerdam <- paste(paras1_leerdam, collapse = " ") %>% str_squish()

paras2_leerdam <- read_html("https://sports.yahoo.com/articles/watch-jutta-leerdam-react-fianc-140000105.html") %>%
  html_elements("p") %>% html_text2()
paras2_leerdam <- paras2_leerdam[-c(1, 12, 13, 14)]
paras2_leerdam <- paste(paras2_leerdam, collapse = " ") %>% str_squish()

paras3_leerdam <- read_html("https://www.nytimes.com/athletic/7033765/2026/02/09/jutta-leerdam-olympic-speed-skating-record-1000-meter/") %>%
  html_elements("p") %>% html_text2()
paras3_leerdam <- paras3_leerdam[-c(4, 13, 22)]
paras3_leerdam <- paste(paras3_leerdam, collapse = " ") %>% str_squish()


# ── 3. ADDITIONAL ARTICLE SCRAPING ──────────────────────────

# ---- Alex Ferreira (new athlete — male freestyle skiing) ----

ferreira1 <- read_html("https://time.com/7362805/alex-ferreira-olympics-2026-hotdog-hans-skier-interview/") %>%
  html_elements("div.article-body p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

ferreira2 <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/freestyle-skiing-halfpipe-olympic-champion-alex-ferreira-exclusive-interview-no-shortcuts") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

ferreira3 <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/winter-olympics-2026-alex-ferreira-my-lionel-messi-moment") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

ferreira4 <- read_html("https://sports.yahoo.com/olympics/article/winter-olympics-2026-alex-ferreiras-long-wait-ends-in-olympic-gold--i-am-greatness-and-this-is-my-moment-200120068.html") %>%
  html_elements("div.body-wrapper p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Missing Ilia Malinin article (olympics.com) ----
paras4_malinin <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/ilia-malinin-unplugged-the-american-reflects-on-his-winter-olympics-2026-medals-don-t-really-define-who-you-are") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Missing Alysa Liu articles (olympics.com) ----
paras4_liu <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/winter-olympics-2026-alysa-liu-exclusive-i-was-peak-happiness-out-there-on-the-ice") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras5_liu <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/winter-olympics-2026-alysa-liu-comeback-journey-continued") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Missing Mikaela Shiffrin articles (olympics.com) ----
mikaela3 <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/emotional-mikaela-shiffrin-relives-spiritual-moment-dad-gold") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

mikaela4 <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/alpine-skiing-usa-mikaela-shiffrin-pressure-excitement-fourth-games") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Missing Jordan Stolz articles ----
paras4_stolz <- read_html("https://time.com/7344482/jordan-stolz-speed-skating-2026-olympics-interview/") %>%
  html_elements("div.article-body p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras5_stolz <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/speed-skating-men-usa-jordan-stolz-pursuit-perfection") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras6_stolz <- read_html("https://www.npr.org/2026/02/19/nx-s1-5718087/winter-olympics-jordan-stolz-speedskating-medal") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Missing Jutta Leerdam articles (olympics.com) ----
paras4_leerdam <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/winter-olympics-2026-jutta-leerdam-breaks-olympic-record-to-win-womens-1000m-speed-skating-gold") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras5_leerdam <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/speed-skating-jutta-leerdam-life-fast-lane") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# ---- Missing Chloe Kim article (olympics.com) ----
paras3_kim <- read_html("https://www.olympics.com/en/milano-cortina-2026/news/winter-olympics-snowboard-women-usa-chloe-kim-silver-proud") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

paras4_kim <- read_html("https://athlonsports.com/olympics/u-s-snowboarder-chloe-kim-weird-admission-winter-olympics") %>%
  html_elements("p") %>% html_text2() %>%
  paste(collapse = " ") %>% str_squish()

# NOTE: Alex Ferreira replaces Henrik Kristoffersen as the male skiing counterpart
# to Mikaela Shiffrin (freestyle skiing vs. alpine skiing — closest available pairing).
# Henrik Kristoffersen is retained in the corpus for completeness but is NOT
# included in the primary male vs. female comparison to keep sport pairings balanced.


# ── 4. CORPUS ASSEMBLY ──────────────────────────────────────

jack_corpus <- tibble(
  athlete = "Jack Hughes", sex = "Male", sport = "Ice Hockey",
  article = paste0("article", 1:4),
  text = c(jack1, jack2, jack3, jack4)
)

hilary_corpus <- tibble(
  athlete = "Hilary Knight", sex = "Female", sport = "Ice Hockey",
  article = paste0("article", 1:3),
  text = c(hillary1, hillary2, hillary3)
)

malinin_corpus <- tibble(
  athlete = "Ilia Malinin", sex = "Male", sport = "Figure Skating",
  article = paste0("article", 1:4),
  text = c(paras1_malinin, paras2_malinin, paras3_malinin, paras4_malinin)
)

liu_corpus <- tibble(
  athlete = "Alysa Liu", sex = "Female", sport = "Figure Skating",
  article = paste0("article", 1:5),
  text = c(paras1_liu, paras2_liu, paras3_liu, paras4_liu, paras5_liu)
)

ferreira_corpus <- tibble(
  athlete = "Alex Ferreira", sex = "Male", sport = "Freestyle Skiing",
  article = paste0("article", 1:4),
  text = c(ferreira1, ferreira2, ferreira3, ferreira4)
)

mikaela_corpus <- tibble(
  athlete = "Mikaela Shiffrin", sex = "Female", sport = "Alpine Skiing",
  article = paste0("article", 1:4),
  text = c(mikaela1, mikaela2, mikaela3, mikaela4)
)

jake_corpus <- tibble(
  athlete = "Jake Canter", sex = "Male", sport = "Snowboarding",
  article = paste0("article", 1:3),
  text = c(jake1, jake2, jake3)
)

kim_corpus <- tibble(
  athlete = "Chloe Kim", sex = "Female", sport = "Snowboarding",
  article = paste0("article", 1:4),
  text = c(paras1_kim, paras2_kim, paras3_kim, paras4_kim)
)

stolz_corpus <- tibble(
  athlete = "Jordan Stolz", sex = "Male", sport = "Speed Skating",
  article = paste0("article", 1:6),
  text = c(paras1_stolz, paras2_stolz, paras3_stolz,
           paras4_stolz, paras5_stolz, paras6_stolz)
)

leerdam_corpus <- tibble(
  athlete = "Jutta Leerdam", sex = "Female", sport = "Speed Skating",
  article = paste0("article", 1:5),
  text = c(paras1_leerdam, paras2_leerdam, paras3_leerdam,
           paras4_leerdam, paras5_leerdam)
)

# Henrik retained for reference but excluded from primary analysis
henrik_corpus <- tibble(
  athlete = "Henrik Kristoffersen", sex = "Male", sport = "Alpine Skiing",
  article = paste0("article", 1:3),
  text = c(henrik1, henrik2, henrik3)
)

# Primary 10-athlete corpus (5 male, 5 female, matched by sport)
# Sport pairings:
#   Ice Hockey:        Jack Hughes      / Hilary Knight
#   Figure Skating:    Ilia Malinin     / Alysa Liu
#   Freestyle Skiing:  Alex Ferreira    / Mikaela Shiffrin  (closest skiing pair available)
#   Snowboarding:      Jake Canter      / Chloe Kim
#   Speed Skating:     Jordan Stolz     / Jutta Leerdam

all_articles <- bind_rows(
  jack_corpus, hilary_corpus,
  malinin_corpus, liu_corpus,
  ferreira_corpus, mikaela_corpus,
  jake_corpus, kim_corpus,
  stolz_corpus, leerdam_corpus
)

# Collapse all articles per athlete into one text (unit of analysis = athlete)
all_collapsed <- all_articles %>%
  group_by(athlete, sex, sport) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop")


# ── 5. WORD DICTIONARIES ────────────────────────────────────
# All dictionaries locked here before any analysis is run.

# ---- 5a. Appearance ----
# Manually curated from prior literature on gendered sports coverage.
# Excludes gendered terms (woman, female, male) to prevent trivial results.
appearance_words <- c(
  "beautiful", "beauty", "elegant", "elegance", "graceful", "grace",
  "pretty", "stunning", "stylish", "style", "attractive", "gorgeous",
  "lovely", "radiant", "glamorous", "glamour", "physique", "slender",
  "slim", "fit", "toned", "shapely", "appearance", "looks", "hair",
  "smile", "outfit", "costume", "dress", "wardrobe", "fashionable",
  "petite", "dainty", "charming", "poise", "poised", "aesthetic"
)

# ---- 5b. Emotion ----
# Drawn from the NRC Emotion Lexicon (Mohammad & Turney, 2013).
# Restricted to three categories most relevant to the hypotheses:
# fear, sadness, and joy. Gendered terms excluded.
nrc <- get_sentiments("nrc")

emotion_words <- nrc %>%
  filter(sentiment %in% c("fear", "sadness", "joy")) %>%
  pull(word) %>%
  unique()

# ---- 5c. Personal Life ----
# Drawn from LIWC family and social category terms, supplemented manually.
# Excludes athlete-specific names and gendered terms.
personal_life_words <- c(
  "family", "mother", "mom", "father", "dad", "parent", "parents",
  "sister", "brother", "sibling", "grandma", "grandpa", "grandmother",
  "grandfather", "son", "daughter", "child", "children", "baby",
  "husband", "wife", "spouse", "partner", "boyfriend", "girlfriend",
  "fiance", "fiancé", "fiancée", "relationship", "wedding", "married",
  "marriage", "home", "house", "hometown", "upbringing", "childhood",
  "grew", "grow", "personal", "private", "life", "love", "loved",
  "dating", "romance", "romantic", "heart", "together", "support",
  "supported", "supportive", "friend", "friends", "friendship"
)

# ---- 5d. Athletic Performance ----
# Manually curated list of sport-specific performance vocabulary.
# Broad enough to cover all five sports in the corpus.
performance_words <- c(
  "score", "technique", "training", "strategy", "podium", "medal",
  "gold", "silver", "bronze", "record", "speed", "power", "strength",
  "endurance", "skill", "athlete", "athletic", "competition", "compete",
  "competed", "win", "won", "victory", "champion", "championship",
  "performance", "perform", "performed", "result", "ranking", "ranked",
  "finish", "finished", "qualifying", "qualified", "race", "raced",
  "run", "jump", "skate", "skating", "ski", "skiing", "snowboard",
  "shot", "goal", "assist", "save", "defense", "offense", "play",
  "game", "match", "tournament", "heat", "final", "semifinal",
  "quarterfinal", "lap", "time", "split", "pace", "stamina",
  "conditioning", "drill", "practice", "workout", "fitness",
  "coach", "coaching", "tactics", "execute", "execution", "precision",
  "consistent", "consistency", "dominant", "dominate", "dominance",
  "breakthrough", "comeback", "resilience", "resilient", "perseverance",
  "determination", "focus", "mental", "physical", "preparation",
  "podium", "points", "leaderboard", "standing", "season", "career"
)

# Quick dictionary summary (run this to verify counts before analysis)
cat("Dictionary sizes:\n")
cat("  Appearance:   ", length(appearance_words), "terms\n")
cat("  Emotion:      ", length(emotion_words), "terms (NRC: fear + sadness + joy)\n")
cat("  Personal Life:", length(personal_life_words), "terms\n")
cat("  Performance:  ", length(performance_words), "terms\n")


# ── 6. TOKENIZATION & CLEANING ──────────────────────────────

# Tokenize each athlete's combined text into individual words
tokens <- all_collapsed %>%
  unnest_tokens(word, text) %>%          # lowercase + punctuation stripped
  filter(!word %in% stop_words$word) %>% # remove common stop words
  filter(str_detect(word, "^[a-z]+$"))   # keep only alphabetic tokens

# Word counts per athlete (needed for normalization)
word_counts <- tokens %>%
  count(athlete, name = "total_words")

# Also compute total word count BEFORE stop word removal for normalization
# (we normalize over all words, not just content words, to match standard practice)
total_word_counts <- all_collapsed %>%
  unnest_tokens(word, text) %>%
  count(athlete, name = "total_words_raw")


# ── 7. PROPORTIONAL FREQUENCY COUNTS (per 1,000 words) ──────

count_category <- function(token_df, raw_word_counts, dict, category_name) {
  token_df %>%
    filter(word %in% dict) %>%
    count(athlete, name = "n_hits") %>%
    left_join(raw_word_counts, by = "athlete") %>%
    mutate(
      category = category_name,
      freq_per_1000 = (n_hits / total_words_raw) * 1000
    ) %>%
    select(athlete, category, n_hits, total_words_raw, freq_per_1000)
}

appearance_freq   <- count_category(tokens, total_word_counts, appearance_words,     "Appearance")
emotion_freq      <- count_category(tokens, total_word_counts, emotion_words,         "Emotion")
personal_freq     <- count_category(tokens, total_word_counts, personal_life_words,  "Personal Life")
performance_freq  <- count_category(tokens, total_word_counts, performance_words,    "Performance")

# Combine all categories; fill 0 for athletes with no hits in a category
freq_all <- bind_rows(appearance_freq, emotion_freq, personal_freq, performance_freq) %>%
  complete(athlete, category, fill = list(n_hits = 0, freq_per_1000 = 0)) %>%
  left_join(all_collapsed %>% select(athlete, sex, sport), by = "athlete")

# Fill in missing total_words_raw after complete()
freq_all <- freq_all %>%
  left_join(total_word_counts, by = "athlete") %>%
  mutate(total_words_raw = coalesce(total_words_raw.x, total_words_raw.y)) %>%
  select(-total_words_raw.x, -total_words_raw.y)

# Wide format for easy viewing
freq_wide <- freq_all %>%
  select(athlete, sex, sport, category, freq_per_1000) %>%
  pivot_wider(names_from = category, values_from = freq_per_1000)

cat("\nPer-athlete proportional frequencies (per 1,000 words):\n")
print(freq_wide)


# ── 8. STATISTICAL TESTS ────────────────────────────────────
# Mann-Whitney U (Wilcoxon rank-sum) tests, one per category.
# Bonferroni correction: α = .05 / 4 = .0125
# Effect size: rank-biserial correlation r (via rstatix::wilcox_effsize)

categories <- c("Appearance", "Emotion", "Personal Life", "Performance")
alpha_bonferroni <- 0.05 / length(categories)  # 0.0125

results_list <- lapply(categories, function(cat) {
  dat <- freq_all %>%
    filter(category == cat) %>%
    select(athlete, sex, freq_per_1000)

  # Wilcoxon rank-sum test (exact p-value; appropriate for n=10)
  wt <- wilcox.test(freq_per_1000 ~ sex, data = dat, exact = TRUE)

  # Effect size: rank-biserial r
  eff <- wilcox_effsize(dat, freq_per_1000 ~ sex, ci = FALSE)

  tibble(
    Category          = cat,
    W_statistic       = wt$statistic,
    p_value           = wt$p.value,
    p_adjusted        = p.adjust(wt$p.value, method = "bonferroni", n = length(categories)),
    significant       = p.adjust(wt$p.value, method = "bonferroni", n = length(categories)) < alpha_bonferroni,
    effect_size_r     = eff$effsize,
    effect_magnitude  = eff$magnitude,
    mean_female       = mean(dat$freq_per_1000[dat$sex == "Female"]),
    mean_male         = mean(dat$freq_per_1000[dat$sex == "Male"])
  )
})

results_table <- bind_rows(results_list)

cat("\n── Statistical Results (Mann-Whitney U, Bonferroni-corrected) ──\n")
cat(sprintf("Bonferroni-adjusted α = %.4f\n\n", alpha_bonferroni))
print(results_table, n = Inf)


# ── 9. VISUALIZATIONS ───────────────────────────────────────

theme_ling <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.title    = element_text(size = 11),
    legend.position = "bottom",
    strip.text    = element_text(face = "bold")
  )

sex_colors <- c("Female" = "#E07B8A", "Male" = "#5B8DB8")

# ---- 9a. Bar chart: mean freq per category by sex ----
bar_data <- freq_all %>%
  group_by(sex, category) %>%
  summarise(mean_freq = mean(freq_per_1000), se = sd(freq_per_1000) / sqrt(n()), .groups = "drop")

p_bar <- ggplot(bar_data, aes(x = category, y = mean_freq, fill = sex)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = mean_freq - se, ymax = mean_freq + se),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = sex_colors, name = "Sex") +
  labs(
    title    = "Mean Word Category Frequency by Athlete Sex",
    subtitle = "Occurrences per 1,000 words ± 1 SE; Bonferroni-corrected α = .0125",
    x        = "Word Category",
    y        = "Mean Freq. per 1,000 Words"
  ) +
  theme_ling

# ---- 9b. Dot plot: individual athlete values per category ----
p_dot <- ggplot(freq_all, aes(x = sex, y = freq_per_1000, color = sex)) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.85) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4,
               fatten = 2, color = "black", linewidth = 0.5) +
  facet_wrap(~ category, scales = "free_y", nrow = 1) +
  scale_color_manual(values = sex_colors, name = "Sex") +
  labs(
    title    = "Individual Athlete Frequencies by Category and Sex",
    subtitle = "Horizontal bar = group mean; dots = individual athletes (n = 5 per group)",
    x        = NULL,
    y        = "Freq. per 1,000 Words"
  ) +
  theme_ling +
  theme(legend.position = "none")

# ---- 9c. Heatmap: per-athlete, per-category frequencies ----
heatmap_data <- freq_all %>%
  mutate(athlete_label = paste0(athlete, "\n(", sex, ")"))

p_heat <- ggplot(heatmap_data,
                 aes(x = category, y = reorder(athlete_label, desc(sex)),
                     fill = freq_per_1000)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(freq_per_1000, 1)), size = 3.2, color = "white") +
  scale_fill_gradient(low = "#2C3E6B", high = "#E07B8A",
                      name = "Freq.\nper 1k words") +
  labs(
    title    = "Per-Athlete Word Category Heatmap",
    subtitle = "Values = occurrences per 1,000 words",
    x        = "Word Category",
    y        = NULL
  ) +
  theme_ling +
  theme(axis.text.y = element_text(size = 9))

# ---- Save all plots ----
ggsave("/mnt/user-data/outputs/plot_bar.png",    p_bar,  width = 8,  height = 5,  dpi = 150)
ggsave("/mnt/user-data/outputs/plot_dot.png",    p_dot,  width = 12, height = 5,  dpi = 150)
ggsave("/mnt/user-data/outputs/plot_heatmap.png",p_heat, width = 9,  height = 7,  dpi = 150)

cat("\nAll plots saved to /mnt/user-data/outputs/\n")
cat("Analysis complete.\n")
