---
  title: "TFM"
author: "Alba García Vega"
format: html
editor: visual
---
  
# Load necessary libraries
  
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(textdata)
library(gutenbergr)
library(readr)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)
library(sentimentr)

# Load and process the texts

# To the lighthouse
tothelighthouse_vw <- readLines("to-the-lighthouse.txt", encoding = "UTF-8")
tothelighthouse_vw <- paste(tothelighthouse_vw, collapse = " ")

# The Yellow Wallpaper
theyellowwallpaper <- readLines("the_yellow_wallpaper.txt", encoding = "UTF-8")
theyellowwallpaper <- paste(theyellowwallpaper, collapse = " ")

# The Bell Jar
thebelljar <- readLines("the_bell_jar.txt", encoding = "UTF-8")
thebelljar <- paste(thebelljar, collapse = " ")

# Sons and Lovers

sons_lovers <- gutenberg_download(217)
sons_lovers <- paste(sons_lovers$text, collapse = " ")

# The Sun also Rises

sun_rises <- readLines("sun_also_rises.txt", encoding = "UTF-8")
sun_rises <- paste(sun_rises, collapse = " ")

# The Sound of the Fury

sound_fury <- readLines("sound_and_fury.txt", encoding = "UTF-8")
sound_fury <- paste(sound_fury, collapse = " ")



# Function to preprocess text (clean and tokenize)
preprocess_text <- function(text) {
  clean_text <- text |> 
    str_to_lower() |> 
    str_replace_all("[^[:alnum:][:space:]]", " ") |>  
    str_replace_all("\\s+", " ")
  
  text_df <- data.frame(line = 1, text = clean_text)
  
  tokens <- text_df |> 
    unnest_tokens(word, text)
  
  return(tokens)
}


# Apply

tokens_lighthouse <- preprocess_text(tothelighthouse_vw)
tokens_yellowwallpaper <- preprocess_text(theyellowwallpaper)
tokens_belljar <- preprocess_text(thebelljar)
tokens_sound_fury <- preprocess_text(sound_fury)
tokens_sun_rises <- preprocess_text(sun_rises)
tokens_sons_lovers <- preprocess_text(sons_lovers)



# NRC


# The Bell Jar - Sylvia Plath

nrc <- get_sentiments("nrc")

sentiments_belljar <- tokens_belljar |> 
  inner_join(nrc, by = "word") |> 
  filter(!sentiment %in% c("positive", "negative"))

sentiments_belljar |> 
  count(sentiment, sort = TRUE) |> 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentimientos en The Bell Jar", x = "Sentimiento", y = "Frecuencia")



# To the lighthouse - Virginia Woolf

sentiments_lighthouse <- tokens_lighthouse |> 
  inner_join(nrc, by = "word") |> 
  filter(!sentiment %in% c("positive", "negative"))

sentiments_lighthouse |> 
  count(sentiment, sort = TRUE) |> 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentimientos en To the Lighthouse", x = "Sentimiento", y = "Frecuencia")


# The Yellow Wallpaper - Charlotte Perkins Gilman
sentiments_yellow_wallpaper <- tokens_yellowwallpaper |> 
  inner_join(nrc, by = "word") |> 
  filter(!sentiment %in% c("positive", "negative"))

sentiments_yellow_wallpaper |> 
  count(sentiment, sort = TRUE) |> 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentimientos en The Yellow Wallpaper", x = "Sentimiento", y = "Frecuencia")



# The Hugging Face

## General emotions

# Create function
analize_emotion <- function(text, model, token) {
  url <- paste0("https://api-inference.huggingface.co/models/", model)
  
  answer <- tryCatch({
    POST(
      url,
      add_headers(Authorization = paste("Bearer", token)),
      body = list(inputs = text),
      encode = "json",
      timeout(60)
    )
  }, error = function(e) {
    message("❌ Error en la solicitud: ", e$message)
    return(NULL)
  })
  
  if (!is.null(answer) && status_code(answer) == 200) {
    # Forzar texto para evitar problemas de parsers automáticos
    result_text <- content(answer, as = "text", encoding = "UTF-8")
    
    # Intentar convertir a JSON
    parsed <- tryCatch(fromJSON(result_text), error = function(e) {
      message("⚠️ No se pudo convertir la respuesta a JSON.")
      return(NULL)
    })
    
    return(parsed)
    
  } else {
    message("⚠️ Error en la respuesta: Código ", status_code(answer))
    return(NULL)
  }
}


# Filter out short or empty paragraphs, or those that will give us problem

# The Bell Jar
paragraphs_belljar <- unlist(strsplit(thebelljar, split = "\\.\\s+"))
paragraphs_belljar <- paragraphs_belljar[nchar(paragraphs_belljar) > 30]
paragraphs_belljar <- paragraphs_belljar[!is.na(paragraphs_belljar) & paragraphs_belljar != ""]
paragraphs_belljar <- paragraphs_belljar[nchar(paragraphs_belljar) < 600]

# The Yellow Wallpaper
yellowwallpaper <- tolower(read_file("the_yellow_wallpaper.txt"))
paragraphs_yellow <- unlist(strsplit(yellowwallpaper, split = "\\.\\s+"))
paragraphs_yellow <- paragraphs_yellow[nchar(paragraphs_yellow) > 30]
paragraphs_yellow <- paragraphs_yellow[!is.na(paragraphs_yellow) & paragraphs_yellow != ""]
paragraphs_yellow <- paragraphs_yellow[nchar(paragraphs_yellow) < 1000]


# To the Lighthouse
lighthouse <- tolower(read_file("to-the-lighthouse.txt"))
paragraphs_lighthouse <- unlist(strsplit(lighthouse, split = "\\.\\s+"))
paragraphs_lighthouse <- paragraphs_lighthouse[nchar(paragraphs_lighthouse) > 30]
paragraphs_lighthouse <- paragraphs_lighthouse[!is.na(paragraphs_lighthouse) & paragraphs_lighthouse != ""]
paragraphs_lighthouse <- paragraphs_lighthouse[nchar(paragraphs_lighthouse) < 600]

# Sons and Lovers
sonsandlovers <- tolower(sons_lovers)
paragraphs_sons<- unlist(strsplit(sonsandlovers, split = "\\.\\s+"))
paragraphs_sons <- paragraphs_sons[nchar(paragraphs_sons) > 30]
paragraphs_sons <- paragraphs_sons[!is.na(paragraphs_sons) & paragraphs_sons != ""]
paragraphs_sons <- paragraphs_sons[nchar(paragraphs_sons) < 600]


#The sun also rises
sunalsorises <- tolower(read_file("sun_also_rises.txt"))
paragraphs_sun <- unlist(strsplit(sunalsorises, split = "\\.\\s+"))
paragraphs_sun <- paragraphs_sun[nchar(paragraphs_sun) > 30]
paragraphs_sun <- paragraphs_sun[!is.na(paragraphs_sun) & paragraphs_sun != ""]
paragraphs_sun <- paragraphs_sun[nchar(paragraphs_sun) < 1000]


# The sound of the fury
soundandfury <- tolower(read_file("sound_and_fury.txt"))
paragraphs_sound <- unlist(strsplit(soundandfury, split = "\\.\\s+"))
paragraphs_sound <- paragraphs_sound[nchar(paragraphs_sound) > 30]
paragraphs_sound <- paragraphs_sound[!is.na(paragraphs_sound) & paragraphs_sound != ""]
paragraphs_sound <- paragraphs_sound[nchar(paragraphs_sound) < 1000]


# Añadir todas las aplicaciones de la función en formato de no ejecutar el chunk


# Load the results

# The Bell Jar
results_bert_belljar <- readRDS("results_bert_belljar.rds")

#To the Lighthouse
results_bert_lighthouse <- readRDS("results_bert_lighthouse.rds")

#The Yellow Wallpaper
results_bert_yellow <- readRDS("results_bert_yellow.rds")

# Sons and Lovers
results_bert_sons <- readRDS("results_bert_son.rds")

#The sun also rises
results_bert_sun <- readRDS("results_bert_sun.rds")

# The sound of the fury
results_bert_sound <- readRDS("results_bert_sound.rds")



# Clean the results

# The Bell Jar
processing_belljar_bert <- lapply(seq_along(results_bert_belljar), function(i) {
  res <- results_bert_belljar[[i]]
  if (!is.null(res)) {
    data.frame(
      paragraph = i,
      emotion = sapply(res, function(x) x$label),
      score = sapply(res, function(x) x$score)
    )
  }
}) |> bind_rows()

# The Yellow Wallpaper
processing_wallpaper_bert <- lapply(seq_along(results_bert_yellow), function(i) {
  res <- results_bert_yellow[[i]]
  
  if (!is.null(res) && is.list(res) && all(c("label", "score") %in% names(res[[1]]))) {
    data.frame(
      paragraph = i,
      emotion = sapply(res, function(x) x$label),
      score = sapply(res, function(x) x$score)
    )
  } else {
    NULL
  }
}) |> bind_rows()

# To the Lighthouse
processing_lighthouse_bert <- lapply(seq_along(results_bert_lighthouse), function(i) {
  res <- results_bert_lighthouse[[i]]
  
  if (!is.null(res) && nrow(res) > 0) {
    data.frame(
      paragraph = i,
      emotion = res$label,
      score = res$score
    )
  } else {
    NULL
  }
}) |> bind_rows()

# Sons and Lovers
processing_bert_sons <- lapply(seq_along(block_results_sons), function(block_index) {
  block <- block_results_sons[[block_index]]
  
  if (!is.null(block) && length(block) > 0) {
    lapply(seq_along(block), function(paragraph_index) {
      paragraph <- block[[paragraph_index]]
      
      if (!is.null(paragraph) && is.list(paragraph) && all(c("label", "score") %in% names(paragraph[[1]]))) {
        data.frame(
          block = block_index,
          paragraph = paragraph_index,
          emotion = sapply(paragraph, function(x) x$label),
          score = sapply(paragraph, function(x) x$score)
        )
      } else {
        NULL
      }
    })
  } else {
    NULL
  }
}) |> unlist(recursive = FALSE) |> bind_rows()

# The sun also rises
processing_bert_sun <- map2_dfr(
  block_results_sun,
  seq_along(block_results_sun),
  function(block, block_index) {
    if (is.null(block) || length(block) == 0 || is.null(block[[1]])) return(NULL)
    
    paragraphs <- block[[1]]  
    
    map2_dfr(
      paragraphs,
      seq_along(paragraphs),
      function(paragraph, paragraph_index) {
        if (is.null(paragraph) || !is.list(paragraph)) return(NULL)
        
        map_dfr(paragraph, function(emotion_entry) {
          if (!is.null(emotion_entry$label) && !is.null(emotion_entry$score)) {
            data.frame(
              block = block_index,
              paragraph = paragraph_index,
              emotion = emotion_entry$label,
              score = emotion_entry$score,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        })
      }
    )
  }
)

# The sound and the fury
processing_bert_sound <- map2_dfr(
  block_results_sound,
  seq_along(block_results_sound),
  function(block, block_index) {
    if (is.null(block) || length(block) == 0 || is.null(block[[1]])) return(NULL)
    
    paragraphs <- block[[1]]  
    
    map2_dfr(
      paragraphs,
      seq_along(paragraphs),
      function(paragraph, paragraph_index) {
        if (is.null(paragraph) || !is.list(paragraph)) return(NULL)
        
        map_dfr(paragraph, function(emotion_entry) {
          if (!is.null(emotion_entry$label) && !is.null(emotion_entry$score)) {
            data.frame(
              block = block_index,
              paragraph = paragraph_index,
              emotion = emotion_entry$label,
              score = emotion_entry$score,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        })
      }
    )
  }
)



# Graph - The Bell Jar
emotion_counts_belljar <- processing_belljar_bert %>%
  count(emotion, name = "n")


total_emotions <- sum(emotion_counts_belljar$n)

emotion_counts_belljar <- emotion_counts_belljar %>%
  mutate(proportion = n / total_emotions)


ggplot(emotion_counts_belljar, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Proportion of different emotions in The Bell Jar",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal()


# Graph - The Yellow Wallpaper

emotion_counts_yellow <- processing_wallpaper_bert %>%
  count(emotion, name = "n")


total_emotions <- sum(emotion_counts_yellow$n)

emotion_counts_yellow <- emotion_counts_yellow %>%
  mutate(proportion = n / total_emotions)

ggplot(emotion_counts_yellow, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Proportion of different emotions in The Yellow Wallpaper",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal()


# Graph - To the Lighthouse
emotion_counts_lighthouse <- processing_lighthouse_bert %>%
  count(emotion, name = "n")

total_emotions <- sum(emotion_counts_lighthouse$n)

emotion_counts_lighthouse <- emotion_counts_lighthouse %>%
  mutate(proportion = n / total_emotions)


ggplot(emotion_counts_lighthouse, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Proportion of different emotions in To the lighthouse",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal()



# Graph - Sons and Lovers
emotion_counts_sons <- processing_bert_sons %>%
  count(emotion, name = "n") %>%
  mutate(proportion = n / sum(n))

ggplot(emotion_counts_sons, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Proportion of emotions in *Sons and Lovers* (BERT)",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal()


# Graph - The sun also rises
emotion_counts_sun <- processing_bert_sun %>%
  count(emotion, name = "n") %>%
  mutate(proportion = n / sum(n))

ggplot(emotion_counts_sun, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Proportion of emotions in *The sun also rises* (BERT)",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal()


# Graph - The sound and the fury
emotion_counts_sound <- processing_bert_sound %>%
  count(emotion, name = "n") %>%
  mutate(proportion = n / sum(n))

ggplot(emotion_counts_sound, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Proportion of emotions in *The sound of the fury* (BERT)",
    x = "Emotion",
    y = "Proportion"
  ) +
  theme_minimal()


## Focusing on mental health: a more focused model (EmoRoBERTa)

# Load the results

# The Bell Jar
results_mental_health_belljar <- readRDS("results_anxiety_belljar.rds")

# The Yellow Wallpaper
results_mental_health_wallpaper <- readRDS("results_anxiety_yellow.rds")

# To the Lighthouse
results_mental_health_lighthouse <- readRDS("results_anxiety_lighthouse.rds")

# Sons and Lovers
block_numbers_sons_depression <- 1:204
file_names_sons_depression <- paste0("results_depression_sons_block_", block_numbers_sons_depression, ".rds")
block_results_sons_depression <- lapply(file_names_sons_depression, readRDS)

# The sun also rises
block_numbers_sun_depression <- 1:81
file_names_sun_depression <- paste0("results_depression_sun_block_", block_numbers_sun_depression, ".rds")
block_results_sun_depression <- lapply(file_names_sun_depression, readRDS)

# The sound and the fury
block_numbers_fury_depression <- 1:158
file_names_fury_depression <- paste0("results_depression_fury_block_", block_numbers_fury_depression, ".rds")
block_results_fury_depression <- lapply(file_names_fury_depression, readRDS)

# Process the results

# The Bell Jar
processing_belljar_mental_health <- lapply(seq_along(results_mental_health_belljar), function(i) {
  res <- results_mental_health_belljar[[i]]
  
  if (!is.null(res) && length(res) > 0 && !is.null(res[[1]])) {
    df <- res[[1]]
    data.frame(
      paragraph = i,
      emotion = df$label,
      score = df$score
    )
  } else {
    message("Empty or null in ", i)
    NULL
  }
}) |> dplyr::bind_rows()

# The Yellow Wallpaper
processing_yellow_mental_health <- lapply(seq_along(results_mental_health_wallpaper), function(i) {
  res <- results_mental_health_wallpaper[[i]]
  
  if (!is.null(res) && length(res) > 0 && !is.null(res[[1]])) {
    df <- res[[1]]
    data.frame(
      paragraph = i,
      emotion = df$label,
      score = df$score
    )
  } else {
    message("Empty or null in ", i)
    NULL
  }
}) |> dplyr::bind_rows()

# To the lighthouse
processing_lighthouse_mental_health <- lapply(seq_along(results_mental_health_lighthouse), function(i) {
  res <- results_mental_health_lighthouse[[i]]
  
  if (!is.null(res) && length(res) > 0 && !is.null(res[[1]])) {
    df <- res[[1]]
    data.frame(
      paragraph = i,
      emotion = df$label,
      score = df$score
    )
  } else {
    message("Empty or null in ", i)
    NULL
  }
}) |> dplyr::bind_rows()


# Sons and Lovers

processing_mental_health_sons <- lapply(seq_along(block_results_sons_depression), function(block_index) {
  block <- block_results_sons_depression[[block_index]]
  
  if (!is.null(block) && length(block) > 0) {
    lapply(seq_along(block), function(paragraph_index) {
      paragraph <- block[[paragraph_index]]
      
      if (!is.null(paragraph) && is.list(paragraph) && all(c("label", "score") %in% names(paragraph[[1]]))) {
        data.frame(
          block = block_index,
          paragraph = paragraph_index,
          emotion = sapply(paragraph, function(x) x$label),
          score = sapply(paragraph, function(x) x$score)
        )
      } else {
        NULL
      }
    })
  } else {
    NULL
  }
}) |> unlist(recursive = FALSE) |> bind_rows()

# The sun also rises

processing_mental_health_sun <- lapply(seq_along(block_results_sun_depression), function(block_index) {
  block <- block_results_sun_depression[[block_index]]
  
  if (!is.null(block) && length(block) > 0) {
    lapply(seq_along(block), function(paragraph_index) {
      paragraph <- block[[paragraph_index]]
      
      if (!is.null(paragraph) && is.list(paragraph) && all(c("label", "score") %in% names(paragraph[[1]]))) {
        data.frame(
          block = block_index,
          paragraph = paragraph_index,
          emotion = sapply(paragraph, function(x) x$label),
          score = sapply(paragraph, function(x) x$score)
        )
      } else {
        NULL
      }
    })
  } else {
    NULL
  }
}) |> unlist(recursive = FALSE) |> bind_rows()

# The sound and the fury
processing_mental_health_fury <- lapply(seq_along(block_results_fury_depression), function(block_index) {
  block <- block_results_fury_depression[[block_index]]
  
  if (!is.null(block) && length(block) > 0) {
    lapply(seq_along(block), function(paragraph_index) {
      paragraph <- block[[paragraph_index]]
      
      if (!is.null(paragraph) && is.list(paragraph) && all(c("label", "score") %in% names(paragraph[[1]]))) {
        data.frame(
          block = block_index,
          paragraph = paragraph_index,
          emotion = sapply(paragraph, function(x) x$label),
          score = sapply(paragraph, function(x) x$score)
        )
      } else {
        NULL
      }
    })
  } else {
    NULL
  }
}) |> unlist(recursive = FALSE) |> bind_rows()


# Graph mental health - The Bell Jar
summary_belljar_mental_health <- processing_belljar_mental_health %>%
  group_by(emotion) %>%
  summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(mean_score))

ggplot(summary_belljar_mental_health, aes(x = reorder(emotion, mean_score), y = mean_score, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Mental health in The Bell Jar (model EmoRoBERTa)",
       x = "Emotion", y = "Average score") +
  theme_minimal()


# Graph mental health - The Yellow Wallpaper

summary_yellow_mental_health <- processing_yellow_mental_health %>%
  group_by(emotion) %>%
  summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(mean_score))

ggplot(summary_yellow_mental_health, aes(x = reorder(emotion, mean_score), y = mean_score, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Mental health in The Yellow Wallpaper (model EmoRoBERTa)",
       x = "Emotion", y = "Average score") +
  theme_minimal()


# Graph mental health - To the lighthouse

summary_lighthouse_mental_health <- processing_lighthouse_mental_health %>%
  group_by(emotion) %>%
  summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(mean_score))

ggplot(summary_lighthouse_mental_health, aes(x = reorder(emotion, mean_score), y = mean_score, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Mental health in To the lighthouse (model EmoRoBERTa)",
       x = "Emotion", y = "Average score") +
  theme_minimal()


# Graph mental health - Sons and Lovers

mental_health_counts_sons <- processing_mental_health_sons %>%
  count(emotion, name = "n") %>%
  mutate(proportion = n / sum(n))

ggplot(mental_health_counts_sons, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Mental health in To the lighthouse (model EmoRoBERTa)",
    x = "Emotion",
    y = "Average score"
  ) +
  theme_minimal()



# Graph mental health - The sun also rises

mental_health_counts_sun <- processing_mental_health_sun %>%
  count(emotion, name = "n") %>%
  mutate(proportion = n / sum(n))

ggplot(mental_health_counts_sun, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Mental health in To the lighthouse (model EmoRoBERTa)",
    x = "Emotion",
    y = "Average score"
  ) +
  theme_minimal()


# Graph mental health - The sound and the fury
mental_health_counts_fury <- processing_depression_fury %>%
  count(emotion, name = "n") %>%
  mutate(proportion = n / sum(n))

ggplot(mental_health_counts_fury, aes(x = reorder(emotion, proportion), y = proportion, fill = emotion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Mental health in The sound and the fury (model EmoRoBERTa)",
    x = "Emotion",
    y = "Proporción"
  ) +
  theme_minimal()


# Contextual analysis - sentimentr


# Remember to load the library!
library(sentimentr)

# Create a function
analize_sentimentr <- function(paragraphs, book_title) {
  sentiment_scores <- sentiment(paragraphs)
  sentiment_scores$book <- book_title
  return(sentiment_scores)
}


# Analize the books:
results_sentimentr_belljar <- analize_sentimentr(paragraphs_belljar, "The Bell Jar")
results_sentimentr_wallpaper <- analize_sentimentr(paragraphs_yellow, "The Yellow Wallpaper")
results_sentimentr_lighthouse <- analize_sentimentr(paragraphs_lighthouse, "To the lighthouse")
results_sentimentr_sound <- analize_sentimentr(paragraphs_sound, "The sound of the fury")
results_sentimentr_sons <- analize_sentimentr(paragraphs_sons, "Sons and Lovers")
results_sentimentr_sun <- analize_sentimentr(paragraphs_sun, "The sun also rises")


# Create an only data set
all_sentiments_r <- bind_rows(
  results_sentimentr_belljar,
  results_sentimentr_lighthouse,
  results_sentimentr_wallpaper,
  results_sentimentr_sons,
  results_sentimentr_sound,
  results_sentimentr_sun
)

# Graph
library(ggplot2)
all_sentiments_r |> 
  group_by(book) |> 
  summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) |> 
  ggplot(aes(x = reorder(book, mean_sentiment), y = mean_sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sentiment mean by novel using sentimentr", x = "Novel", y = "Mean sentiment") +
  theme_minimal()


# Specific scores

sentiment_summary_r <- all_sentiments_r %>%
  group_by(book) %>%
  summarise(
    mean_sentiment = mean(sentiment, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_sentiment))

print(sentiment_summary_r)
