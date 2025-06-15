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
# Make sure to have saved the .txt files that can be found in the repository

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


# Apply the function to each of the books

tokens_lighthouse <- preprocess_text(tothelighthouse_vw)
tokens_yellowwallpaper <- preprocess_text(theyellowwallpaper)
tokens_belljar <- preprocess_text(thebelljar)
tokens_sound_fury <- preprocess_text(sound_fury)
tokens_sun_rises <- preprocess_text(sun_rises)
tokens_sons_lovers <- preprocess_text(sons_lovers)



# NRC


# The Bell Jar - Sylvia Plath

# Load the NRC sentiment lexicon (includes emotions like anger, fear, joy, etc.)
nrc <- get_sentiments("nrc")

# Join tokenized words from the book with the NRC sentiment lexicon, 
# keeping only the emotion-related sentiments (excluding "positive" and "negative")
sentiments_belljar <- tokens_belljar |> 
  inner_join(nrc, by = "word") |> 
  filter(!sentiment %in% c("positive", "negative"))

# Count the frequency of each sentiment and visualize them in a horizontal bar plot
sentiments_belljar |> 
  count(sentiment, sort = TRUE) |> 
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +                       # Bar chart without legend
  coord_flip() +                                        # Flip axes for horizontal bars
  labs(title = "Sentiments in *The Bell Jar*",         # Plot title
       x = "Sentiment",                                 # X-axis label
       y = "Frequency")    



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
# You must create a function called api_token with your own token to The Hugging Face
api_token <- "introduce_your_token_here"

# Then, load the model BERT:
model_general <- "bhadresh-savani/bert-base-uncased-emotion"

## General emotions
# Create a function to analyze emotions in a given text using a Hugging Face model
analize_emotion <- function(text, model, token) {
  
  # Build the API endpoint URL based on the selected model
  url <- paste0("https://api-inference.huggingface.co/models/", model)
  
  # Send a POST request to the Hugging Face Inference API
  answer <- tryCatch({
    POST(
      url,
      add_headers(Authorization = paste("Bearer", token)),  # Add the authorization token
      body = list(inputs = text),                            # Send the input text as JSON
      encode = "json",                                       # Encode the request as JSON
      timeout(60)                                            # Set a 60-second timeout, you may increase or decrease this timeout
    )
  }, error = function(e) {
    message("❌ Request error: ", e$message)                 # Print error message if request fails
    return(NULL)                                             # Return NULL on error
  })
  
  # If a valid response is received and status code is 200 (OK)
  if (!is.null(answer) && status_code(answer) == 200) {
    
    # Extract response content as text (avoiding automatic parser issues)
    result_text <- content(answer, as = "text", encoding = "UTF-8")
    
    # Try to parse the text content as JSON for future processing and graphing
    parsed <- tryCatch(fromJSON(result_text), error = function(e) {
      message("⚠️ Failed to parse JSON response.")          # Notify if parsing fails
      return(NULL)
    })
    
    return(parsed)                                           # Return the parsed JSON object
    
  } else {
    # Print a warning message if the response status is not 200
    message("⚠️ Response error: Status code ", status_code(answer))
    return(NULL)
  }
}



# Filter out short or empty paragraphs, or those that will give us problem
# The Bell Jar
# Split the full text of The Bell Jar into paragraphs using periods followed by whitespace as delimiters
paragraphs_belljar <- unlist(strsplit(thebelljar, split = "\\.\\s+"))
# Keep only paragraphs longer than 30 characters to filter out short or irrelevant ones
paragraphs_belljar <- paragraphs_belljar[nchar(paragraphs_belljar) > 30]
# Remove empty or NA paragraphs
paragraphs_belljar <- paragraphs_belljar[!is.na(paragraphs_belljar) & paragraphs_belljar != ""]
# Optionally, remove very long paragraphs (e.g., longer than 600 characters) to simplify processing
paragraphs_belljar <- paragraphs_belljar[nchar(paragraphs_belljar) < 600]

# The same will be done for the rest of the books!
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


# You may apply the function to each of the books...
# The Bell Jar
results_bert_belljar <- lapply(paragraphs_belljar, function(p) {
  Sys.sleep(1)  # Espera de 1 seg entre llamadas
  analize_emotion(p, model_general, api_token)
})

# The Yellow Wallpaper
results_bert_yellow <- lapply(paragraphs_yellow, function(p) {
  Sys.sleep(1)
  analize_emotion(p, model_general, api_token)
})

# To the Lighthouse
results_bert_lighthouse <- lapply(paragraphs_lighthouse, function(p) {
  Sys.sleep(1)
  analize_emotion(p, model_general, api_token)
})

# Sons and Lovers
block_size_sons <- 102
n_blocks_sons <- ceiling(length(paragraphs_sons) / block_size_sons)

for (i in 1:n_blocks_sons) {
  start <- (i - 1) * block_size_sons + 1
  end <- min(i * block_size_sons, length(paragraphs_sons))
  block <- paragraphs_sons[start:end]

  message("🔄 Procesando bloque ", i, " de ", n_blocks)

  result <- lapply(block, function(p) {
    Sys.sleep(1.5)  # pausa para evitar timeout
    tryCatch({
      analize_emotion(p, model_general, api_token)
    }, error = function(e) {
      message("⚠️ Error con párrafo: ", e$message)
      return(NULL)
    })
  })

  saveRDS(result, paste0("results_sons_block_", i, ".rds"))
}

# The sun also rises
block_size_sun <- 81
n_blocks_sun <- ceiling(length(paragraphs_sun) / block_size_sun)

for (i in 1:n_blocks_sun) {
  start <- (i - 1) * block_size_sun + 1
  end <- min(i * block_size_sun, length(paragraphs_sun))
  block <- paragraphs_sun[start:end]

  message("🔄 Procesando bloque ", i, " de ", n_blocks)

  result <- lapply(block, function(p) {
    Sys.sleep(1.5)  # pausa para evitar timeout
    tryCatch({
      analize_emotion(p, model_general, api_token)
    }, error = function(e) {
      message("⚠️ Error con párrafo: ", e$message)
      return(NULL)
    })
  })

  saveRDS(result, paste0("results_sun_block_", i, ".rds"))
}

# The Sound and the Fury
block_size_sound <- 48
n_blocks_sound <- ceiling(length(paragraphs_sound) / block_size_sound)

for (i in 1:n_blocks_sound) {
  start <- (i - 1) * block_size_sound + 1
  end <- min(i * block_size_sound, length(paragraphs_sound))
  block <- paragraphs_sound[start:end]

  message("🔄 Procesando bloque ", i, " de ", n_blocks)

  result <- lapply(block, function(p) {
    Sys.sleep(1.5)  # pausa para evitar timeout
    tryCatch({
      analize_emotion(p, model_general, api_token)
    }, error = function(e) {
      message("⚠️ Error con párrafo: ", e$message)
      return(NULL)
    })
  })

  saveRDS(result, paste0("results_sound_block_", i, ".rds"))
}

#... and then save the results...
# The Bell Jar
saveRDS(results_bert_belljar, "results_bert_belljar.rds")

# The Yellow Wallpaper
saveRDS(results_bert_yellow, "results_bert_yellow.rds")

# To the Lighthouse
saveRDS(results_bert_lighthouse, "results_bert_lighthouse.rds")

# Sons and Lovers
block_numbers_sons <- 1:102
file_names_sons <- paste0("results_sons_block_", block_numbers_sons, ".rds")

results_bert_sons <- lapply(file_names_sons, readRDS)

# The sun also rises
block_numbers_sun <- 1:81
file_names_sun <- paste0("results_sun_block_", block_numbers_sun, ".rds")

results_bert_sun <- lapply(file_names_sun, readRDS)

# The sound and the fury
block_numbers_sound <- 1:48
file_names_sound <- paste0("results_sound_block_", block_numbers_sound, ".rds")

results_bert_sound <- lapply(file_names_sound, readRDS)

# ... or you may directly load the results!

# The Bell Jar
results_bert_belljar <- readRDS("results_bert_belljar.rds")

#To the Lighthouse
results_bert_lighthouse <- readRDS("results_bert_lighthouse.rds")

#The Yellow Wallpaper
results_bert_yellow <- readRDS("results_bert_yellow.rds")

# Sons and Lovers
results_bert_sons <- readRDS("results_bert_sons.rds")

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
processing_bert_sons <- lapply(seq_along(results_bert_sons), function(block_index) {
  block <- results_bert_sons[[block_index]]

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
  results_bert_sun,
  seq_along(results_bert_sun),
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
  results_bert_sound,
  seq_along(results_bert_sound),
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
# Again, you may apply the same funcition, but changing the model:
model_mental_health <- "SamLowe/roberta-base-go_emotions"

# As this!
# The Bell Jar
results_emoroberta_belljar <- lapply(paragraphs_belljar, function(p) {
  Sys.sleep(1)
  analize_emotion(p, model_mental_health, api_token)
})
        
# The Yellow Wallpaper
results_emoroberta_yellow <- lapply(paragraphs_yellow, function(p) {
  Sys.sleep(1)
  analize_emotion(p, model_mental_health, api_token)
})
# To the Lighthouse
results_emoroberta_lighthouse <- lapply(paragraphs_lighthouse, function(p) {
  Sys.sleep(1)
  analize_emotion(p, model_mental_health, api_token)
})
# Sons and Lovers
block_size_sons <- 50  # In this case, we may use blocks, since the books are too long
total_blocks_sons <- ceiling(length(paragraphs_sons) / block_size_sons)

for (i in 1:total_blocks_sons) {
  message("🔄 Procesando bloque ", i, " de ", total_blocks_sons)
  
  start <- (i - 1) * block_size_sons + 1
  end <- min(i * block_size_sons, length(paragraphs_sons))
  block_sons <- paragraphs_sons[start:end]
  
  # Analyze each paragraph
  block_results <- lapply(block_sons, function(p) {
    Sys.sleep(1)  # time in between calls
    tryCatch({
      analize_emotion(p, model_mental_health, api_token)
    }, error = function(e) {
      message("⚠️ Error con un párrafo: ", e$message)
      return(NULL)
    })
  })
  
  # Save the results
  saveRDS(block_results, paste0("results_depression_sons_block_", i, ".rds"))
}
        
# The sun also rises
block_size_sun <- 50  
total_blocks_sun <- ceiling(length(paragraphs_sun) / block_size_sun)

for (i in 1:total_blocks_sun) {
  message("🔄 Procesando bloque ", i, " de ", total_blocks_sun)
  
  start <- (i - 1) * block_size_sun + 1
  end <- min(i * block_size_sun, length(paragraphs_sun))
  block_sun <- paragraphs_sun[start:end]
  
  block_results_sun_depression <- lapply(block_sun, function(p) {
    Sys.sleep(1)  
    tryCatch({
      analize_emotion(p, model_mental_health, api_token)
    }, error = function(e) {
      message("⚠️ Error con un párrafo: ", e$message)
      return(NULL)
    })
  })
  
  # Save the results
  saveRDS(block_results_sun_depression, paste0("results_depression_sun_block_", i, ".rds"))
}
        
# The sound and the fury
block_size_fury <- 30  
total_blocks_fury <- ceiling(length(paragraphs_sound) / block_size_fury)

for (i in 1:total_blocks_fury) {
  message("🔄 Procesando bloque ", i, " de ", total_blocks_fury)
  
  start <- (i - 1) * block_size_fury + 1
  end <- min(i * block_size_fury, length(paragraphs_sound))
  block_fury <- paragraphs_sound[start:end]
  
  block_results <- lapply(block_fury, function(p) {
    Sys.sleep(2)  # tiempo entre llamadas
    tryCatch({
      analize_emotion(p, model_mental_health, api_token)
    }, error = function(e) {
      message("⚠️ Error con un párrafo: ", e$message)
      return(NULL)
    })
  })
  
  # Save the results
  saveRDS(block_results, paste0("results_depression_fury_block_", i, ".rds"))
}

        
# And then save the results:
# The Bell Jar
saveRDS(results_emoroberta_belljar, "results_emoroberta_belljar.rds")

# The Yellow Wallpaper
saveRDS(results_emoroberta_yellow, "results_emoroberta_yellow.rds")

# To the Lighthouse
saveRDS(results_emoroberta_lighthouse, "results_emoroberta_lighthouse.rds")
        
# Or you may directly load the results!

# The Bell Jar
results_mental_health_belljar <- readRDS("results_emoroberta_belljar.rds")

# The Yellow Wallpaper
results_mental_health_wallpaper <- readRDS("results_emoroberta_yellow.rds")

# To the Lighthouse
results_mental_health_lighthouse <- readRDS("results_emoroberta_lighthouse.rds")

# Sons and Lovers
block_numbers_sons_emoroberta <- 1:204
file_names_sons_emoroberta <- paste0("results_depression_sons_block_", block_numbers_sons_emoroberta, ".rds")
block_results_sons_emoroberta <- lapply(file_names_sons_emoroberta, readRDS)

# The sun also rises
block_numbers_sun_emoroberta <- 1:81
file_names_sun_emoroberta <- paste0("results_depression_sun_block_", block_numbers_sun_emoroberta, ".rds")
block_results_sun_emoroberta <- lapply(file_names_sun_emoroberta, readRDS)

# The sound and the fury
block_numbers_fury_emoroberta <- 1:158
file_names_fury_emoroberta <- paste0("results_depression_fury_block_", block_numbers_fury_emoroberta, ".rds")
block_results_fury_emoroberta <- lapply(file_names_fury_emoroberta, readRDS)

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

processing_mental_health_sons <- lapply(seq_along(block_results_sons_emoroberta), function(block_index) {
  block <- block_results_sons_emoroberta[[block_index]]
  
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

processing_mental_health_sun <- lapply(seq_along(block_results_sun_emoroberta), function(block_index) {
  block <- block_results_sun_emoroberta[[block_index]]
  
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
processing_mental_health_fury <- lapply(seq_along(block_results_fury_emoroberta), function(block_index) {
  block <- block_results_fury_emoroberta[[block_index]]
  
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
mental_health_counts_fury <- processing_mental_health_fury %>%
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
