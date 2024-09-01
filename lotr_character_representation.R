library(readr)
library(tidyverse)
library(pdftools)
library(stringr)
library(stringi)
library(ggrepel)

# Extract text from the PDF
lotr_text <- pdf_text("C:/Users/austi/OneDrive/Desktop/R/R-Sandbox/data/lotr_text_full.pdf")
lotr_text_full <- lotr_text |> 
  paste(collapse = " ") |> 
  tolower() |> 
  stri_trans_general("Latin-ASCII") |> 
  str_squish() |> 
  str_remove(".*elder days in middle-earth") |> 
  str_remove("appendix a annals of the kings.*")
  
view(lotr_text_full)


# finding names -----------------------------------------------------------

# Function to replace all names with cannonical names using the mapping
replace_with_canonical <- function(text, mapping) {
  for (i in 1:nrow(mapping)) {
    pattern <- str_c("\\b(", str_replace_all(mapping$names[i], ",", "|"), ")\\b", collapse = "")
    text <- str_replace_all(text, pattern, mapping$canonical_name[i])
  }
  return(text)
}

#function to count occurrences of each character's name
count_occurrences <- function(character, text) {
  str_count(text, fixed(character))
}

# data frame mapping all names to their canonical names
name_mapping <- data.frame(
  canonical_name = c("gollum", "aragorn", "gandalf", "sam", "merry", "pippin", "sauron", "saruman", "nazgul",
                     "galadriel", "wormtongue", "balrog", "witch-king", "radagast",
                     "king of the dead", "celeborn"),
  names = c("gollum,smeagol", "aragorn,strider,elessar,estel", "gandalf,mithrandir,olorin,grey pilgrim", "sam,samwise",
            "merry,meriadoc", "pippin,peregrin took", "sauron,dark lord,necromancer", "saruman,curunir", 
            "nazgul,ringwraith,black rider", "galadriel,lady of lothlorien,lady of the wood", "wormtongue,grima", "balrog,durin's bane", 
            "witch-king,witch-lord,shadow of angmar,king of angmar,lord of the nazgul,pale king,black captain,wraith-lord",
            "radagast,aiwendil", "king of the dead,king of the mountain,rioc", "celeborn,teleporno"),
  stringsAsFactors = FALSE
)

# Apply the replacement function to cannonize all names
lotr_text_full_cannonized_names <- replace_with_canonical(lotr_text_full, name_mapping)


#create character list
characters <- c("frodo", "sam", "gandalf", "aragorn", "legolas", 
                "gimli", "boromir", "merry", "pippin", "gollum", 
                "sauron", "saruman", "faramir", "denethor", "eowyn", 
                "theoden", "nazgul", "bilbo", "bombadil", "glorfindel", 
                "elrond", "arwen", "galadriel", "wormtongue", "shadowfax", 
                "treebeard", "shelob", "eomer", "balrog", "witch-king", 
                "radagast", "haldir", "gothmog", "king of the dead", "isildur",
                "celeborn", "rosie cotton", "gamling", "hama")

# Apply the function
name_counts <- sapply(characters, count_occurrences, text = lotr_text_full_cannonized_names)
name_counts_df <- data.frame(character = characters, count = name_counts)


# screen time -------------------------------------------------------------

screen_time <- data.frame(
  character = c("frodo", "sam", "gandalf", "aragorn", "legolas", 
                "gimli", "boromir", "merry", "pippin", "gollum", 
                "sauron", "saruman", "faramir", "denethor", "eowyn", 
                "theoden", "nazgul", "bilbo", "bombadil", "glorfindel", 
                "elrond", "arwen", "galadriel", "wormtongue", "shadowfax", 
                "treebeard", "shelob", "eomer", "balrog", "witch-king", 
                "radagast", "haldir", "gothmog", "king of the dead", "isildur",
                "celeborn", "rosie cotton", "gamling", "hama"),
  screen_time = c(123.933, 80.5833, 88.3667, 96.1667, 47.9333,
                  19.85, 25.9, 36.2167, 45.6833, 42.4167,
                  4, 10.9667, 14.8167, 7.6833, 21.9667,
                  31.65, NA, 13.6, 0, 0.5,
                  15.0833, 18.6167, 11.4833, 7.1, 4,
                  10, 3.5, 13.3667, 2.75, 6.6,
                  0, 3.5667, 3.5, 1.9167, 2.5,
                  1.4333, 1.5, 4, 2)
)


# join tables -------------------------------------------------------------

lotr_counts_screen <- name_counts_df |> 
  inner_join(screen_time, by = "character")

lotr_counts_screen$character <- str_to_title(lotr_counts_screen$character)

# plotting ----------------------------------------------------------------

# Calculate the reference y-value for each x-value
lotr_counts_screen <- lotr_counts_screen %>%
  mutate(
    ref_y = (slope * log10(count)) + intercept,  # y-values of the line
    color = ifelse(log10(screen_time) > ref_y, "OVER-represented in movies", "UNDER-represented in movies")
  )

ggplot(lotr_counts_screen, aes(x = count, y = screen_time)) +
  geom_point(aes(color = color), size = 3, alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(intercept = -0.25, slope = 0.7, linetype = "dashed", linewidth = 1) +
  geom_label_repel(aes(label = character), box.padding = 0.3) +
  scale_color_manual(values = c("OVER-represented in movies" = "red", 
                                "UNDER-represented in movies" = "blue")) +
  theme_bw() +
  labs(
    title = "Lord of the Rings Characters with the Most Screen Time",
    x = "Number of Mentions in Books (log scale)",
    y = "Screen Time in Films (minutes) (log scale)",
    color = ""
  )
