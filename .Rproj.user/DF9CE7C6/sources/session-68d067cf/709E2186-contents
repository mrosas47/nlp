# 1. INSTALAR Y CARGAR LIBRERIAS
# Instalar librerias si no estan instaladas
install.packages(tm)
install.packages(pdftools)
install.packages("tokenizers")
install.packages('tidytext')
install.packages(dplyr)
install.packages(tidyr)
install.packages("stringr")
install.packages("ggplot2")
install.packages("topicmodels")

# Cargar librerias
library(tm)
library(pdftools)
library(tokenizers)
library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(topicmodels)

# 2. CARGAR EL DOCUMENTO PDF
# Leer el PDF
pdf_path <- "C:/Users/danie/Dropbox/FONDECYT MANE/PDF/Propuestas para Densificación_Equilibrada_2019.pdf"
pdf_text <- pdf_text(pdf_path)

# 3. LIMPIEZA DE TEXTO
# Convertir a Corpus y realizar limpieza
corpus <- Corpus(VectorSource(pdf_text))

corpus_clean <- tm_map(corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("es"))

# 4. FRECUENCIA DE CONCEPTOS
# Frecuencia de conceptos, se pueden agregar mas
tdm <- TermDocumentMatrix(corpus_clean)

# Convertir TDM a matriz
m <- as.matrix(tdm)

# Obtener la frecuencia de palabras/frases
term_frequency <- rowSums(m)

# Filtrar solo los términos que nos interesan
terms_to_find <- c("densidad", "densificación", "verticalización", "renovación")
term_frequency <- term_frequency[names(term_frequency) %in% terms_to_find]

term_frequency

# 5. CONCEPTOS ASOCIADOS A LA DENSIDAD
# Convertir el corpus a un data frame
text_df <- data_frame(line = 1:length(corpus_clean), text = sapply(corpus_clean, as.character))

# Tokenizar en bigramas
bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Elaborar un dataframe que contenga las co-ocurrencias de las palabras
bigrams_df <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 != word2)

# Calcula el "Pointwise Mutual Information" (PMI), que es una medida de asociación entre 
# palabras. Específicamente, mide cuánto más a menudo dos palabras aparecen juntas de lo que 
# esperaríamos si fueran independientes.
total_bigrams <- nrow(bigrams_df)
word_counts <- count(bigrams_df, word1)
bigram_counts <- count(bigrams_df, word1, word2)

pmi_df <- left_join(bigram_counts, word_counts, by = c("word1" = "word1")) %>%
  rename(word1_count = n.x, bigram_count = n.y) %>%
  mutate(p_word1 = word1_count / total_bigrams,
         p_bigram = bigram_count / total_bigrams) %>%
  mutate(pmi = log2(p_bigram / (p_word1 * p_word1))) %>%
  arrange(-pmi)

# Filtrar para la palabra "densidad" (repetir con los demás conceptos)
density_correlations <- pmi_df %>%
  filter(word1 == "densidad" | word2 == "densidad") %>%
  arrange(-pmi)

density_correlations

# Filtrar bigramas que contienen la palabra "densidad" (repetir con los demás conceptos)
density_bigrams_densidad <- bigrams %>%
  filter(grepl("densidad", bigram))

# Contar la frecuencia de bigramas
bigram_counts_densidad <- density_bigrams_densidad %>%
  count(bigram, sort = TRUE)

bigram_counts_densidad

# Opcional: Gráficos de los 10 bigramas más frecuentes con "densidad" (repetir con los demás 
# conceptos)
top_bigrams <- bigram_counts_densidad %>%
  filter(!str_detect(bigram, "^densidad densidad$")) %>% 
  head(10)

ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top Bigrams Associated with 'Densidad'", 
       x = "Bigram", 
       y = "Frequency") +
  theme_minimal()

# 6. INSTRUMENTALIZACION DE LA DENSIDAD
# Esto te proporciona un vector donde cada elemento es un fragmento de texto que contiene
# el concepto junto con 50 caracteres antes y después, para proporcionar contexto.

# Unir todo el texto del corpus en una única cadena de caracteres
full_text <- paste(unlist(sapply(corpus_clean, as.character)), collapse = " ")

# Con la palabra "densidad"
# Extraer el contexto alrededor de las ocurrencias de "densidad" (repetir con los demás conceptos)
context_list_densidad <- str_extract_all(full_text, ".{0,50}densidad.{0,50}")

# Convertir la lista a un vector
context_vector_densidad <- unlist(context_list_densidad)

context_vector_densidad

# 7. ANALISIS DE TOPICOS
# Es una técnica que se utiliza en Procesamiento de Lenguaje Natural (NLP) para identificar
# temas o tópicos recurrentes en un corpus de texto. Una de las técnicas más populares
# para realizar análisis de tópicos es el Modelado de Tópicos Latente Dirichlet (LDA)

# Creamos un corpus
corpus <- Corpus(VectorSource(text_df$text))

# Creamos la DTM
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))

# Aplicar LDA con 5 topicos
num_topics <- 5
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Muestra las 10 palabras más relevantes para cada tópico
terms(lda_model, 10)




