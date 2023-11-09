# Meta-------------------------------------------------------------------------

# Title: NLP testing
# Project: FONDECYT Anatomías de verticalización
# URL: NA
# Author: Martín Rosas Araya
# Contact: admin@mra-portal.dev | mirosas@uc.cl
# Last mod date: 09/11/23

# Notes------------------------------------------------------------------------

# Please uncomment lines 20 & 21 for the first execution unless you know for a fact that the packages `rtoolbox` (dev package), `remotes` and `xfun` are installed. I recommend re-commenting them after the first execution in order to avoid errors or warnings.

# Please remember to replace your working directory in line 54 and your file name in line 84.

# In line 66, file `frecuencia_formas_ortograficas.txt` comes from the Real Academia Española's CORPES XXI resource, specifically the list of total ortographic forms frequency.

# Libs-------------------------------------------------------------------------

# install.packages(c('xfun', 'remotes'))
# remotes::install_github('mrosas47/rtoolbox')

suppressWarnings({
  
  xfun::pkg_attach(c('dplyr', 'stringr', 'pdftools', 'cli', 'tm', 'tidytext', 'tidyr', 'scraEP', 'rtoolbox', 'knitr', 'qdapDictionaries', 'tools'), message = F, install = T)
  
})

# Functions--------------------------------------------------------------------

context <- function(body, word, n) {
  
  context_list <- str_extract_all(
    as.character(body),
    paste(
      '.{0,',
      as.character(n),
      '}',
      as.character(word),
      '.{0,',
      as.character(n),
      '}',
      sep = ''
    )
  )
  
  return(context_list)
  
}

# Defs-------------------------------------------------------------------------

# Ensure working directory
wdEnsure('N:/work/OCUC/nlp')
start_time <- rtime()

# Keywords
key <- unaccent(c('verticalización', 'densificación', 'densidad', 'renovación', 'crecimiento', 'vertical', 'verticalidad', 'verticalizar', 'crecer', 'densificar', 'renovar', 'altura', 'constructibilidad', 'edificabilidad', 'volumen', 'crecimiento'))
# key <- 'densidad'

# Problematic symbols
problems <- c('‘', '’', '“', '”', '„', '°')

# Spanish language handling
cli_alert(str_glue('{rtime()} -- Making Spanish language word list'))

sp_file <- read.delim('assets/frecuencia_formas_ortograficas_1_0.txt')
sp_clean <- str_replace_all(tolower(unaccent(sp_file$Forma)), '[^[:alnum:]]', '')

for (x in problems) {

    sp_clean <- str_replace_all(sp_clean, x, '')

}

# Language dictionaries (list of all words)
sp <- as.vector(unique(sp_clean))[is.na(as.numeric(as.vector(unique(sp_clean))))]
en <- GradyAugmented

# Main-------------------------------------------------------------------------

# Loop over origin
for (sd in list.dirs('data', recursive = F)[1]) {
  
  # Loop over subdirs
  for (ssd in list.dirs(sd, recursive = F)[1]) {
    
    # Loop over files
      for (f in list.files(ssd, full.names = T, include.dirs = F)) {
      
        fileName <- basename(f)
        cli_h1(fileName)
        clean_name <- file_path_sans_ext(fileName)
        
        cli_alert(str_glue('{rtime()} -- Reading PDF...'))
        
        suppressMessages({
          
          testfile <- pdf_text(str_glue('{f}'))
          
          
        })
        
        cli_alert_success(str_glue('{rtime()} -- Done!'))
        
        # Cleaning-------------------------------------------------------------
        
        cli_h2(str_glue('{rtime()} -- Cleaning'))
        
        # Remove problematic symbols
        cli_alert(str_glue('{rtime()} -- Removing problematic symbols'))
        
        for (x in problems) {
          
          testfile <- str_replace_all(testfile, x, '')
          
        }
        
        cli_alert(str_glue('{rtime()} -- To Corpus object'))
        # To Corpus object
        corp <- Corpus(VectorSource(testfile))
        
        cli_alert(str_glue('{rtime()} -- Cleaning Corpus'))
        
        suppressWarnings({
          
          # To lowercase
          corp_lo <- tm_map(corp, content_transformer(tolower))
          # Remove punctuation
          corp_punct <- tm_map(corp_lo, removePunctuation)
          # Remove numbers
          corp_num <- tm_map(corp_punct, removeNumbers)
          # Remove stopwords
          corp_stop <- tm_map(corp_num, removeWords, stopwords('spanish'))
          
        })
        
        # Concept frequency----------------------------------------------------
        
        cli_h2(str_glue('{rtime()} -- Concept frequency'))
        
        # As TermDocumentMatrix
        tdm <- TermDocumentMatrix(corp_stop)
        # As matrix
        m <- as.matrix(tdm)
        # Get word frequency
        word_freq <- sort(rowSums(m), decreasing = T)
        
        # Relevant terms
        relevant <- word_freq[unaccent(names(word_freq)) %in% key]
        relevant <- as.data.frame(relevant) |> rename('frequency' = relevant)
        stringy_relevants <- ''
        
        for (i in 1:nrow(relevant)) {
          
          stringy_relevants <- paste('  ', stringy_relevants, ' \n ', '"', row.names(relevant)[i], '"', ' appears ', relevant[i, ], ' times', sep = '')
          
        }
        
        cli_alert_info(str_glue('In {fileName}, the following relevant terms were found: {stringy_relevants}'))
        
        print(kable(relevant, caption = str_glue('Relevant terms for {fileName}')))
        
        # N-grams--------------------------------------------------------------
        
        cli_h2(str_glue('{rtime()} -- N-grams'))
        
        # Corpus to df
        text_df <- data.frame(line = 1:length(corp_stop), text = sapply(corp_stop, as.character))
        
        # Tokenize in bigrams
        bigrams <- text_df |> unnest_tokens(bigram, text, token = 'ngrams', n = 2)
        
        suppressWarnings({
          
          # Bigram df
          bigrams_df <- bigrams |> 
            mutate(
              clean_bigrams = unaccent(bigram)
            ) |> 
            get_rid('bigram') |> 
            separate(clean_bigrams, c('word1', 'word2', sep = ' ')) |> 
            get_rid(' ') |> 
            filter(word1 != word2)
          
        })
        
        cli_alert(str_glue('{rtime()} -- Counting and filtering'))
        
        # Count bigrams
        total_bigrams <- nrow(bigrams_df)
        unique_word_count <- count(bigrams_df, word1)
        bigram_count <- count(bigrams_df, word1, word2, sort = T)
        
        # Get bigrams with key words
        key_bigrams <- bigram_count |> 
          filter(word1 %in% key | word2 %in% key) |> 
          arrange(desc(n))
        
        cli_alert_info(str_glue('{rtime()} -- {fileName} contains {nrow(key_bigrams)} relevant bigrams, with "{key_bigrams$word1[1]} {key_bigrams$word2[1]}" (n = {key_bigrams$n[1]}) being the most frequent'))
        
        print(kable(head(key_bigrams, n = 15), caption = str_glue('Relevant bigrams for {fileName}')))
        
        # PMI------------------------------------------------------------------
        
        cli_h2(str_glue('{rtime()} -- Pointwise Mutual Information'))
        
        # Calculate PMI
        pmi_df <- left_join(bigram_count, unique_word_count, by = c('word1' = 'word1'))
        pmi_df <- left_join(pmi_df, unique_word_count, by = c('word2' = 'word1')) |> 
          rename(
            n_bigram = n.x, 
            n_w1 = n.y, 
            n_w2 = n
          ) |> 
          na.omit() |> 
          mutate(
            p_bigram = n_bigram / total_bigrams,
            p_w1 = n_w1 / total_bigrams,
            p_w2 = n_w2 / total_bigrams,
            pmi = log2(p_bigram / (p_w1 * p_w2))
          )
        
        # look for keywords
        relevant_pmi <- pmi_df |> 
          filter(word1 %in% key | word2 %in% key) |> 
          arrange(-pmi)
        
        print(kable(head(relevant_pmi, n = 10), caption = str_glue("Highest PMI scores for {fileName}")))
        
        # Operationalization of context----------------------------------------
        
        cli_h2(str_glue('{rtime()} -- Operationalization of context'))
        
        # Full text content as a single string
        full_text <- unaccent(paste(unlist(sapply(corp_stop, as.character)), collapse = ' '))
        
        # Make out directory
        dir.create(str_glue('temp/by_word/{clean_name}'), showWarnings = F)
        
        for (w in unaccent(row.names(relevant))) {
          
          # Get context for word `w`
          ctx <- context(full_text, w, 50)
          
          suppressWarnings({
            
            all_words <- unlist(str_split(ctx, ' '))
            
          })
          
          valid_words <- str_replace_all(tolower(unaccent(all_words)), '[^[:alnum:]]', '')
          valid_words <- valid_words[valid_words %notin% c(w, '', 'c')]
          
          # Check if real words
          real_words <- valid_words[valid_words %in% sp | valid_words %in% en]
          
          # Write keyword-specific tempfiles
          write.table(real_words, str_glue('temp/by_word/{clean_name}/{w}.txt'), row.names = F, col.names = F)
          
          cli_alert_info(str_glue('{rtime()} -- Context for "{w}" in {clean_name} written to file'))
          
          # Unique word count
          unique_word_sum <- as.data.frame(table(real_words))
          
        }
        
    }
    
  }
  
}

cli_alert_success(str_glue('{rtime()} -- All done mate!'))
