library(tidyverse)
files <- list.files("data", pattern = "json")

map_dfr(files, function(json){
  js <- jsonlite::read_json(str_c("data/", json))
  
  map_dfr(seq_along(js$sentences), function(i){
    js$sentences[[i]]$words %>% 
      map("wf") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      word_forms
    
    js$sentences[[i]]$words %>% 
      map("ana") %>% 
      map(1) %>% 
      map("gloss") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      gloss
    
    js$sentences[[i]]$words %>% 
      map("ana") %>% 
      map(1) %>% 
      map("parts") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      morphonology
    
    js$sentences[[i]]$words %>% 
      map("ana") %>% 
      map(1) %>% 
      map("parts") %>% 
      modify_if(is.null, ~ NA) %>% 
      unlist() ->
      morphonology
    
    js$sentences[[i]]$src_alignment %>% 
      map_chr("src") ->
      source_file
    
    js$sentences[[i]]$src_alignment %>% 
      map_chr("off_start_src") ->
      time_start
    
    js$sentences[[i]]$src_alignment %>% 
      map_chr("off_end_src") ->
      time_end
    
    tibble(filename = source_file,
           time_start = time_start,
           time_end = time_end,
           speaker = js$sentences[[i]]$meta$speaker,
           recorded = js$meta$year,
           lang = js$sentences[[i]]$lang,
           text = js$sentences[[i]]$text,
           word_forms,
           morphonology,
           gloss,
           language = "bash1264",
           dataset_creator  = "Maria Ovsjannikova, Sergey Say, Ekaterina Aplonova, Anna Smetina",
           dataset_provider = "George Moroz") 
  }) 
}) ->
  result

result %>% 
  distinct(filename, recorded, lang, text) %>% 
  group_by(filename, recorded, lang) %>%
  mutate(sentence_id = 1:n()) %>% 
  pivot_wider(names_from = lang, values_from = text) %>% 
  rename(text = `0`,
         translation = `1`) ->
  translation_pairs

result %>% 
  filter(lang == 0,
         word_forms != "\n",
         word_forms != "") %>% 
  left_join(translation_pairs) %>%
  select(filename, time_start, time_end, speaker, recorded, sentence_id, text, translation, word_forms, morphonology, gloss, language, dataset_creator, dataset_provider, sentence_id, translation)  %>% 
  write_csv("data_oral_bashkir_corpus.csv")
