library(rperseus)
library(rvest)
library(tidyverse)
library(httr)
library(stringr)
library(xml2)

extract_text <- function(text_url) {
  
  res <- GET(text_url,
             httr::user_agent(
               "rperseus - https://github.com/daranzolin/rperseus")
  )
  if (res$status_code == 500) stop("Nothing available.",
                                   call. = FALSE)
  httr::stop_for_status(res)
  r_list <- res %>%
    content("raw") %>%
    read_xml() %>%
    as_list()
  
  text <- map(r_list$GetPassage$reply$passage$TEI$text$body$div,
              ~ paste(unlist(.), collapse = " ")) %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("\\*", "") %>%
    str_replace_all("/", "") %>%
    str_trim() %>%
    discard(~.=="")
  tibble::tibble(text = text)
}

entry_names <- read_html("http://cts.perseids.org/read/pdlrefwk/viaf88890045/003/perseus-eng1") %>%
  html_nodes(".col-md-1") %>%
  html_text() %>%
  as.character() %>%
  stringr::str_trim()

indices_list <- vector(mode = "list", length = length(LETTERS))
for (i in seq_along(LETTERS)) { 
  l <- keep(entry_names, ~substr(.x, 1, 1) == LETTERS[i])
  if (is.na(l[1])) next
  indices_list[[i]] <- paste(l[1], l[length(l)], sep = "-")
}

indices_list <- compact(indices_list)

BASE_URL <- "http://cts.perseids.org/api/cts/?request=GetPassage&urn=urn:cts:pdlrefwk:viaf88890045.003.perseus-eng1"

perseus_dictionary <- indices_list %>%
  map( ~ paste(BASE_URL, ., sep = ":")) %>%
  map_df(extract_text) %>%
  rowwise() %>% 
  mutate(entry_name = stringr::str_replace(stringr::str_split(entry, " ")[[1]][1], "'", "")) %>% 
  ungroup() %>% 
  select(entry_name, entry = text)

saveRDS(perseus_dictionary, "perseus_dictionary.rds")
