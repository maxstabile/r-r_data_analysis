

library(stringr)
library(dplyr)




fssr_noticias_lemmatizado <- read_delim(
  "data/press/fssr_noticias_lemmatizado.csv",
  delim = ";",
  escape_double = FALSE,
  locale = locale(encoding = "WINDOWS-1252"),
  trim_ws = TRUE
)

#fazendo a limpeza de notícias quem nao tenham conteúdo
fssr_noticias_lemmatizado <- fssr_noticias_lemmatizado[!is.na(fssr_noticias_lemmatizado$texto_lemmatizado_limpo), ]
fssr_noticias_lemmatizado<- fssr_noticias_lemmatizado %>% dplyr::filter(texto_lemmatizado_limpo != "nan")


#pequeno ajuste
fssr_noticias_lemmatizado$veiculo <- gsub("Folha de S. Paulo", "Folha de S.Paulo", fssr_noticias_lemmatizado$veiculo)



# Função para dividir o texto em segmentos
split_segments <- function(texto, segment_size = 40) {
  palavras <- unlist(str_split(texto, "\\s+"))
  segmentos <- split(palavras, ceiling(seq_along(palavras) / segment_size))
  segmentos <- lapply(segmentos, paste, collapse = " ")
  return(segmentos)
}

# Aplicar a divisão e levar o dado da coluna wave para cada segmento
segmentos_df <- fssr_noticias_lemmatizado %>%
  rowwise() %>%
  mutate(segmentos = list(split_segments(texto, segment_size = 40))) %>%
  unnest(segmentos) %>%
  ungroup()

# Calcular as estatísticas de presença por segmento e wave
estatisticas_por_segmento <- segmentos_df %>%
  mutate(
    reducao_gastos = str_count(segmentos, "(redução.*gastos|gastos.*redução)"),
    aumento_investimentos = str_count(segmentos, "(aumento.*investimentos|investimentos.*aumento)"),
  ) %>%
  group_by(wave) %>%
  summarise(
    total_reducao_gastos = sum(reducao_gastos, na.rm = TRUE),
    total_aumento_investimentos = sum(aumento_investimentos, na.rm = TRUE),
    .groups = 'drop'
  )

# Visualizar o resultado
estatisticas_por_segmento

# Calcular a soma total de todas as ocorrências em todos os segmentos
soma_total_estatisticas <- estatisticas_por_segmento %>%
  summarise(
    total_reducao_gastos = sum(total_reducao_gastos, na.rm = TRUE),
    total_aumento_investimentos = sum(total_aumento_investimentos, na.rm = TRUE)

  )

# Visualizar a soma total
soma_total_estatisticas

    