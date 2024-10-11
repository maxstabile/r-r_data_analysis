
pacotes <- c("stringr", "dplyr", "readr")

# Função para verificar e instalar pacotes
instalar_pacotes <- function(pacotes) {
  pacotes_ausentes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if(length(pacotes_ausentes)) install.packages(pacotes_ausentes)
}

# Chamar a função
instalar_pacotes(pacotes)

# Carregar os pacotes
lapply(pacotes, library, character.only = TRUE)




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


#colocar a coluna

segmentos_df <- segmentos_df %>%
  mutate(
    reducao_gastos = str_count(segmentos, "(redução.*gastos|gastos.*redução)"),
    saude_educacao = str_count(segmentos, "(saúde.*educação|educação.*saúde)"),
    investimento_saude_educacao = str_count(segmentos, "(?i)investimento.*(saúde|educação)|(saúde|educação).*investimento"),
    aumento_investimentos = str_count(segmentos, "(aumento.*investimentos|investimentos.*aumento)"),
    reducao_deficit_gastos_despesas = str_count(segmentos, "(?i)redução.*(déficit|gastos|despesas)|(déficit|gastos|despesas).*redução")
    
  )


#exportando

segmentos_df$segmentos <- sapply(segmentos_df$segmentos, function(x) paste(x, collapse = " "))


segmentos_filtrados <- segmentos_df %>%
  filter(reducao_gastos > 0 | aumento_investimentos > 0 | saude_educacao > 0 | investimento_saude_educacao > 0)

# Agora você pode salvar o data.frame em um arquivo CSV
write.csv(segmentos_filtrados, "data/press/segmentos.csv", row.names = FALSE)


# Calcular as estatísticas de presença por segmento e wave
estatisticas_por_segmento <- segmentos_df %>%
  mutate(
    reducao_gastos = str_count(segmentos, "(redução.*gastos|gastos.*redução)"),
    saude_educacao = str_count(segmentos, "(saúde.*educação|educação.*saúde)"),
    investimento_saude_educacao = str_count(segmentos, "(?i)investimento.*(saúde|educação)|(saúde|educação).*investimento"),
    aumento_investimentos = str_count(segmentos, "(aumento.*investimentos|investimentos.*aumento)"),
    reducao_deficit_gastos_despesas = str_count(segmentos, "(?i)redução.*(déficit|gastos|despesas)|(déficit|gastos|despesas).*redução")
  ) %>%
  group_by(wave) %>%
  summarise(
    total_reducao_gastos = sum(reducao_gastos, na.rm = TRUE),
    total_aumento_investimentos = sum(aumento_investimentos, na.rm = TRUE),
    saude_educacao = sum(saude_educacao, na.rm = TRUE),
    investimento_saude_educacao = sum(investimento_saude_educacao, na.rm = TRUE),
    reducao_deficit_gastos_despesas = sum(reducao_deficit_gastos_despesas, na.rm = TRUE),
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

    