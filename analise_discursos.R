# -- CARREGANDO PACOTES E ABRINDO A BASE DE DADOS -----------------------------------


# Lista de pacotes necessários
pacotes <- c("tidyverse", "quanteda", "rainette", "readr", "dplyr", "ggplot2", 
             "scales", "gridExtra", "lubridate", "ggthemes", "stringi")

# Função para verificar e instalar pacotes
instalar_pacotes <- function(pacotes) {
  pacotes_ausentes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if(length(pacotes_ausentes)) install.packages(pacotes_ausentes)
}

# Chamar a função
instalar_pacotes(pacotes)

# Carregar os pacotes
lapply(pacotes, library, character.only = TRUE)


set.seed(100)



speech <- read_delim("data/discursos/data.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Data = col_date(format = "%d/%m/%Y")), 
                   trim_ws = TRUE)

# Carregar as bibliotecas necessárias
library(dplyr)
library(tidyr)
library(lubridate)

all_months <- tibble(mes_ano = seq(from = as.Date("2018-01-01"), 
                                   to = floor_date(Sys.Date(), "month"), 
                                   by = "month"))

# Filtrar os dados a partir de 2018 e contar os termos
speech_consolidated <- speech %>%
  filter(ano >= 2018) %>%
  mutate(mes_ano = make_date(ano, mês, 1)) %>%
  group_by(mes_ano) %>%
  summarise(
    pandemia_count = sum(grepl("pandemia", termo, ignore.case = TRUE), na.rm = TRUE),
    reforma_previdencia_count = sum(grepl("reforma da previdencia", termo, ignore.case = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

# Fazer um join com todos os meses, preenchendo os meses faltantes
speech_consolidated <- all_months %>%
  left_join(speech_consolidated, by = "mes_ano") %>%
  replace_na(list(pandemia_count = 0, reforma_previdencia_count = 0)) %>%
  mutate(ano = year(mes_ano), mês = month(mes_ano))



# Exibir o resultado consolidado
print(speech_consolidated)

