install.packages(c("readxl", "readr", "dplyr", "tidyr", "stringr", "lubridate"))

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Mapea os xlsx para a conversão
arquivos <- list.files(pattern = "\\.xlsx$")

processar_ssp <- function(caminho) {
  
  # Pega a cidade pelo nome do arquivo
  cidade_extraida <- str_extract(caminho, "(?<=Criminal\\)-)[^._]+")
  
  # Pega o ano pelo nome da pag do excel
  aba_nome <- excel_sheets(caminho)[1]
  ano_extraido <- str_extract(aba_nome, "\\d{4}")
  
  df <- read_excel(caminho, skip = 0) 
  colnames(df)[1] <- "Natureza"
  
  # tira a coluna com o total
  df <- df %>% select(-matches("Total|TOTAL"))
  
  # Transposição
  df_longo <- df %>%
    pivot_longer(cols = -Natureza, names_to = "Mes_Nome", values_to = "Qtd") %>%
    filter(!is.na(Qtd), !Natureza %in% c("Total", "TOTAL", "Total Geral")) %>%
    mutate(Natureza = str_squish(Natureza)) %>% 
    pivot_wider(names_from = Natureza, values_from = Qtd)
  
  # Meses para numero
  meses_map <- c(
    "janeiro"=1, "fevereiro"=2, "março"=3, "marco"=3, "abril"=4, "maio"=5, "junho"=6,
    "julho"=7, "agosto"=8, "setembro"=9, "outubro"=10, "novembro"=11, "dezembro"=12,
    "jan"=1, "fev"=2, "mar"=3, "abr"=4, "mai"=5, "jun"=6,
    "jul"=7, "ago"=8, "set"=9, "out"=10, "nov"=11, "dez"=12
  )
  
  df_final <- df_longo %>%
    mutate(
      Mes_Limpo = str_to_lower(str_squish(Mes_Nome)),
      Mes_Num = meses_map[Mes_Limpo],
      Data_Obj = make_date(year = as.numeric(ano_extraido), month = Mes_Num, day = 1),
      Data = format(Data_Obj, "%Y-%m-%d"),
      cidade = cidade_extraida
    ) %>%
    # Seleciona e organiza as colunas
    select(Data, cidade, everything(), -Data_Obj, -Mes_Nome, -Mes_Num, -Mes_Limpo)
  
  return(df_final)
}

# Executa todos os arquivos e junta
dataset_novo <- arquivos %>% 
  map_df(~processar_ssp(.x))

write_csv(dataset_novo, "dados.csv")

message("Processamento concluído! Verifique o arquivo 'dados.csv'")