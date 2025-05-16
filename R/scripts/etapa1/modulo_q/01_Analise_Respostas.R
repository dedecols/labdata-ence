#### Pacotes usados ####
library(PNSIBGE)
library(sidrar)
library(geobr)
library(dplyr)
library(ggiraph)
library(tidyr)
library(janitor)
library(ggplot2)
library(sf)
library(purrr)
library(htmltools)
library(htmlwidgets)



#### Download PNS 2019 ####

savedir <- "C:/Users/a_les/Documents/ENCE/2025.1/LABDATA/"

# Dados da PNS 2019 via PNSIBGE package
# pns_2019 <- PNSIBGE::get_pns(year = 2019, labels = TRUE, savedir = savedir)
# saveRDS(pns_2019, file = paste0(savedir,'pns_2019.rds'))

# Arquivo completo dos dados baixados com o pacote PNSIBGE
desenho_pns <- readRDS(file = paste0(savedir,'pns_2019.rds'))
class(desenho_pns)



#### Informações da PNS 2019 ####

# Dados das respostas obtidas na PNS
dados_pns <- desenho_pns$variables
class(dados_pns)

# Respostas por cada Estado (UF)
unidade_federacao <- dados_pns %>% 
  count(V0001, .drop = FALSE) %>% 
  rename(unidade_federacao = V0001, respostas = n) %>% 
  arrange(desc(respostas)) %>% 
  bind_rows(tibble(unidade_federacao = 'TOTAL',
                  respostas = sum(.$respostas)))

# Respostas do Estado do RJ
dados_rj <- dados_pns %>% filter(V0001 == "Rio de Janeiro")
class(dados_rj)

# Agregação dos dados do RJ por situação censitária e diferentes áreas
situacao_censitaria <- dados_rj %>%
  count(V0026, .drop = FALSE) %>%
  rename(situacao_censitaria = V0026, respostas = n) %>%
  arrange(desc(respostas)) %>%
  bind_rows(tibble(situacao_censitaria = "Total",
                   respostas = sum(.$respostas)))

areas <- dados_rj %>%
  count(V0031, .drop = FALSE) %>% 
  rename(estrato_geografico = V0031, respostas = n) %>% 
  arrange(desc(respostas)) %>%
  bind_rows(tibble(estrato_geografico = "Total", 
                   respostas = sum(.$respostas))) 

# Variáveis de identificação e controle
variaveis_identificacao <- c("V0024",     # Variável de estrato
                             "UPA_PNS",   # Variável de cluster
                             "V0006_PNS", # Nº de ordem do domicílio na PNS
                             "V0015",     # Tipo de entrevista
                             "V0020",     # Ano de referência
                             "V0022",     # Total de moradores no domicílio
                             "V00283")

# Módulo Q - doenças crônicas
variaveis_interesse <- c("Q00201", # Hipertensão arterial
                         "Q03001", # Diabetes
                         "Q060",   # Colesterol alto
                         "Q06306", # Infarto e doenças do coração
                         "Q068"    # AVC (acidente vascular cerebral)
)

# # Agregação 1 - por situação censitária (V0026)
# levels(dados_rj$V0026)
# rj_urbano <- dados_rj %>% 
#   rename(situacao_censitaria = V0026) %>% 
#   filter(situacao_censitaria == 'Urbano') %>% 
#   select(situacao_censitaria, all_of(variaveis_identificacao), all_of(variaveis_interesse))
# 
# rj_rural <- dados_rj %>% 
#   rename(situacao_censitaria = V0026) %>% 
#   filter(situacao_censitaria == 'Rural') %>% 
#   select(situacao_censitaria, all_of(variaveis_identificacao), all_of(variaveis_interesse))

# # Agregação 2 - por tipos de áreas (V0031)
# levels(dados_rj$V0031)
# rj_capital <- dados_rj %>% 
#   rename(areas = V0031) %>% 
#   filter(areas == 'Capital') %>% 
#   select(areas, all_of(variaveis_identificacao), all_of(variaveis_interesse))
# 
# rj_rm <- dados_rj %>% 
#   rename(areas = V0031) %>% 
#   filter(areas == 'Resto da RM (Região Metropolitana, excluindo a capital)') %>% 
#   select(areas, all_of(variaveis_identificacao), all_of(variaveis_interesse))
# 
# rj_resto <- dados_rj %>% 
#   rename(areas = V0031) %>% 
#   filter(areas == 'Resto da UF (Unidade da Federação, excluindo a região metropolitana e RIDE)') %>% 
#   select(areas, all_of(variaveis_identificacao), all_of(variaveis_interesse))



#### Mapa 1 - Respondentes por UF ####

# Mapa com a quantidade de respostas em cada UF
savedir <- "C:/Users/a_les/Documents/ENCE/2025.1/LABDATA/Mapas/"

# 1. Dados geoespaciais com os nomes corrigidos (pacote geobr)
estados_geobr <- geobr::read_state(
  year = 2020,
  simplified = TRUE
) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 1000) %>% 
  mutate(
    centroide = st_centroid(geom),
    name_state = case_when(
      name_state == "Amazônas" ~ "Amazonas",
      name_state == "Rio Grande Do Norte" ~ "Rio Grande do Norte",
      name_state == "Rio De Janeiro" ~ "Rio de Janeiro",
      name_state == "Rio Grande Do Sul" ~ "Rio Grande do Sul",
      name_state == "Mato Grosso Do Sul" ~ "Mato Grosso do Sul",
      TRUE ~ name_state
    )
  ) 

# 2. Dados da PNS 2019 (pacote PNSIBGE)
estados_pns19 <- dados_pns %>% 
  count(V0001, .drop = FALSE) %>% 
  rename(unidade_federacao = V0001, respostas = n) %>% 
  arrange(desc(respostas)) %>% 
  mutate(unidade_federacao = as.character(unidade_federacao))

# 3. Dados do Censo 2022 - Tabela 1209
# info_sidra(1209)
tabela_populacao <- sidrar::get_sidra(
  x = 1209,                          # Número da tabela
  geo = "State",                     # Nível territorial: Estados
  geo.filter = NULL,                 # Todos os estados (sem filtro)
  variable = 606,                    # Código da variável (população total)
  period = "2022",                   # Ano
  classific = "c58",                 # Classificação por idade
  category = list(0),                # Grupo "0" = Total
  format = 3                         
) %>% 
  dplyr::select('Unidade da Federação',population = 'Valor') %>% 
  janitor::clean_names()

# 4. geobr + PNSIBGE + sidrar
estados_completo <- estados_geobr %>% 
  left_join(estados_pns19, by = c("name_state" = "unidade_federacao")) %>% 
  left_join(tabela_populacao, by = c("name_state" = "unidade_da_federacao"))

# 5. Configuração do hover
hover_elements <- paste0(
  # Container principal
  "<div style='",
  "font-family: \"Source Sans Pro\", sans-serif;",
  "padding: 8px;",
  "background: white;",
  "border-radius: 4px;",
  "box-shadow: 2px 2px 5px rgba(0,0,0,0.2);",
  "max-width: 400px;",
  "'>",
  
  # Título: Nome do estado
  "<b style='",
  "font-size: 32px;",
  "display: block;",
  "margin-bottom: 6px;",
  "color: #333;",
  "'>", estados_completo$name_state, "</b>",
  
  # Linha 1: População
  "<span style='",
  "font-size: 20px;",
  "color: #555;",
  "display: block;",
  "margin-bottom: 3px;",
  "'>",
  "<b>População:</b> ", format(
    estados_completo$population, 
    big.mark = ".", 
    decimal.mark = ",", 
    scientific = FALSE, 
    nsmall = 0 
  ),
  "</span>",
  
  # Linha 2: Respondentes
  "<span style='",
  "font-size: 20px;",
  "color: #555;",
  "display: block;",
  "'>",
  "<b>Respondentes:</b> ", format(
    estados_completo$respostas, 
    big.mark = ".", 
    decimal.mark = ",", 
    scientific = FALSE, 
    nsmall = 0
  ),
  "</span>",
  
  "</div>" # Fechamento do container
)

# 6. Incluir informação do hover nos dados
estados_completo <- estados_completo %>% 
  mutate(tooltip = hover_elements)

# 7. Construção do mapa
gg_mapa <- ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(
    data = estados_completo,
    ggplot2::aes(tooltip = tooltip, data_id = abbrev_state),
    fill = "#F5F5F5",
    color = "gray",
    size = 0.2  # Bordas muito finas
  ) +
  ggplot2::geom_sf_text(
    data = estados,
    ggplot2::aes(geometry = centroide, label = abbrev_state),
    color = "#333333",
    size = 2,  # Texto menor
    fontface = "bold"
  ) +
  ggplot2::theme_void() +
  ggplot2::coord_sf(expand = FALSE, clip = "on")  # Maximiza espaço utilizado

# 8. Camada de interatividade
mapa_brasil <- ggiraph::girafe(
  ggobj = gg_mapa,
  width_svg = 5,   # Tamanho significativamente reduzido
  height_svg = 4,  # Proporção 5:4
  options = list(
    ggiraph::opts_sizing(rescale = TRUE, width = 0.7),  # 70% do container
    ggiraph::opts_hover(css = "fill:#D9D9D9;stroke:#999999;stroke-width:0.5px;",
                        reactive = TRUE),
    ggiraph::opts_tooltip(css = "padding:4px;font-size:12px;",  
                          delay_mouseout = 300)
  )
)

# 9. Salvar mapa do Brasil
htmlwidgets::saveWidget(
  mapa_brasil,
  file = paste0(savedir, "Mapa_Brasil.html"),
  libdir = "libs",
  selfcontained = TRUE,
  title = "Respondentes da PNS 2019 por UF",
  knitrOptions = list(out.width = "70%", out.height = "auto")
)



#### Mapa 2 - Respondentes no RJ ####

# Verificar se existem dados espaciais para situação censitária e para área (rm,ride etc)


























