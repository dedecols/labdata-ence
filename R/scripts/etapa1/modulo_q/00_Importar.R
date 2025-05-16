#### Carregar dados ####
library(PNSIBGE)
library(dplyr)

savedir <- "C:/Users/a_les/Documents/ENCE/2025.1/LABDATA/"

# Dados da PNS 2019 via PNSIBGE package
# pns_2019 <- PNSIBGE::get_pns(year = 2019, labels = TRUE, savedir = savedir)
# saveRDS(pns_2019, file = paste0(savedir,'pns_2019.rds'))

dados <- readRDS(file = paste0(savedir,'pns_2019.rds'))
class(dados)
dim(dados)
table(dados$variables$V0001)

#### Tratamento dos dados ####
dados_rj <- subset(dados, V0001 == "Rio de Janeiro")
class(dados_rj)
dim(dados_rj)



# Respostas para situação censitária na PNS
situacao_censitaria <- as.data.frame(table(dados_rj$variables$V0026))

# Renomeando as colunas
names(situacao_censitaria) <- c("situacao_censitaria", "respostas")

# Ordenando em ordem decrescente de respostas
situacao_censitaria <- situacao_censitaria[order(-situacao_censitaria$respostas), ]

# Adicionando a linha com o total
situacao_censitaria <- rbind(situacao_censitaria, 
                             data.frame(situacao_censitaria = "Total",
                                        respostas = sum(situacao_censitaria$respostas)))

print(situacao_censitaria)


# Estimativas do Survey para situação censitária
situacao_censitaria <- svytable(~V0026, dados_rj)



#### Geração de Estimativas ####


# antes de continuar é preciso entender a parte do peso amostral e rodar as bases com as variáveis importantes para usar o survey
# Estimativas para agrupamento 1 (situação censitária)

# Criar objeto de desenho amostral
dados_rj$V00283 <- as.numeric(dados_rj$V00283)
pns_design <- svydesign(id = ~UPA_PNS,       # Cluster primário (UPA_PNS)
                        strata = ~V0024,     # Estrato amostral (V0024)
                        weights = ~V00283,   # Peso amostral pós-estratificado
                        data = dados_rj, 
                        nest = TRUE)


 






