library(survey)


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


