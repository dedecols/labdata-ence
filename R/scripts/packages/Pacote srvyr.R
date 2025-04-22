library(srvyr)
library(tidyverse)
library(survey)

# Converter o objeto survey.design para um tbl_svy do srvyr
dados_srvyr <- as_survey_design(dados)
class(dados_srvyr)

# Agora podemos usar filter normalmente
dados_rj_srvyr <- dados_srvyr %>% filter(V0001 == "Rio de Janeiro")
