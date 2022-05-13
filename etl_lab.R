library(ckanr)
library(purrr)
library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#Busca todos os dados do pacote com dados de despesas totais do governo central incluindo transferências
package<- ckanr::package_show(url = "https://www.tesourotransparente.gov.br/ckan",id="8675a0a4-31c5-4593-a24d-fb8e17376eca")


#Monta dataframe com os dados de cada um dos recursos de dados presentes no pacote
rtn_despesas<-
  purrr::map_dfr(1:length(package[["resources"]]),function(id_resource){
    print(id_resource)
    formato<-package[["resources"]][[id_resource]][["format"]]
    #Só faz download dos arquivos tipo xlsx
    if (str_to_lower(formato)=="xlsx"){
      url<- package[["resources"]][[id_resource]][["url"]]
      download.file(url = url, destfile = "arquivo_despesa.xlsx", mode = "wb")
      sheets<- readxl::excel_sheets("arquivo_despesa.xlsx")

      if (length(sheets)==2){
        sheet_num<- 1
      } else{
        sheet_num <- 2
      }

      readxl::read_xlsx("arquivo_despesa.xlsx", sheet = sheet_num)
    }
  })

unique(rtn_despesas$ID_ANO)


saveRDS(rtn_despesas, file = "rtn_despesas.rds")

unique(rtn_despesas$PRIMARIA_FINANCEIRA)

unique

glimpse(rtn_despesas)

rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         CATEGORIA_RTN %in% c("II.2.1 - Pessoal e Encargos Sociais - Ativo civil"),
         ID_ANO<2022) %>%
  group_by(ID_ANO,NO_FUNCAO_PT, CATEGORIA_RTN) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_line(aes(x=ID_ANO, y=total, color=CATEGORIA_RTN))+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  facet_wrap(NO_FUNCAO_PT~., scales = "free_y")

rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         ID_ANO<2022) %>%
  group_by(ID_ANO,NO_FUNCAO_PT) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_line(aes(x=ID_ANO, y=total))+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  facet_wrap(NO_FUNCAO_PT~., scales = "free_y")



rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         ID_ANO<2022,
         NO_FUNCAO_PT == "SAUDE") %>%
  group_by(ID_ANO,CATEGORIA_RTN) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_line(aes(x=ID_ANO, y=total))+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  facet_wrap(CATEGORIA_RTN~.)


rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         ID_ANO<2022,
         NO_FUNCAO_PT == "SAUDE") %>%
  group_by(ID_ANO,ORGAO_DESCRICAO,CATEGORIA_RTN) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_line(aes(x=ID_ANO, y=total))+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  facet_wrap(ORGAO_DESCRICAO+CATEGORIA_RTN~.)


rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         ID_ANO<2022,
         NO_FUNCAO_PT == "SAUDE") %>%
  group_by(ID_ANO,NO_PROGRAMA_PT,CATEGORIA_RTN) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_line(aes(x=ID_ANO, y=total))+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  facet_wrap(ORGAO_DESCRICAO+CATEGORIA_RTN~.)



unique(rtn_despesas$CATEGORIA_RTN)


rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         ID_ANO<2022,
         NO_FUNCAO_PT == "SAUDE") %>%
  group_by(ID_ANO,UNIDADE_ORCAMENTARIA_DESCRICAO,CATEGORIA_RTN) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_col(aes(x=total, y=UNIDADE_ORCAMENTARIA_DESCRICAO))


rtn_despesas %>%
  filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
         ID_ANO<2022,
         NO_FUNCAO_PT == "SAUDE") %>%
  group_by(ID_ANO,UNIDADE_ORCAMENTARIA_DESCRICAO,CATEGORIA_RTN) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  pivot_longer(cols = total_paga_ano, names_to = "estagio",  values_to = "total") %>%
  ggplot() +
  geom_line(aes(x=ID_ANO, y=total))+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  facet_wrap(CATEGORIA_RTN+UNIDADE_ORCAMENTARIA_DESCRICAO~.)
