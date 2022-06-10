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

rtn_despesas_primarias<-
rtn_despesas %>%
  dplyr::filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"))


##Versão sem IPCA
rtn_dimensional<-
  rtn_despesas_primarias %>%
  group_by(ID_ANO, CATEGORIA_RTN,NO_FUNCAO_PT, ORGAO_DESCRICAO, NO_PROGRAMA_PT,NO_ACAO) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS)
  ) %>%
  ungroup()

saveRDS(rtn_dimensional, file = "rtn_dimensional.rds")

##Versão com IPCA

tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan")


URL_add <- tb_ckan$url

tmp <- tempfile(fileext = ".xlsx")

download.file(URL_add,mode = "wb", destfile = tmp, extra = "-R", method = "libcurl")

names_deflator_IPCA <- read_xlsx(tmp,sheet = 3,skip = 4,n_max = 1, col_names = TRUE)
deflator_IPCA <- read_xlsx(tmp,sheet = 3,skip = 74,n_max = 1, col_names = FALSE)

names(deflator_IPCA) <- names(names_deflator_IPCA)

names(deflator_IPCA)[1]<-"Rubrica"
series_temporais_analise_IPCA<-gather(deflator_IPCA,Data, Valor,-Rubrica)
#series_temporais_analise_IPCA$Data<-gsub("X","",series_temporais_analise_IPCA$Data)
series_temporais_analise_IPCA$Data<-as.Date(as.numeric(series_temporais_analise_IPCA$Data), origin="1899-12-30")
series_temporais_analise_IPCA$Valor <-as.numeric(series_temporais_analise_IPCA$Valor)


rtn_dimensional_ipca<-
  rtn_despesas_primarias %>%
  mutate(Data = as.Date(paste(ID_ANO, ID_MES,"01",sep="-")) ) %>%
  inner_join(series_temporais_analise_IPCA) %>%
  mutate(valor_deflacionado = DESPESAS_PAGAS * Valor) %>%
  group_by(ID_ANO, CATEGORIA_RTN,NO_FUNCAO_PT, ORGAO_DESCRICAO, NO_PROGRAMA_PT,NO_ACAO) %>%
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS),
    total_paga_ano_deflacionado = sum(valor_deflacionado)
  ) %>%
  ungroup()


saveRDS(rtn_dimensional_ipca, file = "rtn_dimensional_ipca.rds")


rtn_dimensional %>%
  filter(NO_FUNCAO_PT %in% "EDUCACAO",
        ID_ANO<max(rtn_despesas$ID_ANO))

saveRDS(rtn_despesas_primarias, file = "rtn_despesas_primarias.rds")


unique(rtn_despesas$PRIMARIA_FINANCEIRA)

unique

glimpse(rtn_despesas)


graph<-
rtn_despesas %>%
  dplyr::filter(PRIMARIA_FINANCEIRA %in% c("Primária","Financeira primária"),
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

print(graph$data)

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
