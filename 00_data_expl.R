# Exploring visceral leishmaniasis data from SINAN
# Original dbf files downloaded from 
# https://datasus.saude.gov.br/transferencia-de-arquivos
# Fonte: SINAN - Sistema de Informacoes de Agravos de Notificacao
# Modalidade: Dados
# Tipo de Arquivo: LEIV - Leishmaniose Visceral
# Ano: 2000-2021 (2022 is available, but preliminary)

library(read.dbc)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)

#### leish

# before anything, let's try to mimic the numbers that appear in the open SINANWEB tables
# example from SINANWEB:
ex07 <- read_csv("./data/sinan/exemplo_sinanweb_2007.csv", col_types = c("cci")) %>% 
  arrange(codmun)

# read data for 2007
lv07 <- read.dbc("./data/sinan/final/LEIVBR07.dbc")

# check variables
names(lv07)
head(lv07)

# most relevant variables are:
# CLASSI_FIN: final case classification (1=confirmed, 2=discarded)
# ID_MN_RESI: municipality of residence code
# NU_ANO: year of notification

# check for NAs in relevant columns
table(is.na(lv07$CLASSI_FIN)) ###
table(is.na(lv07$ID_MN_RESI)) ###
table(is.na(lv07$NU_ANO))

test07 <- lv07 %>% 
  mutate(codmun = as.character(ID_MN_RESI)) %>% 
  drop_na(codmun) %>% 
  filter(CLASSI_FIN == 1) %>% 
  group_by(codmun, NU_ANO) %>% 
  summarise(cases = n()) %>% 
  arrange(codmun)

test07_notif <- lv07 %>% 
  mutate(codmun = as.character(ID_MUNICIP)) %>% 
  drop_na(codmun) %>% 
  filter(CLASSI_FIN == 1) %>% 
  group_by(codmun, NU_ANO) %>% 
  summarise(cases = n()) %>% 
  arrange(codmun)


# now see if number of cases are matching
identical(test07$cases, ex07$cases) # PASSED!


# now repeat the test for 2011 and 2001
ex11 <- read_csv("./data/sinan/exemplo_sinanweb_2011.csv", col_types = c("cci")) %>% 
  arrange(codmun)
ex01 <- read_csv("./data/sinan/exemplo_sinanweb_2001.csv", col_types = c("cci")) %>% 
  arrange(codmun)

lv11 <- read.dbc("./data/sinan/final/LEIVBR11.dbc")
lv01 <- read.dbc("./data/sinan/final/LEIVBR01.dbc")

test11 <- lv11 %>% 
  mutate(codmun = as.character(ID_MN_RESI)) %>% 
  drop_na(codmun) %>% 
  filter(CLASSI_FIN == 1) %>% 
  group_by(codmun, NU_ANO) %>% 
  summarise(cases = n()) %>% 
  arrange(codmun)

identical(test11$cases, ex11$cases) # PASSED!

# 2001 data has different column names, explore: ## CONTINUE FROM HERE

test01 <- lv01 %>% 
  mutate(codmun = str_sub(as.character(ID_MUNICIP), end = 6)) %>% # municipality code edit
  drop_na(codmun) %>% 
  filter(CON_CLASSI == 1, # only confirmed cases
         ID_PAIS == 800) %>% # only Brazilian residents
  group_by(codmun, NU_ANO) %>% 
  summarise(cases = n()) %>% 
  arrange(codmun)

# o municipio Normandia (140040) aparece no exemplo tirado do site (1 caso em 2001). Procurar por ele no dado bruto:
# no dado bruto os geocodigos tem 7 dígitos; o equivalente é 1400407
t <- lv01 %>% 
  filter(CON_INF_MU == 1718204)

matches <- apply(lv01, 2, function(x) any(x == 1400407))

head(lv01)
table(lv01$ID_PAIS)

t <- test01 %>% 
  filter(codmun == "140010")

head(sort(test01$codmun))

# check sums
sum(test01$cases) == sum(ex01$cases)

# quick visual inspection
head(test01)
head(ex01)

tail(test07)
tail(ex07)

head(lv01)



#### For now, merging files for 2007-2021

# read leish files for 2001-2006
filenames <- sprintf("./data/sinan/final/LEIVBR%02d.dbc", 1:6)

leish <- lapply(filenames, read.dbc)

vars <- lapply(leish, names)

# which ones have ID_MN_RESI?
munres_test <- unlist(lapply(leish, function(df) "ID_MN_RESI" %in% names(df)))

identical(vars[[1]], vars[[2]])
test <- lapply(vars, identical)

# read leish files for 2007-2021
filenames <- sprintf("./data/sinan/final/LEIVBR%02d.dbc", 7:21)

leish <- lapply(filenames, read.dbc)


# check if key variables are present
variables <- lapply(leish, function(df) c("CLASSI_FIN", "ID_MN_RESI", "NU_ANO") %in% names(df))

variables # PASSED!

# combining all years
df <- rbindlist(leish)

head(df)

# cleaning
clean_df <- df %>% 
  filter(CLASSI_FIN == 1) %>% 
  mutate(mun_res = as.character(ID_MN_RESI),
         year_notif = year(DT_NOTIFIC),
         mon_notif = month(DT_NOTIFIC)) %>% 
  drop_na(mun_res) %>% 
  group_by(mun_res, year_notif, mon_notif) %>% 
  summarise(cases = n()) %>% 
  arrange(year_notif, mon_notif, mun_res) 

# save clean df
write_csv(clean_df, "./data/clean_df.csv")


# checking time series for some municipalities
araguaina <- filter(clean_df, mun_res == 170210) %>% 
  mutate(mon_year = make_date(year = year_notif, month = mon_notif))
fortaleza <- filter(clean_df, mun_res == 230440) %>% 
  mutate(mon_year = make_date(year = year_notif, month = mon_notif)) 


ggplot(fortaleza) +
  geom_line(aes(x = mon_year, y = cases), size = 1.5) + 
  xlab("Time") +
  ylab("Number of cases") +
  labs(title = "Fortaleza - CE")

ggplot(araguaina) +
  geom_line(aes(x = mon_year, y = cases), size = 1.5) + 
  xlab("Time") +
  ylab("Number of cases") +
  labs(title = "Araguaina - TO")




#### population data

# function wrote by Dani, <https://earth.bsc.es/gitlab/dluhrsen/healthdatatraining>
get_br_pop_data <- function(year, outfile){
  pop <- read.csv(paste0("https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6579.csv&terr=NC&rank=-&query=t/6579/n6/all/v/all/p/",year,"/l/v,p,t"), header = F, sep = ",")
  names(pop) <- c("MUNI_code", "muni_name", "pop")
  pop <- pop[-(1:4),]
  pop <- pop[1:5570,]
  pop$MUNI_code <- str_sub(pop$MUNI_code, end = 6)
  pop$UF_code <- str_sub(pop$MUNI_code, end = 2)
  write_csv(pop, outfile) 
}



years <- seq(2011, 2021, 1)

for(year in years){
  get_br_pop_data(year, paste0("./data/ibge/Popdata_", year, ".csv"))
}


pop15 <- read_csv("./data/ibge/Popdata_2015.csv")
names(pop15)
head(pop15)

