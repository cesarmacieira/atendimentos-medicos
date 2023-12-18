####======================================
#### Trabalho Humberto - Análise de dados
####======================================
#As variáveis explicativas seriam: Idade1, EstadoCivil1, Unidade_Atendimento1*, Unidade_Usuário1**, 
#Sexo1, Num_Membros_Familia1, Energia_Eletrica1, Situacao_Moradia1, Abastecimento_Agua1, Consumo_Agua1, 
#Destino_Lixo1, Numero_Comodos1, QtdAtendimentos (médicos). 

#Rodar também substituindo essas variáveis por Regional_Atendimento1 e Regional_Usuario1 e por Regional_Atendimento1 e MicroArea_Usuario1. 
#Trata-se de variáveis colineares, e vamos usar a que apresentar melhor associação e significância estatística para o modelo. 

# Os desfechos seriam:
# 1. QtdCID
# 2. QtdMedicamentos
# 3. Qtd_ENCAMINHAMENTOS (total)
# 4. Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE
# 5. Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL
# 6. Qtd_ENCAMINHAMENTO_PARA_URGENCIA
# 7. Índice de risco que envolva QtdCID e QtdMedicamentos (normalizados)
# 8. Índice de risco que envolva QtdCID e Qtd_ENCAMINHAMENTOS (normalizados)
# 9. Índice de risco que envolva Qtd_ENCAMINHAMENTOS e QtdMedicamentos (normalizados)
# 10. Índice de risco que envolva QtdCID, QtdMedicamentos e Qtd_ENCAMINHAMENTOS (normalizados)

#Acredito que a Qtd_Atendimentos tenha colinearidade com a quantidade de medicamentos, CIDs e encaminhamentos, 
#porque evidentemente se o paciente é atendido mais vezes, tem mais diagnósticos, mais prescrições e mais chance de ser encaminhado. 
#Portanto, acredito que esse variável poderia ser rodada primeiro como explicativa em um modelo, depois como desfecho em outro modelo, 
#e talvez possa ser excluída completamente do modelo em razão da colinearidade.

####=============================
#### Preparando o R para análise
####=============================
rm(list=ls(all=T))#Limpar ambiente/histórico
setwd("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto")#Diretório

####=================================
#### Instalando e carregando pacotes
####=================================
if(!require(openxlsx)){ install.packages("openxlsx"); require(openxlsx)}#Ler e exportar excel
if(!require(purrr)){ install.packages("purrr"); require(purrr)}#Programação funcional
if(!require(tidyverse)){ install.packages("tidyverse"); require(tidyverse)}#Manipulação de dados
#if(!require(stringr)){ install.packages("stringr"); require(stringr)}#Strings
if(!require(ggplot2)){ install.packages("ggplot2"); require(ggplot2)}
if(!require(splitstackshape)){ install.packages("splitstackshape"); require(splitstackshape)}#Dividir o conjunto de dados em treino e teste
if(!require(randomForest)){ install.packages("randomForest"); require(randomForest)}

####=========
#### Funções
####=========
DescritivaCat = function(x){
  tabela = cbind(table(x), prop.table(table(x)))
  colnames(tabela) = c("Freq. Absoluta (N)", "Freq. Relativa (%)")
  return(tabela)
}

DescritivaNum = function(x, more = F) {
  stats = list();
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$Mín. = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Máx. = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c("N","Média","Variância","D.P.","Mínimo","1ºQ","2ºQ","3ºQ","Máximo")
  t1
}

basic.stats = function(x, more = F) {
  stats = list()
  clean.x = x[!is.na(x)]
  stats$N_validos = round(length(clean.x),3)
  stats$Média = round(mean(clean.x),3)
  stats$Var = round(var(clean.x),3)
  stats$D.P = round(sd(clean.x),3)
  stats$E.P = round(sd(clean.x)/sqrt(length(clean.x)),3)
  stats$Min = round(min(clean.x),3)
  stats$Q1 = round(fivenum(clean.x)[2],3)
  stats$Q2 = round(fivenum(clean.x)[3],3)
  stats$Q3 = round(fivenum(clean.x)[4],3)
  stats$Max = round(max(clean.x),3)
  t1 = unlist(stats)
  names(t1) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo")
  t1
}

QuiQuadrado_Fisher = function(x, y, type.sum, teste){
  t0 = table(x, y)
  if(type.sum==2) {
    t1 = prop.table(t0, 2)
  } else {
    t1 = prop.table(t0, 1)
  }
  colnames(t0) = paste0("X", 1:dim(t0)[2])
  colnames(t1) = paste0("X", 1:dim(t1)[2])
  t2_aux = cbind(t0, t1)
  t3 = t2_aux[, order(colnames(t2_aux))]
  colnames(t3) = c(rep(c("N", "%"), dim(t3)[2]/2))
  if(teste=="chisq") {
    Valor_p = chisq.test(t0)$p.value
  }
  if(teste=="fisher") {
    Valor_p = fisher.test(t0)$p.value
  } 
  if(teste=="chisq.simulate"){
    Valor_p = chisq.test(t0, simulate.p.value=TRUE, B=10000)$p.value
  }
  
  t4 = cbind(t3, Valor_p)
  return(t4)
}

KruskalTeste = function(y, z, more = F){
  tab = matrix(NA, length(levels(factor(z))), 10)
  for(i in 1:length(levels(factor(z)))){ 
    desc = tapply(y, factor(z),  basic.stats)[i]
    desc1 = unlist(desc)
    for(j in 1:10){ 
      tab[i,j] = desc1[j]
    }
  }
  p_valor = rep(kruskal.test(y~factor(z))$p.value, length(levels(factor(z))))
  tab = cbind(tab, p_valor)
  colnames(tab)= c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  rownames(tab)= levels(factor(z))
  
  if(!require(PMCMRplus)){ install.packages("PMCMRplus"); require(PMCMRplus) }
  #CM = posthoc.kruskal.nemenyi.test(y ~ factor(z), dist="Chisq")$p.value
  CM = kwAllPairsNemenyiTest(y ~ factor(z), dist="Chisquare")$p.value
  model=list(tabela=tab, C.Multiplas=CM)
  model
}

MannWhitney = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

WilcoxonDependente = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = wilcox.test(y ~ x, exact=FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

TesteTpareado = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = TRUE, alternative = "two.sided")$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo", "Valor-p")
  return(tab)
}

TesteT = function(y, x, more = F) {
  desc = t(data.frame(tapply(y, factor(x),  basic.stats)[1], tapply(y, factor(x),  basic.stats)[2]))
  p.value = t.test(y ~ x, exact = FALSE, paired = F)$p.value
  tab = data.frame(desc, p.value)
  colnames(tab) = c("N válidos", "Média", "Variância", "D.P.", "E.P.", "Mínimo", "1ºQ", "2ºQ", "3ºQ", "Máximo","Valor-p")
  return(tab)
}

TesteDeNormalidade = function(x){
  if(!require(dgof)){ install.packages("dgof"); require(dgof)}#Teste de Kolmogorov-Smirnov
  if(!require(nortest)){ install.packages("nortest"); require(nortest)}#Anderson-Darling
  AndersonDarling = round(ad.test(x)$p.value,3)
  KolmogorovSmirnov = round(ks.test(x, "pnorm", mean(x, na.rm = T), sd(x, na.rm = T))$p.value,3)
  Lilliefors = round(lillie.test(x)$p.value,3)
  CramerVonMises = round(cvm.test(x)$p.value,3)
  if(length(x) > 5000){
    ShapiroWilk = "N > 5000"
    ShapiroFrancia = "N > 5000"
  }else{
    ShapiroWilk = shapiro.test(x)$p.value
    ShapiroFrancia = sf.test(x)$p.value   
  }
  tabela = cbind(AndersonDarling,KolmogorovSmirnov,Lilliefors,CramerVonMises,
                 ShapiroWilk,ShapiroFrancia)
  colnames(tabela) = c('Anderson-Darling','Kolmogorov-Smirnov','Lilliefors','Cramer Von Mises','Shapiro-Wilk','Shapiro Francia')
  #row.names(tabela) = x
  return(tabela)
}

####=============================
#### Carregando o banco de dados 
####=============================
dados = read.xlsx("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Dados Humberto 10-11-2023.xlsx", sheet = 1)

####=====================
#### Tratamento de dados
####=====================
dados[dados == 'NULL'] = NA
dados[dados == '99) SEM INFORMAÇÃO'] = NA

dados$EstadoCivil_1 = ifelse(dados$EstadoCivil1 == '1) CONVIVE COM COMPANHEIRA (O) E FILHO (S)', 1, 
                             ifelse(is.na(dados$EstadoCivil1), NA, 0))
dados$EstadoCivil_2 = ifelse(dados$EstadoCivil1 == '2) CONVIVE COM COMPANHEIRA(O), COM LACOS CONJUGAIS E SEM FILHOS', 1, 
                             ifelse(is.na(dados$EstadoCivil1), NA, 0))
dados$EstadoCivil_3 = ifelse(dados$EstadoCivil1 == '3) CONVIVE C/ COMPANHEIRA(O),C/ FILHO(S) E/OU OUTROS FAMILIARES', 1, 
                             ifelse(is.na(dados$EstadoCivil1), NA, 0))
dados$EstadoCivil_4 = ifelse(dados$EstadoCivil1 == '4) CONVIVE COM FAMILIA (RES), SEM COMPANHEIRA (O)', 1, 
                             ifelse(is.na(dados$EstadoCivil1), NA, 0))
dados$EstadoCivil_5 = ifelse(dados$EstadoCivil1 == '5) CONVIVE C/ OUTRA(S) PESSOA(S),S/LACOS CONSANG E/OU CONJUGAIS', 1, 
                             ifelse(is.na(dados$EstadoCivil1), NA, 0))
dados$EstadoCivil_6 = ifelse(dados$EstadoCivil1 == '6) VIVE SO', 1, 
                             ifelse(is.na(dados$EstadoCivil1), NA, 0))

dados$Sexo_F = ifelse(dados$Sexo1 == 'FEMININO', 1, ifelse(is.na(dados$Sexo1), NA, 0))

dados$Situacao_Moradia_ALUGADO = ifelse(dados$Situacao_Moradia1 == 'ALUGADO', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_ARRENDADO = ifelse(dados$Situacao_Moradia1 == 'ARRENDADO', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_CEDIDO = ifelse(dados$Situacao_Moradia1 == 'CEDIDO', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_FINANCIADO = ifelse(dados$Situacao_Moradia1 == 'FINANCIADO', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_OCUPACAO = ifelse(dados$Situacao_Moradia1 == 'OCUPAÇÃO', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_OUTRA = ifelse(dados$Situacao_Moradia1 == 'OUTRA', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_PROPRIO = ifelse(dados$Situacao_Moradia1 == 'PRÓPRIO', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))
dados$Situacao_Moradia_SITUACAO_DE_RUA = ifelse(dados$Situacao_Moradia1 == 'SITUAÇÃO DE RUA', 1, ifelse(is.na(dados$Situacao_Moradia1), NA, 0))

dados$Abastecimento_Agua_CISTERNA = ifelse(dados$Abastecimento_Agua1 == 'CISTERNA', 1, ifelse(is.na(dados$Abastecimento_Agua1), NA, 0))
dados$Abastecimento_Agua_OUTRO = ifelse(dados$Abastecimento_Agua1 == 'OUTRO', 1, ifelse(is.na(dados$Abastecimento_Agua1), NA, 0))
dados$Abastecimento_Agua_REDE_ENCANADA = ifelse(dados$Abastecimento_Agua1 == 'REDE ENCANADA ATÉ O DOMICÍLIO', 1, ifelse(is.na(dados$Abastecimento_Agua1), NA, 0))

dados$Consumo_Agua_CLORADA = ifelse(dados$Consumo_Agua1 == 'CLORADA', 1, ifelse(is.na(dados$Consumo_Agua1), NA, 0))
dados$Consumo_Agua_FERVIDA = ifelse(dados$Consumo_Agua1 == 'FERVIDA', 1, ifelse(is.na(dados$Consumo_Agua1), NA, 0))
dados$Consumo_Agua_FILTRADA = ifelse(dados$Consumo_Agua1 == 'FILTRADA', 1, ifelse(is.na(dados$Consumo_Agua1), NA, 0))
dados$Consumo_Agua_SEM_TRATAMENTO = ifelse(dados$Consumo_Agua1 == 'SEM TRATAMENTO', 1, ifelse(is.na(dados$Consumo_Agua1), NA, 0))

dados$Destino_Lixo_CEU_ABERTO = ifelse(dados$Destino_Lixo1 == 'CÉU ABERTO', 1, ifelse(is.na(dados$Destino_Lixo1), NA, 0))
dados$Destino_Lixo_COLETADO = ifelse(dados$Destino_Lixo1 == 'COLETADO', 1, ifelse(is.na(dados$Destino_Lixo1), NA, 0))
dados$Destino_Lixo_OUTRO = ifelse(dados$Destino_Lixo1 == 'OUTRO', 1, ifelse(is.na(dados$Destino_Lixo1), NA, 0))
dados$Destino_Lixo_QUEIMADO_ENTERRADO = ifelse(dados$Destino_Lixo1 == 'QUEIMADO / ENTERRADO', 1, ifelse(is.na(dados$Destino_Lixo1), NA, 0))

dados$Regional_Atendimento_BARREIRO = ifelse(dados$Regional_Atendimento1 == 'BARREIRO', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_CENTRO_SUL = ifelse(dados$Regional_Atendimento1 == 'CENTRO SUL', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_LESTE = ifelse(dados$Regional_Atendimento1 == 'LESTE', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_NORDESTE = ifelse(dados$Regional_Atendimento1 == 'NORDESTE', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_NOROESTE = ifelse(dados$Regional_Atendimento1 == 'NOROESTE', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_NORTE = ifelse(dados$Regional_Atendimento1 == 'NORTE', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_OESTE = ifelse(dados$Regional_Atendimento1 == 'OESTE', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_PAMPULHA = ifelse(dados$Regional_Atendimento1 == 'PAMPULHA', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))
dados$Regional_Atendimento_VENDA_NOVA = ifelse(dados$Regional_Atendimento1 == 'VENDA NOVA', 1, ifelse(is.na(dados$Regional_Atendimento1), NA, 0))

dados_modelos = dados %>% drop_na(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
                                  Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
                                  Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
                                  Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
                                  Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
                                  Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
                                  Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
                                  Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
                                  Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
                                  Regional_Atendimento_VENDA_NOVA)
dados %>% dim
dados_modelos %>% dim

# write.xlsx(dados_modelos, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/Dados para modelos.xlsx', 
#            rowNames = F)

####=====================================
#### Divisão dos dados em treino e teste
####=====================================
####========
#### QtdCID
####========
dados_modelos_QtdCID = dados_modelos %>% drop_na(QtdCID)
TesteDeNormalidade(dados_modelos_QtdCID$QtdCID)

set.seed(13)
split_dados_modelos_QtdCID = stratified(dados_modelos_QtdCID,
                                        c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                          "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                        keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdCID = split_dados_modelos_QtdCID$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdCID)

dados_teste_QtdCID = split_dados_modelos_QtdCID$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdCID)

# write.xlsx(dados_treino_QtdCID, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdCID.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdCID, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdCID.xlsx', 
#            rowNames = F)

# RandomForest1 = randomForest(formula = QtdCID ~ Idade1 + EstadoCivil_1 + EstadoCivil_2 + EstadoCivil_3 + EstadoCivil_4 + EstadoCivil_5 + EstadoCivil_6 + 
#                                Sexo_F + Num_Membros_Familia1 + Energia_Eletrica1 + Situacao_Moradia_ALUGADO + Situacao_Moradia_ARRENDADO + 
#                                Situacao_Moradia_CEDIDO + Situacao_Moradia_FINANCIADO + Situacao_Moradia_OCUPACAO + Situacao_Moradia_OUTRA + 
#                                Situacao_Moradia_PROPRIO + Situacao_Moradia_SITUACAO_DE_RUA + Abastecimento_Agua_CISTERNA + Abastecimento_Agua_OUTRO + 
#                                Consumo_Agua_CLORADA + Consumo_Agua_FERVIDA + Consumo_Agua_FILTRADA + Consumo_Agua_SEM_TRATAMENTO + 
#                                Destino_Lixo_CEU_ABERTO + Destino_Lixo_COLETADO + Destino_Lixo_OUTRO + Destino_Lixo_QUEIMADO_ENTERRADO + 
#                                Numero_Comodos1 + QtdAtendimentosMedicos + Regional_Atendimento_BARREIRO + Regional_Atendimento_CENTRO_SUL + 
#                                Regional_Atendimento_LESTE + Regional_Atendimento_NORDESTE + Regional_Atendimento_NOROESTE + 
#                                Regional_Atendimento_NORTE + Regional_Atendimento_OESTE + Regional_Atendimento_PAMPULHA + 
#                                Regional_Atendimento_VENDA_NOVA, data = dados_treino1, #ntree= 500,
#                              importance = TRUE, proximity = TRUE)

####=================
#### QtdMedicamentos
####=================
dados_modelos_QtdMedicamentos = dados_modelos %>% drop_na(QtdMedicamentos)
TesteDeNormalidade(dados_modelos_QtdMedicamentos$QtdMedicamentos)

set.seed(13)
split_dados_modelos_QtdMedicamentos = stratified(dados_modelos_QtdMedicamentos,
                                                 c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                   "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                 keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdMedicamentos = split_dados_modelos_QtdMedicamentos$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdMedicamentos)

dados_teste_QtdMedicamentos = split_dados_modelos_QtdMedicamentos$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdMedicamentos)

# write.xlsx(dados_treino_QtdMedicamentos, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdMedicamentos.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdMedicamentos, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdMedicamentos.xlsx', 
#            rowNames = F)

####=====================
#### Qtd_ENCAMINHAMENTOS
####=====================
dados_modelos_QtdENCAMINHAMENTOS = dados_modelos %>% drop_na(Qtd_ENCAMINHAMENTOS)
TesteDeNormalidade(dados_modelos_QtdENCAMINHAMENTOS$Qtd_ENCAMINHAMENTOS)

set.seed(13)
split_dados_modelos_QtdENCAMINHAMENTOS = stratified(dados_modelos_QtdENCAMINHAMENTOS,
                                                    c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                      "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                    keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdENCAMINHAMENTOS = split_dados_modelos_QtdENCAMINHAMENTOS$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTOS)

dados_teste_QtdENCAMINHAMENTOS = split_dados_modelos_QtdENCAMINHAMENTOS$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTOS)

# write.xlsx(dados_treino_QtdENCAMINHAMENTOS, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdENCAMINHAMENTOS.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdENCAMINHAMENTOS, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdENCAMINHAMENTOS.xlsx', 
#            rowNames = F)

####=======================================
#### Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE
####=======================================
dados_modelos_Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE = dados_modelos %>% drop_na(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE)
TesteDeNormalidade(dados_modelos_Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE)

set.seed(13)
split_dados_modelos_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE = stratified(dados_modelos_Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE,
                                                                      c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                                        "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                                      keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE = split_dados_modelos_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE)

dados_teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE = split_dados_modelos_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE)

# write.xlsx(dados_treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdENCAMINHAMENTO_PARA_ESPECIALIDADE.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdENCAMINHAMENTO_PARA_ESPECIALIDADE.xlsx', 
#            rowNames = F)

####======================================
#### Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL
####======================================
dados_modelos_Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL = dados_modelos %>% drop_na(Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL)
TesteDeNormalidade(dados_modelos_Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL$Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL)

set.seed(13)
split_dados_modelos_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL = stratified(dados_modelos_Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL,
                                                                     c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                                       "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                                     keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL = split_dados_modelos_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL)

dados_teste_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL = split_dados_modelos_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL)

# write.xlsx(dados_treino_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL.xlsx', 
#            rowNames = F)

####==================================
#### Qtd_ENCAMINHAMENTO_PARA_URGENCIA
####==================================
dados_modelos_Qtd_ENCAMINHAMENTO_PARA_URGENCIA = dados_modelos %>% drop_na(Qtd_ENCAMINHAMENTO_PARA_URGENCIA)
TesteDeNormalidade(dados_modelos_Qtd_ENCAMINHAMENTO_PARA_URGENCIA$Qtd_ENCAMINHAMENTO_PARA_URGENCIA)

set.seed(13)
split_dados_modelos_QtdENCAMINHAMENTO_PARA_URGENCIA = stratified(dados_modelos_Qtd_ENCAMINHAMENTO_PARA_URGENCIA,
                                                                 c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                                   "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                                 keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdENCAMINHAMENTO_PARA_URGENCIA = split_dados_modelos_QtdENCAMINHAMENTO_PARA_URGENCIA$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTO_PARA_URGENCIA)

dados_teste_QtdENCAMINHAMENTO_PARA_URGENCIA = split_dados_modelos_QtdENCAMINHAMENTO_PARA_URGENCIA$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,Qtd_ENCAMINHAMENTO_PARA_URGENCIA)

# write.xlsx(dados_treino_QtdENCAMINHAMENTO_PARA_URGENCIA, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdENCAMINHAMENTO_PARA_URGENCIA.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdENCAMINHAMENTO_PARA_URGENCIA, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdENCAMINHAMENTO_PARA_URGENCIA.xlsx', 
#            rowNames = F)

####=====================================================================
#### Índice de risco que envolva QtdCID e QtdMedicamentos (normalizados)
####=====================================================================
dados_modelos_QtdCID_QtdMedicamentos = dados_modelos %>% drop_na(QtdCID,QtdMedicamentos)
TesteDeNormalidade(dados_modelos_QtdCID_QtdMedicamentos$QtdMedicamentos)

set.seed(13)
split_dados_modelos_QtdCID_QtdMedicamentos = stratified(dados_modelos_QtdCID_QtdMedicamentos,
                                                        c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                          "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                        keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdCID_QtdMedicamentos = split_dados_modelos_QtdCID_QtdMedicamentos$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdCID,QtdMedicamentos)

dados_teste_QtdCID_QtdMedicamentos = split_dados_modelos_QtdCID_QtdMedicamentos$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdCID,QtdMedicamentos)

# write.xlsx(dados_treino_QtdCID_QtdMedicamentos, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdCID QtdMedicamentos.xlsx', 
#            rowNames = F)
# write.xlsx(dados_teste_QtdCID_QtdMedicamentos, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdCID QtdMedicamentos.xlsx', 
#            rowNames = F)

####=========================================================================
#### Índice de risco que envolva QtdCID e Qtd_ENCAMINHAMENTOS (normalizados)
####=========================================================================
dados_modelos_QtdCID_QtdENCAMINHAMENTOS = dados_modelos %>% drop_na(QtdCID,Qtd_ENCAMINHAMENTOS)
TesteDeNormalidade(dados_modelos_QtdCID_QtdENCAMINHAMENTOS$Qtd_ENCAMINHAMENTOS)

set.seed(13)
split_dados_modelos_QtdCID_QtdENCAMINHAMENTOS = stratified(dados_modelos_QtdCID_QtdENCAMINHAMENTOS,
                                                           c("EstadoCivil1","Sexo1","Situacao_Moradia1","Abastecimento_Agua1",
                                                             "Consumo_Agua1","Destino_Lixo1","Regional_Atendimento1"), 0.75, 
                                                           keep.rownames = TRUE, bothSets = TRUE)

dados_treino_QtdCID_QtdENCAMINHAMENTOS = split_dados_modelos_QtdCID_QtdENCAMINHAMENTOS$SAMP1 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdCID,Qtd_ENCAMINHAMENTOS)

dados_teste_QtdCID_QtdENCAMINHAMENTOS = split_dados_modelos_QtdCID_QtdENCAMINHAMENTOS$SAMP2 %>% 
  select(Idade1,EstadoCivil_1,EstadoCivil_2,EstadoCivil_3,EstadoCivil_4,EstadoCivil_5,EstadoCivil_6,
         Sexo_F,Num_Membros_Familia1,Energia_Eletrica1,Situacao_Moradia_ALUGADO,Situacao_Moradia_ARRENDADO,
         Situacao_Moradia_CEDIDO,Situacao_Moradia_FINANCIADO,Situacao_Moradia_OCUPACAO,Situacao_Moradia_OUTRA,
         Situacao_Moradia_PROPRIO,Situacao_Moradia_SITUACAO_DE_RUA,Abastecimento_Agua_CISTERNA,Abastecimento_Agua_OUTRO,
         Consumo_Agua_CLORADA,Consumo_Agua_FERVIDA,Consumo_Agua_FILTRADA,Consumo_Agua_SEM_TRATAMENTO,
         Destino_Lixo_CEU_ABERTO,Destino_Lixo_COLETADO,Destino_Lixo_OUTRO,Destino_Lixo_QUEIMADO_ENTERRADO,
         Numero_Comodos1,QtdAtendimentosMedicos,Regional_Atendimento_BARREIRO,Regional_Atendimento_CENTRO_SUL,
         Regional_Atendimento_LESTE,Regional_Atendimento_NORDESTE,Regional_Atendimento_NOROESTE,
         Regional_Atendimento_NORTE,Regional_Atendimento_OESTE,Regional_Atendimento_PAMPULHA,
         Regional_Atendimento_VENDA_NOVA,QtdCID,Qtd_ENCAMINHAMENTOS)

write.xlsx(dados_treino_QtdCID_QtdENCAMINHAMENTOS, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/treino QtdCID QtdENCAMINHAMENTOS.xlsx', 
           rowNames = F)
write.xlsx(dados_teste_QtdCID_QtdENCAMINHAMENTOS, 'C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto/Python/Arquivos/teste QtdCID QtdENCAMINHAMENTOS.xlsx', 
           rowNames = F)

# 8. Índice de risco que envolva QtdCID e Qtd_ENCAMINHAMENTOS (normalizados)
# 9. Índice de risco que envolva Qtd_ENCAMINHAMENTOS e QtdMedicamentos (normalizados)
# 10. Índice de risco que envolva QtdCID, QtdMedicamentos e Qtd_ENCAMINHAMENTOS (normalizados)

DescritivaCat(dados_modelos$QtdCID)
DescritivaCat(dados_modelos$QtdMedicamentos)

dados2$Q10_cat = factor(case_when(dados2$Q10_cat == 1 ~ 'Não tem renda',
                                  dados2$Q10_cat == 2 ~ 'Até 1 salário-mínimo',
                                  dados2$Q10_cat == 3 ~ 'Mais de 1 até 2 SM',
                                  dados2$Q10_cat == 4 ~ 'Mais de 2 a 3 SM',
                                  dados2$Q10_cat == 5 ~ 'Mais de 3 a 5 SM',
                                  dados2$Q10_cat == 6 ~ 'Mais de 5 a 10 SM',
                                  dados2$Q10_cat == 7 ~ 'Mais de 10 SM'), 
                        levels = c('Não tem renda','Até 1 salário-mínimo','Mais de 1 até 2 SM','Mais de 2 a 3 SM',
                                   'Mais de 3 a 5 SM','Mais de 5 a 10 SM','Mais de 10 SM'))

####=======================
#### Comparações por etapa
####=======================
Tabela1 = rbind(QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_1_cat,NaoResponsavel_emp$Etapa,'2','chisq.simulate'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_2_cat,NaoResponsavel_emp$Etapa,'2','chisq.simulate'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_3_cat,NaoResponsavel_emp$Etapa,'2','chisq.simulate'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_4_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_5_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_6_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_7_cat,NaoResponsavel_emp$Etapa,'2','chisq.simulate'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_8_cat,NaoResponsavel_emp$Etapa,'2','chisq.simulate'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_9_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_10_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_13_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                #QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_14_cat,NaoResponsavel_emp$Etapa,'2','chisq'),
                QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_15_cat,NaoResponsavel_emp$Etapa,'2','chisq'))
#QuiQuadrado_Fisher(NaoResponsavel_emp$Q14_16_cat,NaoResponsavel_emp$Etapa,'2','chisq'))
