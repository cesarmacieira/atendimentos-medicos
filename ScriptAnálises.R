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
#setwd("C:/Users/User_/Desktop/Trabalhos/NESCON/Trabalho - Humberto")#Diretório
setwd('C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos')

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
if(!require(MLmetrics)){ install.packages("MLmetrics"); require(MLmetrics)}

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

TabelaModelo = function(modelo,casasdecimaisExpB=F){
  options(OutDec=",")
  if(casasdecimaisExpB == F){
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = exp(summary(modelo)$coefficients[,1]),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C. (Exp β)" = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),3),"; ",
                                                round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),3),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,2),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }else{
    Tabela = data.frame("Variáveis" = rownames(summary(modelo)$coefficients),
                        "β" = summary(modelo)$coefficients[,1],
                        "Exp β" = round(exp(summary(modelo)$coefficients[,1]),casasdecimaisExpB),
                        "Alteração" = (exp(summary(modelo)$coefficients[,1]) - 1),
                        "I.C. (Exp β)" = paste0("[",round(exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"; ",
                                                round(exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2]),casasdecimaisExpB),"]"),
                        "I.C. (Alteração)" = paste0("[",round((exp(summary(modelo)$coefficients[,1]-1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%; ",
                                                    round((exp(summary(modelo)$coefficients[,1]+1.96*summary(modelo)$coefficients[,2])-1)*100,casasdecimaisExpB),"%]"),
                        "Valor-p" = round(summary(modelo)$coefficients[,4],4))
  }
  row.names(Tabela) = NULL
  return(Tabela)
}

MetricasRegressao = function(modelo,dados_teste,var_resposta_teste){
  erro_mae = MAE(predict(modelo,dados_teste), var_resposta_teste)
  erro_mse = MSE(predict(modelo,dados_teste), var_resposta_teste)
  erro_rmse = RMSE(predict(modelo,dados_teste), var_resposta_teste)
  erro_rmsle = RMSLE(predict(modelo,dados_teste), var_resposta_teste)
  erro_mape = MAPE(predict(modelo,dados_teste), var_resposta_teste)
  r2_mod = R2_Score(predict(modelo,dados_teste), var_resposta_teste)
  Tabela = data.frame('Métricas' = c('MAE','MSE','RMSE','RMSLE','MAPE','R²'),
                      'Valores' = c(erro_mae,erro_mse,erro_rmse,erro_rmsle,erro_mape,r2_mod))
  return(Tabela)
}

####========
#### QtdCID
####========
####=======
#### Geral
####=======
treino_QtdCID = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/treino QtdCID.xlsx", sheet = 1)
teste_QtdCID = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/teste QtdCID.xlsx", sheet = 1)

treino_QtdCID$QtdCID = treino_QtdCID$QtdCID + 0.001 
teste_QtdCID$QtdCID = teste_QtdCID$QtdCID + 0.001 

mod_QtdCID1 = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + Situacao_Moradia1 + 
      Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + QtdAtendimentosMedicos +
      Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID1)
car::vif(mod_QtdCID1)

#Num_Membros_Familia1
mod_QtdCID2 = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Energia_Eletrica1 + Situacao_Moradia1 + 
                    Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + QtdAtendimentosMedicos +
                    Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID2)

#Energia_Eletrica1
mod_QtdCID3 = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Situacao_Moradia1 + 
                    Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + QtdAtendimentosMedicos +
                    Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID3)

#Abastecimento_Agua1
mod_QtdCID4 = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Situacao_Moradia1 + 
                    Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + QtdAtendimentosMedicos +
                    Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID4)

#Idade1
mod_QtdCID5 = glm(QtdCID ~ EstadoCivil1 + Sexo1 + Situacao_Moradia1 + 
                    Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + QtdAtendimentosMedicos +
                    Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID5)
TabelaModelo(mod_QtdCID5)
MetricasRegressao(mod_QtdCID5,teste_QtdCID,teste_QtdCID$QtdCID)
1-pchisq(sum(residuals(mod_QtdCID5, type = 'pearson')^2), 127299)
car::influenceIndexPlot(mod_QtdCID5)
par(mfrow = c(2,2));plot(mod_QtdCID5)

#jpeg("Tabela 23.jpg", width = 700, height = 500, quality = 100)
ggplot(treino_QtdCID %>% select(EstadoCivil1,Sexo1,Situacao_Moradia1,Consumo_Agua1,Destino_Lixo1,Numero_Comodos1,QtdAtendimentosMedicos,
                                Regional_Atendimento1) %>% na.omit(),
       aes(x = fitted(mod_QtdCID5), y = residuals(mod_QtdCID5))) +
  geom_point() + labs(x = "Valores ajustados", y = "Resíduos") +
  theme_bw() + geom_hline(yintercept = 0, color = 'red') + 
  theme(text=element_text(size = 20), plot.title = element_text(hjust = 0.5),
        #axis.text.x = element_text(angle = 50, vjust = 1, hjust=1),
        legend.position = "none")#+ ggtitle("Resíduos vs valores ajustados")
dev.off()

####==================
#### Sem atendimentos
####==================
mod_QtdCID1_SA = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + Situacao_Moradia1 + 
                    Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 +
                    Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID1_SA)
car::vif(mod_QtdCID1_SA)

#Num_Membros_Familia1
mod_QtdCID2_SA = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Energia_Eletrica1 + Situacao_Moradia1 + 
                       Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 +
                       Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID2_SA)

#Energia_Eletrica1
mod_QtdCID3_SA = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + Situacao_Moradia1 + 
                       Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 +
                       Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID3_SA)

#Situacao_Moradia1
mod_QtdCID4_SA = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + 
                       Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 +
                       Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID4_SA)

#Abastecimento_Agua1
mod_QtdCID5_SA = glm(QtdCID ~ Idade1 + EstadoCivil1 + Sexo1 + 
                       Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 +
                       Regional_Atendimento1, data = treino_QtdCID, family = Gamma(link = 'log'))
summary(mod_QtdCID5_SA)
MetricasRegressao(mod_QtdCID5_SA,teste_QtdCID,teste_QtdCID$QtdCID)
1-pchisq(sum(residuals(mod_QtdCID5_SA, type = 'pearson')^2), 127306)

####=================
#### QtdMedicamentos
####=================
####=======
#### Geral
####=======
treino_QtdMedicamentos = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/treino QtdMedicamentos.xlsx", sheet = 1)
teste_QtdMedicamentos = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/teste QtdMedicamentos.xlsx", sheet = 1)

treino_QtdMedicamentos$QtdMedicamentos = treino_QtdMedicamentos$QtdMedicamentos + 0.01 
teste_QtdMedicamentos$QtdMedicamentos = teste_QtdMedicamentos$QtdMedicamentos + 0.01 

mod_QtdMedicamentos1 = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                             Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                             QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdMedicamentos, family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos1)
car::vif(mod_QtdMedicamentos1)

#Num_Membros_Familia1
mod_QtdMedicamentos2 = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Energia_Eletrica1 + 
                             Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                             QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdMedicamentos, family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos2)

#Abastecimento_Agua1
mod_QtdMedicamentos3 = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Energia_Eletrica1 + 
                             Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                             QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdMedicamentos, family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos3)

#Energia_Eletrica1
mod_QtdMedicamentos4 = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + 
                             Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                             QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdMedicamentos, family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos4)

#Destino_Lixo1
mod_QtdMedicamentos5 = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + 
                             Situacao_Moradia1 + Consumo_Agua1 + Numero_Comodos1 + 
                             QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdMedicamentos, family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos5)
TabelaModelo(mod_QtdMedicamentos5)
MetricasRegressao(mod_QtdMedicamentos5,teste_QtdMedicamentos,teste_QtdMedicamentos$QtdMedicamentos)
1-pchisq(sum(residuals(mod_QtdMedicamentos5, type = 'pearson')^2), 127299)

####==================
#### Sem atendimentos
####==================
mod_QtdMedicamentos1_SA = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                                Regional_Atendimento1, data = treino_QtdMedicamentos %>% mutate(QtdMedicamentos = QtdMedicamentos+0.01), 
                              family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos1_SA)
car::vif(mod_QtdMedicamentos1_SA)

#Abastecimento_Agua1
mod_QtdMedicamentos2_SA = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                                Regional_Atendimento1, data = treino_QtdMedicamentos %>% mutate(QtdMedicamentos = QtdMedicamentos+0.01), 
                              family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos2_SA)

#Energia_Eletrica1
mod_QtdMedicamentos3_SA = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + 
                                Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                                Regional_Atendimento1, data = treino_QtdMedicamentos %>% mutate(QtdMedicamentos = QtdMedicamentos+0.01), 
                              family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos3_SA)

#Destino_Lixo1
mod_QtdMedicamentos4_SA = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + 
                                Situacao_Moradia1 + Consumo_Agua1 + Numero_Comodos1 + 
                                Regional_Atendimento1, data = treino_QtdMedicamentos %>% mutate(QtdMedicamentos = QtdMedicamentos+0.01), 
                              family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos4_SA)

#Num_Membros_Familia1
mod_QtdMedicamentos5_SA = glm(QtdMedicamentos ~ Idade1 + EstadoCivil1 + Sexo1 + 
                                Situacao_Moradia1 + Consumo_Agua1 + Numero_Comodos1 + 
                                Regional_Atendimento1, data = treino_QtdMedicamentos %>% mutate(QtdMedicamentos = QtdMedicamentos+0.01), 
                              family = Gamma(link = 'log'))
summary(mod_QtdMedicamentos5_SA)
TabelaModelo(mod_QtdMedicamentos5_SA)
MetricasRegressao(mod_QtdMedicamentos5_SA,teste_QtdMedicamentos,teste_QtdMedicamentos$QtdMedicamentos)
1-pchisq(sum(residuals(mod_QtdMedicamentos5_SA, type = 'pearson')^2), 127302)

####=====================
#### Qtd_ENCAMINHAMENTOS
####=====================
####=======
#### Geral
####=======
treino_QtdENCAMINHAMENTOS = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/treino QtdENCAMINHAMENTOS.xlsx", sheet = 1)
teste_QtdENCAMINHAMENTOS = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/teste QtdENCAMINHAMENTOS.xlsx", sheet = 1)

mod_QtdENCAMINHAMENTOS1 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS1)
car::vif(mod_QtdENCAMINHAMENTOS1)

#Destino_Lixo1
mod_QtdENCAMINHAMENTOS2 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS2)

#Num_Membros_Familia1
mod_QtdENCAMINHAMENTOS3 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + Energia_Eletrica1 + 
                                Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS3)

#Consumo_Agua1
mod_QtdENCAMINHAMENTOS4 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + Energia_Eletrica1 + 
                                Situacao_Moradia1 + Abastecimento_Agua1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS4)

#Energia_Eletrica1
mod_QtdENCAMINHAMENTOS5 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + 
                                Situacao_Moradia1 + Abastecimento_Agua1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS5)

#Situacao_Moradia1
mod_QtdENCAMINHAMENTOS6 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + 
                                Abastecimento_Agua1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS6)

#EstadoCivil1
mod_QtdENCAMINHAMENTOS7 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + Sexo1 + 
                                Abastecimento_Agua1 + Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS7)

#Abastecimento_Agua1
mod_QtdENCAMINHAMENTOS8 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + Sexo1 + 
                                Numero_Comodos1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS8)

#Numero_Comodos1
mod_QtdENCAMINHAMENTOS9 = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + Sexo1 + 
                                QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS9)
TabelaModelo(mod_QtdENCAMINHAMENTOS9)
MetricasRegressao(mod_QtdENCAMINHAMENTOS9,teste_QtdENCAMINHAMENTOS,teste_QtdENCAMINHAMENTOS$Qtd_ENCAMINHAMENTOS)
1-pchisq(sum(residuals(mod_QtdENCAMINHAMENTOS9, type = 'pearson')^2), 127302)

####==================
#### Sem atendimentos
####==================
mod_QtdENCAMINHAMENTOS1_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                   Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS1_SA)
car::vif(mod_QtdENCAMINHAMENTOS1_SA)

#Destino_Lixo1
mod_QtdENCAMINHAMENTOS2_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                   Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS2_SA)

#Sexo1
mod_QtdENCAMINHAMENTOS3_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
                                   Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS3_SA)

#Num_Membros_Familia1
mod_QtdENCAMINHAMENTOS4_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + Energia_Eletrica1 + 
                                   Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS4_SA)

#Energia_Eletrica1
mod_QtdENCAMINHAMENTOS5_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + 
                                   Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS5_SA)

#Consumo_Agua1
mod_QtdENCAMINHAMENTOS6_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + 
                                   Situacao_Moradia1 + Abastecimento_Agua1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS6_SA)

#Situacao_Moradia1
mod_QtdENCAMINHAMENTOS7_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + 
                                   Abastecimento_Agua1 + Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS7_SA)

#Abastecimento_Agua1
mod_QtdENCAMINHAMENTOS8_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + 
                                   Numero_Comodos1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS8_SA)

#Numero_Comodos1
mod_QtdENCAMINHAMENTOS9_SA = glm(Qtd_ENCAMINHAMENTOS ~ Idade1 + EstadoCivil1 + 
                                   Regional_Atendimento1, data = treino_QtdENCAMINHAMENTOS, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTOS9_SA)
TabelaModelo(mod_QtdENCAMINHAMENTOS9_SA)
MetricasRegressao(mod_QtdENCAMINHAMENTOS9_SA,teste_QtdENCAMINHAMENTOS,teste_QtdENCAMINHAMENTOS$Qtd_ENCAMINHAMENTOS)
1-pchisq(sum(residuals(mod_QtdENCAMINHAMENTOS9_SA, type = 'pearson')^2), 127302)

####=======================================
#### Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE
####=======================================
####=======
#### Geral
####=======
treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/treino QtdENCAMINHAMENTO_PARA_ESPECIALIDADE.xlsx", sheet = 1)
teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/teste QtdENCAMINHAMENTO_PARA_ESPECIALIDADE.xlsx", sheet = 1)

treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE + 0.01 
teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE = teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE + 0.01 

mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE1 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
        Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE1)
car::vif(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE1)

#Numero_Comodos1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE2 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
        Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE2)

#Abastecimento_Agua1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE3 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
        Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE3)

#Energia_Eletrica1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE4 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + 
        Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE4)

#Num_Membros_Familia1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE5 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + 
        Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE5)

#Destino_Lixo1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE6 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + 
        Situacao_Moradia1 + Consumo_Agua1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE6)

#Sexo1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + 
        Situacao_Moradia1 + Consumo_Agua1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7)

#Situacao_Moradia1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE8 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + 
        Consumo_Agua1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE8)

#EstadoCivil1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE9 = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + 
        Consumo_Agua1 + 
        QtdAtendimentosMedicos + Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE9)
TabelaModelo(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE9)
MetricasRegressao(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE9,teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE,
                  teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE)
1-pchisq(sum(residuals(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE9, type = 'pearson')^2), 127302)

####==================
#### Sem atendimentos
####==================
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE1_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
        Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + Numero_Comodos1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE1_SA)

#Numero_Comodos1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE2_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
        Situacao_Moradia1 + Abastecimento_Agua1 + Consumo_Agua1 + Destino_Lixo1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE2_SA)

#Abastecimento_Agua1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE3_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + Energia_Eletrica1 + 
        Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE3_SA)

#Energia_Eletrica1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE4_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + 
        Situacao_Moradia1 + Consumo_Agua1 + Destino_Lixo1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE4_SA)

#Destino_Lixo1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE5_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + 
        Situacao_Moradia1 + Consumo_Agua1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE5_SA)

#Situacao_Moradia1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE6_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + Num_Membros_Familia1 + 
        Consumo_Agua1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE6_SA)

#Num_Membros_Familia1
mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7_SA = 
  glm(Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE ~ Idade1 + EstadoCivil1 + Sexo1 + 
        Consumo_Agua1 + 
        Regional_Atendimento1, data = treino_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE, family = Gamma(link = 'log'))
summary(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7_SA)
TabelaModelo(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7_SA)
MetricasRegressao(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7_SA,teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE,
                  teste_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE$Qtd_ENCAMINHAMENTO_PARA_ESPECIALIDADE)
1-pchisq(sum(residuals(mod_QtdENCAMINHAMENTO_PARA_ESPECIALIDADE7_SA, type = 'pearson')^2), 127302)

####======================================
#### Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL
####======================================
####=======
#### Geral
####=======
treino_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/treino QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL.xlsx", sheet = 1)
teste_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/teste QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL.xlsx", sheet = 1)

DescritivaCat(treino_QtdENCAMINHAMENTO_PARA_SAUDE_MENTAL$Qtd_ENCAMINHAMENTO_PARA_SAUDE_MENTAL)

####==================================
#### Qtd_ENCAMINHAMENTO_PARA_URGENCIA
####==================================
treino_QtdENCAMINHAMENTO_PARA_URGENCIA = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/treino QtdENCAMINHAMENTO_PARA_URGENCIA.xlsx", sheet = 1)
teste_QtdENCAMINHAMENTO_PARA_URGENCIA = read.xlsx("C:/Users/cesar_macieira/Desktop/Usiminas/Nescon/atendimentos-medicos/teste QtdENCAMINHAMENTO_PARA_URGENCIA.xlsx", sheet = 1)

DescritivaCat(treino_QtdENCAMINHAMENTO_PARA_URGENCIA$Qtd_ENCAMINHAMENTO_PARA_URGENCIA)

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
