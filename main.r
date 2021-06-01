##########################
## Projeto com R Studio ##
##########################

# Projeto realizado pelos alunos:
# Alexandre Ribeiro Carneiro 
# João Victor Oliveira Couto

########################################
# Instalação dos pacotes no ambiente R #
########################################
install.packages("scales") # Install and load scales
install.packages('readxl') # importar banco em excel
install.packages('descr') # crosstable e teste qui-quadrado

##################
# Chamada pacote #
##################                
library(readxl, descr) # Load libraries
library(scales)

#################################
# Selecionando o banco de dados #
#################################
alcohol_database <- read_excel(
  "Banco alcool_Trabalho final.xlsx",  sheet = "grupo 1"
)
View(alcohol_database)


#######################################################
# CRIANDO FUNÇÕES, PORQUE CANSEI DE PESQUISAR NO GOOGLE
#######################################################
giveme_percentage <- function(column_values, by=0) {
  counter_specific <- 0
  total <- 0
  for(element in column_values) {
    if(!is.na(element)) {
      if (!is.null(element) && element == by) {
        counter_specific <- counter_specific + 1
      }
    }
    total <- total + 1
  }
  return(scales::percent(counter_specific / total))
}

percentage_wrapper <- function(column) {
  for(key in unique(column)) {
    if(!is.na(key)) {
      percentage_calculated <- giveme_percentage(column, key)
      cat(sprintf("\"%s\" \"%s\"\n", key, percentage_calculated))
    }
  }
}

getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#########################################
# Começar a pegar os dados para análise #
#########################################

################### Porcentagens #######################
# 1. Porcentagem de pessoas por sexo
GENDER <- factor(alcohol_database$SEXO, levels=c(1,2), labels=c("homem", "mulher"))
percentage_wrapper(GENDER)

# 2. Porcentagem de pessoas por curso
percentage_wrapper(alcohol_database$CURSO)


# 3. Porcentagem de pessoas que moram com a familia e não bebem
#alcohol_database[alcohol_database$MORA_COM == 3,13] #pegar todos que tem3 na outra tab
percentage_wrapper(alcohol_database$MORA_COM)
########################################################


################ Média Idade ##########################################
# 1. Média da idade daqueles que responderam
avg_age <- mean(alcohol_database$IDADE)
print(avg_age)

# 2. Média de idade por curso
tapply(alcohol_database$IDADE, alcohol_database$CURSO, mean)

# 3. Média de idade em que começou a beber por vontade própria ou por “pressão” de algum conhecido
tapply(alcohol_database$IDADE, alcohol_database$PQ_COMECOU, mean)
tapply(alcohol_database$IDADE, alcohol_database$PRESSAO, mean)

# 4. Qual o coeficiente de variação da idade daqueles que são solteiros?
cv <- sd(alcohol_database$IDADE)/mean(alcohol_database$IDADE)*100 
print(cv)

#######################################################################

############### Relação com o álcool #################################
# 1. Escola em que estudou e toma bebida alcoolica
table (alcohol_database$ESCOLA_Q_ESTUDOU,alcohol_database$VC_BEBI)# Mostra tudo sem separar

# 2. Escola em que estudou e na família as pessoas tomam bebidas alcoólicas
table (alcohol_database$ESCOLA_Q_ESTUDOU,alcohol_database$FAMILIA_BEBI)

# 3. Qual a média de casos que uma pessoa solteira acha importante a bebida alcoólica na vida dela?

# 4. Com que frequência cada estado civil toma bebida alcoólica nos fins de semana?

# 5. Com que frequência pessoas casadas tomam bebida , e quantos copos tomam por dia?

# 6. Pessoas que responderam que a bebida é importante na vida baseado na idade que começaram a beber

# 7. Imagem que a bebida alcoólica causa em você e Já deu algum vexame por causa da bebida

# 8. Por que começou a tomar bebida alcoólica e que imagem causa em você a bebida alcoólica

######################################################################

###################### Estado Civil #####################################
# 1. Com que frequencia viuvos e divorciados tomam bebidas alcoolicas

# 2. Quantos copos por dias viuvos e divorciados tomam bebidas alcoolicas

#########################################################################

############################ Moda #######################################
# 1. Moda da Idade
#which.max(alcohol_database$IDADE)# Pega a posição do maior valor
#max(alcohol_database$IDADE)# Pega o maior valor
e <- c(alcohol_database$IDADE)
getmoda(e)

# 1. Moda da Renda Familiar
v <- c(alcohol_database$RENDA_FAMILI)
getmoda(v)


#########################################################################

#################### Semestre e bebida ##################################
# 1. Qual o tipo de bebida mais consumido por semestre

# 2. Qual o tipo de bebida mais consumido por semestre e por curso

# 3. Porcentagem de pessoas que tomam bebida alcoolica por semestre

# 4. A Moda de semestres com relação ao motivo pelo qual começaram a beber 

######################################################################

##################### Embriaguez ao volante ##########################
# 1. Já sofreu algum acidente de carro por conta da bebida e dirigiu sobre efeito do álcool

# 2. Já sofreu algum acidente de carro por conta da bebida e pegou carona com alguém que dirigiu sobre efeito do álcool

# 3. Já sofreu algum acidente de carro por conta da bebida e (dirigiu sobre efeito do álcool) ou (pegou carona com alguém que dirigiu sobre efeito do álcool)

# 4. Já sofreu algum acidente de carro por conta da bebida e possui veiculo

# 5. Quantas pessoas tomam bebida alcoólica e já pegaram carona com alguém que dirigiu sob efeito do álcool?

######################################################################

###################### Curso #########################################
# 1. Qual a amplitude das pessoas do curso de civil morar com os alguém?
civil_mora <- alcohol_database[alcohol_database$CURSO == 'civil' ,9]
diff(range(civil_mora, na.rm=T))

# 2. Variância da idade das pessoas do curso de civil em relação a de matemática.
var_idade <- alcohol_database[alcohol_database$CURSO == 'civil' ,4]
variancia <- var(var_idade$IDADE, na.rm=T)
print(variancia)

######################################################################

#################### Renda ###########################################
# 1. Qual o desvio padrão da renda familiar daqueles que bebem 8 copos de bebida alcoólica por dia?
rend_bebem <- alcohol_database[alcohol_database$QNTOS_COPOS == 8 ,8]
copos <- sd(rend_bebem$RENDA_FAMILI, na.rm=T)
print(copos)

# 2. Desvio padrão entre a renda familiar de pessoas que estudaram em escola particular e pessoas que estudaram em escola publica
esc_public <- alcohol_database[alcohol_database$ESCOLA_Q_ESTUDOU == 2 ,8]
rend <- sd(esc_public$RENDA_FAMILI, na.rm=T)
print(rend)
esc_parti <- alcohol_database[alcohol_database$ESCOLA_Q_ESTUDOU == 1 ,8]
renda <- sd(esc_parti$RENDA_FAMILI, na.rm=T)
print(renda)

# 3. Média da renda familiar de pessoas que moram com a familia
renda_family <- alcohol_database[alcohol_database$MORA_COM == 3 ,9]
med <- mean(renda_family$MORA_COM,na.rm=T)
print(med)

######################################################################

################### Religião #########################################
# 1. Média de pessoas que começaram a beber por vontade própria agrupado por religião
religian <- factor(alcohol_database$religiao, levels=c(1,2,3), labels=c("Católica", "Evangélica","Outra "))
tapply(alcohol_database$PRESSAO, religian, mean, na.rm=T)

# 2. Moda da imagem que a bebida alcoólica causa, agrupado por religião
c <- c(alcohol_database$Q_IMAGEM_CAUSA)
getmoda(c)

######################################################################
