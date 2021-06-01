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

get_mode <- function(received_vector) {
  unique_values <- unique(received_vector)
  valid_values <- unique_values[!is.na(unique_values)]
  matches <- match(received_vector, unique_values)
  #b <- tabulate(matches)
  b <- table(valid_values)
  return(names(b)[which.max(b)])
}

get_semester_labels <- function(received_vector) {
  semester_levels <- c(1,2,3,4,5,6,7,8,9,10)
  semester_labels <- c("primeiro", "segundo", "terceiro", "quarto", "quinto", "sexto", "setimo", "oitavo", "nono", "decimo")
  return(factor(received_vector, levels=semester_levels, labels=semester_labels))
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
# SELECT * FROM ALCOHOL_DATABASE WHERE MORA_COM = 3 AND VC_BEBI = 1
live_family <- alcohol_database[alcohol_database$MORA_COM == 3 ,13]
drink_live_family <- factor(live_family$VC_BEBI, levels=c(1,2), labels=c("Sim", "Não"))
percentage_wrapper(drink_live_family)

########################################################


################ Média Idade ##########################################
# 1. Média da idade daqueles que responderam
avg_age <- mean(alcohol_database$IDADE)
print(avg_age)

# 2. Média de idade por curso
mean_age_course <- tapply(alcohol_database$IDADE, alcohol_database$CURSO, mean)
print(mean_age_course)

# 3. Média de idade em que começou a beber por vontade própria ou por “pressão” de algum conhecido
why_start <- factor(alcohol_database$PQ_COMECOU, levels = c(1,2,3,4), labels=c("iniciativa própria", "incentivo de amigos", "incentivo da família", "outros motivos"))
tapply(alcohol_database$IDADE, why_start, mean)
tapply(alcohol_database$IDADE, alcohol_database$PRESSAO, mean)

# 4. Qual o coeficiente de variação da idade daqueles que são solteiros?
single_ages <- alcohol_database[alcohol_database$ESTADO_CIVIL == 1, 4]
cv <- sd(single_ages$IDADE)/mean(single_ages$IDADE)*100
print(cv)

#######################################################################

############### Relação com o álcool #################################
# 1. Escola em que estudou e toma bebida alcoolica
school_type <- factor(alcohol_database$ESCOLA_Q_ESTUDOU, levels = c(1,2), labels=c("Pública", "Privada"))
you_drink <- factor(alcohol_database$VC_BEBI, levels = c(1,2), labels=c("Sim", "Não"))
school_drink <- tapply(school_type, you_drink, table)
print(school_drink)

# 2. Escola em que estudou e na família as pessoas tomam bebidas alcoólicas
table (alcohol_database$ESCOLA_Q_ESTUDOU,alcohol_database$FAMILIA_BEBI)

# 3. Qual a média de casos que uma pessoa solteira acha importante a bebida alcoólica na vida dela?
single_and_drink <- alcohol_database[alcohol_database$ESTADO_CIVIL == 1, 13]
mean_single_drink <- factor(single_and_drink$VC_BEBI, levels = c(1,2), labels=c("Sim", "Não"))
percentage_wrapper(mean_single_drink)

# 4. Com que frequência cada estado civil toma bebida alcoólica nos fins de semana?

# 5. Com que frequência pessoas casadas tomam bebida , e quantos copos tomam por dia?

######################################################################

###################### Estado Civil #####################################
# 1. Com que frequencia casados tomam bebidas alcoolicas
casado <- alcohol_database[alcohol_database$ESTADO_CIVIL == 2 & 1 ,13]
drink_live_family <- factor(casado$VC_BEBI, levels=c(1,2), labels=c("Toma", "Não toma"))
percentage_wrapper(drink_live_family)

# 2. Quantos copos por dias estudantes com relacionamento est?vel tomam bebidas alcoolicas
total <- alcohol_database[alcohol_database$ESTADO_CIVIL == 5 ,15]
mean(total[!is.na(total)])# Somatorio

#########################################################################

############################ Moda #######################################
# 1. Moda da Idade
#which.max(alcohol_database$IDADE)# Pega a posição do maior valor
#max(alcohol_database$IDADE)# Pega o maior valor
e <- c(alcohol_database$IDADE)
getmoda(e)

# 2. Moda da Renda Familiar
v <- c(alcohol_database$RENDA_FAMILI)
getmoda(v)


#########################################################################

#################### Semestre e bebida ##################################
# 1. Qual o tipo de bebida mais consumido por semestre
drink_type <- factor(alcohol_database$TIPO, levels = c(1,2,3,4), labels = c("Cerveja", "Vinho", "\"ICE\"", "Outro"))
semester <- get_semester_labels(alcohol_database$SEMESTRE)
mode_drink_per_semester <- tapply(drink_type, semester, get_mode)
print(mode_drink_per_semester)

# 2. Quantidade de pessoas por semestre e por curso
semester_course <- tapply(get_semester_labels(alcohol_database$SEMESTRE), alcohol_database$CURSO, table)
print(semester_course)

# 3. Porcentagem de pessoas que tomam bebida alcoolica por semestre
drink_by_semester <- alcohol_database[alcohol_database$VC_BEBI == 1 ,2]
drink_semester_labels <- get_semester_labels(drink_by_semester$SEMESTRE)
percentage_wrapper(drink_semester_labels)

# 4. A Moda de semestres com relação ao motivo pelo qual começaram a beber
start_drink <- factor(alcohol_database$PQ_COMECOU, levels=c(1,2,3,4), labels=c("iniciativa própria", "incentivo de amigos", "incentivo da família","outros motivos"))
start_drink_per_semester_mode <- tapply(start_drink, semester, get_mode)
print(start_drink_per_semester_mode)
######################################################################

###################### Curso #########################################
# 1. Qual a amplitude das pessoas do curso de civil morar com os alguém?
civil_mora <- alcohol_database[alcohol_database$CURSO == 'civil' ,9]
amplitude <- diff(range(civil_mora, na.rm=T))
print(amplitude)

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
renda_family <- alcohol_database[alcohol_database$MORA_COM == 3, 8]
med <- mean(renda_family$RENDA_FAMILI,na.rm=T)
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
