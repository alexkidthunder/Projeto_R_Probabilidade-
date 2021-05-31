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
library(readxl, descr, scales) # Load libraries

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
percentage_wrapper(alcohol_database$MORA_COM)
########################################################


################ Média Idade ###########################
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

########################################################

############### Relação com o álcool #################################
# 1. Escola em que estudou e toma bebida alcoolica

# 2. Escola em que estudou e na família as pessoas tomam bebidas alcoólicas

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
alcohol_database[which.max(alcohol_database$IDADE),4]

# 1. Moda da Renda Familiar
alcohol_database[which.max(alcohol_database$RENDA_FAMILI),8]

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
# 1. Qual a amplitude das pessoas do curso de civil morar com os amigos e que não toma bebida alcoólica?

# 2. Variância da idade das pessoas do curso de civil em relação a de matemática.

######################################################################

#################### Renda ###########################################
# 1. Qual o desvio padrão da renda familiar daqueles que bebem 8 copos de bebida alcoólica por dia?

# 2. Desvio padrão da entre a renda familiar de pessoas que estudaram em escola particular e pessoas que estudaram em escola publica

# 3. Média da renda familiar de pessoas que moram com a familia

######################################################################

################### Religião #########################################
# 1. Média de pessoas que começaram a beber por vontade própria ou por “pressão” de algum conhecido agrupado por religião

# 2. Moda da imagem que a bebida alcoólica causa, agrupado por religião

# 3. Porcentagem de pessoas que bebem na familia agrupados por religião

######################################################################
