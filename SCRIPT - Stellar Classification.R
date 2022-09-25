######################################
# Carregando base de dados stellar #

df <- read_csv("DATA - star_classification.csv")
df %>% head 
df %>% glimpse()
table(df$class, df$class)

#################################
# Objetivo:
#      Classificar objetos como estrela de acordo 
#      somente com variáveis do registro deles
################################


#################################### 
# Vamos fazer uma breve descritiva #

# Vamos criar uma base temporária para manter a base original intacta
tmp <- df
tmp$class <- as.integer(tmp$class=="STAR")
tmp %>% head(n=30)

table(tmp$class, tmp$class)

dim(filter(df, class=="STAR"))
dim(filter(tmp, class==1))
##########################################
# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de estrelas por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de estrelas por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="class", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/100000, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=class, ymin=class-se, ymax=class+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=class, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=class, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Indice de estrelas") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*100000, name = "Frequencia"), labels = scales::percent)
}

descritiva("alpha")




# Listagem das variáveis com algumas características
tmp %>% str

#############################################
# Vamos construir a árvore de classificação #
arvore <- rpart(class ~ alpha + delta + u + g + r + i + z + run_ID + rerun_ID + cam_col + field_ID + spec_obj_ID + fiber_ID,
                data=tmp,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de ser estrela
prob = predict(arvore, tmp)

# Classificação das estrelas
class = prob[,2]>.5
# Matriz de confusão
tab <- table(class, tmp$class)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

especificidade <- tab[1,1] / (tab[1,1] + tab[2,1])
especificidade

sensibilidade <- tab[2,2] / (tab[2,2] + tab[1,2])
sensibilidade

resultado <- data.frame(acc, sensibilidade, especificidade)
resultado
