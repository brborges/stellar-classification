###############################################
# Vamos separar a base em treinamento e teste #
set.seed(123)
bool_treino <- stats::runif(dim(tmp)[1])>.25

treino <- tmp[bool_treino,]
teste  <- tmp[!bool_treino,]

tmp %>% str
# Deixar a árvore ser feliz
# ATENÇÂO! NÃO PLOTAR ESTA ÁRVORE!
tmp %>% names
set.seed(123)
arvore <- rpart(class ~ alpha + delta + u + g + r + i + z + run_ID + rerun_ID + cam_col + field_ID + spec_obj_ID + fiber_ID + redshift + plate + MJD + fiber_ID,
                data=tmp,
                parms = list(split = 'gini'),
                method='class',
                xval=5,
                control = rpart.control(cp = 0, 
                                        minsplit = 1, 
                                        maxdepth = 30)
)

# Verificando a complexidade da árvore
arvore$frame

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores


############################################
# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_treino, treino$class)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

tab <- table(c_teste, teste$class)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de treino: %s ', percent(acc))


###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_treino <- data.frame(obs=treino$class, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$class, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore <- rpart(class ~ alpha + delta + u + g + r + i + z + run_ID + rerun_ID + cam_col + field_ID + spec_obj_ID + fiber_ID + redshift + plate + MJD + fiber_ID,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

p_treino = stats::predict(arvore_poda, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore_poda, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
aval_treino <- data.frame(obs=treino$class, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$class, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC


########################################
# Avaliação básica da árvore TREINO#

# Predizendo com a árvore

# Probabilidade de ser estrela
prob = predict(arvore, treino)

# Classificação das estrelas
class = prob[,2]>.5
# Matriz de confusão
tab <- table(class, treino$class)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

especificidade <- tab[1,1] / (tab[1,1] + tab[2,1])
especificidade

sensibilidade <- tab[2,2] / (tab[2,2] + tab[1,2])
sensibilidade

resultado <- data.frame(acc, sensibilidade, especificidade)
resultado


########################################
# Avaliação básica da árvore TESTE#

# Predizendo com a árvore

# Probabilidade de ser estrela
prob = predict(arvore, teste)

# Classificação das estrelas
class = prob[,2]>.5
# Matriz de confusão
tab <- table(class, teste$class)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

especificidade <- tab[1,1] / (tab[1,1] + tab[2,1])
especificidade

sensibilidade <- tab[2,2] / (tab[2,2] + tab[1,2])
sensibilidade

resultado <- data.frame(acc, sensibilidade, especificidade)
resultado

