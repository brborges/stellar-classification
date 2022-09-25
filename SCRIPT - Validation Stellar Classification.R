############################################################################
# Carregando base de dados stellar Classification #
############################################################################

df_stellar <- read_csv("DATA - star_classification.csv")
df_stellar %>% head
tmp <- df_stellar

tmp$class <- as.integer(df_stellar$class=="GALAXY")
tmp %>% glimpse


df_stellar %>%
  group_by(class) %>%
  summarise(count(df_stellar$class))

######### Matriz de correlacao

matrix_cor <- stats::cor(tmp, y = NULL, use = "everything",
           method = c("pearson", "kendall", "spearman"))

matrix_cor %>% View

corrplot::corrplot(matrix_cor, method = "pie")

tmp <- select(tmp, everything(), -rerun_ID)



######### Graficos para visualizacao dos dados

#todas as classes
ggplot(df_stellar)+
  geom_point(aes(x=alpha, y=redshift, color=class))+
  facet_grid(~class)+
  theme_bw()

ggplot(df_stellar)+
  geom_bar(aes(x=class, fill=class))

ggplot(df_stellar)+
  geom_violin(aes(x=alpha, y=redshift, fill=class))+
  facet_wrap(~class)+
  theme_bw()
  
#somente galaxy
ggplot(tmp)+
  geom_density2d_filled(aes(x=alpha, y=i))

ggplot(tmp)+
  geom_point(aes(x=alpha, y=i, color="red"))



############################################################################
# Decision Tree #
############################################################################

set.seed(123)
bool_treino <- stats::runif(dim(tmp)[1])>.25

treino <- tmp[bool_treino,]
teste  <- tmp[!bool_treino,]

arvore <- rpart(class ~ alpha + delta + u + g + r + i + z + run_ID + cam_col + field_ID + spec_obj_ID + fiber_ID + redshift + plate + MJD + fiber_ID,
                data=tmp,
                parms = list(split = 'gini'),
                method='class',
                xval=5,
                control = rpart.control(cp = 0, 
                                        minsplit = 1, 
                                        maxdepth = 30)
)

# Verificando a complexidade da árvore #
arvore$frame

# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores


############################################################################
# Prunning Decision Tree (Grid Search) #
############################################################################

tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda <- rpart(class ~ alpha + delta + u + g + r + i + z + run_ID + cam_col + field_ID + spec_obj_ID + fiber_ID + redshift + plate + MJD + fiber_ID,
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

# Evaluating Decision Tree by train database
aval_treino <- data.frame(obs=treino$class, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, fill="blue")) + 
  plotROC::geom_roc(n.cuts = 0) +
  theme(legend.position = "none") +
  theme_bw()+
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Evaluating Decision Tree by teste database

aval_teste <- data.frame(obs=teste$class, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour="red")) + 
  plotROC::geom_roc(n.cuts = 0) +
  theme(legend.position = "none") +
  theme_bw()+
  ggtitle("Curva ROC - base de teste")

CurvaROC


########################################
# Avaliação básica da árvore TREINO #

# Predizendo com a árvore

# Probabilidade de ser estrela
prob = predict(arvore_poda, treino)

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
# Avaliação básica da árvore TESTE #

# Predizendo com a árvore

# Probabilidade de ser estrela
prob = predict(arvore_poda, teste)

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

resultado <- data.frame(acc, sensibilidade, especificidade)
resultado %>% View

