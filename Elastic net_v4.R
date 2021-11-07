
rm(list = ls())
options(scipen=999)

# Librer?as ----

require(glmnet)  
require(data.table)
require(ggplot2)
require(tidyverse)
require(broom)
require(useful)
require(ggrepel)
require(vip)

# 1.- FACTORES MUY CORRELACIONADOS ENTRE S?. ----

  # Caso de uso con una clara aplicaci?n para algoritmo de regresi?n penalizada, Lasso, Ridge & Elastic net Elastic net

  # Simulamos distancia, n?mero de estaciones, precios y retrasos con f(X) de las dos primeras 

n <- 1000

DT_tibble <-tibble(distancia = rnorm(n, 500, 100), 
                   estaciones = rpois(n, lambda = 15), 
                   precio = rnorm(1000, 50, 10)) %>%  glimpse()

DT_tibble <- DT_tibble %>% 
  mutate(retrasos = 1 +  distancia /100 + estaciones / 5 + rnorm(n,0,1)) 

DT_tibble %>% 
  ggplot(aes(distancia, retrasos, color = cut(estaciones,5))) + 
  geom_smooth(se = FALSE)

lm <- lm(retrasos ~ ., data = DT_tibble)
summary(lm)

  # Simulamos de nuevo el precio para que recoja la correlaci?n real con la distancia y el n?mero de estaciones
  # Ajustamos de nuevo la regresi?n lineal

DT_tibble <- DT_tibble %>% 
  mutate(precio = distancia/10 - estaciones + rnorm(n,0,.1))

require(GGally)

ggpairs(DT_tibble)

lm <- lm(retrasos ~ ., data = DT_tibble)
summary(lm)   # Est?n tan correlacionados que ninguno de los factores sale significativo


  # Aplicamos regresiones de lasso (alpha = 1, por defecto en glmnet) y ridge (alpha = 0) para ver los diferentes resultados

y <-  DT_tibble$retrasos
x <-  base::as.matrix(DT_tibble)

x <- DT_tibble %>% select(-retrasos) %>% as.matrix()


lasso <- glmnet(x,y)
plot(lasso)

cvlasso <- cv.glmnet(x,y)
plot(cvlasso)
coef(cvlasso, s = "lambda.min")

ridge <- cv.glmnet(x, y, alpha = 0)
plot(ridge)
coef(ridge, s = "lambda.min")

DT_tibble <-  DT_tibble %>% 
  mutate(precio_fake = rgamma(n, shape = 50,scale = 5))

x <- DT_tibble %>% select(-retrasos) %>% as.matrix()

lasso_check <-cv.glmnet(x, y, alpha = 1)
plot(lasso_check)
coef(lasso_check, s = "lambda.min")




# 2.- OVERFITTING ----

# FACTORES REDUNDANTES Y CON SOBREAJUSTE. 

n = 100

DT_tibble <- tibble(distancias = rnorm(n, 0, 3)) %>% 
  mutate(media = 2 + exp(distancias)/(1+exp(distancias)),
         retrasos = media + rnorm(n, 0, 0.5))

p <- DT_tibble %>% 
  ggplot(aes(distancias, media)) + 
  geom_line(size = 1) + 
  ylim(range(DT_tibble$retrasos)) + 
  xlab("distancias estandarizadas")

DT_tibble <- DT_tibble %>% 
  mutate(muestra = sample(c("train", "test"),size = n, prob = c(2/3,1/3),replace = TRUE))

p <- p + geom_point(aes(y = retrasos, color = DT_tibble$muestra)) 
p

  # realizamos una regresi?n polin?mica,  con un claro overfitting


for (i in 2:10) {
  
  DT_tibble <- DT_tibble %>% 
    mutate(polinomio = map(.$distancias, function(x) x^i))  
    
    colnames(DT_tibble)[ncol(DT_tibble)] = paste0("polinomio_", i)
  
}

DT_tibble <- DT_tibble %>% 
  unnest(-c(distancias, media, retrasos, muestra))

DT_tibble %>%  glimpse()

ols <- lm(retrasos ~ ., subset(DT_tibble, muestra == "train", select = -c(media, muestra)))
summary(ols) 

ols$fitted.values


p <- p + geom_line(aes(x = distancias, y= ols$fitted.values), 
                   data = DT_tibble %>% filter(muestra == "train"), 
                   color = "orange", size = 1)

p

  # ajustamos ridge y lasso sobre los mismo datos que la regresi?n polin?mica

x <- DT_tibble %>% filter(muestra == "train") %>% select(-c("media","retrasos","muestra")) %>% as.matrix() 

Y <- DT_tibble %>% filter(muestra == "train") %>% select(retrasos) %>% as.matrix()

lasso_poli <- cv.glmnet(x,Y)
plot(lasso_poli)
plot(lasso_poli$glmnet.fit)
coef(lasso_poli, s= "lambda.min")
print(lasso_poli)

ridge_poli <- cv.glmnet(x,Y, alpha = 0)
plot(ridge_poli)
coef(ridge_poli, s= "lambda.min")
print(ridge_poli)

p <- p + geom_line(aes(distancias, y = predict(object = lasso_poli, newx = as.matrix(x))),  
                   data = DT_tibble %>% filter(muestra == "train"), 
                   color = "blue",
                   size = 1 )   # A?adimos el ajuste de Lasso regression

p <- p +geom_line(aes(distancias, y = predict(object = ridge_poli, newx = as.matrix(x))),
                  data = DT_tibble %>%  filter(muestra == "train"),
                  color = "red", 
                  size = 1) + 
                  theme(legend.position = "bottom")     # A?adimos el ajuste de Ridge regression
  
p



# 3.- RESUMEN DE MODELOS ----


  # Sumarizaci?n de info de los modelos usando tidy tables -  broom

  # Lasso

glance_cv <- broom::glance(lasso_poli)
tidy_lasso <- broom::tidy(lasso_poli)
tidy_glmnetfit_lasso <- broom::tidy(lasso_poli$glmnet.fit)
  tidy_glmnetfit_lasso %>% filter(lambda == glance_cv$lambda.min)
  
tidy_lasso %>% ggplot(aes(log(lambda), estimate)) + #replicamos el gr?fico por defecto de plot(cv.glmnet), plot(lasso_poli)
  geom_line(size =1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) + 
  geom_vline(xintercept = log(glance_cv$lambda.min)) + 
  geom_vline(xintercept = log(glance_cv$lambda.1se), lty = 2) +
  labs(y = "Mean-squared error", x = "Log(lambda)") 
  

plot(lasso_poli)
plot(lasso_poli$glmnet.fit)

op <- par(mfrow=c(1, 2))
plot(lasso_poli$glmnet.fit, "norm",   label=TRUE)
plot(lasso_poli$glmnet.fit, "lambda", label=TRUE)
par(op)

vip(lasso_poli)

    #lm 

glance_lm <- glance(ols)
tidy_lm <- tidy(ols)
augment_ols <- augment(ols,newdata = DT_tibble %>% filter(muestra == "test")) %>% select(retrasos,distancias, .fitted:.resid)


  # calculamos de forma "manual " el MSE de la regresi?n penalizada vs MSE regresi?n polin?mica (no)

mse_glmnet <- predict.glmnet(lasso_poli$glmnet.fit,
                             newx = DT_tibble %>% filter(muestra == "test") %>% select(-c("media", "retrasos", "muestra")) %>% as.matrix(),
                             s = tidy_lasso %>% filter(lambda == glance_cv$lambda.min) %>% .$lambda) %>% 
              as.tibble() %>% 
              rename(predicciones = 1) %>% 
              cbind(DT_tibble %>% filter(muestra == "test")) %>% 
              as.tibble() %>% 
              summarize(mse = mean((predicciones-retrasos)^2)) 
              # no coincide con el valor de estimate de (tidy_lasso %>% filter(lambda == glance_cv$lambda.min)) --> Por qu?!?

mse_lm <- augment_ols %>% summarize(mean(.resid^2))





# 4.- ELASTIC NET - Combinaci?n ?ptima de Lasso & Ridge -----


  # Descargamos el dataset

#acs <- read.table("http://jaredlander.com/data/acs_ny.csv", sep=",", header=TRUE, stringsAsFactors = FALSE) %>% as_tibble()

dir_actual <- getwd()
acs <- read.csv(paste0(dir_actual, "/acs_ny.csv"))

  # Matrices de dise?o para el modelo

acs <- acs %>% mutate(Income = if_else(FamilyIncome >= 15000,1,0)) # Variable respuesta

acs <- acs %>% mutate(muestra = sample(c("train","test"), size = nrow(acs),prob = c(.8, .2),replace = TRUE))

acs_train <- acs %>%  filter(muestra == "train")

acs_test <- acs %>% filter(muestra == "test")

acsX_train <- build.x(Income~NumBedrooms + NumChildren+NumPeople+NumRooms+NumUnits+NumVehicles+NumWorkers+OwnRent+
                  YearBuilt+ElectricBill+FoodStamp+HeatingFuel+Insurance+Language -1, data=acs_train, contrasts = FALSE)

acsY_train <- build.y(Income~NumBedrooms + NumChildren+NumPeople+NumRooms+NumUnits+NumVehicles+NumWorkers+OwnRent+
                  YearBuilt+ElectricBill+FoodStamp+HeatingFuel+Insurance+Language -1, data=acs_train)

      # matrices de dise?o tb para test para  aplicar predict sobre ellas

acsX_test <- build.x(Income~NumBedrooms + NumChildren+NumPeople+NumRooms+NumUnits+NumVehicles+NumWorkers+OwnRent+
                        YearBuilt+ElectricBill+FoodStamp+HeatingFuel+Insurance+Language -1, data=acs_test, contrasts = FALSE)

acsY_test <- build.y(Income~NumBedrooms + NumChildren+NumPeople+NumRooms+NumUnits+NumVehicles+NumWorkers+OwnRent+
                        YearBuilt+ElectricBill+FoodStamp+HeatingFuel+Insurance+Language -1, data=acs_test)


  # MODELLING, lasso y Ridge por separado, & Graphs

set.seed(123456)

  # Lasso 

acsCV1 <- cv.glmnet(acsX_train, acsY_train, family="binomial", nfolds=5)

plot(acsCV1)

coef(acsCV1, s="lambda.1se")
coef(acsCV1, s= "lambda.min")

  # gr?fico de par?metros de la regresi?n penalizada Lasso

p_lasso_joint <- par(mfrow=c(1, 2))
par(op)

p_lasso <- tidy(acsCV1$glmnet.fit) %>% filter(lambda == glance(acsCV1)$lambda.min) %>%
  mutate(term = as.factor(term),
         term = fct_reorder(term, abs(estimate))) %>% 
  ggplot(aes(estimate, term)) +  
  geom_point(color = "red", size = 2) +
  geom_segment(aes(x=0, xend = estimate, yend = term), color = "black", size = 1) +
  geom_vline(xintercept = 0, lty = 2) + 
  ggrepel::geom_label_repel(aes(label = round(estimate, 3)))
p_lasso


plot(acsCV1$glmnet.fit, xvar="lambda")
abline(v=log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty=2)


  # Ridge

acsCV2 <- cv.glmnet(acsX_train, acsY_train, family="binomial", nfolds=5, alpha=0)

plot(acsCV2) # path de lambdas

coef(acsCV2, S= "lambda.min") # muchos m?s par?metros que en Lasso

plot(acsCV2$glmnet.fit, xvar = "lambda")
abline(v=log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty=2)

  # Gr?fico de la regresi?n penalizada Ridge

p_ridge <- tidy(acsCV2$glmnet.fit) %>% filter(lambda == glance(acsCV2)$lambda.min) %>%
  mutate(term = as.factor(term),
         term = fct_reorder(term, abs(estimate))) %>% 
  ggplot(aes(estimate, term)) + 
  geom_point(color = "blue", size =2) + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_segment(aes(x=0, xend = estimate, yend = term), color = "black", size = 0.6) + 
  ggrepel::geom_label_repel(aes(label = round(estimate, 3)))
p_ridge

  # Graficamos conjuntamente los par?metros de lasso y ridge en lambda.min para comparar

glmnetfit_lambdamin_lasso <- tidy(acsCV1$glmnet.fit) %>% filter(lambda == glance(acsCV1)$lambda.min) %>% 
  rename(estimate_lasso = estimate)

glmnetfit_lambdamin_ridge <- tidy(acsCV2$glmnet.fit) %>% filter(lambda == glance(acsCV2)$lambda.min) %>% 
  rename(estimate_ridge = estimate)

glmnetfit_lambdamin <- glmnetfit_lambdamin_lasso %>% full_join(glmnetfit_lambdamin_ridge, by = c("term")) %>% 
  select(term, estimate_lasso, estimate_ridge) %>% 
  gather(key = "model", value = "estimate", -term) %>% 
  mutate(estimate = if_else(is.na(estimate), 0, estimate),
         term = as.factor(term) %>% fct_reorder(abs(estimate))) 

glmnetfit_lambdamin %>% ggplot(aes(estimate, term)) + 
  geom_point(aes(color = model)) + 
  ggrepel::geom_label_repel(aes(label = round(estimate, 3)),data = glmnetfit_lambdamin %>% filter(estimate != 0))


# 4.1 Cross-validation con alpha, Elastic Net. RANDOM SEARCH ----

fldr_output <- 'C:/Users/e106379/Desktop/FD - TCL/Training/R&D'

#Ranges to search
rangeAlpha <- c(0,1)

foldid_randomsearch <- sample(rep(1:10, length.out=nrow(acsX_train)))

bestScore <-  Inf

inicio <- Sys.time()

for (ii in 1:5) {
  
  tAlpha <- runif(n=1, min=rangeAlpha[1], max=rangeAlpha[2]) # Random search
  
  print(paste0("Testing Alpha: ",tAlpha))
  
  enet <- cv.glmnet(x = acsX_train, 
                    y = acsY_train,
                    family = "binomial",
                    alpha = tAlpha,
                    foldid = foldid_randomsearch) 
  
  # currScore <- min(deviance(enet$glmnet.fit)) Lo reemplazamos por m?tricas de broom
  
  currscore <- tidy(enet) %>% filter(lambda == glance(enet)$lambda.min) %>% select(estimate) %>% pull()
  
  if (currscore < bestScore)
    {
    cat(paste0("Alpha updated, now: ", tAlpha, "\n\n"))
    bestScore <- currscore
    bestEnet <- enet
  }
  
  # rm(enet, tAlpha, currScore)
  
}

fin <- Sys.time()

fin-inicio

## save this model
save(bestEnet, file = file.path(fldr_output, "Best_enet.rda"))
coef(bestEnet) 

## load the model when needed
#load(file = file.path(fldr_output, "Best_enet.rda"))

# Calculate predictions and save the data
acsX_train$Prediction_enet <- predict(bestEnet, newx = acsX_train, type = "response")
acsX_test$Prediction_enet <- predict(bestEnet, newx = acsX_test, type = "response")

saveRDS(acsX_train, file = file.path(fldr_output, '010 - TRAIN SET.rds'))
saveRDS(acsX_test, file = file.path(fldr_output, '010 - TEST SET.rds'))

glance(bestEnet)
plot(bestEnet)
tidy(bestEnet) %>%  filter(lambda == glance(bestEnet)$lambda.min)


# 4.2 Cross-validation con alpha, Elastic Net. GRID SEARCH & FOREACH & PARALLELL ----

  # foreach with %do%/ %dopar% is used to execute an R expression repeatedly, and return the results in some data structure or object, a list by default

library(parallel)
library(doParallel)

set.seed(127)

alphas <- seq(from=0, to = 1, by=0.05) # Grid search

foldid_gridsearch <- sample(rep(1:5, length.out=nrow(acsX_train))) # Con CV doble cada registro tiene que caer siempre en el mismo fold  

    #Para ejecutar en paralelo siempre es necesario iniciar y registrar el cluster con makecluster y registerdoparalell

detectCores() # 12 cores

    # Inicio c?digo a ejecutar como un bloque para ajuste modelos

cl <- makeCluster(11) # Inicializamos el cluster con 11 workers (12 cores -1)
registerDoParallel(cl)

Inicio <- Sys.time()

lista_modelos <- foreach(i=1:length(alphas),
                         .errorhandling = "remove", #indicamos la forma de gestionar los errores. Con remove, si hay error salta esa iteraci?n
                         .inorder = FALSE, # no es necesario procesar cada foreach en order, por lo que mejora el rendimiento sustancialmente
                         .multicombine = TRUE, # dado que almancenamos resultados de foreach en una lista (default), multicombine =TRUE mejora el rendimiento
                         .export=c("acsX_train", "acsY_train", "alphas", "foldid_gridsearch"), # cargamos de forma expl?cita en el foreach environment las variables indicadas en .export
                         .packages="glmnet", #cargamos glmnet en cada worker
                         .verbose = TRUE) %dopar% 
  
  {
    print(alphas[i])
    cv.glmnet(x=acsX_train, 
              y=acsY_train, 
              family="binomial", 
              nfolds=5, # Si se introduce vector para foldid no es necesario indicar nfolds 
              foldid = foldid_gridsearch, 
              alpha=alphas[i],
              trace.it = 1)
    
  }        

Fin <- Sys.time()

print(Fin-Inicio) # Interesante comparar tiempos de ejecuci?n con %do% y %dopar%

stopCluster(cl)

lista_modelos <- lista_modelos %>% set_names(alphas)

  # Fin c?digo a ejecutar

map_chr(lista_modelos, class) # Comprobamos la clase de todos los elementos de la lista creada

plot(lista_modelos$"0")  # podemos llamar como habitualmente a cada uno de los elementos de la lista foreach
plot(lista_modelos[[1]])


  #  4.3 Gr?ficos ------

# Gr?fico de desvianza para cada alpha, tanto para lambdamin como para lambda1se

Results_glmnet_tibble <- tibble() # Iniciamos el tibble, vac?o

for (i in 1:length(alphas)) {
  
  Results_glmnet_tibble_temp <- 
    
    glance(lista_modelos[[i]]) %>% 
    select(-nobs) %>% 
    mutate(estimate_lambdamin = tidy(lista_modelos[[i]]) %>% filter(lambda == glance(lista_modelos[[i]])$lambda.min) %>% pull(estimate),
           estimate_lamda1se = tidy(lista_modelos[[i]]) %>% filter(lambda == glance(lista_modelos[[i]])$lambda.1se) %>% pull(estimate),
           alpha = alphas[i])
  
  Results_glmnet_tibble <- rbind(Results_glmnet_tibble, Results_glmnet_tibble_temp)
  
}

grafico_rm <- Results_glmnet_tibble %>%
  set_names(names(.) %>% str_replace("estimate_", "")) %>% 
  gather(key = "lambda", value = "estimate", -c("lambda.min", "lambda.1se", "alpha")) %>% 
  ggplot(aes(x = alpha, y = estimate, color = lambda)) + 
  geom_line(size = 1) + geom_point() + 
  ggrepel::geom_label_repel(aes(label = round(estimate,4))) + 
  facet_wrap(~lambda, scales = "free_y") + 
  theme(legend.position = "bottom")


  # Ploteamos en un grid todos los path de lambdas de cada alpha

# cowplot::plot_grid(p1, p2, nrow = 1) # no permite graficar los plots de glmnet

lista_modelos_tibble_tidy <- tibble(x = lista_modelos) %>% 
  mutate(tidy = map(.$x, tidy),
         alpha = alphas,
         glance = map(.$x, glance)) %>%
  unnest(c(tidy, glance)) %>% 
  select(-c(x, nobs))

lista_modelos_tibble_tidy_grouped <-lista_modelos_tibble_tidy %>% 
  group_by(alpha) %>% 
  summarise(lambda.min = min(lambda.min),
            lambda.1se = min(lambda.1se),
            estimate = min(estimate)) %>% 
  ungroup()

lista_modelos_tibble_tidy %>% 
  ggplot(aes(x = log(lambda),y = estimate, color = alpha)) + 
  geom_line(size = 1) + 
  geom_point(size = 0.25) + 
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high, color = alpha), alpha = .25)+
  geom_vline(aes(xintercept = log(lambda.min), color = alpha), data = lista_modelos_tibble_tidy_grouped, lty = 2) + 
  geom_vline(aes(xintercept = log(lambda.1se), color = alpha), data = lista_modelos_tibble_tidy_grouped, lty = 2) +
  # geom_label(aes(label = min(estimate), color = alpha), size = 0.2) + 
  facet_wrap(~ alpha,
             ncol  = 4, 
             scales = "free") 
  # ggrepel::geom_label_repel(aes(label = round(min(estimate),6)), size = 2, data = lista_modelos_tibble_tidy %>% filter(lambda.min == lambda))



# glance_cv <- broom::glance(lasso_poli)
# tidy_lasso <- broom::tidy(lasso_poli)
# tidy_glmnetfit_lasso <- broom::tidy(lasso_poli$glmnet.fit)
# 
# tidy_glmnetfit_lasso %>% filter(lambda == glance_cv$lambda.min)
# 
# tidy_lasso %>% ggplot(aes(log(lambda), estimate)) +
#   geom_line(size =1) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) + 
#   geom_vline(xintercept = log(glance_cv$lambda.min)) + 
#   geom_vline(xintercept = log(glance_cv$lambda.1se), lty = 2)

# PENDIENTE GR?FICO 3d lambda, alpha, desvianza

# 5. AJUSTE MODELOS CON H20 ----

# 6. Interpretabilidad Elastic nets ----


# 7. Detecci?n de interacciones con Earth ----







  






















