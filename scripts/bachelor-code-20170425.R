##Bachelor Arbeit Code
rm(list=ls())
library(ggplot2)
##simple lineare Regression
## _s steht für die simple lineare Regression
n.xtrue_s <- c(10,50,100,500)#Anzahl der Stichprobengrößen
x.mean <-  0
var.xtrue_s <- c(15, 55, 255)   #Verschiedene Varianzen bei x.true
var.y.error_s <- c(15, 55, 255) #Verschiedene Fehler Varianzen für den Fehler bei Y (regressionsfehler)
Rel._s <- seq(0.1, 1, 0.1)      #Reliabilität bei Fehler der X Variablen 
repl.n_s <- 1000                  #Anzahl der Wiederholungen bei replicate
beta0_s <- 0                    #Intercept
beta1_s <- 1                    #Slope

##Macht eine Matrix mit allen möglichen Zusammenstellungen
param <- expand.grid(n.xtrue_s, var.xtrue_s,var.y.error_s, repl.n_s, Rel._s, beta0_s, beta1_s)



##setzen eines Seeds zwecks replizierbarkeit
set.seed(2018)
##Simulation des Datensatzes für die simple lineare Regression
list_data_simulated_simple_regression <-apply(param, 1, wrapper_simple)
head(list_data_simulated_simple_regression[[1]][3,])


### Funktionen um die Schätzwerte der Regression zu erhalten
## zuerst den Mittelwert des Schätzwertes von beta
b.dach_mean_simple <- b.dach_mean_funktion_simple(list_data_simulated_simple_regression)

##dann die durchschnittliche quadratische Abweichung von beta.dach
#input ist die Liste mit den Daten der simulierten simplen Regression
b.dach_var_simple <-  b.dach_var_funktion_simple(list_data_simulated_simple_regression)

###dann der dazugehörige p.value
##input ist die Liste mit den Daten der simulierten simplen Regression
p.value_simple <- p.value_power_funktion_simple(list_data_simulated_simple_regression)
#r.hoch2
r.hoch2_simple <- r.hoch2_mean_funktion_simple(list_data_simulated_simple_regression)

head(list_data_simulated_simple_regression[[1]][2,])

## Mittelwerte der Parameter Schätzungen
b.dach_mean_simple
p.value_simple

r.hoch2_simple
##Varianz der Mittelwerte
b.dach_var_simple

##durschnittliche quadratische Abweichung von beta.True
mse.b.dach_simple <- (b.dach_mean_simple-1)^2+b.dach_var_simple

colnames(param) <- c("n.true", "var.xtrue", "var.y.error", "repl.n", "rel.", "beta0", "beta1" )
r.hoch2 <-param$var.xtrue / (param$var.xtrue+ param$var.y.error)

bias.b_simple <- 1-Daten_simple$b.dach_mean_simple

Daten_simple <- cbind(param,r.hoch2, b.dach_mean_simple, b.dach_var_simple, 
                      mse.b.dach_simple, p.value_simple, r.hoch2_simple, bias.b_simple)


Daten_simple
dummy_cod_n.true_s <- factor(n.xtrue_s)
dummy_cod_var.y.error <- factor(var.y.error_s)
Daten_simple <- cbind(Daten_simple, dummy_cod_n.true_s, dummy_cod_var.y.error)
Daten_simple <- tbl_df(Daten_simple)
Daten_simple

###Auswertung Regression und Varianzanalyse
##Regression um zu zeigen, dass die Minderungskorrktur richtig vorhergesagt wurde

apa.reg.table(lm(b.dach_mean_simple~ n.true + var.xtrue + var.y.error +rel. + r.hoch2   ,  data = Daten_simple), filename = "mean_simple")


anova(mylm1)
round(mylm1$coefficients, digits = 2)


summary(lm(b.dach_mean_simple~rel.    ,  data = Daten_simple))
summary(lm(b.dach_mean_simple~var.xtrue    ,  data = Daten_simple))

plot(x = Daten_simple$b.dach_mean_simple, Daten_simple$rel.)

ggplot(Daten_simple, aes( x = rel., y = b.dach_mean_simple))+
  xlab("Reliabilität der X-Variable")+
  ylab("Schätzwert der Steigung Beta")+
  scale_x_continuous(  breaks = seq(0.1,1,0.1))+
  scale_y_continuous(  breaks = seq(0.1,1,0.1))+
  geom_jitter(alpha = 0.5)+
  geom_smooth(method = "lm")+
  apatheme

## bias b simple

apa.reg.table( lm(bias.b_simple~ n.true + var.xtrue + var.y.error +rel. + r.hoch2   ,  data = Daten_simple), filename = "bias_simple")

ggplot(Daten_simple, aes(x = rel., y = bias.b_simple ))+
  xlab("Reliabilität der X-Variable")+
  ylab("Bias der Schätzung")+
  scale_x_continuous(breaks = seq(0.1,1,0.1))+
  scale_y_continuous(breaks = seq(0.0,1,0.1))+
  geom_jitter(alpha = 0.5)+
  geom_smooth(method = "lm")+
  apatheme


##Varianzanalyse um zu schauen ob interaktion zwischen der Varianz der Schätzwertes vorhanden
##ist mit n.true und Varianz des Y-fehler
summary(aov(b.dach_var_simple~Daten_simple$dummy_cod_n.true_s * Daten_simple$dummy_cod_var.y.error, data = Daten_simple))
apa.aov.table (aov(b.dach_var_simple~Daten_simple$dummy_cod_n.true_s * Daten_simple$dummy_cod_var.y.error, data = Daten_simple))
apa.aov.table ( aov(b.dach_var_simple~dummy_cod_n.true_s, data = Daten_simple))

apa.aov.table(aov1)


aov1$coefficients ##bei verfünffachen der Stichprobe wird bereits fast die ganze Varianz aufgeklärt

apa.reg.table( lm(b.dach_var_simple ~n.true + var.xtrue + var.y.error +rel. + r.hoch2   , data = Daten_simple ), filename = "var_simple")

ggplot(Daten_simple, aes(x = rel., y = b.dach_var_simple, col = factor(r.hoch2)))+
  geom_jitter()+
  facet_grid(~n.true, labeller = labeller(
    n.true = c(`10` = "n = 10", `50` = "n = 50", `100` = "n = 100", `500` = "n = 500")))+
  xlab("Reliabilität der X-Variable")+
  ylab("Varianz des Schätzwertes")+
  scale_x_continuous(breaks = seq(.0,1,0.25))+
  scale_color_discrete(name = expression(paste("Varianzaufklärung ", italic(R^2))), labels = c(0.05, 0.17, 0.21, 0.5, 0.78, 0.82, 0.94))+
  apatheme.legend


  # Analyse von r.hoch_simple
aov(lm(r.hoch2_simple~r.hoch2 + n.true + var.y.error+ var.xtrue, data = Daten_simple ))$coefficients
apa.reg.table(lm(r.hoch2_simple~ n.true+ var.xtrue + var.y.error + rel.+  r.hoch2    , data = Daten_simple ), filename = "r.hoch2_simple")

ggplot(Daten_simple, aes(x =r.hoch2, y= r.hoch2_simple, col = factor(rel.)))+
  geom_jitter()+
  xlab(expression(paste("wahre Varianzaufklärung ", italic(R^2))))+
  geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.3)+
  facet_grid(~n.true, labeller = labeller(
    n.true = c(`10` = "n = 10", `50` = "n = 50", `100` = "n = 100", `500` = "n = 500")))+
  ylab(expression(paste("geschätzte Varianzaufklärung ", italic(R^2))))+
  scale_color_discrete(name = "Reliabilität X-Variable")+
  scale_x_continuous( breaks = seq(0.0,1,0.25))+
  scale_y_continuous( breaks = seq(0.0,1,0.25))+
  apatheme.legend.space

##analyse der power
lm(p.value_simple~n.true + var.xtrue + var.y.error +rel. + r.hoch2 , data = Daten_simple)
apa.reg.table(lm(p.value_simple~n.true + var.xtrue + var.y.error +rel. + r.hoch2 , data = Daten_simple), filename = "power_simple")

ggplot(Daten_simple, aes (x = rel., y = p.value_simple, col =factor(r.hoch2)))+
  facet_grid(~n.true, labeller = labeller(
    n.true = c(`10` = "n = 10", `50` = "n = 50", `100` = "n = 100", `500` = "n = 500")))+
  geom_jitter()+
  xlab("Reliabilität der X-Variable")+
  ylab("Power der X-Variable")+
  scale_y_continuous( breaks = seq(0.0,1,0.2))+
  scale_x_continuous( breaks = seq(0.0,1,0.2))+
  scale_color_discrete(name = expression(paste("wahre Varianzaufklärung ",italic(R^2))), labels = c(0.05, 0.17, 0.21, 0.5, 0.78, 0.82, 0.94))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(0.8, "lines"),
        axis.line=element_line(),
        text=element_text(family='Times'))

##MSE

lm(mse.b.dach_simple~n.true + var.xtrue + var.y.error +rel. + r.hoch2 , data = Daten_simple)
apa.reg.table(lm(mse.b.dach_simple~n.true + var.xtrue + var.y.error + rel. + r.hoch2 , data = Daten_simple), filename = "mse_simple.docx")

ggplot(Daten_simple, aes(y = mse.b.dach_simple, x = rel., col = factor(r.hoch2)))+
  geom_jitter()+
  xlab("Reliabilität der X-Variable")+
  ylab("Mittlere quadratische Abweichung")+
  scale_y_continuous(breaks = seq(0,4,.25))+
  facet_grid(~n.true, labeller = labeller(
    n.true = c(`10` = "n = 10", `50` = "n = 50", `100` = "n = 100", `500` = "n = 500")))+
  scale_color_discrete(name = expression(paste("wahre Varianzaufklärung ", italic(R^2))), labels = c(0.05, 0.17, 0.21, 0.5, 0.78, 0.82, 0.94))+
  geom_smooth(method = "loess")+
  apatheme.legend



###Mulitple lineare Regression

n.xtrue_m <- c(10,50, 100, 500)  
x1mean_m <- 0
x1sd_m <- 1
x2mean_m <- 0
x2sd_m <- 1
Correlation.x_m <- c(0, sqrt(0.25), sqrt(0.5))
e1mean_m <- 0
Rel.x1_m <- seq(0.6, 1 ,0.1)   #1
e2mean_m <- 0
Rel.x2_m <- seq(0.6, 1 ,0.1)  #1
correlation.e_m <- c(0, sqrt(0.25), sqrt(0.5))
R.hoch2_m <- seq(0.1,0.9,0.1) 
repl.n_m <- 1000
beta0_m <- 0
beta1_m <- 1
beta2_m <- 1

##alle möglichen Zusammensetzungen durch expand.grid
#output Matrix über die apply funktion angewendet wird
param2 <- expand.grid(n.xtrue_m, x1mean_m, x1sd_m, x2mean_m, x2sd_m, correlation.x_m, e1mean_m,
                      Rel.x1_m, e2mean_m, Rel.x2_m, correlation.e_m, R.hoch2_m, repl.n_m, beta0_m, beta1_m, beta2_m)
colnames(param2) <- c("n.xtrue","x1mean","x1sd","x2mean","x2sd","correlation.x","e1mean","Rel.x1","e2mean","Rel.x2","correlation.e","R.hoch2","repl.n","beta0","beta1","beta2")


##setzen eines Seeds zwecks replizierbarkeit
set.seed(2018)
##Simulation des Datensatzes für die simple lineare Regression
list_data_simulated_multiple_regression <-apply(param2, 1, wrapper_multiple)

#oder

list_data_simulated_multiple_regression1_1999<- apply(param2[1:1999,],1, wrapper_multiple)
list_data_simulated_multiple_regression2000_3999<- apply(param2[2000:3999,],1, wrapper_multiple)
list_data_simulated_multiple_regression4000_5999<- apply(param2[4000:5999,],1, wrapper_multiple)
list_data_simulated_multiple_regression6000_8100<- apply(param2[6000:8100,],1, wrapper_multiple)

list_data_simulated_multiple_regression <- c(list_data_simulated_multiple_regression1_1999,
                                             list_data_simulated_multiple_regression2000_3999,
                                             list_data_simulated_multiple_regression4000_5999,
                                             list_data_simulated_multiple_regression6000_8100)



### Funktionen um die Schätzwerte der Regression zu erhalten
##beta dach1
b.dach1_mean_multiple <- b.dach1_mean_funktion_multiple(list_data_simulated_multiple_regression)
##beta dach2
b.dach2_mean_multiple <- b.dach2_mean_funktion_multiple(list_data_simulated_multiple_regression)
##p.value1
p.value1_multiple <- p.value1_power_funktion_multiple(list_data_simulated_multiple_regression)
##p.value2
p.value2_multiple <- p.value2_power_funktion_multiple(list_data_simulated_multiple_regression)
##r.hoch2
r.hoch2_multiple <- r.hoch2_mean_funktion_multiple(list_data_simulated_multiple_regression)

##Varianz der Parameterschätzungen beta1 und beta2
b.dach1_var_multiple <- b.dach1_var_funktion_multiple(list_data_simulated_multiple_regression)
b.dach2_var_multiple <- b.dach2_var_funktion_multiple(list_data_simulated_multiple_regression)

##durschnittliche quadratische Abweichung von beta.True
mse.b.dach1_multiple <- (b.dach1_mean_multiple-1)^2 + b.dach1_var_multiple
mse.b.dach2_multiple <- (b.dach2_mean_multiple-1)^2 + b.dach2_var_multiple


##Datensatz ergänzungen für Varianzanalyse
dummy_cod_n.true_multiple <- as.factor(n.xtrue_m)
dummy_cod_correlation.x_m <- as.factor(correlation.x_m)
dummy_cod_correlation.e_m <- as.factor(correlation.e_m)
bias_b.dach1_multiple <- 1 - Daten_multiple$b.dach1_mean_multiple
bias_b.dach2_multiple <- 1 - Daten_multiple$b.dach2_mean_multiple

Daten_multiple <- cbind(param2, b.dach1_mean_multiple, b.dach2_mean_multiple ,b.dach1_var_multiple,
                             b.dach2_var_multiple,mse.b.dach1_multiple , mse.b.dach2_multiple,
                             p.value1_multiple, p.value2_multiple, r.hoch2_multiple)

Daten_multiple <- cbind(Daten_multiple, bias_b.dach1_multiple, bias_b.dach2_multiple)

Daten_multiple <- tbl_df(Daten_multiple)
Daten_multiple

getwd()

##Auswertung
##Regression und Varianzanalyse

str(Daten_multiple)

lm(b.dach1_mean_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, 
   data = Daten_multiple)

apa.reg.table(lm(b.dach1_mean_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, 
                 data = Daten_multiple), filename="Table1_b.dach1_mean.doc", table.number=1)

summary(lm(b.dach1_mean_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2,
           data = Daten_multiple))


ggplot(Daten_multiple, aes(x   = Rel.x1, y =b.dach1_mean_multiple, col = factor(Rel.x2) ))+
  geom_jitter(alpha = 0.5)+
  xlab("Reliabilität der x1- Variable")+
  ylab("Schätzwert von Beta1")+
  facet_grid(correlation.e~correlation.x, 
             labeller = labeller(
    correlation.x = c(`0` = "Korrelation x-Variablen: 0", `0.5` = "Korrelation x: 0.5", `0.707106781186548` = "Korrelation x: 0.7"),
    correlation.e = c(`0` = "Korrelation Fehler: 0", `0.5` = "Korrelation Fehler: 0.5", `0.707106781186548` = "Korrelation Fehler: 0.7")
    ))+
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.3)+
  scale_y_continuous( breaks = seq(0.1,1.6,0.1))+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  apatheme.legend

###besser so 
ggplot(Daten_multiple, aes(x   = Rel.x1, y =b.dach1_mean_multiple, col = factor(Rel.x2) ))+
  geom_jitter(alpha = 0.5)+
  xlab("Reliabilität der x1- Variable")+
  ylab("Schätzwert von Beta1")+
  facet_grid(correlation.e_factor~correlation.x_factor, 
             labeller = label_parsed)+
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.3)+
  scale_y_continuous( breaks = seq(0.1,1.6,0.1))+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  apatheme.legend

apa.reg.table(lm(b.dach2_mean_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2,
           data = Daten_multiple), filename = "bdach_mean_multiple.docx")


#gleicher Plot wie bei b.dach1
ggplot(Daten_multiple, aes(x   = Rel.x2, y =b.dach2_mean_multiple, col = factor(Rel.x1) ))+
  geom_jitter(alpha = 0.5)+
  xlab("Reliabilität der x1- Variable")+
  ylab("Schätzwert von Beta2")+
  facet_grid(correlation.e~correlation.x)+
  scale_y_continuous( breaks = seq(0.1,1.6,0.1))+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  apatheme.legend

## schön um Reliabilität von x2
ggplot(Daten_multiple, aes(x   = Rel.x1, y =b.dach2_mean_multiple, col = factor(Rel.x2) ))+
  geom_jitter(alpha = 0.5)+
  xlab("Reliabilität der x1- Variable")+
  ylab("Schätzwert von Beta2")+
  facet_grid(correlation.e_factor~correlation.x_factor, 
             labeller = label_parsed)+
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.3)+
  scale_y_continuous( breaks = seq(0.1,1.6,0.1))+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  apatheme.legend

summary(lm(b.dach2_mean_multiple~n.xtrue + poly(Rel.e1,2) + poly(Rel.e2,2) + correlation.x + correlation.e + R.hoch2,
           data = Daten_multiple))

ggplot(Daten_multiple, aes(x = Rel.x2 , y = b.dach2_mean_multiple, col = factor(Rel.x1) ))+
  geom_jitter(alpha = 0.5)+
  facet_grid(~n.xtrue)+
  xlab("Reliabilität der x2- Variable")+
  ylab("Schätzwert von Beta2")+
  geom_smooth(method = "lm")+
  scale_y_continuous( breaks = seq(0.1,1.6,0.1))+
  scale_color_discrete(name = "Reliabilität der x1-Variable")+
  apatheme.legend
  

Daten_multiple_REl.x1 <- Daten_multiple%>%
  filter(Rel.x1 == 1)

ggplot(Daten_multiple_REl.x1, aes (x = b.dach1_mean_multiple ))+
 geom_density()+
  geom_density(data = Daten_multiple_REl.x1, aes( x = b.dach1_mean_multiple, fill = factor(Rel.x2)))
  


ggplot(Daten_multiple, aes (x = b.dach1_mean_multiple, col = factor(Rel.x1)))+
  geom_density()+
  scale_color_discrete(name = "Reliabilität der x1-Variable")+
  scale_x_continuous(breaks = seq(0.1,1.6, 0.1))+
  xlab("Schätzwert von beta1")+
  ylab("Dichte")+
  apatheme.legend

ggplot(Daten_multiple, aes ( y = b.dach1_mean_multiple, x = factor(Rel.x1), col = factor(correlation.x)))+
  facet_grid(~Rel.x2)+
  scale_y_continuous(breaks = seq(0.1,1.6, 0.1))+
  xlab("Reliabilität der x1-Variable")+
  ylab("Schätzwert von beta1")+
  geom_jitter()+
  scale_color_discrete(name = "Korrelation der x-Variable")+
  apatheme.legend

ggplot(Daten_multiple, aes (y = b.dach1_mean_multiple, x = factor(correlation.x), col = factor(Rel.x1)))+
  geom_jitter()+
  facet_grid(~correlation.e)+
  apatheme

ggplot(Daten_multiple, aes (y = b.dach1_mean_multiple, x = factor(correlation.e), col = factor(Rel.x1)))+
  facet_grid(~correlation.x_factor, labeller = label_parsed)+
  scale_y_continuous(breaks = seq(0.1,1.6, 0.1))+
  ylab("Schätzwert von beta1")+
  xlab("Korrelationen der Fehler")+
  scale_color_discrete( name = "Reliabilität der x1-Variable")+
  scale_x_discrete(labels = c(0,expression(sqrt(0.25)), expression(sqrt(0.5))))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes (y = b.dach2_mean_multiple, x = factor(correlation.e), col = factor(Rel.x2)))+
  facet_grid(~correlation.x)+
  scale_y_continuous(breaks = seq(0.1,1.6, 0.1))+
  ylab("Schätzwert von beta2")+
  xlab("Korrelationen der Fehler")+
  scale_color_discrete( name = "Reliabilität der x2-Variable")+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes(x =Rel.x2, y = b.dach2_mean_multiple, col = factor(correlation.x)))+
  facet_grid(~Rel.x1)+
  geom_jitter(alpha = 0.5)+
  ylab("Schätzwert von beta2")+
  xlab("Reliabilität der x2-Variable")+
  scale_y_continuous(breaks = seq(0.1,1.6, 0.1))+
  scale_color_discrete( name = "Korrelation der x-Variablen")+
  apatheme.legend


ggplot(Daten_multiple, aes(y = b.dach2_mean_multiple, x = Rel.x2, col = Rel.x1))+
  geom_jitter()

ggplot(Daten_multiple, aes(x = b.dach1_mean_multiple, y = b.dach2_mean_multiple, col = factor(correlation.x) ))+
  facet_grid(Rel.x2~Rel.x1, labeller = label_both)+
  xlab("Schätzwert beta1")+
  ylab("Schätzwert beta2")+
  geom_vline(xintercept = 1.0, linetype = 2, alpha = 0.3)+
  geom_hline(yintercept = 1.0, linetype = 2, alpha = 0.3)+
  scale_x_continuous(breaks = seq(0.4,1.6,0.2), expand =c(0.000001,0.009) )+
  scale_y_continuous(breaks = seq(0.4,1.6,0.2))+
  scale_color_discrete(name = "Korrelation der x-Variablen", labels = c(0, expression(sqrt(.25)), expression(sqrt(.5)) ))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_spezial, aes(x = b.dach1_mean_multiple, y = b.dach2_mean_multiple, col = factor(correlation.x) ))+
  facet_grid(Rel.x2~Rel.x1, labeller = label_both)+
  xlab("Schätzwert beta1")+
  ylab("Schätzwert beta2")+
  geom_vline(xintercept = 1.0, linetype = 2, alpha = 0.3)+
  geom_hline(yintercept = 1.0, linetype = 2, alpha = 0.3)+
  scale_x_continuous(breaks = seq(0.4,1.6,0.2), expand =c(0.000001,0.009) )+
  scale_y_continuous(breaks = seq(0.4,1.6,0.2))+
  scale_color_discrete(name = "Korrelation der X-Variablen", labels = c(0, expression(sqrt(.5))))+
  geom_jitter(size = 0.5)+
  apatheme.legend

ggplot(Daten_multiple, aes(x = b.dach1_mean_multiple, y = b.dach2_mean_multiple, col = factor(correlation.e) ))+
  facet_grid(Rel.x2~Rel.x1, labeller = label_both)+
  xlab("Schätzwert beta1")+
  ylab("Schätzwert beta2")+
  geom_vline(xintercept = 1.0, linetype = 2, alpha = 0.3)+
  geom_hline(yintercept = 1.0, linetype = 2, alpha = 0.3)+
  scale_x_continuous(breaks = seq(0.4,1.6,0.2), expand =c(0.000001,0.009) )+
  scale_y_continuous(breaks = seq(0.4,1.6,0.2))+
  scale_color_discrete(name = "Korrelation der Fehler", labels = c(0, expression(sqrt(.25)), expression(sqrt(.5)) ))+
  geom_jitter()+
  apatheme.legend

##Beta dach Mean 1 & 2 fertig 

#Bias b1

apa.reg.table(lm(bias_b.dach1_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2,
           data = Daten_multiple), filename = "bias_b1_multiple.docx")

ggplot(Daten_multiple, aes(x = Rel.x1, y = bias_b.dach1_multiple, col = factor(Rel.x2)))+
  facet_grid(~correlation.x)+
  xlab("Reliabilität der x1-Variable")+
  ylab("Bias des Schätzwert beta1")+
  scale_y_continuous(breaks = seq(-0.5,0.5,0.1))+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes(x = Rel.x1, y = bias_b.dach1_multiple, col = factor(Rel.x2)))+
  facet_grid(~correlation.e)+
  xlab("Reliabilität der x1-Variable")+
  ylab("Bias des Schätzwert beta1")+
  scale_y_continuous(breaks = seq(-0.5,0.5,0.1))+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  geom_jitter()+
  apatheme.legend

#bias b2

apa.reg.table(lm(bias_b.dach2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2,
           data = Daten_multiple), filename = "bias_b2_multiple.docx")

ggplot(Daten_multiple, aes(x = Rel.x2, y = bias_b.dach2_multiple, col = factor(Rel.x1)))+
  facet_grid(~correlation.x)+
  xlab("Reliabilität der x2-Variable")+
  ylab("Bias des Schätzwert beta2")+
  scale_y_continuous(breaks = seq(-0.5,0.5,0.1))+
  geom_smooth(method = "lm")+
  scale_color_discrete(name = "Reliabilität der x1-Variable")+
  geom_jitter()+
  apatheme.legend

##Varianz Beta dach1

lm(b.dach1_var_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(b.dach1_var_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple),
              filename = "var1_multiple.docx")

ggplot(Daten_multiple, aes( y = b.dach1_var_multiple, x = R.hoch2, col = factor(correlation.x) ))+
  facet_grid(n.xtrue~Rel.x1)+
  geom_jitter()+
  geom_smooth(method = "loess", size = 1)+
  apatheme.legend

## Das ist besser
ggplot(Daten_multiple, aes( y = b.dach1_var_multiple, x = R.hoch2, col = factor(n.xtrue) ))+
  facet_grid(~correlation.x_factor, labeller = label_parsed)+
  geom_jitter()+
  geom_smooth(method = "loess", size = 1)+
  ylab("Varianz des Schätzwert beta1")+
  xlab(expression(paste("Varianzaufklärung ",italic(R^2) )))+
  scale_color_discrete(name = "Stichprobengröße")+
  apatheme.legend

###ggplot(Daten_multiple, aes( y = b.dach1_var_multiple, x = R.hoch2, col = factor(n.xtrue) ))+
  #facet_grid(~Rel.e1)+
  #geom_jitter()+
  #geom_smooth(method = "loess", size = 1)+
  #apatheme

##Varianz b dach2

lm(b.dach2_var_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(b.dach2_var_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 +
                   correlation.e+ R.hoch2, data = Daten_multiple), filename = "var2_multiple.docx")

ggplot(Daten_multiple, aes( y = b.dach2_var_multiple, x = R.hoch2, col = factor(n.xtrue) ))+
  facet_grid(~correlation.e)+
  geom_jitter()+
  geom_smooth(method = "loess", size = 1)+
  ylab("Varianz des Schätzwert beta2")+
  xlab("R.hoch2")+
  scale_color_discrete(name = "Stichprobengröße")+
  apatheme.legend

ggplot(Daten_multiple, aes (x = b.dach1_var_multiple, y = b.dach2_var_multiple, col = factor(R.hoch2) ))+
  facet_grid(correlation.e_factor ~correlation.x_factor, labeller = label_parsed)+
  xlab("Varianz des Schätzwert Beta1")+
  ylab("Varianz des Schätzwert Beta2")+
  scale_color_discrete(name = expression(paste("Varianzaufklärung ", italic(R^2) )))+
  geom_jitter(alpha = 0.5, size = 1)+
  apatheme.legend

ggplot(Daten_spezial, aes (x =b.dach1_var_multiple, y = b.dach2_var_multiple, col = factor(n.xtrue)))+
  facet_grid(R.hoch2_factor~correlation.x_factor, labeller = label_parsed)+
  geom_jitter()+
  ylab("Schätzwert der Varianz von Beta2")+
  xlab("Schätzwert der Varianz von Beta1")+
  scale_color_discrete(name = "Stichprobengröße")+
  apatheme.legend



## MSE1

lm(mse.b.dach1_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(mse.b.dach1_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 +
                   correlation.e+ R.hoch2, data = Daten_multiple), filename = "mse1_multiple.docx")

ggplot(Daten_multiple, aes ( y = mse.b.dach1_multiple, x  =factor(R.hoch2), col = factor(correlation.x)))+
  facet_grid(~n.xtrue)+
  xlab(expression(paste("Varianzaufklärung ", italic(R^2))))+
  ylab("mittlere quadratische Abweichung vom Schätzwert beta1")+
  scale_color_discrete(name = "Korrelation der x-Variablen", labels = c(0, expression(sqrt(.25)), expression(sqrt(.5)) ))+
  geom_jitter()+
  apatheme.legend


lm(mse.b.dach2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(mse.b.dach2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 +
                   correlation.e+ R.hoch2, data = Daten_multiple), filename = "mse2_multiple.docx")

ggplot(Daten_multiple, aes ( y = mse.b.dach2_multiple, x = factor(R.hoch2), col = factor(correlation.x)))+
  facet_grid(~n.xtrue)+
  xlab("R.hoch2")+
  ylab("mittlere quadratische Abweichung vom Schätzwert beta2")+
  scale_color_discrete(name = "Korrelation der x-Variablen")+
  geom_jitter()+
  apatheme.legend


ggplot(Daten_multiple, aes (x = mse.b.dach1_multiple, y =mse.b.dach2_multiple, col = factor(R.hoch2)))+
  facet_grid(correlation.e_factor~correlation.x_factor, labeller = label_parsed)+
  xlab("mittlere quadratische Abweichung vom wahren Beta1")+
  ylab("mittlere quadratische Abweichung vom wahren Beta2")+
  scale_color_discrete(name = expression(paste("Varianzaufklärung ", italic(R^2))))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_spezial, aes (x =mse.b.dach1_multiple,  y = mse.b.dach2_multiple, col = factor(n.xtrue)))+
  facet_grid (Rel.x1 +Rel.x2~correlation.x_factor+R.hoch2_factor, labeller = labeller(.rows = label_both,.cols = label_parsed))+
  geom_jitter()+
  ylab("mittlere quadratische Abweichung vom wahren Beta2")+
  xlab("mittlere quadratische Abweichung vom wahren Beta1")+
  scale_color_discrete(name = "Stichprobengröße")+
  apatheme.legend


##Fertig
##p.value 1
lm(p.value1_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(p.value1_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2,
                 data = Daten_multiple), filename = "pvalue1_multiple.docx")

ggplot(Daten_multiple, aes (y = p.value1_multiple,x = factor(Rel.x1) , col = factor(R.hoch2) ))+
  facet_grid(correlation.x~n.xtrue, labeller = labeller(.rows = label_parsed)) +
  ylab("Power der x1-Variable")+
  xlab("Reliabilität der x1-Variable")+
  scale_color_discrete(name = expression(paste("Varianzaufklärung ", italic(R^2))))+
  scale_x_discrete(breaks = seq(0.6,1,.1))+
  geom_jitter(alpha = 0.5)+
  geom_smooth(method = "loess")+
  apatheme.legend


##p.value 2
lm(p.value2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(p.value2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2,
                 data = Daten_multiple), filename = "pvalue2_multiple.docx")

ggplot(Daten_multiple, aes (y = p.value2_multiple,x = factor(R.hoch2), col = factor(Rel.e2) ))+
  facet_grid(~n.xtrue)+
  geom_smooth(method = "loess")+
  geom_jitter( size = .5)+
  xlab("R.hoch2")+
  ylab("Power der x2-Variable")+
  scale_color_discrete(name = "Reliabilität der x2-Variable")+
  geom_jitter()+
  apatheme.legend


##Spezial Fall

ggplot(Daten_spezial, aes (x = p.value1_multiple, y = p.value2_multiple, col = factor(R.hoch2)))+
  facet_grid(n.xtrue~Rel.x1 + Rel.x2, labeller = labeller (.cols = label_both))+
  geom_jitter()+
  xlab("Power der x1-Variable")+
  ylab("Power der x2-Variable")+
  scale_color_discrete(name = expression(paste("Varianzaufklärung ", italic(R^2))))+
  apatheme.legend


#r.hoch2 multiple
lm(r.hoch2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+ R.hoch2, data = Daten_multiple)
apa.reg.table(lm(r.hoch2_multiple~n.xtrue +correlation.x + Rel.x1 + Rel.x2 + correlation.e+R.hoch2,
                 data = Daten_multiple), filename = "rhoch2_multiple.docx")

ggplot(Daten_multiple, aes (x = r.hoch2_multiple,y = factor(R.hoch2), col = factor(Rel.x2) ))+
  facet_grid(~Rel.x1)+
  ylab("R.hoch2")+
  xlab("Schätzwert R.hoch2")+
  scale_colour_discrete(name = "Reliabilität der x2-Variable")+
  scale_x_continuous(breaks = seq(0.2,1.1,0.2))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes(x = r.hoch2_multiple,y = factor(n.xtrue), col = factor(R.hoch2)))+
  facet_grid(Rel.x1~Rel.x2)+
  ylab("R.hoch2")+
  xlab("SChätzwert R.hoch2")+
  scale_colour_discrete(name = "Reliabilität der x2-Variable")+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes (x = r.hoch2_multiple,y = factor(R.hoch2), col = factor(correlation.e) ))+
  facet_grid(~correlation.x)+
  ylab("R.hoch2")+
  xlab("Schätzwert R.hoch2")+
  scale_colour_discrete(name = "Reliabilität der x2-Variable")+
  scale_x_continuous(breaks = seq(0.2,1.1,0.2))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes (y = r.hoch2_multiple, x = factor(R.hoch2), col = factor(n.xtrue)))+
  geom_jitter()

ggplot(Daten_multiple, aes (y = r.hoch2_multiple, x = R.hoch2, col = factor(n.xtrue) ))+
  ylab(expression(paste("geschätzte Varianzaufklärung ", italic(R^2))))+
  xlab(expression(paste("wahre Varianzaufklärung ", italic(R^2))))+
  scale_x_continuous( breaks = seq(0.1,1,0.1))+
  scale_y_continuous( breaks = seq(0.1,1,0.1))+
  scale_color_discrete(name = "Stichprobengröße")+
  geom_jitter(size = 0.5, alpha =0.5)+
  geom_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.7)+
  apatheme.legend

## unter tricks schauen für Gerade (1)
ggplot(Daten_spezial, aes (x = factor(R.hoch2_factor), y = r.hoch2_multiple, col = factor(correlation.x)))+
  facet_grid(n.xtrue~Rel.x1+ Rel.x2, labeller = labeller(.cols = label_both))+
  scale_y_continuous(breaks = c(0.1,0.3,0.5))+
  xlab(expression(paste("wahre Varianzaufklärung ", italic(R^2))))+
  ylab(expression(paste("geschätzte Varianzaufklärung ", italic(R^2))))+
  scale_color_discrete(name = "Korrelation der X-Variablen", labels = c(0, expression(sqrt(.5))))+
  geom_point()+
  geom_abline(intercept = -0.3, slope = 0.4, linetype = 2, alpha = 0.5)+
  apatheme.legend

##Auswertung erster Teil finito

##Zusatz auswertungen



ggplot(Daten_multiple, aes(x = b.dach1_mean_multiple, y = b.dach2_mean_multiple, col = factor(correlation.e) ))+
  facet_grid(Rel.x2~Rel.x1)+
  xlab("Schätzwert beta1")+
  ylab("Schätzwert beta2")+
  geom_vline(xintercept = 1.0, linetype = 2, alpha = 0.3)+
  geom_hline(yintercept = 1.0, linetype = 2, alpha = 0.3)+
  scale_x_continuous(breaks = seq(0.4,1.6,0.2), expand =c(0.000001,0.009) )+
  scale_y_continuous(breaks = seq(0.4,1.6,0.2))+
  scale_color_discrete(name = "Korrelation der Fehler", labels = c(0, expression(sqrt(.25)), expression(sqrt(.5)) ))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes (x = b.dach1_var_multiple, y = b.dach2_var_multiple, col = factor(R.hoch2) ))+
  facet_grid(Rel.x1~Rel.x2)+
  geom_jitter()


ggplot(Daten_multiple, aes (x = b.dach1_var_multiple, y = b.dach2_var_multiple, col = factor(R.hoch2) ))+
  facet_grid(~correlation.x, labeller = label_both )+
  xlab("Varianz des Schätzwert Beta1")+
  ylab("Varianz des Schätzwert Beta2")+
  scale_color_discrete(name = "R.hoch2")+
  geom_jitter()+
    apatheme.legend



ggplot(Daten_multiple, aes (x = mse.b.dach1_multiple, y =mse.b.dach2_multiple, col = factor(R.hoch2)))+
  facet_grid(Rel.x2~Rel.x1, labeller = label_both)+
  xlab("mittlere quadratische Abweichung vom wahren Beta1")+
  ylab("mittlere quadratische Abweichung vom wahren Beta2")+
  scale_color_discrete(name = "R.hoch2")+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_multiple, aes(x = p.value1_multiple, y = p.value2_multiple, col = factor(R.hoch2) ))+
  facet_grid(n.xtrue~correlation.x, labeller = label_both)+
  scale_x_continuous(breaks = seq(0.0,1,0.25), expand =c(0.000001,0.009))+
  geom_smooth(method = "loess")+
  xlab("Power der x1-Variable")+
  ylab("Power der x2-Variable")+
  scale_color_discrete(name = "Stichprobengröße")+
  geom_jitter()+
  apatheme.legend




ggplot(Daten_spezial, aes(y = r.hoch2_multiple, x = factor(R.hoch2), col = factor(correlation.e)))+
  facet_grid(correlation.x~Rel.x1 +Rel.x2, labeller = labeller( .cols =  label_both))+
  ylab("Geschätzte Varianzaufklärung")+
  xlab("Varianzaufklärung")+
  scale_color_discrete(name = "Korrelation der X-Variablen", labels = c(0,expression(sqrt(.5))))+
  geom_jitter()+
  apatheme.legend



##zusatz Can 13.April

Daten1_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.9 & R.hoch2 == 0.1 & correlation.x == sqrt(0.5) )

Daten1_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.9 & R.hoch2 == 0.1 & correlation.x == 0) 

Daten2_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.7 & R.hoch2 == 0.1 & correlation.x == sqrt(0.5)) 

Daten2_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.7 & R.hoch2 == 0.1 & correlation.x == 0) 

Daten3_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.7 & R.hoch2 == 0.1 & correlation.x == sqrt(0.5)) 

Daten3_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.7 & R.hoch2 == 0.1 & correlation.x == 0) 

Daten4_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.9 & R.hoch2 == 0.1 & correlation.x == sqrt(0.5)) 

Daten4_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.9 & R.hoch2 == 0.1 & correlation.x == 0) 

Daten5_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.9 & R.hoch2 == 0.5 & correlation.x == sqrt(0.5) )

Daten5_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.9 & R.hoch2 == 0.5 & correlation.x == 0) 

Daten6_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.7 & R.hoch2 == 0.5 & correlation.x == sqrt(0.5)) 

Daten6_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.9 & Rel.x2 == 0.7 & R.hoch2 == 0.5 & correlation.x == 0) 

Daten7_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.7 & R.hoch2 == 0.5 & correlation.x == sqrt(0.5)) 

Daten7_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.7 & R.hoch2 == 0.5 & correlation.x == 0) 

Daten8_Relx1_x2_rHoch2_cor_hoch <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.9 & R.hoch2 == 0.5 & correlation.x == sqrt(0.5)) 

Daten8_Relx1_x2_rHoch2_cor_null <- Daten_multiple%>%
  filter(Rel.x1 == 0.7 & Rel.x2 == 0.9 & R.hoch2 == 0.5 & correlation.x == 0) 


Daten_spezial <- rbind(Daten1_Relx1_x2_rHoch2_cor_hoch,Daten1_Relx1_x2_rHoch2_cor_null,
                       Daten2_Relx1_x2_rHoch2_cor_hoch,Daten2_Relx1_x2_rHoch2_cor_null,
                       Daten3_Relx1_x2_rHoch2_cor_hoch,Daten3_Relx1_x2_rHoch2_cor_null,
                       Daten4_Relx1_x2_rHoch2_cor_hoch,Daten4_Relx1_x2_rHoch2_cor_null,
                       Daten5_Relx1_x2_rHoch2_cor_hoch,Daten5_Relx1_x2_rHoch2_cor_null,
                       Daten6_Relx1_x2_rHoch2_cor_hoch,Daten6_Relx1_x2_rHoch2_cor_null,
                       Daten7_Relx1_x2_rHoch2_cor_hoch,Daten7_Relx1_x2_rHoch2_cor_null,
                       Daten8_Relx1_x2_rHoch2_cor_hoch,Daten8_Relx1_x2_rHoch2_cor_null)


ggplot(Daten_spezial, aes(x = b.dach1_mean_multiple, y =b.dach2_mean_multiple, col = factor(correlation.e)))+
  facet_grid(correlation.x~Rel.x1+ Rel.x2, labeller = label_both)+
  xlab("Schätzwert der Steigung Beta1")+
  ylab("Schätzwert der Steigung Beta2")+
  scale_color_discrete(name = "Korrelation der Fehler", labels = c(0, expression(sqrt(.25)),expression(sqrt(.5)) ))+
  geom_point()+
  apatheme.legend

ggplot(Daten_multiple, aes (x = b.dach1_mean_multiple, y = b.dach2_mean_multiple, col = ..level..))+
  geom_density_2d()+
  facet_grid(~correlation.x)


Daten_density <- Daten_multiple%>%
  filter(Rel.e1 == 1)

ggplot(Daten_density, aes (x = b.dach1_mean_multiple, fill = factor(correlation.x)))+
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name = "Korrelation der X-Variable")+
  xlab("Schätzwert der Steigung Beta1")+
  ylab("Dichte")+
  apatheme.legend

correlation_x_names <- c(
  `correlation:0` = "Korrelation = 0",
  `correlation:0.7071068781186548` = expression(sqrt(.5)))
  
ggplot(Daten_spezial, aes(x = b.dach1_mean_multiple, y =b.dach2_mean_multiple, col = factor(correlation.e)))+
  facet_grid(correlation.x~Rel.x1+ Rel.x2, labeller = labeller( .cols =  label_both ))+
  xlab("Schätzwert der Steigung Beta1")+
  ylab("Schätzwert der Steigung Beta2")+
  scale_color_discrete(name = "Korrelation der Fehler", labels = c(0, expression(sqrt(.25)),expression(sqrt(.5)) ))+
  geom_point()+
  apatheme.legend


levels(Daten_spezial$correlation.x)

Daten_spezial1 <- Daten_spezial

Daten_spezial1$correlation.x <- as.factor(Daten_spezial1$correlation.x)
levels(Daten_spezial1$correlation.x)

as_cor <- as_labeller(c(`0` ="Keine", `0.707106781186548` = expression(sqrt(.5))))

##Spezial Datensatz

ggplot(Daten_spezial1, aes(x = b.dach1_mean_multiple, y =b.dach2_mean_multiple, col = factor(correlation.e)))+
  facet_grid(correlation.x~Rel.x1+ Rel.x2, labeller = labeller( .cols =  label_both, .rows = as_cor ))+
  xlab("Schätzwert der Steigung Beta1")+
  ylab("Schätzwert der Steigung Beta2")+
  scale_color_discrete(name = "Korrelation der Fehler", labels = c(0, expression(sqrt(.25)),expression(sqrt(.5)) ))+
  geom_point()+
  apatheme.legend


ggplot(Daten_spezial, aes (x = factor(R.hoch2), y = r.hoch2_multiple, col = factor(correlation.x)))+
  facet_grid(n.xtrue~Rel.x1+ Rel.x2)+
  geom_point()+
  xlab("Varianzaufklärung")+
  ylab("geschätzte Varianzaufklärung")+
  scale_color_discrete(name = "Korrelation der X-Variablen")+
  apatheme.legend











ggplot(Daten_spezial, aes (x = mse.b.dach1_multiple, y = mse.b.dach2_multiple, col = factor(R.hoch2)))+
  facet_grid(n.xtrue~Rel.x1+ Rel.x2, labeller = labeller(.cols = label_both))+
  geom_jitter()


ggplot(Daten_density, aes (x = b.dach1_mean_multiple, fill = factor(Rel.x2)))+
  geom_density(alpha = 0.5)+
  xlab("Schätzwert der Steigung Beta1")+
  ylab ("Dichte")+
  scale_fill_discrete(name = "Reliabilität der X2 Variable")+
  apatheme.legend

Daten_spezial$Rel.x1 = factor(Daten_spezial$Rel.x1, levels =c(.9, .7))

temp$size_f = factor(temp$size, levels=c('50%','100%','150%','200%'))

ggplot(Daten_spezial, aes ( x =b.dach1_mean_multiple, b.dach2_mean_multiple, col = factor(correlation.x)))+
  facet_grid(Rel.x1_f~Rel.x2)+
  geom_jitter()

ggplot(Daten_multiple, aes (x = b.dach1_var_multiple, y = b.dach2_var_multiple, col = factor(R.hoch2)))+
  facet_grid(Rel.x1~Rel.x2)+
  geom_jitter()
