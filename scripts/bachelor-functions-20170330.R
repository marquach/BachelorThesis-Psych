##Funktionen 30.März

##Funktion um alle Daten für die simple lineare Regression zu erhalten
## und gleichzeitig den Steigungskoeffizienten zu bekommen
# Output ist demnach b und r.hoch2

f_sim_data_simple <- function(n.xtrue_s, var.xtrue_s, var.y.error_s, repl.n_s, Rel._s , beta0_s, beta1_s){
  replicate(repl.n_s, {     #Anazhl der Wiederholung wird davor festgesetzt
    
    x.true <- rnorm(n = n.xtrue_s, mean = 0, sd = sqrt(var.xtrue_s)) #x.true wird gebildet. Durch replicate jedes mal neu, vll falsch
    error  <- rnorm(n = n.xtrue_s, mean = 0, sd = sqrt(var.xtrue_s*((1/Rel._s)-1))) # Fehler für x.true wird gebildet durch die Relialbilität
    xs <- x.true + error # x mit Fehler wird gebildet
    
   
    y.Fehler <- rnorm(n.xtrue_s, mean = 0, sd = sqrt(var.y.error_s)) # fehler für lineare Regression wird übersichthalber 
    # vorher gebildet
    y <- beta0_s + beta1_s* x.true + y.Fehler
    b <- coef(lm(y~ xs))[2]
    fit.sim <- lm(y~ xs)
    p.value <- summary(fit.sim)$coefficients[2,4]
    r.hoch2 <-  summary(fit.sim)$r.squared
    
    
    return(list(beta.dach = b , p.value = p.value, r.hoch2 = r.hoch2  ))})
}

## Funktion um über den Grid zu "rotieren"
#Funktion wird gebildet um in apply angewandt zu werden

wrapper_simple <- function(x) {
  f_sim_data_simple(n.xtrue_s = x[1], var.xtrue_s = x[2], var.y.error_s = x[3], repl.n_s = x[4], Rel._s = x[5], beta0_s = x[6], beta1_s = x[7] )
}

##beta dach simple lineare regression

b.dach_mean_funktion_simple <- function (x){
  b.dach_mean_simple <- rep(NA, length(x))
  for (i in 1:length(x)){
    b.dach <- unlist(x[[i]][1,])
    b.mean <- mean(b.dach)
    b.dach_mean_simple[i] <- b.mean
  }
  return(b.dach_mean_simple)
}

##p.value für die simple lineare Regression
##p.value
p.value_power_funktion_simple <- function (x){
  p.value_power_simple <- rep(NA, length(x))
  for (i in 1:length(x)){
    p.value <- unlist(x[[i]][2,])
    p.value.power <- mean(p.value < 0.05)
    p.value_power_simple[i] <- p.value.power
  }
  return(p.value_power_simple)
}

##r.hoch2 für die simple lineare Regression

r.hoch2_mean_funktion_simple <- function (x){
  r.hoch2_mean_simple <- rep(NA, length(x))
  for (i in 1:length(x)){
    r.hoch2 <- unlist(x[[i]][3,])
    r.hoch2.mean <- mean(r.hoch2)
    r.hoch2_mean_simple[i] <- r.hoch2.mean
  }
  return(r.hoch2_mean_simple)
}


##Varianz b für simple lineare Regression

b.dach_var_funktion_simple <- function (x){
  b.dach_var_simple <- rep(NA, length(x))
  for (i in 1:length(x)){
    b.dach <- unlist(x[[i]][1,])
    b.var <- var(b.dach)
    b.dach_var_simple[i] <- b.var
  }
  return(b.dach_var_simple)
}


##multiple lineare Regression
##Funktionen laufen lassen


set.seed(2018)
f_sim_data_multiple <- function(n.xtrue_m, x1mean_m, x1sd_m, x2mean_m, x2sd_m, correlation.x_m,
                                    e1mean_m, Rel.e1_m, e2mean_m, Rel.e2_m, correlation.e_m, R.hoch2_m, repl.n_m,
                                    beta0_m, beta1_m, beta2_m){
  replicate(repl.n_m, {     #Anazhl der Wiederholung wird davor festgesetzt
    
    x1 <- rnorm(n.xtrue_m) #normalverteiltes x1 mit mean = 0, sd = 1
    x2 <- rnorm(n.xtrue_m) #normalverteiltes x2 mit mean = 0, sd = 1
    z1 <- correlation.x_m * scale(x1)[,1] + sqrt(1 - correlation.x_m^2) * 
      scale(resid(lm(x2 ~ x1)))[,1]
    x.true1 <- x1mean_m + x1sd_m * scale(x1)[,1]
    x.true2 <- x2mean_m + x2sd_m * z1
    
    e1sd <- sqrt((x1sd_m^2)*((1/Rel.e1_m)-1))
    e2sd <- sqrt((x2sd_m^2)*((1/Rel.e2_m)-1))
    
    e1 <- rnorm(n.xtrue_m) #normalverteiltes e1 mit mean = 0, sd = 1
    e2 <- rnorm(n.xtrue_m) #normalverteiltes e2 mit mean = 0, sd = 1
    z2 <- correlation.e_m * scale(e1)[,1] + sqrt(1 - correlation.e_m^2) * 
      scale(resid(lm(e2 ~ e1)))[,1]
    e.1 <- e1mean_m + e1sd * scale(e1)[,1]
    e.2 <- e2mean_m + e2sd * z2
    
    xs.1 <- x.true1+e.1
    xs.2 <- x.true2+e.2
    
    var.y.error <-(beta1_m^2*x1sd_m^2 + beta2_m^2*x2sd_m^2 +
                     beta1_m*beta_m2* correlation.x_m*x1sd_m*x2sd_m ) * (1/R.hoch2_m-1) 
    y.Fehler <- rnorm(n.xtrue_m, mean = 0, sd = sqrt(var.y.error)) # fehler für lineare Regression wird übersichthalber vorher gemacht
    y <- beta0_m + beta1_m* x.true1 + beta2_m*x.true2 + y.Fehler
    
    b1 <- coef(lm(y~ xs.1 + xs.2))[2]
    b2 <- coef(lm(y ~xs.1 + xs.2))[3]
    fit.sim <- lm(y~ xs.1 + xs.2)
    
    p.value1 <- summary(fit.sim)$coefficients[2,4]
    p.value2 <- summary(fit.sim)$coefficients[3,4]
    r.hoch2 <-  summary(fit.sim)$r.squared
    list_p.value1 <- p.value1
    list_p.value2 <- p.value2
    list_r.hoch2 <- r.hoch2
    
    
    return(list(b.dach1 = b1, b.dach2 = b2, p.value1 = list_p.value1, 
                p.value2 = list_p.value2, r.hoch2 = list_r.hoch2))})
  
}

## Funktion um über den Grid zu "rotieren"
#Funktion wird gebildet um in apply angewandt zu werden

wrapper_multiple <- function(x) {
  f_sim_data_multiple(n.xtrue_m = x[1], x1mean_m = x[2], x1sd_m = x[3], x2mean_m = x[4], x2sd_m = x[5], correlation.x_m = x[6],
                          e1mean_m = x[7], Rel.e1_m = x[8], e2mean_m = x[9], Rel.e2_m = x[10], correlation.e_m = x[11],
                          R.hoch2_m = x[12], repl.n_m = x[13], beta0_m = x[14], beta1_m = x[15], beta2_m = x[16] )
}



##beta dach1
b.dach1_mean_funktion_multiple <- function (x){
  b.dach1_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    b.dach <- unlist(x[[i]][1,])
    b.mean <- mean(b.dach)
    b.dach1_multiple[i] <- b.mean
  }
  return(b.dach1_multiple)
}

##beta dach2
b.dach2_mean_funktion_multiple <- function (x){
  b.dach2_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    b.dach <- unlist(x[[i]][2,])
    b.mean <- mean(b.dach)
    b.dach2_multiple[i] <- b.mean
  }
  return(b.dach2_multiple)
}

##p.value1
p.value1_power_funktion_multiple <- function (x){
  p.value1_power_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    p.value1 <- unlist(x[[i]][3,])
    p.value1.power <- mean(p.value1 < 0.05)
    p.value1_power_multiple[i] <- p.value1.power
  }
  return(p.value1_power_multiple)
}

##p.value2
p.value2_power_funktion_multiple <- function (x){
  p.value2_power_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    p.value2 <- unlist(x[[i]][4,])
    p.value2.power <- mean(p.value2 < 0.05)
    p.value2_power_multiple[i] <- p.value2.power
  }
  return(p.value2_power_multiple)
}

##r.hoch2
r.hoch2_mean_funktion_multiple <- function (x){
  r.hoch2_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    r.hoch2 <- unlist(x[[i]][5,])
    r.hoch2 <- mean(r.hoch2)
    r.hoch2_multiple[i] <- r.hoch2
  }
  return(r.hoch2_multiple)
}

##Varianz der Parameterschätzungen beta1 und beta2

##beta dach1
b.dach1_var_funktion_multiple <- function (x){
  b.dach1_var_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    b.dach <- unlist(x[[i]][1,])
    b.var <- var(b.dach)
    b.dach1_var_multiple[i] <- b.var
  }
  return(b.dach1_var_multiple)
}

##beta dach2
b.dach2_var_funktion_multiple <- function (x){
  b.dach2_var_multiple <- rep(NA, length(x))
  for (i in 1:length(x)){
    b.dach <- unlist(x[[i]][2,])
    b.var <- var(b.dach)
    b.dach2_var_multiple[i] <- b.var
  }
  return(b.dach2_var_multiple)
}

##Fertig Funktionen für Auswertung
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank())

apatheme.legend=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        
        axis.line=element_line(),
        text=element_text(family='Times')
)

apatheme.legend.space=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.line=element_line(),
        text=element_text(family='Times'))





ifelse(Daten_spezial$correlation.x == 0, 0, expression(sqrt(.5)))
