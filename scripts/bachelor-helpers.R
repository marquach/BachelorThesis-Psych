## (1)
#Für letzten Plot, keine Ahnung warum, dass nicht normal funktioniert
ggplot(Daten_spezial, aes (x = factor(R.hoch2_factor), y = r.hoch2_multiple, col = factor(correlation.x)))+
  facet_grid(n.xtrue~Rel.x1+ Rel.x2, labeller = labeller(.cols = label_both))+
  scale_y_continuous(breaks = c(0.1,0.3,0.5))+
  xlab(expression(paste("wahre Varianzaufklärung ", italic(R^2))))+
  ylab(expression(paste("geschätzte Varianzaufklärung ", italic(R^2))))+
  scale_color_discrete(name = "Korrelation der X-Variablen", labels = c(0, expression(sqrt(.5))))+
  geom_point()+
  geom_vline(xintercept = 1.015)+
  geom_vline(xintercept = 2.015)+
  geom_hline(yintercept = 0.5)+
  geom_hline(yintercept = 0.1)+
  geom_abline(intercept = -0.3, slope = 0.4, linetype = 2, alpha = 0.5)+
  apatheme.legend


Daten_spezial$Rel.x1 = factor(Daten_spezial$Rel.x1, levels =c(.7, .9))
Daten_spezial$Rel.x2 = factor(Daten_spezial$Rel.x2, levels =c(.7, .9))
Daten_spezial$Rel.x1 = factor(Daten_spezial$Rel.x1, levels =c(.9, .7))
Daten_spezial$Rel.x2 = factor(Daten_spezial$Rel.x2, levels =c(.9, .7))
Daten_spezial$R.hoch2 = factor(Daten_spezial$R.hoch2, levels = c(0.1,0.5))
Daten_spezial$R.hoch2 = factor(Daten_spezial$R.hoch2, levels = c(0.5,0.1))
Daten_spezial$R.hoch2_factor = factor(Daten_spezial$R.hoch2, levels = c(0.1,0.5))
Daten_spezial$R.hoch2_factor = factor(Daten_spezial$R.hoch2, levels = c(0.5,0.1))
Daten_spezial$n.xtrue = factor(Daten_spezial$n.xtrue, levels = c(500,100,50,10))


Daten_multiple$Rel.x2 = factor(Daten_multiple$Rel.x2, levels = c(1, 0.9, 0.8, 0.7, 0.6))

Daten_multiple$correlation.e_factor = factor(Daten_multiple$correlation.e_factor,
                                             levels = c("paste(\"Korrelation Fehler: \", sqrt(0.5))",
                                                        "paste(\"Korrelation Fehler: \", sqrt(0.25))",
                                                        "paste(\"Korrelation Fehler: \", 0)"))

Daten_multiple$correlation.e_factor = factor(Daten_multiple$correlation.e_factor,
                                             levels = c("paste(\"Korrelation Fehler: \", 0)",
                                                        "paste(\"Korrelation Fehler: \", sqrt(0.25))",
                                                        "paste(\"Korrelation Fehler: \", sqrt(0.5))"))
Daten_spezial$R.hoch2_factor = factor(Daten_spezial$R.hoch2_factor,
                                      levels = c("paste(R^2, ` 0.1`)", "paste(R^2, ` 0.5`)"))


Daten_multiple$correlation.x_factor = factor(Daten_multiple$correlation.x_factor, levels = c(sqrt(0.5), sqrt(0.25),0)) 
Daten_multiple$correlation.x_factor = factor(Daten_multiple$correlation.x_factor, levels = c(0, sqrt(0.25),sqrt(0.5)))


levels(Daten_multiple$correlation.e_factor)
levels(Daten_multiple$correlation.e)
levels(Daten_spezial$R.hoch2_factor)


levels(Daten_multiple$R.hoch2)

ggplot(Daten_multiple, aes (y = b.dach1_mean_multiple, x = factor(correlation.e), col = factor(Rel.x1)))+
  facet_grid(~correlation.x, labeller = labeller(
    correlation.x = c(`0` = "Korrelation x: 0", `0.5` = "Korrelation x: 0.25^{2}", `0.707106781186548` = "Korrelation x: 0.7")))+
  scale_y_continuous(breaks = seq(0.1,1.6, 0.1))+
  ylab("Schätzwert von beta1")+
  xlab("Korrelationen der Fehler")+
  scale_color_discrete( name = "Reliabilität der x1-Variable")+
  scale_x_discrete(labels = c(0,expression(sqrt(0.25)), expression(sqrt(0.5))))+
  geom_jitter()+
  apatheme.legend

Daten_multiple$correlation.x_factor <- factor(Daten_multiple$correlation.x,
                                              labels = c(expression(paste("Korrelation x: ", 0)),
                                                expression(paste("Korrelation x: ", sqrt(0.25))),
                                                expression(paste("Korrelation x: ", sqrt(0.5)))
                                                ))

Daten_multiple$correlation.x_factor <- factor(Daten_multiple$correlation.x,
                                              labels = c(expression(paste("Korrelation x: ", sqrt(0.5))),
                                                         expression(paste("Korrelation x: ", sqrt(0.25))),
                                                         expression(paste("Korrelation x: ", 0))
                                              ))
Daten_multiple$correlation.e_factor <- factor(Daten_multiple$correlation.e,
                                              labels = c(expression(paste("Korrelation Fehler: ", 0)),
                                                      expression(paste("Korrelation Fehler: ", sqrt(0.25))),
                                                      expression(paste("Korrelation Fehler: ", sqrt(0.5)))))

Daten_spezial$correlation.x_factor <- factor(Daten_spezial$correlation.x,
                                              labels = c(expression(paste("Korrelation x: ", 0)),
                                                         expression(paste("Korrelation x: ", sqrt(0.5)))
                                              )) 

Daten_spezial$R.hoch2_factor <- factor(Daten_spezial$R.hoch2,
                                       labels = c(expression(paste(R^2, ` 0.5`)),
                                      expression(paste(R^2, ` 0.1`))))

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
                                              
                                              
mtcars$cyl2 <- factor(mtcars$cyl, labels = c("delta^{15}*N-NO[3]^-{}", "NO[3]^-{}", "sqrt(x,y)"))

ggplot(Daten_multiple, aes (y = b.dach1_mean_multiple, x = factor(correlation.e), col = factor(Rel.x1)))+
  facet_grid(~correlation.x_factor, labeller = label_parsed)+
  scale_y_continuous(breaks = seq(0.1,1.6, 0.1))+
  ylab("Schätzwert von beta1")+
  xlab("Korrelationen der Fehler")+
  scale_color_discrete( name = "Reliabilität der x1-Variable")+
  scale_x_discrete(labels = c(0,expression(sqrt(0.25)), expression(sqrt(0.5))))+
  geom_jitter()+
  apatheme.legend

ggplot(Daten_spezial, aes (x = factor(R.hoch2_factor), y = r.hoch2_multiple, col = factor(correlation.x)))+
  facet_grid(n.xtrue~Rel.x1+ Rel.x2, labeller = labeller(.cols = label_both))+
  scale_y_continuous(breaks = c(0.1,0.3,0.5))+
  xlab(expression(paste("wahre Varianzaufklärung", italic(R^2))))+
  ylab(expression(paste("geschätzte Varianzaufklärung", italic(R^2))))+
  scale_color_discrete(name = "Korrelation der X-Variablen", labels = c(0, expression(sqrt(.5))))+
  geom_point()+
 
  apatheme.legend


