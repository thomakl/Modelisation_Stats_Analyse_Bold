# Analyse de l'activation du signal BOLD

data <- readRDS("activation.Rdata")

#Analyse descriptive
summary(data)
data <- data[ , - c(1:2)]
plot(data, main="Nuage de points des variables deux à deux")
signal = data
par(mar=c(11,4,4,2)+0.1,mgp=c(3,1,0))
boxplot(list(signal$PROD_G_Frontal_Inf_Tri_1_L, signal$PROD_G_Angular_2_L , signal$PROD_G_Occipital_Lat_1_L , signal$PROD_G_Rolandic_Oper_1_L , signal$PROD_S_Sup_Temporal_4_L , signal$PROD_G_Hippocampus_1_L , signal$PROD_G_Frontal_Inf_Tri_1_R , signal$PROD_G_Angular_2_R , signal$PROD_G_Occipital_Lat_1_R , signal$PROD_G_Rolandic_Oper_1_R , signal$PROD_S_Sup_Temporal_4_R , signal$PROD_G_Hippocampus_1_R),
        las=2, names=c("G_Frontal_Inf_Tri_1_L", "G_Angular_2_L" , "G_Occipital_Lat_1_L" , "G_Rolandic_Oper_1_L" , "S_Sup_Temporal_4_L" , "G_Hippocampus_1_L" , "G_Frontal_Inf_Tri_1_R" , "G_Angular_2_R" , "G_Occipital_Lat_1_R" , "G_Rolandic_Oper_1_R" , "S_Sup_Temporal_4_R" , "G_Hippocampus_1_R")
        , ylab="Taux d'activation", main="Taux d'activations (variation du signal BOLD : blood-oxygen-
level dependent) au cours d'une tâche de production langagiere chez 124 sujets."
        )

if(!require(PCAmixdata)) {
  install.packages(("PCAmixdata"))
  library(PCAmixdata)
}


res_acp <- PCAmix(data, ndim=2, graph=FALSE) #contribution des variables aux axes principaux
res_acp$sqload #carrÃ© du coef de corrÃ©lation entre les axes principaux et les variables
plot(res_acp, choice="sqload") 

plot(res_acp,axes=c(1,2),choice="cor")




#
res <- lm(signal$PROD_G_Frontal_Inf_Tri_1_L ~ signal$Age + signal$Volume_Cerebral + signal$Index_Lateralisation_Hemispherique + signal$PROD_G_Angular_2_L + signal$PROD_G_Occipital_Lat_1_L + signal$PROD_G_Rolandic_Oper_1_L + signal$PROD_S_Sup_Temporal_4_L + signal$PROD_G_Hippocampus_1_L + signal$PROD_G_Frontal_Inf_Tri_1_R + signal$PROD_G_Angular_2_R + signal$PROD_G_Occipital_Lat_1_R + signal$PROD_G_Rolandic_Oper_1_R + signal$PROD_S_Sup_Temporal_4_R + signal$PROD_G_Hippocampus_1_R)
summary(res)

res <- lm(signal$PROD_G_Frontal_Inf_Tri_1_L ~ 1, data=data)
step(res, ~ signal$Age + signal$Volume_Cerebral + signal$Index_Lateralisation_Hemispherique + signal$PROD_G_Angular_2_L + signal$PROD_G_Occipital_Lat_1_L + signal$PROD_G_Rolandic_Oper_1_L + signal$PROD_S_Sup_Temporal_4_L + signal$PROD_G_Hippocampus_1_L + signal$PROD_G_Frontal_Inf_Tri_1_R + signal$PROD_G_Angular_2_R + signal$PROD_G_Occipital_Lat_1_R + signal$PROD_G_Rolandic_Oper_1_R + signal$PROD_S_Sup_Temporal_4_R + signal$PROD_G_Hippocampus_1_R, trace = TRUE)

res2 <- lm(signal$PROD_G_Frontal_Inf_Tri_1_L ~ signal$PROD_G_Frontal_Inf_Tri_1_R + 
             signal$PROD_S_Sup_Temporal_4_L + signal$PROD_G_Angular_2_L + 
             signal$Index_Lateralisation_Hemispherique + signal$PROD_G_Occipital_Lat_1_R + 
             signal$PROD_S_Sup_Temporal_4_R + signal$PROD_G_Hippocampus_1_L + 
             signal$PROD_G_Hippocampus_1_R, data = data)
summary(res2)


res3 <- lm(signal$PROD_G_Frontal_Inf_Tri_1_L ~ signal$PROD_G_Frontal_Inf_Tri_1_R + 
             signal$PROD_S_Sup_Temporal_4_L + signal$PROD_G_Angular_2_L + 
             signal$Index_Lateralisation_Hemispherique + signal$PROD_G_Occipital_Lat_1_R + signal$PROD_G_Hippocampus_1_L + 
             signal$PROD_G_Hippocampus_1_R, data = data)
summary(res3)


residus.stud <- rstudent(res2) #studentiser permet d'égaliser les écarts types
plot(residus.stud, ylab="Résidus studentisés", ylim=c(-3.5,3.5),main="Distribution des résidus studentisés dans l'échantillon")
abline(h=c(-2,0,2), lty=c(2,1,2), col=c(2,1,2)) #on peut supprimer ou garde les valeurs en dehors des lignes rouges
plot(res3$fitted, res2$residuals, main="Résidus en fonction des valeurs prédites du modèle 3", ylab="Résidus du modèle", xlab="Valeurs prédites par le modèle")
shapiro.test(res2$residuals)



#ANOVA
data <- readRDS("activation.Rdata")
data <- data[ , c(5:7, 10:12, 14, 16:17)] #12, 10, 7, 5, 14, 16, 17, 11
