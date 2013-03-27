res <- read.csv("resultats6002.csv") #chargement des données

# on calcule les pourcentages (inutile pour le modèle lui-même, mais utile pour des statistiques descriptives par exemple)
res$progFN <- (res$Italiani2 - res$Italiani)/res$Inscrits
res$progFN2 <- (res$Italiani2 - res$Italiani)/res$Italiani
res$FN.ins <- res$Italiani / res$Inscrits
res$FN.exp <- res$Italiani / res$Exprimés
res$FN.ins2 <- res$Italiani2 / res$Inscrits
res$FN.exp2 <- res$Italiani2 / res$Exprimés
res$PS.ins <- res$Houssin /res$Inscrits
res$UMP.ins <- res$Mancel /res$Inscrits
res$UMP.ins2 <- res$Mancel2 / res$Inscrits2
res$FG.ins <- res$Ripart / res$Inscrits
res$BN.ins <- res$BN / res$Inscrits
res$BN.ins2 <- res$B2 / res$Inscrits2
res$abs.ins <- (res$Inscrits - res$votants) / res$Inscrits
res$abs.ins2 <- (res$Inscrits2 - res$votants2) / res$Inscrits2
res$abs <- res$Inscrits - res$votants
res$abs2 <- res$Inscrits - res$votants2 # sinon problèmes avec ei
res$pirate.ins <- res$Lesaege / res$Inscrits
res$autres.ins <- (res$Ramel + res$Potchtovik) / res$Inscrits

library(ei)
# la spécification du modèle
formula <- cbind(Italiani2, Mancel2, B2, abs2) ~ cbind(abs, BN, Mancel, Ripart, Lesaege, Ramel, Potchtovik, Houssin, Italiani)
# on le fait tourner
dbuf <- ei(formula, data=res, sample=50000) # paramètre sample à moduler en fonction de la puissance de calcul disponible...
# on crée une matrice pour stocker les estimations
tour1 <- names(res)[c(34, 5,7:13)]
tour2 <- names(res)[c(35,16,18,19)]
df <- array(dim=c(length(tour1), length(tour2)))
dimnames(df)[[1]] <- names(res)[c(34, 5,7:13)]
dimnames(df)[[2]] <- names(res)[c(35,16,18,19)]


# calcul des estimations moyennes 
for (i in tour1) {
  for (j in tour2) {
    df[i,j] <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.",i,".",j,".",1:196, sep="")])$statistics[,1], res$Inscrits)
  }
}

# représentation de la densité de l'estimation d'un paramètre
plot(density(dbuf$draws$Beta[,"beta.Houssin.Italiani2.1"]))

# calcul des quantiles pour le report Houssin sur Italiani
q1 <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.Houssin.Italiani2.",1:196, sep="")])$quantiles[,1], res$Inscrits)
q2 <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.Houssin.Italiani2.",1:196, sep="")])$quantiles[,2], res$Inscrits)
q3 <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.Houssin.Italiani2.",1:196, sep="")])$quantiles[,3], res$Inscrits)
q4 <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.Houssin.Italiani2.",1:196, sep="")])$quantiles[,4], res$Inscrits)
q5 <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.Houssin.Italiani2.",1:196, sep="")])$quantiles[,5], res$Inscrits)

# représentation de la densité de toutes les estimations moyennes d'un paramètre
plot(density(summary(dbuf$draws$Beta[,paste("beta.Houssin.Italiani2.",1:196, sep="")])$statistics[,1]))
# idem pour le report abstention sur abstenton
plot(density(summary(dbuf$draws$Beta[,paste("beta.abs.abs2.",1:196, sep="")])$statistics[,1]))

# représentation graphique du lien entre vote PS et vote FN
library(ggplot2)
ggplot(res, aes(PS.ins, progFN)) + geom_point(aes(size=Inscrits), alpha=0.4) + scale_size(trans="log", breaks=c(50,100,500,1000))+theme_bw() + geom_smooth(method="lm") + xlab("Vote Sylvie Houssin") + ylab("Progression de Florence Italiani")

# pour les cantons
cantons <- c("Aunueil", "Beauvais", "Chaumont", "Le Coudray", "Formerie", "Grandvilliers", "Noailles", "Songeons")
index <- list(1:21, 22:36, 37:74, 75:95, 96:119, 120:143, 144:168, 169:196)
resultats <- list()
for (n in 1:8) {
  resultats[[n]] <- array(dim=c(length(tour1), length(tour2)))
  dimnames(resultats[[n]])[[1]] <- names(res)[c(34, 5,7:13)]
  dimnames(resultats[[n]])[[2]] <- names(res)[c(35,16,18,19)]
  for (i in tour1) {
    for (j in tour2) {
      resultats[[n]][i,j] <- weighted.mean(summary(dbuf$draws$Beta[,paste("beta.",i,".",j,".",index[[n]], sep="")])$statistics[,1], res$Inscrits[index[[n]]])
    }
  }
  names(tab) <- c("Abstention", "BN", "Mancel", "Italiani")
  tab <- apply(tab,  2, function(x) sprintf("%1.2f%%", 100*x))
  tab <- cbind(tab, Voix = apply(res[index[[n]],c("abs", "BN", "Mancel", "Ripart", "Lesaege", "Ramel", "Potchtovik", "Houssin", "Italiani")],2,sum))
  tab <- rbind(tab, Voix = c(apply(res[index[[n]],c("abs2", "B2", "Mancel2", "Italiani2")],2,sum), sum(res$Inscrits[index[[n]]])))
  write.csv(resultats[[n]], file=paste(cantons[n], ".csv", sep="")) 
}