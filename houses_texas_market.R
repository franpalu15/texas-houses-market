###quesito 1
dati = read.csv("Real Estate Texas.csv", sep = ",") ##carico il csv 
attach(dati)
##quesito 2
str(dati) ## verifico il tipo di variabili presenti nel dataframe.
###city è una variabile qualitativa su scala nominale
###year è una variabile quantitativa discreta
###il mese è una variabile quantitativa discreta
###sales, volume, median_price, listings  e months inventory
###sono tutte variabili quantitative continue
###

##quesito 3 - begin
###calcolo gli indici di posizione per le variabili numeriche
###non andrò a calcolarli per mese, anno e città perché per l'ultimo 
##probabilmente mi darebbe errore, mentre per gli altri due penso non abbia senso
###utilizzo la funzione base di R

###calcolo delle medie
mean(dati$sales)
mean(dati$volume)
mean(dati$median_price)
mean(dati$listings)
mean(dati$months_inventory)

###calcolo dei massimi
max(dati$sales)
max(dati$volume)
max(dati$median_price) 
max(dati$listings)
max(dati$months_inventory)

###calcolo dei minimi
min(dati$sales)
min(dati$volume)
min(dati$median_price) 
min(dati$listings)
min(dati$months_inventory)

###calcolo delle mediane
median(dati$sales)
median(dati$volume)
median(dati$median_price) 
median(dati$listings)
median(dati$months_inventory)

###moda
table(dati$sales)
table(dati$volume)
table(dati$median_price)
table(dati$listings)
table(dati$months_inventory)


###indici di variabilità
###anche in questo caso per me ha poco senso calcolarli per mese, anno e città
##VARIANZA
var(dati$sales)
var(dati$listings)
var(dati$volume)
var(dati$median_price)
var(dati$months_inventory)

###DEV STD
sd(dati$sales)
sd(dati$listings)
sd(dati$volume)
sd(dati$median_price)
sd(dati$months_inventory)

###CV coefficiente di variazione

coeff_var <-function(x){
  return( sd(x)/mean(x) * 100 )
}
cv_sales <- coeff_var(dati$sales)
cv_listings<- coeff_var(dati$listings)
cv_median_price<- coeff_var(dati$median_price)
cv_months_inventory<- coeff_var(dati$months_inventory)
cv_volume<- coeff_var(dati$volume)


###INDICE DI GINI

gini.index <- function(x){
  num = table(x)
  freq = num/length(x)
  freq2 = freq^2
  J = length(table(x))
  
  gini = 1-sum(freq2)
  gini_n = gini/((J-1)/J)
  
  return(gini_n)
}

##dopo aver inizializzato la funzione per il calcolo del Gini-Index, andiamo
###a dividere in classi i nostri dati e calcoliamo l'indice 
###ho creato le classi cercando dei limiti sensati, distanziati in modo uguale
sales_classes <- cut(dati$sales, breaks=c(85, 151,217,283, 350)) 
gi_sales <- gini.index((sales_classes)) 

list_classes <- cut(dati$listings, breaks = c(800, 1200, 1600, 2000, 2400, 2800, 3200))
gi_listings <- gini.index((list_classes))

months_inv_classes<- cut(dati$months_inventory, breaks = c(4,7, 9 ,11 ,14))
gi_mo_inv <- gini.index((months_inv_classes))

volume_classes <- cut(dati$volume, breaks = c(9,27,45,63,81))
gi_volume <- gini.index((volume_classes))

median_price_classes<-cut(dati$median_price, breaks = c(75000, 100000, 125000, 150000, 175000))
gi_med_price <- gini.index((median_price_classes))

##dall'osservazione dei valori dell'indice di gini, possiamo dire che le variabili sono molto eterogenee
###quindi vien difficile parlare di omogeneità, diciamo che la variabili meno eterogenea è quella relativa al 
##volume, e quindi al costo totale.
###per year, month e city, l'indice di gini sarà ovviamente uguale a 1, essendo equamente distribuite (vedremo dopo)

##INDICI DI FORMA
library(moments) ###carichiamo la libreria moments che contiene le funzioni
###già costruite per il calcolo dell'indice di asimmetria (skewness) e kurtosis (curtosi)
skewness(dati$sales)
kurtosis(dati$sales)-3
##l'indice di asimmetria è positivo, mentre quello di curtosi è negativo
##quindi mi aspetto una distribuzione con asimmetria a destra e abbastanza "schiacciata"
##e quindi una platicurtica
hist(dati$sales)

skewness(dati$listings)
kurtosis(dati$listings)-3
##mi aspetto qui la stessa cosa per la variabile precedente, ma una platicurtica più accentuata
hist(dati$listings)

skewness(dati$volume)
kurtosis(dati$volume)-3
###in questo caso l'indice di asimmetria è positivo, quindi l'asimmetria è ancora
###a destra, mentre la curtosi è leggermente positiva quindi è più mesocurtica che leptocurtica
hist(dati$volume) 

skewness(dati$months_inventory)
kurtosis(dati$months_inventory)-3
##in questo caso dovremmo avere una quasi totale simmetria (infatti parliamo di mesi)
hist(dati$months_inventory)

skewness(dati$median_price)
kurtosis(dati$median_price)-3
##qui abbiamo una asimmetria a sinistra per via dell'indice negativo, e una distribuzione abbastanza appiattita
hist(dati$median_price)

###creo distribuzioni di frequenze delle variabili per cui ha senso farlo
N = dim(dati)[1]
freq_rel_month <- table(month)/N
freq_rel_year <- table(year)/N
city[city=="Beaumont"] <- 1
city[city=="Bryan-College Station"] <- 2
city[city=="Tyler"] <- 3
city[city=="Wichita Falls"] <- 4
city <- as.numeric(city)
hist(city)
freq_rel_city <- table(city)/N
##dalle distribuzioni di frequenze non si evince molto e i dati sono distribuiti con le medesime
###frequenze sia per anno che per mese che per città e quindi calcolare gli indici non avrebbe senso

##quesito 3 - end

##quesito 4: il prezzo mediano dovrebbe essere la variabile a variabilità più elevata per via
##della varianza e della deviazione standard più elevate, seguito dal volume che possiede invece
##il gini index meno alto e soprattutto il coefficiente di variazione più alto.


####QUESITO 5 
library(ggplot2)
dati$classi_prezzo<-cut(dati$median_price, breaks=c(73800, 95040, 116280,
                                                    137520, 158760,180000))
N <- dim(dati)[1]

attach(dati)

ni_price<-table(classi_prezzo)
fi_price<-ni_price/N
Ni_price<-cumsum(ni_price)
Fi_price<-Ni_price/N
cbind(ni_price,fi_price,Ni_price,Fi_price)
distribuzione_prezzi<-as.data.frame(cbind(ni_price,fi_price,Ni_price,Fi_price))
barplot(distribuzione_prezzi$ni_price,
        xlab = "Classi di prezzo ($)",
        ylab = "Frequenze assolute",
        names.arg = rownames(distribuzione_prezzi),
        ylim = c(0,100), main = "Distribuzione dei prezzi ($) per classi",
        sub = "Studio condotto su dati americani",
        col="darkviolet", width = 0.75, space = 0.15, border = TRUE)

gini_index_med_price <- gini.index(dati$median_price)



###quesito 6, l'indice di gini per la variabile city è 1


###quesito 7 - 1)
library(ggplot2)

variabile <- dati$city
n_variabili <- sample(variabile,1000000,replace = T)

ggplot()+
  geom_histogram(aes(x=n_variabili),
                 stat = "count",
                 col="orange",
                 fill="blue")
prob = 250000/1000000*100
####calcolo la probabilità estrapolando il dato dal grafico e dividendolo per
##il numero totale di casi e moltiplicando il risultato per 100
###la probabilità è del 25%, ma si poteva capire già dal fatto che ci fossero 4 città

###quesito 7 - 2)
library(ggplot2)
variabile_2 <- dati$month
n_variabili_2 <- sample(variabile_2,1000000,replace = T)

ggplot()+
  geom_histogram(aes(x=n_variabili_2),
                 stat = "count",
                 col="black",
                 fill="red")

prob3 = 80000/1000000*100 #---> siamo all'incirca sull'8% 

###quesito 7 - 3)
library(ggplot2)

variabile_2 <- c(10,11,12,13,14)
variabile_3 <- c(1,2,3,4,5,6,7,8,9,10,11,12)

prob_combinata <- function(variabile_2,variabile_3,n){
  
  prob <- sample(variabile_2,n,replace = T) + sample(variabile_3,n,replace = T)
  return(prob)
}

vet_prob_comb <- prob_combinata(variabile_2,variabile_3,1000000)


ggplot()+
  geom_histogram(aes(x=vet_prob_comb),
                 stat = "count",
                 col="black",
                 fill="lightblue")
prob2 = (50000/1000000)*100
###la probabilità che venga estratto dicembre del 2012 è di circa 8 su 100 (8%)
###se deve essere dicembre 2012 allora essendo dicembre il numero 12 nella variabile_2
### e essendo il 2012 il numero 12 nella variabile_3
###ho calcolato la probabilità combinata come somma di due variabili "codificate"
###quindi 12 + 12 = 24; 
###Dal grafico il count risulta 50000, quindi siamo attorno al 5%.

##quesito 8
dati$mean_price <- (dati$volume * 1000000)/dati$sales 
###moltiplicato il volume totale per 100000 per convertire da milioni a dollari
###e poi dividiamo il volume per il numero totale di vendite.

###quesito numero 9
dati$indice_efficacia <- dati$sales/dati$listings
###ho scelto di dividere il numero delle vendite per gli annunci attivi in modo tale che
###dal numero che se ne tira fuori si può avere un'idea del compromesso che ci deve essere
###tra i due parametri: un numero basso di annunci attivi può voler dire che le vendite vanno bene
###così come ovviamente il numero elevato di vendite può significare che gli annunci sono efficaci
###un rapporto tra i due numeri è un compromesso tra l'ottenimento di un elevato numero di vendite
###ed un certo numero di annunci, con la premessa che meno ce ne sono e meglio è perché
###significa che si è operato bene.






##QUESITO-1-BIS
library(dplyr)
price_vs_listings <- dati %>%
  group_by(city) %>%
  summarise(m_med_price=mean(median_price),
            med_listings=mean(listings))

sales_vs_volume <- dati %>%
  group_by(year) %>%
  summarise(med_sales=mean(sales),
            med_volume=mean(volume))


dati$mean_price <- (dati$volume * 1000000)/dati$sales 

mean_vs_median_price_by_year <- dati %>%
  group_by(year) %>%
  summarise(tot_mean_price=mean(mean_price),
            tot_med_price=mean(median_price))

mean_vs_median_price_by_month <-dati %>%
  group_by(month) %>%
  summarise(tot_mean_price=mean(mean_price),
            tot_med_price=mean(median_price))


###QUESITO 1 - BIS
library(ggthemes)
ggplot(dati, aes(x=city, y=median_price, fill = city)) + 
  geom_boxplot() +
  labs(x = "Cities", y = "Prezzo mediano ($)") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")+
  theme_stata() + 
  scale_color_stata()



###QUESITO 1 - 2 - BIS
library(ggthemes)
ggplot(dati, aes(x=city, y=median_price, fill = city)) + 
  geom_boxplot() +
  labs(x = "Cities", y = "Prezzo mediano ($)") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")+
  theme_stata() + 
  scale_color_stata() + 
  facet_wrap(~year, scale="free")


###QUESITO 3 - BIS
dati$month <- as.factor(dati$month) ###trasformo la colonna dei mesi in fattori
##filtro i dati in base all'anno e ne creo un plot ciascuno per poi 
###unirli tutti insieme con la libreria cowplot
dati2010 <- filter(dati, year==2010)
dati2011 <- filter(dati, year==2011)
dati2012 <- filter(dati, year==2012)
dati2013 <- filter(dati, year==2013)
dati2014 <- filter(dati, year==2014)

p_2010 <- ggplot(dati2010, aes(x = month, y = sales, fill = city)) +
  geom_col() +
  labs(x = "mesi dell'anno", y = "numero di vendite") +
  theme(legend.position = "none") +
  ggtitle("Vendite mensili per città") + 
  scale_fill_brewer(palette="BuPu")+
  theme_stata() + 
  scale_color_stata() + 
  facet_wrap(~year, scale="free")

p_2011 <- ggplot(dati2011, aes(x = month, y = sales, fill = city)) +
    geom_col() +
    labs(x = "mesi dell'anno", y = "numero di vendite") +
    theme(legend.position = "none") +
    ggtitle("Vendite mensili per città") + 
    scale_fill_brewer(palette="BuPu")+
    theme_stata() + 
    scale_color_stata() + 
    facet_wrap(~year, scale="free")
  
p_2012 <- ggplot(dati2012, aes(x = month, y = sales, fill = city)) +
    geom_col() +
    labs(x = "mesi dell'anno", y = "numero di vendite") +
    theme(legend.position = "none") +
    ggtitle("Vendite mensili per città") + 
    scale_fill_brewer(palette="BuPu")+
    theme_stata() + 
    scale_color_stata() + 
    facet_wrap(~year, scale="free")

p_2013 <- ggplot(dati2013, aes(x = month, y = sales, fill = city)) +
    geom_col() +
    labs(x = "mesi dell'anno", y = "numero di vendite") +
    theme(legend.position = "none") +
    ggtitle("Vendite mensili per città") + 
    scale_fill_brewer(palette="BuPu")+
    theme_stata() + 
    scale_color_stata() + 
    facet_wrap(~year, scale="free")

  
p_2014 <- ggplot(dati2014, aes(x = month, y = sales, fill = city)) +
    geom_col() +
    labs(x = "mesi dell'anno", y = "numero di vendite") +
    theme(legend.position = "none") +
    ggtitle("Vendite mensili per città") + 
    scale_fill_brewer(palette="BuPu")+
    theme_stata() + 
    scale_color_stata() + 
    facet_wrap(~year, scale="free")
library(cowplot)
plot_grid(p_2010, p_2011, p_2012, p_2013, p_2014,
            labels = c("A", "B", "C", "D", "E"),
            ncol = 2, nrow = 3)



####QUESITO 4 - bis
attach(dati)
ggplot(data = dati) +
  geom_line(aes(x=month, y=mean_price, col=city), lwd = 1) +
  geom_point(aes(x=month, y=mean_price), col="black", lwd=1.25)+
  ggtitle("time plot mean price vs city") +
  theme_stata() + 
  ylab("millions ($)") + 
  scale_color_stata() + 
  facet_wrap(~year, scale="free")


