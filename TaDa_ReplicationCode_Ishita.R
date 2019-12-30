# Use data till April end 

# set working directory
setwd("/Users/ishitagopal/Box/twitter_data/")

# import the dataset of tweets from 31 national assembly members
data <-read.csv("assemblyR.csv", encoding="UTF-8", stringsAsFactors=FALSE)

# names(data)

# dim(data)

# preprocessing

processed <- textProcessor(data$alltext, metadata = data,
                           language = 'spanish',             
                           #lowercase = FALSE,
                           removestopwords = TRUE, 
                           removenumbers = FALSE, 
                           removepunctuation = TRUE,
                           stem = FALSE, 
                           verbose = TRUE, 
                           customstopwords = c( "t.co", "https", "rt", "http"))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=1)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta


#####Topic Models with meta ######
#############  k = 31 ############

############ k= 0,5,10,15,20,25 checked using the same code ####

tweetFit.31 <- stm(documents = out$documents, vocab = out$vocab,
                   K = 31, prevalence =~ s(created_at_day),
                   max.em.its = , data = out$meta,
                   init.type = "Spectral")

prep.31 <- estimateEffect(1:31 ~ s(created_at_day), tweetFit.31, 
                       meta = out$meta, uncertainty = "Global")


# check FREX key words   
labelTopics(tweetFit.31, n=30)

# Graphical summary of topic proportions
pdf("plot_31.pdf", height=10, width=15)
plot(tweetFit.31, labeltype = 'frex',n=20)
dev.off()


# get tweets for Protest Topics
thoughts21 <- findThoughts(tweetFit.31, texts = meta$alltext,
                           n = 6, topics = 21)$docs[[1]]

thoughts21.english <- c(
"We continue in #CabildoAbierto this time from Machiques de Perijá where its people are determined to go out to the street # 23ENE. United, pressing and focused we will drive the change that Venezuela so much needs. # NosVemosEl23 https://t.co/LruiJO42Fm ",
"Today # 17Ene visited my Alma Máter @lacatolica and I met a new generation of Lawyers. It is motivating to see how young people in spite of the crisis are still forming and betting on the country. \ N \ nI trust you, trust me and let's take #Venezuela! https://t.co/e3h8uYZASt ",
"Venezuela was born in a Cabildo and in Open Cabildos, hope is reborn in every corner of our homeland, a town that claims reconciliation and being free! \ N \ n # TodaVzlaEnCabildo https://t.co/8Pb24mAfNR"
)

thoughts5 <- findThoughts(tweetFit.31, texts = meta$alltext,
                          n = 10, topics = 5)$docs[[1]]

thoughts5.english <- c("At the moment #cacerolazo is reported in El Valle, El Cementerio, Andrés Bello, La Candelaria, Catia, Quinta Crespo, Santa Monica, Cotiza, Sabana Grande, Petare, Baralt Avenue, Rocking, El Paraiso, Antimano. The town expresses his rejection of the usurper 10:32 pm ",
                       "Throwing the tank against a person, which at first sight does not constitute any danger to the official, with premeditation and alevosía, constitutes a flagrant aggression to his physical integrity. The truth always comes out, no matter his desperate attempts to silence it. Https : //t.co/kb6bIaMes6 ",
                       "IMPORTANT. Today the main protagonist is the citizen, we will record in photos and videos the development of the marches and concentrations throughout the country. Today is a day where we all have to make a great effort to communicate the expression of our people # GritemosConBrío"
                       )


thoughts29 <- findThoughts(tweetFit.31, texts = meta$alltext,
                           n = 10, topics = 29)$docs[[1]]
thoughts29.english <- c("After the illegitimate election of Nicolás Maduro in May 2018, Europe supports the restoration of democracy. I acclaim the courage of hundreds of thousands of Venezuelans who walk for their freedom. Https://t.co/beTSGVuAyd",
                        "# 21Ene They ask us if this will be worth it, they ask us if we are afraid. \ N \ nPlease know that from this Palace of laws, for the future for all Venezuelans, we will be at the forefront of reconciling the people, achieving justice, democracy and freedom. \ n \ n # NosVemosEl23 https://t.co/ywsPj7RRwm ",
                        "Above all, the 'battle of ideas' is possible in a criminal dictatorship that holds props and has been in power for 60 years, plundering the Cuban people tremendous example Mrs. Polevnsky https://t.co/1S4IHtLc6T"
                        
)


thought18 <- findThoughts(tweetFit.31, texts = meta$alltext,
                          n = 10, topics = 18)$docs[[1]]


thoughts18.english <- c("#AVANCE This is how the San José de Cotiza parish is located in Caracas, after the rise of at least 40 troops of the National Guard Command # 21Ene (8:19 am) https://t.co/BbxZ3lcir5 https: // t .co / dnGbzleW9R ",
                        "The doctors on call at the Vargas hospital in Caracas, which was taken by the GNB, informed me of the number of wounded who enter by firearm and pellets from the widespread protest throughout #Caracas #Cacerolazo",
                        ". @ RedesVP \ n With the creation of the popular promoter network in the Simon Bolivar parish of the sector Laja of the Caroni municipality. \ N @ VPBolivar \ n manages to train and activate more than 100 activists in Popular Networks. \ N @ RedesVP_Caroni \ n @ MarianellaPuchi \ n @ VoluntadPopular \ n @ leopoldolopez https://t.co/hjWB0USgcR "
)

# Plot thoughts 
pdf('thoughts.protest.pdf')
par(mfrow = c(1, 4),mar = c(.5, .5, 1, .5))
plotQuote(thoughts21.english, width = 30, main = "Topic 21")
plotQuote(thoughts5.english, width = 30, main = "Topic 5")
plotQuote(thoughts29.english, width = 30, main = "Topic 29")
plotQuote(thoughts18.english, width = 30, main = "Topic 18")
dev.off()


# Plot trend in Protest topics 

pdf("plot_trend.pdf", height=6, width=7)

plot(prep.31, "created_at_day", method = "continuous", topics = c(21,18,5,29),
    linecol = c('dark green','purple','orange','red','black','blue'),  model = tweetFit.31, 
    printlegend = F, xaxt = "n", xlab = "Time(Jan 2019)")
dayseq <-(1:31)
axis(1,at = dayseq)
abline(v = 21, col = "black", lwd = 1)
abline(v = 23, col = "black", lwd = 1)
abline(v = 12, col = "blue", lwd = 1)
text(family = "sans")
legend('topleft',
       legend=c("21: Calls to Protest",
                "18: Sharing Locations",
                "5: Aggresive Calls to Protest",
                '29: Denouncing Regime',
                'Wikipedia Blocked',
                'Social Media Blocked 1st instance',
                'Social Media Blocked 2nd instance'),
       col=c('dark green','purple','orange','red','blue','black','black'),
       lty=1,
       cex = .7,
       pch = c(15,15,15,15,NA,NA,NA)
       )

dev.off()

## Targetting without calls to action 

# get actual tweets 

thoughts23 <- findThoughts(tweetFit.31, texts = meta$alltext,
                           n = 15, topics = 23)$docs[[1]]


thoughts23.english <- c("We ask the countries to instruct their regulatory bodies of financial activities the prohibition of liquid assets management of the Venezuelan state to guarantee their protection against the usurpation of Nicolás Maduro #ANDeclaraAMaduroUsurpador https://t.co/8pgGXnJZBA",
                        "Unfortunately PDVSA ceased to be an oil industry to become a company dedicated to money laundering due to corruption. We must protect the assets of the Vzlnos so that Maduro does not end up looting everything and thus be able to put it at the service of the people",
                        "As of tomorrow, Nicolás Maduro usurps the presidency only to continue sinking Venezuela. That is why, together with the people, unions and organizations it is necessary to consolidate a fight itinerary that allows a real way out of the crisis that is going through Venezuela #MaduroUsurpador https: //t.co/rkKTMgXQgB "
)



thoughts13 <- findThoughts(tweetFit.31, texts = meta$alltext,
                           n = 6, topics = 13)$docs[[1]]

thoughts13.english <- c("Help us spread the word! \ N \ nThe journalists Maiker Yriarte, Ana Rodríguez and Chilean colleagues Rodrigo Pérez and Gonzalo Barahona are still deprived of liberty in #Mitaflores. \ N \ nVan nine hours and the condition they are in is unknown. \ n #LiberenAMaikeryAna # 30Ene ",
                        "Officials with long weapons ran to my vehicle, they took our ID card and forced us to force the Sebin headquarters, they took our phones, they asked us for the keys, they took our wallets, they entered my vehicle to the Sebin and they ordered us to stay under ",
                        "This is how the situation is right now after more than 5 hours of blackout in the HUC. Patients connected to shut down machines, and medical staff doing their best to protect the lives of their patients. And those responsible where they are? \ N https://t.co/WfzmLkIb5L "
)


pdf('thoughts_implicit.pdf')
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thoughts23.english, width = 30, main = "Topic 13")
plotQuote(thoughts13.english, width = 30, main = "Topic 23")
dev.off()

# Plot trend in Criticism topics

pdf("plot_trend_2.pdf", height=6, width=7)
plot(prep.31, "created_at_day", method = "continuous", topics = c(13,23),
     linecol = c('light blue','indianred4'), model = tweetFit.31, 
     printlegend = F,xaxt = "n", xlab = "Time(Jan 2019)")
dayseq <-(1:31)
axis(1,at = dayseq)
abline(v = 21, col = "black", lwd = 1)
abline(v = 23, col = "black", lwd = 1)
abline(v = 12, col = "blue", lwd = 1)
legend('topleft',
       legend=c("13",
                '23',
                'Wikipedia Blocked',
                'Social Media Blocked 1st instance',
                'Social Media Blocked 2nd instnace'),
       col=c('light blue','indianred4','blue','black','black'),
       lty=1,
       cex = .7,
       pch = c(15,15,NA,NA,NA)
)

dev.off()

###### Allowing Default topic assignment ######
tweetFit.0 <- stm(documents = out$documents, vocab = out$vocab,
                   K = 0, prevalence =~ s(created_at_day),
                   max.em.its = , data = out$meta,
                   init.type = "Spectral")

summary(tweetFit.0)


##### Plotting Correlations #######
mod.out.corr <- topicCorr(tweetFit.31,cutoff = 0.01)
pdf('corr.pdf')
plot(mod.out.corr, vertex.size = 5,vertex.label.cex = 1)
dev.off()





     