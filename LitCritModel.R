library(quanteda)
library(stm)
library(ggplot2)
library(plotly)
library(readtext)
library(tidytext)
library(reshape2)
library(dplyr)
library(stringr)
library(mgsub)
library(wordcloud)
library(gutenbergr)
library(RColorBrewer)
library(ngram)

# This uses the package 'readtext' to transfer text files into R

DIR <- getwd()
data <- readtext(paste0(DIR,  "/korpus/*"), 
                 docvarsfrom = "filenames",  
                 docvarnames = c("Forfatter", "Titel",  "Årstal",  "Køn"), 
                 dvsep = "_", 
                 encoding = "UTF-8")



korpus <- corpus(data)
korpus <- tolower(korpus)

#removes hyphens between words that have been split between lines and 
#contracts them

korpus <- gsub("-\\s*", "", korpus)
korpus <- gsub("¬\\s*", "", korpus)


#streamlines a set of common ways of archaic spelling with modern versions

gamle_staveformer_c <- c( "aa", "ct", "ee", "ii", "qv", "x", 
                          "theat", "mexik", "gjen", "thron", "kiø")
moderne_staveformer_c <- c("å", "kt", "e", "i", "kv", "ks", "teat",
                           "mexic", "gen", "tron", "kø")

korpus <- mgsub(korpus, gamle_staveformer_c, moderne_staveformer_c)


#a list of archaic spellings of particular words

gamle_ord_c <- c("correct", "correkt", "chinesisk", "canon", 
                 "publicum", "partie",
                 "skjøn", "negte", "phantasie", "prindsesse", 
                 "kjæmpe", "individualitæt",  "psychologisk", "conjungere", 
                 "claver", "detailleret", "neppe", "æsthetisk", 
                 "phænomen" ,"feil", "kjend", "sækulær", "forudaned", "linie", 
                 "tredie", "hefte", "hovede", 
                 "attityde", "osse", "fision", "skulde", "literatur", 
                 "giør","giort", "comoedie", "fortien", 
                 "discret", "øie", "hjærte", "icke", "skuespill", "stycke", 
                 "skiøn", "comisk", "giennem", "antike", 
                 "charakter", "sielden", "vei", "øine",
                 "afhielpe", "colonne", "nøiere", "eien", 
                 "kjærlighed", "igien","forskjel", "æventyr", 
                 "condition", "gjore", "compleks", "tydsk",
                 "nøye", "kiende", "methode", "fløi",
                 "melkevei", "høi", "litcratur", "skjøndt", "kjære", 
                 "taushed", "øhlenschlåger", "øhlenschliger", "øhlenschläger",
                 "umuelig", "deilig", "och", "stilistiken", "censores", 
                 "academisk", "egenlig", "christne", "gierne", "critik", 
                 "gjøre", "siel", "skiøn", "udentvivl", "igien", "gield",
                 "convenienz", "akcept", "lyriken", "bestandigen",
                 "bedrivt", "tragoedie", "gienstand", "forsikkre",
                 "manereret", "oeconomie", "altar", "krands", "yttre",
                 "daligdagse", "ligesålidt", "carakter", "sangvinisk",
                 "kiøkken", "vacuum", "idag", "igår", "imorgen", "hvergang",
                 "hemme", "tærskelen", "romantiken", "forlængst", "roboter",
                 "istedet", "ejheller", "principer", "avgørende", "tekniken",
                 "kritiken", "harcellere", "hvadenten", "kroniken", "zirlig", 
                 "nårsomhelst", "nogetsomhelst", 
                 "literær", "fritar", "varetar", "fjærn", "ihvertfald", 
                 "somom", "schizofreni", "spontaneitet", "igang", 
                 "frådse", "fetisch", "fornylig", "sålænge", "focus", "mangelen",
                 "hørde", "tredie", "musæer", 
                 "familje", "giorde", "ståer", "blant",
                 "blevn", "vilie", "villie", "sverrig", "digtekonst", 
                 "pedanterie", "merckelig", "historicus", "harmonie", "beståe",
                 "regelen", "såsnart", "crystallisere", "pleie", "tøile",
                 "physisk", "physik", "ahne", "leilighed", "friemodighed", 
                 "tilståe", "begåe", "litterair", "literair", "bereid", 
                 "istedetfor", "vei", "componere", "copi", "kierlig", "bielker",
                 "skieve", "kierne", "forekastelse", "prase", "colorit", "reise",
                 "costume", "kiæk", "keithåndet", "ingredients", "forskiel",
                 "monotonie", "pandser", "strophe", "radical", "prosodie", 
                 "osillere", "begvem", "refleter", "trochæ", "ængsteligen",
                 "musicalsk", "siæl", "scansion", "scandere", "rythmus", 
                 "ubesværget", "analogie", "bebreid", "overdækt", "nedhængne", 
                 "kielder", "class", "kiend", "mathematisk", "mathematik", 
                 "hvilketsomhelst", "gjæld", "atheist", "courant", "captain", 
                 "yttring", "punct", "forsikkring", "theologi", "absolute", 
                 "skjælne", "skielne", "theori", "orthodoks", "stakkelsk", 
                 "skjæbne", "skjænke", "thema", "kjærlig", "pathos", "cynisme",
                 "kynisme", "naivitet", "kjed", "neppe", "feig", "fremstilt",
                 "beroer", "vædske", "vindve", "repliken", "havt", "kjølig", 
                 "lytted", "daled", "stjærne", "nejed", "moleculer", "fuldkomne",
                 "dreie", "locale", "decorum", "seir", "clericale", "triumph", 
                 "continent", "arbeid", "production", "productiv", "discussion",
                 "bøie", "abstract", "gjænger", "psychologi", "plebeier", 
                 "regjering", "sympathi", "affairer", "epoche", "redacteur", 
                 "nåer", "drapperiet", "geist", "dekadence", "convention", 
                 "convenients", 
                 "logiken", "raisonnere", "tarrain", "philosoph", "svarte", 
                 "istedenfor", "paranthes", "ævne", "gærne", "repliker", 
                 "repliken", "skemte", "tjæneste", "horizont", 
                 "tillæmpe", "fjæld", "tredserne", "fremstilte", "uselig", 
                 "selsom", "dækte", "nelde", "læssed", "japanesisk",
                 "hvalv", "offenlig", "dybtsørgmodig", "tilsidst", "grønsværet",
                 "bibliothek", "pludseligen", "moer", "engelen", "skjælv", 
                 "løgte", "glands", "gothisk", "sjunkne", "speil", "laterna", 
                 "gjær", "toug", "muur", "forbause", "mythologi", "luttre", 
                 "gjør", "electro", "chocolade", "olje", "principet", "principer",
                 "halvfier", "filosofen", "renaissancen", "mystiken", "drøi", 
                 "tendents", "sænkte", "gientag", "representere", "pathetisk", 
                 "restriction", "hvormeget", "sphære", "technisk", "modification",
                 "critisk", "skjebne", "skjød", "skruptudse", 
                 "hjærne", "marsch", "sjelden", "konsekvents", "snærpet", 
                 "snærperi", "beskjæftige", "hypothese", "ialfald", "begjær", 
                 "såvidt", "stræng", "streng", "medlemsskab", "forbudene", 
                 "iforvejen", "mueligen", "væsentligen", "provindsiel", "ændse",
                 "physiognomie", "gjerne", "jydsk", "begrændsning", "melancholsk",
                 "propietair", "sanddrue", "detmere", "phlegmatisk", 
                 "catastrophe", "contrast", "sandselig", "heldigen", "affect", 
                 "accurat", "curere", "hyppigen", "vanzire", 
                 "conversation", "phase", "kjed", "physiologi", "modernt", 
                 "hannem", "nysgjerrig", "gjemme", "skjær", "bouket", "detailer",
                 "offre", "skierpe", "hielp", "forstode", "frekke", 
                 "legge", "genie", "sprogkonst", "merke", "skiemt", "treffe", 
                 "kjøb", "tilstrækkeligen", "løseligen", "gierning", "skiæl", 
                 "skelv", "kiær", "nødvendigen", "hykkelsk", "virkeligen", 
                 "muelig", "ærligen", "uophørligen", "tilværen", "fuldstændigen",
                 "roligen", "uroelig", "alvorligen", "giød", "opstode", 
                 "oeconomie", "litterarisk", "tidligen", "sildigen", 
                 "umiskendeligen", "hyklerie", "eremiten", "lettteligen", 
                 "vægelsindighed", "tvertimod", "religiositet", "ostlig", 
                 "uundgåeligen", "hiem", "creditor", "prophet", 
                 "elegie", "studerte", "theoretisk", "delede", "udelukte", 
                 "mestere", "catholsk", "skribentere", "mønstere", "værde", 
                 "skiæbne", "mystiken", "gjerning", "colossale", 
                 "rethorisk", "diction", "continuitet", "perspectiv", "adæquat",
                 "constitution", "idetmindste", "qualitet", "hjelp", "particulær", 
                 "versification", "intrigue", "opnåes", "crasse", "ironie", 
                 "subject", "consolidere",
                 "ordenlig", "studeringer", "eien", "contour", "kraftigen", 
                 "kunstigen", "laconisk", "giest", "formeget", "nogentid", 
                 "jevn", "vekke", "såmeget", "opmerksom", "elskte",
                 "begier", "zitre", "sette", "enthusiastisk", "enthusiasmus", 
                 "efterat", "rector", "anedocte", "stial", "leksicon", "formegen",
                 "giering", "declamere", "forverre", "skrekkelig", 
                 "skiald", "adspredede", "bierg", "skræmmet", "konsten", 
                 "skiebne", "slegt", "brekke", "kierlig", "strekke", "tidt", 
                 "studentere", "hialp", "konstig", "skienk", "lectie", "verre", 
                 "act",
                 "hvidske", "declamation", "discurs", "klekke", "elskte", 
                 "shakespear", "konster", "action", "gienstridig", "fiende", 
                 "correction", "anmerk", "theisme", "fysiken", 
                 "hieroglyfer", "forkommer", "kvern", "overskudet", "gøer", 
                 "affichere", "skielv", "historico", "philologo", 
                 "historicum", "derfore", "puure", "studium", "judicium", 
                 "hitorici", "beged",
                 "crisis", "materialier", "erfarend", "prægtigen", "hærlig", 
                 "hvormeget", "forderv", "udaf", "grammatica", "contribuere", 
                 "syntaksi",  "underkasted", "forvirrelse", "monarchi", 
                 "endeligen", 
                 "litturgie", "øjene", "applausu", "acteur", "rosede", "hvorudi",
                 "capacitet", "maverhed", "hendelse", "mældt", "stedsvarende", 
                 "plaidere", "suurt", "confunder", "roese", "gemenligen", 
                 "bryllop", "recommendere", "geleidet", "derudi", "maverste", 
                 "tenkning", "comoedianter", "ublue", "apothek","menge", "smørre", 
                 "gandske", "giordte", "munck", "smuck", "jesuiter", "forre", 
                 "styrcke", "gesant", "spurdte", "regiere", "kydsk", "dobbel", 
                 "dissonanc", "menniske", "schyttisk", "objection", "beskicke", 
                 "engeland", "caffe", "høy", "sckickelig", "parterren", "agered",
                 "ufornøden", "ridiculum", "teatro", "teatrum", "minst", "klygte",
                 "knæppe", "smycke", "slåprok", "iligemåde", "keyser", "sæck", 
                 "flytted", "delicat", "stilled", "hvorfore", "selland", 
                 "sielland", "bemægte", "herredomme", "befænget", "forliebt", 
                 "henge", "ickun", "kundet", "førdt", "censurered", "lycke", 
                 "beskyldet", "vertshus", "criticere", "honnete", "tenck", 
                 "campagne", "spilled", "møjed", "desmindre", "begrebe", "øyne", 
                 "locke", "forderved", "tankker", "corrigere", "arbeyd", 
                 "fornemmeligen", "feyl", "gierrig", "øye", "ikkun", "mavre", 
                 "giendrive", "læset", "naturel", "inclinere", "titelen",
                 "reuissere", "literis", "incommodere", "speyl", 
                 "christelig", "christne", "begjær", "imøde", "enhversomhelst",
                 "biographisk", "sikkre", "ethisk", "offrelse", "idetmindste", 
                 "streif", "correspondance", "vanvidet", "tilbunds", "skjævt", 
                 "nøiagtig", "døttre", "collision", "cholera", "pretention", 
                 "dramer", "soel", "veir", "giæld", "psalme", "apostelen", 
                 "skiæv", "skabde", "skiænk", "tabernakelet", "disciplerne", 
                 "gjærne", "hørde", "skjemt", "gjeld", "dørre", "forbluffe", 
                 "kneise", "forgjæves", "forgiæves", "trykde", "nøgelen", 
                 "tabde", "thing", "kyndig", "feide", "commentar", "spodsk", 
                 "kjætter", 
                 "tilfælleds", "adskildte", "stundum", "sødskende", "mythe", 
                 "zittren", "velbehagen", "dispyt", "uimodsigeling", "bisar", 
                 "nærværende", "mylre", "vilkårligen", "spasere", "sjungne", 
                 "deminutiv", "ekscerpere", "kryderier", "sjieldne", "prædicat",
                 "consegvent", "lereår", "sentents", "sympathie", "parodie", 
                 "gotiken", "fejghed", "coutume", "vedhængen", "patroillerer", 
                 "kvidrrer", "feie", "bittre", "intelligents", "mechanisk", 
                 "contruction", "synthese", "gærning", "jærn", "hierte", 
                 "detaillert")


#modern versions of the same words

moderne_ord_c <- c("korrekt", "korrekt", "kinesisk",
                   "kanon", "publikum", "parti", "skøn", "nægte", 
                   "fantasi", "prinsesse", "kæmpe", "individualitet", 
                   "psykologisk", "konjungere", "klaver", "detaljeret", 
                   "næppe", "æstetisk", "fænomen", "fejl", "kend", "sekulær", 
                   "forudanet", "linje", "trejde", "hæfte", 
                   "hoved", "attitude", "også", "fission", "skulle", "litteratur", 
                   "gør", "gjort", "komedie", "fortjen", "diskret", "øje", 
                   "hjerte", "ikke", "skuespil", "stykke", "skøn",  "komisk", 
                   "gennem", "antikke", "karakter", "sjælden", 
                   "vej", "øjne", "afhjælpe", "kolonne", 
                   "nøjere", "ejen", "kærlighed", "igen",
                   "forskel", "eventyr", "kondition", 
                   "gøre", "kompleks", "tysk", "nøje", 
                   "kende", "metode", "fløj", "mælkevej", "høj",
                   "litteratur", "skønt", "kære", "tavshed", "øhlenschlæger", 
                   "øhlenschlæger", "øhlenschlæger", "umulig", "dejlig", "og", 
                   "stilistikken", "censorer", "akademisk", "egentlig",
                   "kristne", "gerne", "kritik", "gøre", "sjæl", 
                   "skøn", "uden tvivl", "igen", "gæld", "konveniens", "accept",
                   "lyrikken", "bestandigt", "bedrift",
                   "tragedie", "genstand", "forsikre", "manieret", "økonomi",
                   "alter", "krans", "ytre", "daligdags", "lige så lidt",
                   "karakter", "sangvnisk", "køkken", "vakuum", "i dag", "i går",
                   "i morgen", "hver gang", "hæmme", "tærsklen", "romatikken", "for længst",
                   "robotter", "i stedet", "ej heller", "principper", "afgørende",
                   "teknikken", "kritikken", "harcelere",
                   "hvad enten", "kronikken", "sirlig", "når som helst", 
                   "noget som helst", "litterær", "fritager", 
                   "varetager", "fjern", "i hvert fald", "som om", 
                   "skizofreni", "spontanitet", "i gang", "fråse", 
                   "fetich", "for nylig", "så længe", "fokus", "manglen", 
                   "hørte", "tredje", "museer", 
                   "familie", "gjorde", "står", "blandt", "bleven",
                   "vilje", "vilje", "sverige", "digtekunst", "pedanteri", 
                   "mærkelig", "historiker", "harmoni", "bestå", "reglen", 
                   "så snart", "krystallisere", "pleje", "tøjle", "fysisk", 
                   "fysik", "ane", "lejlighed", "frimodighed", "tilstå", "begå",
                   "litterær", "litterær","bebrejd", "istedet for", "vej", "komponere", 
                   "kopi", "kierlig", "bjælker", "skive", "kerne", "forkastelse",
                   "frase", "kolorit", "rejse", "kostume", "kæk", "kejthåndet", 
                   "ingrediens", "forskel", "monotoni", "panser", "strofe", 
                   "radikal", "prosodi", "oscillere", "bekvem", "reflekter", 
                   "trokæ", "ængsteligt", "musikalsk", "sjæl", "skansion", 
                   "skandere", "rytme", "ubesværet", "analogi", 
                   "bebrejd", "overdækket", "nedhængende", "kælder", "klass", 
                   "kend", "matematisk", "matematik", "hvilket som helst", "gæld",
                   "ateist", "ateisme", "kurant", "kaptajn", "ytring", "punkt", 
                   "forsikring", "teologi", "absolutte", "skelne", "skelne", 
                   "teori", "ortodoks", "stakkels", "skæbne", "skænke", "tema", 
                   "kærlig", "patos", "køre", "naivitet", "ked", "næppe", "fej",
                   "fremstillet", "beror", "væske", "vindue", "replikken", 
                   "havde", "kølig", "lyttede", "dalede", "stjerne", "nejede", 
                   "molekyler", "fuldkommene", "dreje", "lokale", "dekorum", 
                   "sejr", "klerikale", "triumf", "kontinent", "arbejd", 
                   "produktion", "produktiv", "diskussion", "bøje", "abstrakt", 
                   "gænger", "psykologi", "plebejer", 
                   "regering", "sympati", "affære", "epoke", "redaktør",
                   "når", "draperiet", "gejst", "dekadance", 
                   "konvention", "konveniens", "logikken", "ræsonnere", "terræn",
                   "filosof", "svarede", "istedet for", "parantes", "evne", 
                   "gerne", "replikker", "replikken", "skimte", "tjeneste", 
                   "horisont", "tillempe", "fjeld", "tresserne", "fremstillede",
                   "ussel", "sælsom", "dækkede", "nælde", "læssede", "japansk", 
                   "hvælving",
                   "offentlig", "dybt sørgmodig", "til sidst", "grønsværen", 
                   "bibliotek", "pludseligt", "mor", "englen", "skælv", "lygte",
                   "glans", "gotisk", "sunkne", "spejl", "lanterne", "gær", "tov",
                   "mur", "forbavse", "mytologi", "lutre", "gør", "elektro", 
                   "chokolade",
                   "olie", "princippet", "principper", "halvfjers", "filosoffen",
                   "renaissancen", "mystiken", "drøi", "tendens", "sænkede", 
                   "gentag", "repræsentere", "patetisk","restriktion", 
                   "hvor meget", "sfære",
                   "teknisk", "modifikation", "kritisk", "skæbne", "skød", 
                   "skrubtudse", "hjerne", "march", "sjælden", "konsekvens", 
                   "snerpet", "snerperi", "beskæftige", "hypotese", "i al fald", 
                   "begær", "så vidt", "streng", "trussel", "medlemskab", 
                   "forbuddene", "i forvejen", "muligt", "væsentligt", 
                   "proviensiel", "ænse", "fysiognomi", "gerne", "jysk", 
                   "begrænsning", "melankolsk", "proprietær", "sanddru", 
                   "det mere", "flegmatisk", "katastrofe", "kontrast", "sanselig",
                   "heldigt", "affekt", "akkurat", "kurere", "hyppigt", 
                   "vansire", "konversation", "fase", "ked", "fysiologi", 
                   "moderne", "han", "nysgjerrig", "gemme", "skær", "buket", 
                   "detaljer", "ofre", "skærpe", "hjælp", "forstod", "frække", 
                   "lægge", "geni", "sprogkonst", "mærke", "skæmt", "træffe", 
                   "køb", "tilstrækkeligt", "løseligt", "gerning", "skeln", 
                   "skælv", "kær", "nødvendigt", "hyklerisk", "virkeligt", 
                   "mulig", "ærligt", "uophørligt", "tilværelsen", "fuldstændigt",
                   "roligt", "urolig", "alvorligt", "gød", "opstod", "økonomi", 
                   "litterær", "tidligt", "sildigt", "umiskendeligt",
                   "hykleri", "eremitten", "let", "vægelsindethed", "tværtimod",
                   "religiøsitet", "østlig", "uundgåeligt", "hjem", "kreditor", 
                   "profet", "elegi", "studerede", "teoretisk", "delte", 
                   "udelukkede", "mestre", "katolsk", "skribenter", "mønstrer",
                   "værd", "skæbne", "mystikken", "gerning", "kolossale",
                   "retorisk", "diktion", "kontinuitet", "perspektiv", "adækvat", 
                   "konstitution", "i det mindste", "kvalitet", "hjælp", 
                   "partikulær", "versifikation", "intrige", "opnås", "krasse",
                   "ironi", "subjekt", "konsolidere", "ordentlig", "studier", 
                   "ejen", "kontur", "kraftigt", "kunstigt", "lakonisk", "gæst", 
                   "for meget", "nogen tid", "jævn", "vække", "så meget", 
                   "opmærksom", "elskede", "begær", "sitre", "sætte", 
                   "entusiastisk", 
                   "entusiasme", "efter at", "rektor", "anekdote", "stjal", 
                   "leksikon", "for megen", "gæring", "deklamere", 
                   "forværre", "skrækkelig", "skjald", "adspredte", "bjerg", 
                   "skræmt", "kunsten", "skæbne", "slægt", "brække", "kærlig", 
                   "stræke", "tit", "studenter", "hjalp", 
                   "kunstig", "skænk", "lektie", "værre", "akt", "hviske", 
                   "deklamation", "diskurs", "klække", "elskede", "shakespeare", 
                   "kunster", "aktion", "genstridig", "fjende", "korrektion", 
                   "anmærk", "teisme", "fysikken", "hieroglyffer", "forekommer",
                   "kværn", "overskuddet", "gør", "afficere", "skælv", "historie",
                   "filolog", "historiker", "derfor", "pure", "studie", 
                   "dømmekraft", " histories", "beget", "kritisk evne",
                   "materialer", "erfaren", 
                   "prægtigt", "herlig", "hvor meget", "fordærv", "ud af", 
                   "grammatik", "konstribuere", "syntaks", "underkastet",
                   "forvirring", "monarki", "endeligt", "liiturgi", "øjnene", 
                   "applaus", "aktør", "rost", "hvori", "kapacitet", 
                   "magerhed", "hændelse", "meldt", "stedsevarende", "plædere", 
                   "surt", "konfunder", "rose", "gemenlig", "bryllup", 
                   "rekommandere", "glejdet", "deri", "magerste", 
                   "tænkning", "komdieanter", "ublu", "apotek", "mænge", "smøre", 
                   "ganske", "gjort", "munk", "smuk", "jesuitter", "fore", 
                   "styrke", "gesandt", "spurgte", "regere", "kysk", 
                   "dobbelt", "dissonans", "menneske", "skytisk", "objektion", 
                   "beskikke", "england", "kaffe", "høj", "skikkelig", 
                   "parterret", "ageret", "ufornødent", "latterligt", "teater", 
                   "teater", "mindst", "klygte", "knappe", "smykke", "slåbrok", 
                   "i lige måde", "kejser", "sæk", "flyttet", 
                   "delikat", "stillet", "hvorfor", "sjælland", "sjælland", 
                   "bemægtige", "herredømme", "befængt", "forlibt", "hænge", 
                   "kun", "kunnet", "ført", "censureret", "lykke", 
                   "beskyldt", "værtshus", "kritisere", "honnet", "tænk", 
                   "kampagne", 
                   "spillet", "møjet", "des mindre", "begreb", "øjne", "lokke", 
                   "for derved", "tanker", "korrigere", "arbejde", "fornemmeligt",
                   "fejl", "gerrig", "øje", "kun", "magre", "gendrive", "læst", 
                   "natur", "inklinere", "titlen", 
                   "reussere", "litteraturen", "inkommodere", "spejl", 
                   "kristelig", "kristne", 
                   "begær", "i møde", "en hver som helst", "biografisk", "sikre",
                   "etisk", "ofre", "i det mindste", "strejf", "korrespondance", 
                   "vanviddet", "til bunds", "skævt", "nøjagtig", "døtre", 
                   "kollision", "kolera", "prætention", "dramaer", "sol", "vejr", 
                   "gæld", "salme", "apostlen", "skæv", "skabte", "skænk", 
                   "tabernaklet", "disciplene", "gerne", "hørte", "skæmt", "gæld", 
                   "døre", "forbløffe", "knejse", "forgæves", "forgæves", "trykte", 
                   "nøglen", "tabte", "ting", "kyndig", "fejde", "kommentar", 
                   "spotsk", "kætter", "tilfælles", "adskilte", "stundom", 
                   "søskende", "myte", "sitren", "velbehaget", "disput", 
                   "uimdodsigeligt", "bizar", "nærværende", "myldre", "vilkårligt", 
                   "spadsere", "sungne", "diminutiv", "ekserpere", "krydderier", 
                   "sjældne", "prædikat", "konsekvent", "læreår", 
                   "sentens", "sympati", "parodi", "gotikken", "fejhed", "kutyme", 
                   "vedhængte", "patruljerer", "kvidrer", "feje", "bitre", 
                   "intelligens", "mekanisk", "konstruktion", "syntese", "gerning", 
                   "jern", "hjerte", "detaljeret")



#exchanges archaic spellings with modern

korpus <- mgsub(korpus, gamle_ord_c, moderne_ord_c, fixed = TRUE)

korpus <- gsub("marks", "marx", korpus)


#this does the same as the previous lists and commands 
#but here exact matching is nessecary

exact_match_ord_fra <- c("selvog", "bir", "wekend", "jeget", "billed", "hvo", 
                         "kand", "mer", "fler", "ere", "anm", "forf", "vill", "troe", 
                         "konst", "ha", "blie", "gir", "ber", "be", "døe", "bleve", 
                         "poesie", "blir", "osse", "angåer", "allene", "hand", 
                         "forståe", "påståe", "gåer", "måe", "såe", "låe", "ei", "gåe",
                         "ståe", "kopie", "karakteristiken", "formåe", "formåer", 
                         "poesies", "bleve", "slåe", "helder", "filosofie", 
                         "parantese", "låe", "glede", "spejlbilled", "choc", "trusel",
                         "frøe", "skelning", "roe", "spilte", "tilfredstille", "tact",
                         "verk", "verket", "afskye", "got", "gotte", "chor", "best", 
                         "beste",
                         "ruus", "konsts", "filologos", "ackirere", "satt", "kiede", 
                         "øjen", "tvil", "udi", "erholdet", "sant", "outrered", 
                         "crere", "ald", "karatere", "mætt", "ey", "sahl", 
                         "skribentes", "filosofis", "austeritet", "auster", "asutere", 
                         "holdte",
                         "offre", "pg", "flye", "skye", "følde", "døer", "trode", 
                         "eier", "toe", "vort", "måt", "værdie")

exact_match_ord_til <- c("selv og", "bliver", "weekend", "jeg'et", "billede", 
                         "hvor", "kan", "mere", "flere", "er", "anmelderen",
                         "forfatteren", "vil", "tro", "kunst", "ha", "blive", "giver", 
                         "beder", "bede", "dø", "blev", "poesi", "bliver", "også",
                         "angår", "alene", "han", "forstå", "påstå", "går", "må", "så",
                         "lå", "ej", "gå", "stå", "kopi", "karakteristikken",
                         "formå", "formår", "poesi",
                         "blev", "slå", "hælder", "filosofi", "parantes", "lå", "gled",
                         "spejlbillede", "chok", "trussel", "frø", "skelnen", "ro", 
                         "spildte", "tilfredsstille", "takt", "værk", "værket", "afsky", 
                         "godt", "godte", "kor", "bedst", "bedste", "rus", "kunsts", 
                         "filolog", "akkvirere", "satte", "kæde", "øjne", "tvivl", "i",
                         "erholdt", "sandt", "outreret", "udnævne", "al", "karakter", 
                         "mæt", "ej", "sal", "skribents", "filosoffer", "alvorlighed", 
                         "alvorlig", "alvorlige", "holdt", "ofre", "på side", "fly", 
                         "sky", "føle", "dør", "troede", "ejer", "tog", "vores", 
                         "måtte", "værdi")


fejl_ord <- c("fakts", "ide", "ider", "iden", "iderne", "idernes", "fantasir",  
              "fåt", "seks", "markveret", "måttete")

rettelses_ord <- c("facts", "idee", "ideer", "ideen", "ideerne", "ideernes", 
                   "fantasi", "fået", "sex", "makeret", "måtte")



#this transforms the corpus into a dataframe

korpus_dfm <- dfm(korpus, remove = stopwords("da", source = "stopwords-iso"), 
                  remove_punct = TRUE, remove_numbers = TRUE)
korpus_dfm <- dfm_select(korpus_dfm, "er", "remove")


korpus_dfm <- dfm_replace(korpus_dfm, exact_match_ord_fra, exact_match_ord_til)
korpus_dfm <- dfm_replace(korpus_dfm, fejl_ord, rettelses_ord)

#this removes stopwords
korpus_dfm_stopord <- dfm_select(korpus_dfm, stopord, selection = "remove")



#this is for if you want to cut the corpus into chunnks

tokens_korpus <- tokens(korpus)

#you can experiment wit vaious chunk sizes
chunks <- tokens_chunk(tokens_korpus, 500)

tokens_dfm <- dfm(chunks, remove = stopwords("da", source = "stopwords-iso"), 
                  remove_punct = TRUE, remove_numbers = TRUE)


tokens_dfm <- dfm_replace(tokens_dfm, exact_match_ord_fra, exact_match_ord_til)
tokens_dfm <- dfm_replace(tokens_dfm, fejl_ord, rettelses_ord)



#this is a built-in function in the STM-package that searches the corpus 
#for an
searchK(korpus_dfm, K = c(5, 10, 15, 20, 25, 30), N = 1)

#This plots the values of the simualation
plot.searchK(.Last.value)


mod_20 <- stm(korpus_dfm, K = 20, prevalence = ~Køn + s(Årstal), 
              data = docvars(korpus))


#this gives you the probability of a word given a topic

termprob <- function(w,t,model) {
  m <- tidy(model)
  num <- as.numeric(filter(m, term == as.character(w), topic == t)[,3])
  return(num)
  
}

#this computes th geometric mean of a word over the number of topics in the model

gmean <- function(model,w,k) {
  m <- tidy(model)
  num <- do.call(sum, lapply(subset(m, term == as.character(w))[,3], log))
  mean <- exp(num/k)
  return(mean)
}


#this computes a words termscore - an alternative to its probability rating
#see David Blei for more info
termscore <- function(w,t,model,k) {
  num <- termprob(w,t,model)*log(termprob(w,t,model)/gmean(model,w,k))
  return(num)
}


#this returns a list of the words most assocaited with a topic 
#based on their termscores
ts_model_topics <- function(model, t, w, k) {
  topicwords <- labelTopics(model, topics = t , n = w)[["prob"]][t,]
  termscore <- lapply(topicwords, termscore, t, model, k)
  termscore_c <- as.character(termscore)
  names(termscore_c) <- topicwords
  emne <- names(termscore_c[order(as.numeric(termscore_c), decreasing = TRUE)])
  emne_stopord <- emne[! emne %in% stopord]
  return(emne_stopord)
  
  
  
}


#this creates a barplot of each documents association to the different topics
td_theta <- tidy(mod_20,  matrix = "theta")
selectiontdthteta<-td_theta[td_theta$document%in%c(1:5), ]

thetaplot1<-ggplot(selectiontdthteta,  aes(y=gamma,  x=as.factor(topic),  fill = as.factor(topic))) +
  geom_bar(stat="identity", alpha = 0.8,  show.legend = FALSE) +
  facet_wrap(~ document,  ncol = 3) +
  labs(title = "Theta values per document", 
       y = expression(theta),  x = "Topic")
thetaplot1


#a list of stopwords. Removing stopwords is usually done before modelling
#but is David Mimno among others have pointed out, 
#it doesn't make a lot of difference if you do it before or after

stopord <- c("hele", "måske", "mellem", "uden", "derfor", "bo", "altid", "går",
             "ejvind", "netop", "ligger", "marianne", "sam", "således", 
             "sidste", "allerede", "nok", "gå", "dens", "larsen", "f.eks", 
             "heller", "samtidig", "søren", "står", "set", "taget", "gøre", 
             "gør", "findes", "finde", "larsen", "ingvarsen", "larsens", 
             "altså", "michael", "blot", "forskellige", "gennem", "både",
             "stadig", "vel", "samme", "vilde", "kunde", "endnu", "skulle",
             "niels", "er", "einar", "enten", "vist", "ligesom", "gang", 
             "enten", "vore", "lyhne", "al", "måtte", "elkær", "ganske",
             "hvert", "siger", "hvilke", "lade", "ofte", "jacobsen", "frem",
             "måde", "langt", "give", "snart", "tilbage", "tager", "f", "eks",
             "a", "mindre", "sagde", "marcolfus", "salomon", "v", "gjort",
             "kampmann", "alligevel", "dermed", "eksempel", "især", "enkelte",
             "slags", "dets", "finder", "blevet", "hr", "s", "vilhelm", 
             "siden", "bør", "o", "derved", "engang", "udi", "kan", "ikke",
             "aldeles", "mere", "mest", "haver", "derimod", "just", "allene",
             "hand", "større", "deraf", "ligeså", "slige", "hvori", "b", 
             "igen", "klart", "ei", "lader", "selve", "villy", "naturligvis",
             "andersen", "ting", "nemlig", "cæsar", "oeh", "oehs", "længere",
             "bjørn", "senere", "hvilket", "viser", "bleven", "slet", "længe",
             "vort", "dag", "kistrup", "sagt", "goethe", "egentlig", "deri",
             "støvgran", "hin", "vise", "gåe", "muligt", "beckett", "mary",
             "egentlige", "poulsen", "sara", "ellers", "blandt", "hvormed",
             "endog", "visse", "såsom", "efterdi", "sådant", "stod", 
             "hvorledes", "overfor", "givet", "egen", "inden", "mallarme",
             "rifbjerg", "osv", "erik", "klaus", "baggesen", "brutus", 
             "antonius", "cassius", "gerne", "dels", "holde", "rette",
             "sætter", "g", "videre", "hænger", "hine", "bag", "haft", "hen",
             "b", "hjernøe", "bl", "vidt", "sørensen", "bredsdorff",
             "imidlertid", "selvfølgelig", "pludselig", "sidder", "sker",
             "overhovedet", "nå", "franck", "fris", "oe", "hundstein",
             "hejlskov", "monte", "lema", "erichsen", "maja", "holger",
             "aladdin", "istedet", "helst", "per", "robert", "jens", "fået",
             "helveg", "bønnelycke", "bomholt", "galster", "kr", "atter",
             "helst", "dina", "etc", "an", "hidindtil", "price", "heri",
             "ulf", "johannes","esther", "pg", "medens", "ii", "molieres",
             "pan", "baudelaire", "p", "jacobsens", "jørgen", "pontoppidan",
             "la","fks", "claussen", "c", "h", "enquist", "mccarthy",
             "påskud", "viggo", "petersen", "diderichsen", "mikkel", "asta",
             "olivia", "nordenhof", "frantzen", "franck", "samuel", "morten", 
             "rifbjergs", "elisabeth", "thomsen", "fischer", "poulsens",
             "cage", "harrits", "bl.a", "baggesens", "lukrets", "endel", "r",
             "ewald", "sanchez", "jane", "christine", "fennimore", "frithiof",
             "j", "m", "hansjørgen", "lewis", "thommesen", "lidman", "peter",
             "bs", "ci", "dinas", "ulfeld", "uldfelds", "n", "t", "erne",
             "frank", "johan", "d", "så", "rec", "voss", "holberg", "vil", 
             "austen", "des", "georg", "dvs", "5o", "knudsen", "gaulle",
             "maccarthys", "blicher", "dickens", "joyce", "sophus", "thorkild",
             "johs", "jensen", "k", "otto", "stangerup", "thomas", "lidmans", 
             "henrik", "d", "hvor", "o.s.v", "gik", "nielsen", "omendskønt", "i",
             "gjorde", "tit", "gik", "gider", "må", "ej", "lembourn", "imellem",
             "ske", "snarere", "omkring", "rundt", "sættes", "høre", "lagt", 
             "fried", "clausen", "koestler", "reich", "fried", "engbergpedersen",
             "dalland", "walter", "cave", "wace", "hamm", "harald", "vores", 
             "maksimilian", "igennem", "han", "svend", "corydon", "vivendi", 
             "karl", "philine", "wivel", "ebbe", "reykjavik", "anse", "blev", 
             "henseende", "åge", "holdt", "tvende", "fuldkommen", "endda", 
             "bestandig", "betræffer", "først", "første", "tid", "tiden", "tidens", 
             "gives", "ens", "lave", "tværtimod", "drejer", "bruge", "indenfor",
             "heraf", "tidligere", "hverken", "dertil", "forbi", "dersom", 
             "ethvert", "dengang", "såvel", "år", "mennesker", "verden", "mak",
             "okkultisten", "programudvalget", "privatisme", "virkeligheden", 
             "næppe", "møblement", "nye", "gamle", "escarpit")


vocab_stopwords <- remove_stopwords(mod_20[["vocab"]])

#this simply returns the topic words of each topic 
#but with the stopwords removed
emner <- function(model, t, w) {
  emne <- labelTopics(model, n = w)[["prob"]][t,]
  emne_stopord <- emne[! emne %in% stopord]
  return(emne_stopord)
  
}

#this is a different ranking of topic words based on frex score
emner_frex <- function(model, t, w){
  emne_frex <- remove_stopwords(labelTopics(model, n = w)[["frex"]][t,])
  return(emne_frex)
  
  
}



remove_stopwords <- function(w) {
  w <- w[! w %in% stopord]
  return(w)
  
}

#this returs the highest topic score for a given document
doc_theta <- function (i, model) {
  theta <- tidy(model, matrix = "theta")
  doc <- subset(theta, document == i)
  doc[which.max(doc$gamma),]

  
}

#this returns a list of all documents in the corpus 
#paired with their most associated topi
topic_docs <- function(t, model, n) {
  
  doc_list <- lapply(1:n, doc_theta, model)
  for(i in 1:n) 
    if (t == as.numeric(doc_list[[i]][,2]))
      (print(as.numeric(doc_list[[i]][,1])))
}


#this creates a wordcloud of a topic based on the words' probabillity ratings
cloud_stopwords <- function(mod, topic, wordcount, scale){
  words <- emner(mod, topic, wordcount)
  weigths <- as.numeric(lapply(words, termprob, topic, mod))
  wordcloud(words, weigths, scale)
}

#this does the same but based on termscore
cloud_ts <- function(mod, topic, wordcount, k, scale){
  words <- ts_model_topics(mod, topic, wordcount, k)
  weigths <- as.numeric(lapply(words, termscore, topic, mod, k))
  wordcloud(words, weigths, scale)
  
  
}


#this is where the larger target corpus is imported


target_data_tidsskrifter <- readtext(paste0(DIR, "/Target korpus/Tidsskrifter/*"),
                                     docvarsfrom = "filenames",
                                     docvarnames = c("Forfatter", "Titel",  "Årstal",  "Køn"), 
                                     dvsep = "_", 
                                     encoding = "UTF-8")


#here the same preprocessing is done to the target corpus
korpus_tidsskrifter <- corpus(target_data_tidsskrifter)

korpus_tidsskrifter <- tolower(korpus_tidsskrifter)
korpus_tidsskrifter <- gsub("-\\s*", "", korpus_tidsskrifter)
korpus_tidsskrifter <- gsub("¬\\s*", "", korpus_tidsskrifter)

korpus_tidsskrifter <- mgsub(korpus_tidsskrifter, gamle_staveformer_c, moderne_staveformer_c)
korpus_tidsskrifter <- mgsub(korpus_tidsskrifter, gamle_ord_c, moderne_ord_c, fixed = TRUE)

korpus_tidsskrifter <- gsub("marks", "marx", korpus_tidsskrifter)


dfm_korpus_tidsskrifter <- dfm(korpus_tidsskrifter, 
                               remove = stopwords("da", source = "stopwords-iso"),
                               remove_punct = TRUE, remove_numbers = TRUE)

dfm_korpus_tidsskrifter <- dfm_select(dfm_korpus_tidsskrifter, stopord, 
                                      selection = "remove")


dfm_korpus_tidsskrifter <- dfm_replace(dfm_korpus_tidsskrifter, exact_match_ord_fra, exact_match_ord_til)
dfm_korpus_tidsskrifter <- dfm_replace(dfm_korpus_tidsskrifter, fejl_ord, rettelses_ord)


#this changes to target corpus to the stm format which 
#makes it possible to search throug using the 'fitNewDocuments function'
target_korpus_tidsskrifter_stmformat <- convert(dfm_korpus_tidsskrifter, "stm")

target_korpus_tidsskrifter_stmformat_aligned <- alignCorpus(target_korpus_tidsskrifter_stmformat, mod_20$vocab)

topic_fit_tidsskrifter <- fitNewDocuments(mod_20, target_korpus_tidsskrifter_stmformat_aligned$documents)



#this computes the mean topic fit for the new doucments which makes 
#it possible to get an overview of the best matches
gamma_mean <- function(topic, fit){
  sum(fit$theta[,topic])/length(fit$theta[,topic])
}

#this sorts the documents in the target corpus based on their fit
gamma_sort <- function(topic, fit){
  gamma_values <- fit$theta[,topic]
  names(gamma_values) <- c(1:length(gamma_values))
  sort(gamma_values, decreasing = TRUE)
  
}







