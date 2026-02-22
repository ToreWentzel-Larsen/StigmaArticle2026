library(foreign)
library(tis)
library(psy)
library(psych)
library(nlme)

# function for half rule, for use in rows of data, to compute mean score
# til beregning av gjennomsnittsskC%r
halfrulef <- function(xx) {
  nx <- length(xx)
  nv <- sum(1-is.na(xx))
  res <- NA
  if (2*nv>=nx) res <- mean(xx, na.rm=TRUE)
  return(res)
} # end function halfrulef

# enter data from the two studies, ec for ECHO and ti fromTIM
ec <- read.spss("../ECHO_elevskjema_masterfil.sav", to.data.frame=TRUE)
ti <- read.spss("../T1_T2_Barn_Masterfil.sav", to.data.frame=TRUE)
names(ec)
names(ti)

# ECHO 
summary(ec[,c("t2stigmac01_1","t2stigmac02_2","t2stigmac03_3","t2stigmac04_4",
           "t2stigmac05_5","t2stigmac06_6","t2stigmac07_7","t2stigmac08_8",
              "t2stigmac09_9","t2stigmac10_10")])
# ECHO data with valid stigma variable
es <- ec[is.na(ec$t2stigmac01_1)==0,]

# TIM
summary(ti[,c("ace01","ace02","ace03","ace04","ace05","ace06","ace07",
              "ace08","ace09","ace10")])
# TIM with valid stigma (checked same for all stigma variables) 
ts <- ti[is.na(ti$ace01)==0,]

# preparing for merging
es$userid <- as.character(es$barnid) # reconciling id variables in ECHO and TIM
es$skole <- factor(es$skole) # variable name skole: school name
ts$skole <- factor(ts$skole)
table(ts$skole, useNA="always") # some schools have trouble with Norwegian letters, fix
levels(ts$skole) # identify troubles
levels(ts$skole)[c(4,16)] # these are the troubles, fix
levels(ts$skole)[c(4,16)] <- c(".senhagen skole",".stbyen")
ts$trinn <- factor(ts$trinn) # variable name trinn_ grade level
summary(es$trinn)
levels(es$trinn)
summary(ts$trinn) # one missing, lenger categiry names, reconcile
levels(ts$trinn)
levels(ts$trinn) <- 3:6
summary(ts$trinn) # ok
summary(c(es$trinn, ts$trinn)) # looks ok
# look at the sex variable, variablename bkjonn
summary(es$bkjonn) # Gutt and Jente, Norwegian for Boy and Gilr, ok
# makes sex variable in es called gender
es$gender <- es$bkjonn
summary(ts$gender) # longer category names, fix
levels(ts$gender)
levels(ts$gender) <- c("Gutt","Jente") # to reconcile
summary(ts$gender)

# enter a type variable, 1 for ECHO and 2 for <TIM
es$type <- 1
es$type <- factor(es$type, levels=1:2, labels=c("ECHO","TIM"))
ts$type <- 2
ts$type <- factor(ts$type, levels=1:2, labels=c("ECHO","TIM"))
summary(es$type)
summary(ts$type)
summary(c(es$type,ts$type)) # ok

# look at the  stigma variables
summary(es$t2stigmac01_1)
summary(ts$ace01)
summary(c(es$t2stigmac01_1,ts$ace01)) # looks ok
levels(c(es$t2stigmac01_1,ts$ace01))  # looks ok
# change to reconcile variable names i es (ECHO) to the same as in ts (TIM) 
varnr.es.stigma <- (1:dim(es)[2])[names(es) %in% c("t2stigmac01_1","t2stigmac10_10")]
varnr.es.stigma # check
names(es)[varnr.es.stigma[1]:varnr.es.stigma[2]]
names(es)[varnr.es.stigma[1]:varnr.es.stigma[2]] <- 
  c("ace01","ace02","ace03","ace04","ace05","ace06","ace07","ace08","ace09","ace10")
# change to numeric variables
es$ace01n <- as.numeric(es$ace01)
es$ace02n <- as.numeric(es$ace02)
es$ace03n <- as.numeric(es$ace03)
es$ace04n <- as.numeric(es$ace04)
es$ace05n <- as.numeric(es$ace05)
es$ace06n <- as.numeric(es$ace06)
es$ace07n <- as.numeric(es$ace07)
es$ace08n <- as.numeric(es$ace08)
es$ace09n <- as.numeric(es$ace09)
es$ace10n <- as.numeric(es$ace10)
ts$ace01n <- as.numeric(ts$ace01)
ts$ace02n <- as.numeric(ts$ace02)
ts$ace03n <- as.numeric(ts$ace03)
ts$ace04n <- as.numeric(ts$ace04)
ts$ace05n <- as.numeric(ts$ace05)
ts$ace06n <- as.numeric(ts$ace06)
ts$ace07n <- as.numeric(ts$ace07)
ts$ace08n <- as.numeric(ts$ace08)
ts$ace09n <- as.numeric(ts$ace09)
ts$ace10n <- as.numeric(ts$ace10)
summary(es[,c("ace01n","ace02n","ace03n","ace04n","ace05n",
              "ace06n","ace07n","ace08n","ace09n","ace10n")]) # 1:10, ok
summary(ts[,c("ace01n","ace02n","ace03n","ace04n","ace05n",
              "ace06n","ace07n","ace08n","ace09n","ace10n")]) # 1:10, ok

# look at MASC T1
summary(
  es[,c("t1mascc01_07_t1mascc01","t1mascc01_07_t1mascc02","t1mascc01_07_t1mascc03",
        "t1mascc01_07_t1mascc04","t1mascc01_07_t1mascc05","t1mascc01_07_t1mascc06",          
        "t1mascc01_07_t1mascc07","t1mascc08_14_t1mascc08","t1mascc08_14_t1mascc09",         
        "t1mascc08_14_t1mascc10","t1mascc08_14_t1mascc11","t1mascc08_14_t1mascc12",          
        "t1mascc08_14_t1mascc13","t1mascc08_14_t1mascc14","t1mascc15_21_t1mascc15",          
        "t1mascc15_21_t1mascc16","t1mascc15_21_t1mascc17","t1mascc15_21_t1mascc18",          
        "t1mascc15_21_t1mascc19","t1mascc15_21_t1mascc20","t1mascc15_21_t1mascc21",          
        "t1mascc22_28_t1mascc22","t1mascc22_28_t1mascc23","t1mascc22_28_t1mascc24",          
        "t1mascc22_28_t1mascc25","t1mascc22_28_t1mascc26","t1mascc22_28_t1mascc27",          
        "t1mascc22_28_t1mascc28","t1mascc29_35_t1mascc29","t1mascc29_35_t1mascc30",          
        "t1mascc29_35_t1mascc31","t1mascc29_35_t1mascc32","t1mascc29_35_t1mascc33",          
        "t1mascc29_35_t1mascc34","t1mascc29_35_t1mascc35","t1mascc36_39_t1mascc36",          
        "t1mascc36_39_t1mascc37","t1mascc36_39_t1mascc38","t1mascc36_39_t1mascc39")])
levels(es$t1mascc01_07_t1mascc06) # no missing, 4-delte factor with 4 categories, 
# levels:  Aldri,Sjelden,Noen ganger,Ofte, mening resp. Never, Seldom, Sometimes, Often
summary(ts[,c("bac01_1","bac01_2","bac01_3","bac01_4","bac01_5","bac01_6",
              "bac01_7","bac02_8","bac02_9","bac02_10","bac02_11","bac02_12",
              "bac02_13","bac02_14","bac03_15","bac03_16","bac03_17",
              "bac03_18","bac03_19","bac03_20","bac03_21","bac04_22",
              "bac04_23","bac04_24","bac04_25","bac04_26","bac04_27",
              "bac04_28","bac05_29","bac05_30","bac05_31","bac05_32",
              "bac05_33","bac05_34","bac05_35","bac06_36","bac06_37",
              "bac06_38","bac06_39")])
levels(ts$bac01_1) # same as in es (ECHO), merging should work
summary(c(es$t1mascc01_07_t1mascc01,ts$bac01_1)) # ok

# change to reconcile variabl enames for MASC
varnr.es.masct1 <- (1:dim(es)[2])[
  names(es) %in% c("t1mascc01_07_t1mascc01","t1mascc36_39_t1mascc39")]
varnr.es.masct1 # sjekker
names(es)[varnr.es.masct1[1]:varnr.es.masct1[2]]
names(es)[varnr.es.masct1[1]:varnr.es.masct1[2]] <- 
  c("bac01_1","bac01_2","bac01_3","bac01_4","bac01_5","bac01_6","bac01_7",
    "bac02_8","bac02_9","bac02_10","bac02_11","bac02_12",
    "bac02_13","bac02_14","bac03_15","bac03_16","bac03_17",
    "bac03_18","bac03_19","bac03_20","bac03_21","bac04_22",
    "bac04_23","bac04_24","bac04_25","bac04_26","bac04_27",
    "bac04_28","bac05_29","bac05_30","bac05_31","bac05_32",
    "bac05_33","bac05_34","bac05_35","bac06_36","bac06_37",
    "bac06_38","bac06_39")

# look at SMFQ T1
summary(
  es[,c("t1smfqc01_07_t1smfqc01","t1smfqc01_07_t1smfqc02","t1smfqc01_07_t1smfqc03",         
        "t1smfqc01_07_t1smfqc04","t1smfqc01_07_t1smfqc05","t1smfqc01_07_t1smfqc06",
        "t1smfqc01_07_t1smfqc07","t1smfqc08_14_t1smfqc08","t1smfqc08_14_t1smfqc09",          
        "t1smfqc08_14_t1smfqc10","t1smfqc08_14_t1smfqc11","t1smfqc08_14_t1smfqc12",          
        "t1smfqc08_14_t1smfqc13","t1smfqc08_14_t1smfqc14")])
# no misisng, values Ikke riktig,Noen ganger riktig,Riktig, meaning resp.
# Not correct, sometimes correct, Correct
summary(ts[,c("bmc01_1","bmc01_2","bmc01_3","bmc01_4","bmc01_5","bmc01_6",
              "bmc01_7","bmc02_8","bmc02_9","bmc02_10","bmc02_11","bmc02_12",
              "bmc02_13","bmc02_14")]) # samme som i es

# change variable names in es (ECHO) to reconcile for SMFQ T1 
varnr.es.smfqt1 <- (1:dim(es)[2])[
  names(es) %in% c("t1smfqc01_07_t1smfqc01","t1smfqc08_14_t1smfqc14")]
varnr.es.masct1 # sjekker
names(es)[varnr.es.smfqt1[1]:varnr.es.smfqt1[2]]
names(es)[varnr.es.smfqt1[1]:varnr.es.smfqt1[2]] <- 
  c("bmc01_1","bmc01_2","bmc01_3","bmc01_4","bmc01_5","bmc01_6",
    "bmc01_7","bmc02_8","bmc02_9","bmc02_10","bmc02_11","bmc02_12",
    "bmc02_13","bmc02_14")

# look at MASC T2
summary(
  es[,c("t2mascc01_07_t2mascc01","t2mascc01_07_t2mascc02","t2mascc01_07_t2mascc03",          
        "t2mascc01_07_t2mascc04","t2mascc01_07_t2mascc05","t2mascc01_07_t2mascc06", 
        "t2mascc01_07_t2mascc07","t2mascc08_14_t2mascc08","t2mascc08_14_t2mascc09",          
        "t2mascc08_14_t2mascc10","t2mascc08_14_t2mascc11","t2mascc08_14_t2mascc12",          
        "t2mascc08_14_t2mascc13","t2mascc08_14_t2mascc14","t2mascc15_21_t2mascc15",          
        "t2mascc15_21_t2mascc16","t2mascc15_21_t2mascc17","t2mascc15_21_t2mascc18",          
        "t2mascc15_21_t2mascc19","t2mascc15_21_t2mascc20","t2mascc15_21_t2mascc21",          
        "t2mascc22_28_t2mascc22","t2mascc22_28_t2mascc23","t2mascc22_28_t2mascc24",          
        "t2mascc22_28_t2mascc25","t2mascc22_28_t2mascc26","t2mascc22_28_t2mascc27",          
        "t2mascc22_28_t2mascc28","t2mascc29_35_t2mascc29","t2mascc29_35_t2mascc30",          
        "t2mascc29_35_t2mascc31","t2mascc29_35_t2mascc32","t2mascc29_35_t2mascc33",          
        "t2mascc29_35_t2mascc34","t2mascc29_35_t2mascc35","t2mascc36_39_t2mascc36",          
        "t2mascc36_39_t2mascc37","t2mascc36_39_t2mascc38","t2mascc36_39_t2mascc39")])
# no missing, factor, velues Aldri, Sjelden, Noen ganger, Ofte, meaning resp.
# Never, Seldom, Sometimes, Often
summary(
  ts[,c("pac01_1","pac01_2","pac01_3","pac01_4","pac01_5","pac01_6","pac01_7",
        "pac02_8","pac02_9","pac02_10","pac02_11","pac02_12","pac02_13",
        "pac02_14","pac03_15","pac03_16","pac03_17","pac03_18","pac03_19",
        "pac03_20","pac03_21","pac04_22","pac04_23","pac04_24","pac04_25",
        "pac04_26","pac04_27","pac04_28","pac05_29","pac05_30","pac05_31",
        "pac05_32","pac05_33","pac05_34","pac05_35","pac06_36","pac06_37",
        "pac06_38","pac06_39")])
# no missing, factor, values Aldri, Sjelden, Noen ganger, Ofte, meaning rwsp.
# Never, Seldom, Sometimes, Often

# change variable names in es (ECHO) to reconcile 
varnr.es.masct2 <- (1:dim(es)[2])[
  names(es) %in% c("t2mascc01_07_t2mascc01","t2mascc36_39_t2mascc39")]
varnr.es.masct2 
names(es)[varnr.es.masct2[1]:varnr.es.masct2[2]] <- 
  c("pac01_1","pac01_2","pac01_3","pac01_4","pac01_5","pac01_6","pac01_7",
    "pac02_8","pac02_9","pac02_10","pac02_11","pac02_12","pac02_13",
    "pac02_14","pac03_15","pac03_16","pac03_17","pac03_18","pac03_19",
    "pac03_20","pac03_21","pac04_22","pac04_23","pac04_24","pac04_25",
    "pac04_26","pac04_27","pac04_28","pac05_29","pac05_30","pac05_31",
    "pac05_32","pac05_33","pac05_34","pac05_35","pac06_36","pac06_37",
    "pac06_38","pac06_39")

# look at SMFQ T2
summary(
  es[,c("t2smfqc01_07_t2smfqc01","t2smfqc01_07_t2smfqc02","t2smfqc01_07_t2smfqc03",         
        "t2smfqc01_07_t2smfqc04","t2smfqc01_07_t2smfqc05","t2smfqc01_07_t2smfqc06",
        "t2smfqc01_07_t2smfqc07","t2smfqc08_14_t2smfqc08","t2smfqc08_14_t2smfqc09",          
        "t2smfqc08_14_t2smfqc10","t2smfqc08_14_t2smfqc11","t2smfqc08_14_t2smfqc12",          
        "t2smfqc08_14_t2smfqc13","t2smfqc08_14_t2smfqc14")])
# no missing, factor, values Aldri, Sjelden, Noen ganger, Ofte, meaning resp.
# Never, Seldom, Sometimes, Often
summary(ts[,c("pmc01_1","pmc01_2","pmc01_3","pmc01_4","pmc01_5","pmc01_6",
              "pmc01_7","pmc02_8","pmc02_9","pmc02_10","pmc02_11","pmc02_12",
              "pmc02_13","pmc02_14")]) # samme som i es

# # change variable names in es (ECHO) to reconcile for SMFQ T2
varnr.es.smfqt2 <- (1:dim(es)[2])[
  names(es) %in% c("t2smfqc01_07_t2smfqc01","t2smfqc08_14_t2smfqc14")]
varnr.es.masct1 # sjekker
names(es)[varnr.es.smfqt2[1]:varnr.es.smfqt2[2]]
names(es)[varnr.es.smfqt2[1]:varnr.es.smfqt2[2]] <- 
  c("pmc01_1","pmc01_2","pmc01_3","pmc01_4","pmc01_5","pmc01_6",
    "pmc01_7","pmc02_8","pmc02_9","pmc02_10","pmc02_11","pmc02_12",
    "pmc02_13","pmc02_14")

# make dichotomous school variable in ts, look first at es
summary(es$str) # character, check table
table(es$str, useNA="always") # liten, stor, ultraliten, meaning 
# resp. small, large ultra-small, no missing,
# look at labels
unique(es$str) # ending blanks occurr, take into account
# makes dichotomous factor (stor means "large", liten means "little")
es$stor <- factor(stripBlanks(es$str)=="stor", labels=c("liten","stor"))
table(es$str, es$stor, useNA="always") # ok

# look at school i ts
summary(ts$skole)
# make version without Norwegian letters
ts$skolee <- ts$skole
levels(ts$skole)
levels(ts$skolee)[c(5,7:9,11:13,15)]
levels(ts$skolee)[c(5,7:9,11:13,15)] <- 
  c("Vevelstad.sen skole","Gr.nn.sen","Asp.sen","By.sen","Brekk.sen",
    "H.yeggen","Rosm.len","H.rstad")
table(ts$skolee, useNA="always") # ok
levels(ts$skolee) # no hidden blanks, ok
ts$stor <- factor(!(ts$skolee %in% c("Gullhaug skole","Brekk.sen","Rosm.len")),
                  labels=c("liten","stor"))
table(ts$skole, ts$stor, useNA="always") # ok

# makes merged data called et
et <- rbind(es[,c("userid","famid","type","skole","trinn","gender","stor",
                  "ace01n","ace02n","ace03n","ace04n","ace05n",
                  "ace06n","ace07n","ace08n","ace09n","ace10n",
                  "bac01_1","bac01_2","bac01_3","bac01_4","bac01_5","bac01_6","bac01_7",
                  "bac02_8","bac02_9","bac02_10","bac02_11","bac02_12",
                  "bac02_13","bac02_14","bac03_15","bac03_16","bac03_17",
                  "bac03_18","bac03_19","bac03_20","bac03_21","bac04_22",
                  "bac04_23","bac04_24","bac04_25","bac04_26","bac04_27",
                  "bac04_28","bac05_29","bac05_30","bac05_31","bac05_32",
                  "bac05_33","bac05_34","bac05_35","bac06_36","bac06_37",
                  "bac06_38","bac06_39",
                  "pac01_1","pac01_2","pac01_3","pac01_4","pac01_5","pac01_6","pac01_7",
                  "pac02_8","pac02_9","pac02_10","pac02_11","pac02_12","pac02_13",
                  "pac02_14","pac03_15","pac03_16","pac03_17","pac03_18","pac03_19",
                  "pac03_20","pac03_21","pac04_22","pac04_23","pac04_24","pac04_25",
                  "pac04_26","pac04_27","pac04_28","pac05_29","pac05_30","pac05_31",
                  "pac05_32","pac05_33","pac05_34","pac05_35","pac06_36","pac06_37",
                  "pac06_38","pac06_39",
                  "bmc01_1","bmc01_2","bmc01_3","bmc01_4","bmc01_5","bmc01_6",
                  "bmc01_7","bmc02_8","bmc02_9","bmc02_10","bmc02_11","bmc02_12",
                  "bmc02_13","bmc02_14",
                  "pmc01_1","pmc01_2","pmc01_3","pmc01_4","pmc01_5","pmc01_6",
                  "pmc01_7","pmc02_8","pmc02_9","pmc02_10","pmc02_11","pmc02_12",
                  "pmc02_13","pmc02_14")],
            ts[,c("userid","famid","type","skole","trinn","gender","stor",
                  "ace01n","ace02n","ace03n","ace04n","ace05n",
                  "ace06n","ace07n","ace08n","ace09n","ace10n",
                  "bac01_1","bac01_2","bac01_3","bac01_4","bac01_5","bac01_6","bac01_7",
                  "bac02_8","bac02_9","bac02_10","bac02_11","bac02_12",
                  "bac02_13","bac02_14","bac03_15","bac03_16","bac03_17",
                  "bac03_18","bac03_19","bac03_20","bac03_21","bac04_22",
                  "bac04_23","bac04_24","bac04_25","bac04_26","bac04_27",
                  "bac04_28","bac05_29","bac05_30","bac05_31","bac05_32",
                  "bac05_33","bac05_34","bac05_35","bac06_36","bac06_37",
                  "bac06_38","bac06_39",
                  "pac01_1","pac01_2","pac01_3","pac01_4","pac01_5","pac01_6","pac01_7",
                  "pac02_8","pac02_9","pac02_10","pac02_11","pac02_12","pac02_13",
                  "pac02_14","pac03_15","pac03_16","pac03_17","pac03_18","pac03_19",
                  "pac03_20","pac03_21","pac04_22","pac04_23","pac04_24","pac04_25",
                  "pac04_26","pac04_27","pac04_28","pac05_29","pac05_30","pac05_31",
                  "pac05_32","pac05_33","pac05_34","pac05_35","pac06_36","pac06_37",
                  "pac06_38","pac06_39",
                  "bmc01_1","bmc01_2","bmc01_3","bmc01_4","bmc01_5","bmc01_6",
                  "bmc01_7","bmc02_8","bmc02_9","bmc02_10","bmc02_11","bmc02_12",
                  "bmc02_13","bmc02_14",
                  "pmc01_1","pmc01_2","pmc01_3","pmc01_4","pmc01_5","pmc01_6",
                  "pmc01_7","pmc02_8","pmc02_9","pmc02_10","pmc02_11","pmc02_12",
                  "pmc02_13","pmc02_14")])
summary(et)
# look userid and famid (family id)
table(et$userid, useNA="always") # need to look at the end of table separately
table(et$userid, useNA="always")[-c(1:100)] # nomissing, presumably uniquea
# numbers in both data sets
table(table(et$userid)) # yes, unique id
table(et$famid, useNA="always") # look at the end of table separately
table(et$famid, useNA="always")[-c(1:100)] # no missing
table(table(et$famid)) # unique family id, looks like long texts, take a look
names(table(et$famid)) # seems so in ECHO, correct
et$famid[et$type=="ECHO"]
et$famid <- stripBlanks(et$famid)
table(et$famid) # ok
# look at skole, stor, trinn, gender, meaning school, large, grade level, gender
table(et$skole, useNA="always") # greit, ingen missing, ser nC&rmere
levels(et$skole) # no leading or trailing blanks, ok
table(et$stor)
levels(et$stor) # no leading or trailing blanks, ok
table(et$trinn, useNA="always") # ok, one missing
summary(et$trinn) # factor
summary(et$gender) # ok, no missing
# make nimerics for items in masc and smfq
names(et)
summary(et[,c("bac01_1","pac01_1","bmc01_1","pmc01_5")])
# MACS (bac and bmc) have 4 categories, SMFQ (bmc and pmc) have 3 categories
et$bac01_1n <- as.numeric(et$bac01_1)
et$bac01_2n <- as.numeric(et$bac01_2)
et$bac01_3n <- as.numeric(et$bac01_3)
et$bac01_4n <- as.numeric(et$bac01_4)
et$bac01_5n <- as.numeric(et$bac01_5)
et$bac01_6n <- as.numeric(et$bac01_6)
et$bac01_7n <- as.numeric(et$bac01_7)
et$bac02_8n <- as.numeric(et$bac02_8)
et$bac02_9n <- as.numeric(et$bac02_9)
et$bac02_10n <- as.numeric(et$bac02_10)
et$bac02_11n <- as.numeric(et$bac02_11)
et$bac02_12n <- as.numeric(et$bac02_12)
et$bac02_13n <- as.numeric(et$bac02_13)
et$bac02_14n <- as.numeric(et$bac02_14)
et$bac03_15n <- as.numeric(et$bac03_15)
et$bac03_16n <- as.numeric(et$bac03_16)
et$bac03_17n <- as.numeric(et$bac03_17)
et$bac03_18n <- as.numeric(et$bac03_18)
et$bac03_19n <- as.numeric(et$bac03_19)
et$bac03_20n <- as.numeric(et$bac03_20)
et$bac03_21n <- as.numeric(et$bac03_21)
et$bac04_22n <- as.numeric(et$bac04_22)
et$bac04_23n <- as.numeric(et$bac04_23)
et$bac04_24n <- as.numeric(et$bac04_24)
et$bac04_25n <- as.numeric(et$bac04_25)
et$bac04_26n <- as.numeric(et$bac04_26)
et$bac04_27n <- as.numeric(et$bac04_27)
et$bac04_28n <- as.numeric(et$bac04_28)
et$bac05_29n <- as.numeric(et$bac05_29)
et$bac05_30n <- as.numeric(et$bac05_30)
et$bac05_31n <- as.numeric(et$bac05_31)
et$bac05_32n <- as.numeric(et$bac05_32)
et$bac05_33n <- as.numeric(et$bac05_33)
et$bac05_34n <- as.numeric(et$bac05_34)
et$bac05_35n <- as.numeric(et$bac05_35)
et$bac06_36n <- as.numeric(et$bac06_36)
et$bac06_37n <- as.numeric(et$bac06_37)
et$bac06_38n <- as.numeric(et$bac06_38)
et$bac06_39n <- as.numeric(et$bac06_39)
et$pac01_1n <- as.numeric(et$pac01_1)
et$pac01_2n <- as.numeric(et$pac01_2)
et$pac01_3n <- as.numeric(et$pac01_3)
et$pac01_4n <- as.numeric(et$pac01_4)
et$pac01_5n <- as.numeric(et$pac01_5)
et$pac01_6n <- as.numeric(et$pac01_6)
et$pac01_7n <- as.numeric(et$pac01_7)
et$pac02_8n <- as.numeric(et$pac02_8)
et$pac02_9n <- as.numeric(et$pac02_9)
et$pac02_10n <- as.numeric(et$pac02_10)
et$pac02_11n <- as.numeric(et$pac02_11)
et$pac02_12n <- as.numeric(et$pac02_12)
et$pac02_13n <- as.numeric(et$pac02_13)
et$pac02_14n <- as.numeric(et$pac02_14)
et$pac03_15n <- as.numeric(et$pac03_15)
et$pac03_16n <- as.numeric(et$pac03_16)
et$pac03_17n <- as.numeric(et$pac03_17)
et$pac03_18n <- as.numeric(et$pac03_18)
et$pac03_19n <- as.numeric(et$pac03_19)
et$pac03_20n <- as.numeric(et$pac03_20)
et$pac03_21n <- as.numeric(et$pac03_21)
et$pac04_22n <- as.numeric(et$pac04_22)
et$pac04_23n <- as.numeric(et$pac04_23)
et$pac04_24n <- as.numeric(et$pac04_24)
et$pac04_25n <- as.numeric(et$pac04_25)
et$pac04_26n <- as.numeric(et$pac04_26)
et$pac04_27n <- as.numeric(et$pac04_27)
et$pac04_28n <- as.numeric(et$pac04_28)
et$pac05_29n <- as.numeric(et$pac05_29)
et$pac05_30n <- as.numeric(et$pac05_30)
et$pac05_31n <- as.numeric(et$pac05_31)
et$pac05_32n <- as.numeric(et$pac05_32)
et$pac05_33n <- as.numeric(et$pac05_33)
et$pac05_34n <- as.numeric(et$pac05_34)
et$pac05_35n <- as.numeric(et$pac05_35)
et$pac06_36n <- as.numeric(et$pac06_36)
et$pac06_37n <- as.numeric(et$pac06_37)
et$pac06_38n <- as.numeric(et$pac06_38)
et$pac06_39n <- as.numeric(et$pac06_39)
# SMFQ (bmc and pmc)
et$bmc01_1n <- as.numeric(et$bmc01_1)
et$bmc01_2n <- as.numeric(et$bmc01_2)
et$bmc01_3n <- as.numeric(et$bmc01_3)
et$bmc01_4n <- as.numeric(et$bmc01_4)
et$bmc01_5n <- as.numeric(et$bmc01_5)
et$bmc01_6n <- as.numeric(et$bmc01_6)
et$bmc01_7n <- as.numeric(et$bmc01_7)
et$bmc02_8n <- as.numeric(et$bmc02_8)
et$bmc02_9n <- as.numeric(et$bmc02_9)
et$bmc02_10n <- as.numeric(et$bmc02_10)
et$bmc02_11n <- as.numeric(et$bmc02_11)
et$bmc02_12n <- as.numeric(et$bmc02_12)
et$bmc02_13n <- as.numeric(et$bmc02_13)
et$bmc02_14n <- as.numeric(et$bmc02_14)
et$pmc01_1n <- as.numeric(et$pmc01_1)
et$pmc01_2n <- as.numeric(et$pmc01_2)
et$pmc01_3n <- as.numeric(et$pmc01_3)
et$pmc01_4n <- as.numeric(et$pmc01_4)
et$pmc01_5n <- as.numeric(et$pmc01_5)
et$pmc01_6n <- as.numeric(et$pmc01_6)
et$pmc01_7n <- as.numeric(et$pmc01_7)
et$pmc02_8n <- as.numeric(et$pmc02_8)
et$pmc02_9n <- as.numeric(et$pmc02_9)
et$pmc02_10n <- as.numeric(et$pmc02_10)
et$pmc02_11n <- as.numeric(et$pmc02_11)
et$pmc02_12n <- as.numeric(et$pmc02_12)
et$pmc02_13n <- as.numeric(et$pmc02_13)
et$pmc02_14n <- as.numeric(et$pmc02_14)
# Cronbach's alpha, first at t1
# MASC, all 39 items shoul db eused, this is alpha at T0
cronbach(et[et$type=="ECHO",c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
                              "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
                              "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
                              "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
                              "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
                              "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
                              "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
                              "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
cronbach(et[et$type=="TIM",c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
                              "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
                              "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
                              "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
                              "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
                              "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
                              "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
                              "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
cronbach(et[,c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
                              "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
                              "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
                              "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
                              "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
                              "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
                              "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
                              "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
# MASC T1, Cronbach's alpha 0.87 in ECHO and 0.84 i TIM, 0.87 merged
# next look at T2
cronbach(et[et$type=="ECHO",c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
                              "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
                              "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
                              "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
                              "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
                              "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
                              "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
                              "pac06_36n","pac06_37n","pac06_38n","pac06_39n")])
cronbach(et[et$type=="TIM",c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
                             "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
                             "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
                             "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
                             "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
                             "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
                             "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
                             "pac06_36n","pac06_37n","pac06_38n","pac06_39n")])
#sammenslC%tt materiale Kicki 070824
cronbach(et[,c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
               "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
               "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
               "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
               "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
               "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
               "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
               "pac06_36n","pac06_37n","pac06_38n","pac06_39n")])

# MASC T2, Cronbach's alpha 0.91 in ECHO and 0.92 in TIM, merged 0.91
# SMFQ, use item 1-13 
cronbach(et[et$type=="ECHO",c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
                              "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
                              "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
                              "bmc02_13n")])
cronbach(et[et$type=="TIM",c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
                             "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
                             "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
                             "bmc02_13n")])
# merged
cronbach(et[,c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
               "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
               "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
               "bmc02_13n")])
# SMFQ T1, Cronbach's alpha 0.84 in ECHO and 0.83 in TIM, 0.84 merged
cronbach(et[et$type=="ECHO",c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
                              "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
                              "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
                              "pmc02_13n")])
cronbach(et[et$type=="TIM",c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
                             "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
                             "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
                             "pmc02_13n")])
# merged
cronbach(et[,c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
               "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
               "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
               "pmc02_13n")])
# SMFQ T2, Cronbach's alpha 0.89 in ECHO and 0.88 in TIM, merged 0.89
# stigma, use item 5,6,7
cronbach(et[et$type=="ECHO",c("ace05n","ace06n","ace07n")])
cronbach(et[et$type=="TIM",c("ace05n","ace06n","ace07n")])
cronbach(et[,c("ace05n","ace06n","ace07n")])
# stigma, Cronbach's alpha 0.53 in ECHO and 0.31 in TIM, merged 0.47
# user satisfaction, use item  1-4,10
cronbach(et[et$type=="ECHO",c("ace01n","ace02n","ace03n","ace04n","ace10n")])
cronbach(et[et$type=="TIM",c("ace01n","ace02n","ace03n","ace04n","ace10n")])
cronbach(et[,c("ace01n","ace02n","ace03n","ace04n","ace10n")])
#  Cronbach's alpha 0.88 in ECHO and 0.86 in TIM, merged 0.87

# compute McDonald's Omega
# first at t1 for ECHO, TIM, and merged
omega(et[et$type=="ECHO",c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
                              "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
                              "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
                              "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
                              "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
                              "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
                              "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
                              "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
# Omega ECHO T1 = 0.89
omega(et[et$type=="TIM",c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
                             "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
                             "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
                             "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
                             "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
                             "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
                             "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
                             "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
# Omega TIM T1 = 0.86
omega(et[,c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
               "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
               "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
               "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
               "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
               "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
               "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
               "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
# Omega merged T1 = 0.88
# Omega at T2 for MASC
omega(et[et$type=="ECHO",c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
                              "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
                              "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
                              "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
                              "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
                              "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
                              "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
                              "pac06_36n","pac06_37n","pac06_38n","pac06_39n")])
omega(et[et$type=="TIM",c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
                             "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
                             "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
                             "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
                             "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
                             "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
                             "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
                             "pac06_36n","pac06_37n","pac06_38n","pac06_39n")])
omega(et[,c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
               "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
               "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
               "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
               "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
               "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
               "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
               "pac06_36n","pac06_37n","pac06_38n","pac06_39n")])
# MASC T2, omega 0.92 in ECHO and 0,93 in TIM, merged  0.93

# omega SMFQ
# First T1
omega(et[et$type=="ECHO",c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
                              "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
                              "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
                              "bmc02_13n")])
omega(et[et$type=="TIM",c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
                             "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
                             "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
                             "bmc02_13n")])
omega(et[,c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
               "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
               "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
               "bmc02_13n")])
# SMFQ T1, omega 0.86 in ECHO and 0.86 in TIM, merged 0.86
# Omega at T2 for SMFQ
omega(et[et$type=="ECHO",c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
                              "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
                              "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
                              "pmc02_13n")])
omega(et[et$type=="TIM",c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
                             "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
                             "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
                             "pmc02_13n")])
omega(et[,c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
               "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
               "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
               "pmc02_13n")])
# SMFQ T2, omega 0.91 in ECHO and 0.91 i TIM, merged 0.91

# omega for stigma, use item 5,6,7
omega(et[et$type=="ECHO",c("ace05n","ace06n","ace07n")])
omega(et[et$type=="TIM",c("ace05n","ace06n","ace07n")])
omega(et[,c("ace05n","ace06n","ace07n")])
# stigma, Cronbach's alpha 0,53 in ECHO and 0,31 in TIM, merged 0.47
# user satisfaction, item 1-4,10
omega(et[et$type=="ECHO",c("ace01n","ace02n","ace03n","ace04n","ace10n")])
omega(et[et$type=="TIM",c("ace01n","ace02n","ace03n","ace04n","ace10n")])
omega(et[,c("ace01n","ace02n","ace03n","ace04n","ace10n")])

# Coping
summary(et[,c("ace08n")])
summary(et[,c("ace09n")])
table(et$ace08n)
table(et$ace09n)
table(et$ace08n,et$ace09n)

# scale scors using  half rule
# mean scores
summary(et[,c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
               "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
               "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
               "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
               "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
               "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
               "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
               "bac06_36n","bac06_37n","bac06_38n","bac06_39n")])
et$MASC.T1.mean.hr <- 
  apply(et[,c("bac01_1n","bac01_2n","bac01_3n","bac01_4n","bac01_5n",
              "bac01_6n","bac01_7n","bac02_8n","bac02_9n","bac02_10n",
              "bac02_11n","bac02_12n","bac02_13n","bac02_14n","bac03_15n",
              "bac03_16n","bac03_17n","bac03_18n","bac03_19n","bac03_20n",
              "bac03_21n","bac04_22n","bac04_23n","bac04_24n","bac04_25n",
              "bac04_26n","bac04_27n","bac04_28n","bac05_29n","bac05_30n",
              "bac05_31n","bac05_32n","bac05_33n","bac05_34n","bac05_35n",
              "bac06_36n","bac06_37n","bac06_38n","bac06_39n")], 1, halfrulef) - 1 
# subtract 1 to make a acore from 0 to 3
et$MASC.T2.mean.hr <- 
  apply(et[,c("pac01_1n","pac01_2n","pac01_3n","pac01_4n","pac01_5n",
              "pac01_6n","pac01_7n","pac02_8n","pac02_9n","pac02_10n",
              "pac02_11n","pac02_12n","pac02_13n","pac02_14n","pac03_15n",
              "pac03_16n","pac03_17n","pac03_18n","pac03_19n","pac03_20n",
              "pac03_21n","pac04_22n","pac04_23n","pac04_24n","pac04_25n",
              "pac04_26n","pac04_27n","pac04_28n","pac05_29n","pac05_30n",
              "pac05_31n","pac05_32n","pac05_33n","pac05_34n","pac05_35n",
              "pac06_36n","pac06_37n","pac06_38n","pac06_39n")], 1, halfrulef) -1 
# subtract 1 to make a acore from 0 to 3
summary(et[,c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
               "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
               "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
               "bmc02_13n","bmc02_14n")])
et$SMFQ.T1.mean.hr <- 
  apply(et[,c("bmc01_1n","bmc01_2n","bmc01_3n","bmc01_4n",
              "bmc01_5n","bmc01_6n","bmc01_7n","bmc02_8n",
              "bmc02_9n","bmc02_10n","bmc02_11n","bmc02_12n",
              "bmc02_13n")], 1, halfrulef) - 1 
#subtract 1 to make a acore from 0 to 2
et$SMFQ.T2.mean.hr <- 
  apply(et[,c("pmc01_1n","pmc01_2n","pmc01_3n","pmc01_4n",
              "pmc01_5n","pmc01_6n","pmc01_7n","pmc02_8n",
              "pmc02_9n","pmc02_10n","pmc02_11n","pmc02_12n",
              "pmc02_13n")], 1, halfrulef)- 1 
#subtract 1 to make a acore from 0 to 2
et$stigma.mean.hr <- 
  apply(et[,c("ace05n","ace06n","ace07n")], 1, halfrulef)
et$brukforn.mean.hr <- 
  apply(et[,c("ace01n","ace02n","ace03n","ace04n","ace10n")], 1, halfrulef)
summary(et[,c("MASC.T1.mean.hr","MASC.T2.mean.hr","SMFQ.T1.mean.hr",
              "SMFQ.T2.mean.hr","brukforn.mean.hr","brukforn.mean.hr")])
# 0:3 for MASC, 0:2 for SMFQ, 1:10 for stigma and user satisfaction, ok,
# no missing

# sum scores
et$MASC.T1.sum.hr <- 39*et$MASC.T1.mean.hr
et$MASC.T2.sum.hr <- 39*et$MASC.T2.mean.hr
et$SMFQ.T1.sum.hr <- 13*et$SMFQ.T1.mean.hr
et$SMFQ.T2.sum.hr <- 13*et$SMFQ.T2.mean.hr
et$stigma.sum.hr <- 3*et$stigma.mean.hr
et$brukforn.sum.hr <- 5*et$brukforn.mean.hr
summary(et[,c("MASC.T1.sum.hr","MASC.T2.sum.hr","SMFQ.T1.sum.hr",
              "SMFQ.T2.sum.hr","stigma.sum.hr","brukforn.sum.hr")])
# use this in descriptives (sum scores)

# 0:100-scores
et$MASC.T1.0.100.hr <- (100/3)*(et$MASC.T1.mean.hr)
et$MASC.T2.0.100.hr <- (100/3)*(et$MASC.T2.mean.hr)
et$SMFQ.T1.0.100.hr <- (100/2)*(et$SMFQ.T1.mean.hr)
et$SMFQ.T2.0.100.hr <- (100/2)*(et$SMFQ.T2.mean.hr)
et$stigma.0.100.hr <- (100/9)*(et$stigma.mean.hr-1)
et$brukforn.0.100.hr <- (100/9)*(et$brukforn.mean.hr-1)
summary(et[,c("MASC.T1.0.100.hr","MASC.T2.0.100.hr",
              "SMFQ.T1.0.100.hr","SMFQ.T2.0.100.hr","stigma.0.100.hr")])

# simple analyses using sum scores for MASC, SMFQ and alle 10 ACE items, 
# by gender and grade levels
summary(et[,c("MASC.T1.sum.hr","MASC.T2.sum.hr","SMFQ.T1.sum.hr",
              "SMFQ.T2.sum.hr","stigma.sum.hr","brukforn.sum.hr")])
apply(et[,c("MASC.T1.sum.hr","MASC.T2.sum.hr","SMFQ.T1.sum.hr",
            "SMFQ.T2.sum.hr","stigma.sum.hr","brukforn.sum.hr")],2,mean) #computing mean
apply(et[,c("MASC.T1.sum.hr","MASC.T2.sum.hr","SMFQ.T1.sum.hr",
            "SMFQ.T2.sum.hr","stigma.sum.hr","brukforn.sum.hr")],2,sd) #computing mean
# test gender differences
t.test(MASC.T1.sum.hr ~ gender, data=et)
t.test(MASC.T2.sum.hr ~ gender, data=et)
t.test(SMFQ.T1.sum.hr ~ gender, data=et)
t.test(SMFQ.T2.sum.hr ~ gender, data=et)

# SD for gender differences
sd(et$MASC.T1.sum.hr, na.rm = TRUE) # standard deviation for merged data
by(et$MASC.T1.sum.hr, et$gender, sd, na.rm = TRUE) # separately by gender
# the same for SMFQ
by(et$SMFQ.T1.sum.hr, et$gender, sd, na.rm = TRUE) # separate by gender
sd(et$MASC.T2.sum.hr, na.rm = TRUE) # overall standard deviation
by(et$MASC.T2.sum.hr, et$gender, sd, na.rm = TRUE) # eparate by gender
# same for SMFQ
by(et$SMFQ.T2.sum.hr, et$gender, sd, na.rm = TRUE) # eparate by gender
#test for age differences via grade level (trinn)
et$trinn3 <- relevel(et$trinn, ref="3")
summary(et$trinn3)# recode with grade level 3 first
summary(lm(MASC.T1.sum.hr ~ trinn3, data=et))
by(et$MASC.T1.sum.hr,et$trinn3,mean,na.rm=TRUE)
by(et$MASC.T1.sum.hr,et$trinn3,sd,na.rm=TRUE)
summary(lm(MASC.T2.sum.hr ~ trinn3, data=et))#copy script and adapt
by(et$MASC.T2.sum.hr,et$trinn3,mean,na.rm=TRUE)
by(et$MASC.T2.sum.hr,et$trinn3,sd,na.rm=TRUE)
summary(lm(SMFQ.T1.sum.hr ~ trinn3, data=et))
# check contrasts from grade level 4
summary(lm(SMFQ.T1.sum.hr ~ relevel(trinn3,ref="4"), data=et))

by(et$SMFQ.T1.sum.hr,et$trinn3,mean,na.rm=TRUE)
by(et$SMFQ.T1.sum.hr,et$trinn3,sd,na.rm=TRUE)
summary(lm(SMFQ.T2.sum.hr ~ trinn3, data=et))
by(et$SMFQ.T2.sum.hr,et$trinn3,mean,na.rm=TRUE)
by(et$SMFQ.T2.sum.hr,et$trinn3,sd,na.rm=TRUE)

#STIGMA, mean and standard deviation, table 2
summary(et[,c("ace01n", "ace02n","ace03n","ace04n","ace05n","ace06n","ace07n","ace08n","ace09n","ace10n")])
apply(et[,c("ace01n", "ace02n","ace03n","ace04n","ace05n","ace06n","ace07n",
            "ace08n","ace09n","ace10n")],2, mean)
apply(et[,c("ace01n", "ace02n","ace03n","ace04n","ace05n","ace06n","ace07n",
            "ace08n","ace09n","ace10n")],2, sd)
# mage diagram, preparation, variables to use have no missing
et$usersatmean <- apply(et[,c("ace01n", "ace02n","ace03n","ace04n","ace10n")], 
                        1,mean)
et$stigmamean <- apply(et[,c("ace05n","ace06n","ace07n")], 
                        1,mean)
summary(et[,c("usersatmean","stigmamean","ace08n","ace09n")])
# make ACE diagram
acefigur.mean <- apply(et[,c("usersatmean","stigmamean","ace08n","ace09n")], 
                       2,mean)
acefigur.sd <- apply(et[,c("usersatmean","stigmamean","ace08n","ace09n")], 
                       2,sd)
acefigur.mean
acefigur.sd
?par
?barplot
par(mar = c(bottom =2, left = 3, top = 2, right = 0.3)+ 0.1)
barplot(acefigur.mean,  names.arg = c("User satisfaction","Stigma",
                                      "Coping T1", "Coping T2"),
        axes = FALSE,ylim = c(0,10),
        col=c("cyan3","brown1","chocolate1", "chartreuse2"))
box()
axis(2,las =1, at=c(0,2,4,6,8,10))
mtext(side = 2,line = 2,text = "Mean score")
mtext(side = 3, line = 0.3, 
      text = "ACE Evaluation: User satisfaction, Stigma and Coping")
par(mar = c(bottom =5, left = 4, top = 4, right = 2)+ 0.1)

# ACE, test gender differences
t.test(ace01n ~ gender, data=et)
t.test(ace02n ~ gender, data=et)
t.test(ace03n ~ gender, data=et)
t.test(ace04n ~ gender, data=et)
t.test(ace05n ~ gender, data=et)
t.test(ace06n ~ gender, data=et)
t.test(ace07n ~ gender, data=et)
t.test(ace08n ~ gender, data=et)
t.test(ace09n ~ gender, data=et)
t.test(ace10n ~ gender, data=et)

# ACE, test age differences via grade level (trinn), for Table 3
summary(et$trinn3)# recode with trimm 3 first 
summary(lm(ace01n ~ trinn3, data=et))#enten rapportere samlet p verdi R2, eller alle 
# all contrasts include contrasts between grade level 4-6 also, altogether 6 contrasts
summary(lm(ace02n ~ trinn3, data=et))
summary(lm(ace03n ~ trinn3, data=et)) #all grade levels below grade 3
summary(lm(ace04n ~ trinn3, data=et))
summary(lm(ace05n ~ trinn3, data=et))
summary(lm(ace06n ~ trinn3, data=et))
summary(lm(ace07n ~ trinn3, data=et))
summary(lm(ace08n ~ trinn3, data=et))
summary(lm(ace09n ~ trinn3, data=et))
summary(lm(ace10n ~ trinn3, data=et))
confint(lm(ace01n ~ trinn3, data=et))
confint(lm(ace02n ~ trinn3, data=et))
confint(lm(ace03n ~ trinn3, data=et))
confint(lm(ace04n ~ trinn3, data=et))
confint(lm(ace05n ~ trinn3, data=et))
confint(lm(ace06n ~ trinn3, data=et))
confint(lm(ace07n ~ trinn3, data=et))
confint(lm(ace08n ~ trinn3, data=et))
confint(lm(ace09n ~ trinn3, data=et))
confint(lm(ace10n ~ trinn3, data=et))
# make grade level (trinn) variable with 6 as reference, rerun
summary(et$trinn3)
et$trinn6 <- factor(5-as.numeric(et$trinn3), labels = c("6", "5", "4", "3"))
table(et$trinn3,et$trinn6,useNA = "always")
summary(lm(ace01n ~ trinn6, data=et))
summary(lm(ace02n ~ trinn6, data=et))
summary(lm(ace03n ~ trinn6, data=et))# all grade levels below grade level (trinn) 3
summary(lm(ace04n ~ trinn6, data=et))
summary(lm(ace05n ~ trinn6, data=et))
summary(lm(ace06n ~ trinn6, data=et))
summary(lm(ace07n ~ trinn6, data=et))
summary(lm(ace08n ~ trinn6, data=et))
summary(lm(ace09n ~ trinn6, data=et))
summary(lm(ace10n ~ trinn6, data=et))
confint(lm(ace01n ~ trinn6, data=et))
confint(lm(ace02n ~ trinn6, data=et))
confint(lm(ace03n ~ trinn6, data=et))
confint(lm(ace04n ~ trinn6, data=et))
confint(lm(ace05n ~ trinn6, data=et))
confint(lm(ace06n ~ trinn6, data=et))
confint(lm(ace07n ~ trinn6, data=et))
confint(lm(ace08n ~ trinn6, data=et))
confint(lm(ace09n ~ trinn6, data=et))
confint(lm(ace10n ~ trinn6, data=et))

# look at distribution of scores
hist(et$stigma.0.100.hr, col="lightgreen") # substantially skewed with tail to the right
hist(et$brukforn.0.100.hr, col="lightgreen") # somewhat skewed with tail to the left 
hist(et$MASC.T1.0.100.hr, col="lightgreen") # ok
hist(et$MASC.T2.0.100.hr, col="lightgreen") # ok
hist(et$SMFQ.T1.0.100.hr, col="lightgreen") # a little skewed
hist(et$SMFQ.T2.0.100.hr, col="lightgreen") # some tail to the left
# single intems for stigma
table(et$ace05n) # many at 1 (lowest)
table(et$ace06n) # even more
table(et$ace07n) # about as ace06n
summary(et[,c("ace05n","ace06n","ace07n")])

# the grade level (trinn) variable
summary(et$trinn) # grad elevel 3 last, fix
et$trinn3 <- relevel(et$trinn, ref = "3")
table(et$trinn, et$trinn3, useNA = "always")

# what characterises children reporting perceived stigma; age, gender, 
# symptomatology, reseqarch question  1
# for Table 4
lm1.flau <- lm(ace05n~gender+trinn6+stor, data = et) # flau is embarrassed
summary(lm1.flau)
drop1(lm1.flau, test= "Chisq") # overall  p-value for multi-category variables
confint(lm1.flau)
# skewed variable, standard erors may be somewhat too low, with concequences for 
# confidence intgervalss (somewhat too narrow) and p-values (somewhat too small)

lm1.ertet <- lm(ace06n~gender+trinn6+stor, data = et) # ertet is teased
summary(lm1.ertet)
drop1(lm1.ertet, test= "Chisq")
confint(lm1.ertet)

lm1.kritisert <- lm(ace07n~gender+trinn6+stor, data = et) # kritisert is criticised
summary(lm1.kritisert)
drop1(lm1.kritisert, test= "Chisq")
confint(lm1.kritisert)

# hieararchical set-up, first only background variables, 
# then also anxiety and depression 
# flau is embarrassed
lm2.flau <- lm(ace05n~gender+trinn6+stor+MASC.T1.0.100.hr+SMFQ.T1.0.100.hr, data = et)
summary(lm2.flau)
drop1(lm2.flau, test= "Chisq")
confint(lm2.flau)
#weak relationships (low estimates), but significant for depression

# ertet is teased
lm2.ertet <- lm(ace06n~gender+trinn6+stor+MASC.T1.0.100.hr+SMFQ.T1.0.100.hr, data = et)
summary(lm2.ertet)
drop1(lm2.ertet, test= "Chisq") # viser samlet pverdi der variabelen er flerdelt
confint(lm2.ertet)
#ingen sign funn

# kritisert is critisised
lm2.kritisert <- lm(ace07n~gender+trinn6+stor+MASC.T1.0.100.hr+SMFQ.T1.0.100.hr, data = et)
summary(lm2.kritisert)
drop1(lm2.kritisert, test= "Chisq")
confint(lm2.kritisert)

# mixed-effects models for all models, clustering by school (skole)
# First ace05 embarrassed
names(et)
summary(et$skole) #no missing
summary(et[,c("ace05n", "gender","trinn6", "stor")]) 
#trinn6 (grade levelwith reference 6 has one missing
eg <- groupedData(et, formula = ace05n~gender|skole)
lme1.flau <- lme(ace05n~gender+trinn6+stor, random =~1,  
                 data = eg[is.na(eg$trinn6)==0,])
intervals(lme1.flau) # to look for stability in random effects, uok
anova(lme1.flau,type = "marginal") #p-values, only gender significant
summary(lme1.flau)
intervals(lme1.flau)# now to look at fixed effects, girls a little lower than boys

#hierarchical modell
summary(et[,c("ace05n", "gender","trinn6", "stor","MASC.T1.0.100.hr","SMFQ.T1.0.100.hr")])
# onlytrinn (grade level) has (one) missing
lme2.flau <- lme(ace05n~gender+trinn6+stor+MASC.T1.0.100.hr+SMFQ.T1.0.100.hr, 
                 random =~1, data = eg[is.na(eg$trinn6)==0,])
intervals(lme2.flau) # stability check, ok
anova(lme2.flau,type = "marginal")
summary(lme2.flau)
intervals(lme2.flau)#

# next ace06, ertet (teased)
names(et)
summary(et$skole) #no missing school
summary(et[,c("ace06n", "gender","trinn6", "stor")])
#trinn6 (grade level with reference 6) has one missing
eg <- groupedData(et, formula = ace06n~gender|skole)
lme1.ertet <- lme(ace06n~gender+trinn6+stor, random =~1,  
                 data = eg[is.na(eg$trinn6)==0,])
intervals(lme1.ertet) # stability check, ok
anova(lme1.ertet,type = "marginal") #p-velues, only gender significant
summary(lme1.ertet)
intervals(lme1.ertet)# look at fixed effects, girls a little lower than boys

#hierarchical set-up
summary(et[,c("ace06n", "gender","trinn6", "stor","MASC.T1.0.100.hr","SMFQ.T1.0.100.hr")])
lme2.ertet <- lme(ace06n~gender+trinn6+stor+MASC.T1.0.100.hr+SMFQ.T1.0.100.hr, 
                 random =~1, data = eg[is.na(eg$trinn6)==0,])
intervals(lme2.ertet) 
anova(lme2.ertet,type = "marginal")
summary(lme2.ertet)
intervals(lme2.ertet)

# next ace07 Kritisert (criticised)
names(et)
summary(et$skole) #no missing
summary(et[,c("ace07n", "gender","trinn6", "stor")])
eg <- groupedData(et, formula = ace07n~gender|skole)
lme1.kritisert <- lme(ace07n~gender+trinn6+stor, random =~1,  
                  data = eg[is.na(eg$trinn6)==0,])
intervals(lme1.kritisert)
anova(lme1.kritisert,type = "marginal") 
summary(lme1.kritisert)
intervals(lme1.kritisert)

# hierarkisk modell
summary(et[,c("ace07n", "gender","trinn6", "stor","MASC.T1.0.100.hr","SMFQ.T1.0.100.hr")])
# kun trinn som har en missing
lme2.kritisert <- lme(ace07n~gender+trinn6+stor+MASC.T1.0.100.hr+SMFQ.T1.0.100.hr, 
                  random =~1, data = eg[is.na(eg$trinn6)==0,])
intervals(lme2.kritisert) 
anova(lme2.kritisert,type = "marginal") #p-velues, gender close tosignificant
summary(lme2.kritisert)
intervals(lme2.kritisert)#

summary(lme1.flau)$tTable
summary(lme2.flau)$tTable
intervals(lme2.flau)$fixed
intervals(lme2.flau)$fixed[2:8,1]
anova(lme2.flau, type= "marginal")
anova(lme2.flau, type= "marginal")[3,4]

# make table (tabell) in R, first without adjustment for anxiety and depression
tabell.lme <- data.frame(
  koef.flau.ju= summary(lme2.flau)$tTable[2:8,1])
tabell.lme$koef.flau.uj <- c(summary(lme1.flau)$tTable[2:6,1],NA,NA)
tabell.lme$koef.ertet.ju <- summary(lme2.ertet)$tTable[2:8,1]
tabell.lme$koef.ertet.uj <- c(summary(lme1.ertet)$tTable[2:6,1],NA,NA)
tabell.lme$koef.kritisert.ju <- summary(lme2.kritisert)$tTable[2:8,1]
tabell.lme$koef.kritisert.uj <- c(summary(lme1.kritisert)$tTable[2:6,1],NA,NA)
tabell.lme$lower.flau.ju <- intervals(lme2.flau)$fixed[2:8,1]
tabell.lme$lower.flau.uj <- c(intervals(lme1.flau)$fixed[2:6,1], NA, NA)
tabell.lme$lower.ertet.ju <- intervals(lme2.ertet)$fixed[2:8,1]
tabell.lme$lower.ertet.uj <- c(intervals(lme1.ertet)$fixed[2:6,1], NA, NA)
tabell.lme$lower.kritisert.ju <- intervals(lme2.kritisert)$fixed[2:8,1]
tabell.lme$lower.kritisert.uj <- c(intervals(lme1.kritisert)$fixed[2:6,1], NA, NA)
tabell.lme$upper.flau.ju <- intervals(lme2.flau)$fixed[2:8,3]
tabell.lme$.upper.flau.uj <- c(intervals(lme1.flau)$fixed[2:6,3], NA, NA)
tabell.lme$upper.ertet.ju <- intervals(lme2.ertet)$fixed[2:8,3]
tabell.lme$upper.ertet.uj <- c(intervals(lme1.ertet)$fixed[2:6,3], NA, NA)
tabell.lme$upper.kritisert.ju <- intervals(lme2.kritisert)$fixed[2:8,3]
tabell.lme$upper.kritisert.uj <- c(intervals(lme1.kritisert)$fixed[2:6,3], NA, NA)
tabell.lme$p.flau.ju <- summary(lme2.flau)$tTable[2:8,5]
tabell.lme$p.flau.uj <- c(summary(lme1.flau)$tTable[2:6,5], NA, NA)
tabell.lme$p.ertet.ju <- summary(lme2.ertet)$tTable[2:8,5]
tabell.lme$p.ertet.uj <- c(summary(lme1.ertet)$tTable[2:6,5], NA, NA)
tabell.lme$p.kritisert.ju <- summary(lme2.kritisert)$tTable[2:8,5]
tabell.lme$p.kritisert.uj <- c(summary(lme1.kritisert)$tTable[2:6,5], NA, NA)
samlet.p.grade <- c(rep(NA,18), # samlet means overall
                    anova(lme2.flau, type= "marginal")[3,4],
                    anova(lme1.flau, type= "marginal")[3,4],
                    anova(lme2.ertet, type= "marginal")[3,4],
                    anova(lme1.ertet, type= "marginal")[3,4],
                    anova(lme2.kritisert, type= "marginal")[3,4],
                    anova(lme1.kritisert, type= "marginal")[3,4])
tabell.lme <- rbind(tabell.lme,samlet.p.grade)
row.names(tabell.lme)[8] <- "Grade" # move overall p velue to just after gender
tabell.lme <- tabell.lme[c(1,8,2:7),]

names(tabell.lme)
tabell.lme.uj <- tabell.lme[,c(2,8,14,20,4,10,16,22,6,12,18,24)]
tabell.lme.uj.round <- tabell.lme.uj
tabell.lme.uj.round[,c(1:3,5:7, 9:11)] <- 
  round(tabell.lme.uj[,c(1:3,5:7, 9:11)],2)
tabell.lme.uj.round[,c(4,8,12)] <- 
  round(tabell.lme.uj[,c(4,8,12)],3)

#take the table without adjustment for anxiety and depression bout of R
?write.table
write.table(x=tabell.lme.uj.round,file="lme unadjusted.txt",
            quote = FALSE,sep = ";",na ="")

# make table (tabell) in R with adjustment for anxiety and depression
tabell.lme <- data.frame(
  koef.flau.ju= summary(lme2.flau)$tTable[2:8,1])
tabell.lme$koef.flau.uj <- c(summary(lme1.flau)$tTable[2:6,1],NA,NA)
tabell.lme$koef.ertet.ju <- summary(lme2.ertet)$tTable[2:8,1]
tabell.lme$koef.ertet.uj <- c(summary(lme1.ertet)$tTable[2:6,1],NA,NA)
tabell.lme$koef.kritisert.ju <- summary(lme2.kritisert)$tTable[2:8,1]
tabell.lme$koef.kritisert.uj <- c(summary(lme1.kritisert)$tTable[2:6,1],NA,NA)
tabell.lme$lower.flau.ju <- intervals(lme2.flau)$fixed[2:8,1]
tabell.lme$lower.flau.uj <- c(intervals(lme1.flau)$fixed[2:6,1], NA, NA)
tabell.lme$lower.ertet.ju <- intervals(lme2.ertet)$fixed[2:8,1]
tabell.lme$lower.ertet.uj <- c(intervals(lme1.ertet)$fixed[2:6,1], NA, NA)
tabell.lme$lower.kritisert.ju <- intervals(lme2.kritisert)$fixed[2:8,1]
tabell.lme$lower.kritisert.uj <- c(intervals(lme1.kritisert)$fixed[2:6,1], NA, NA)
tabell.lme$upper.flau.ju <- intervals(lme2.flau)$fixed[2:8,3]
tabell.lme$.upper.flau.uj <- c(intervals(lme1.flau)$fixed[2:6,3], NA, NA)
tabell.lme$upper.ertet.ju <- intervals(lme2.ertet)$fixed[2:8,3]
tabell.lme$upper.ertet.uj <- c(intervals(lme1.ertet)$fixed[2:6,3], NA, NA)
tabell.lme$upper.kritisert.ju <- intervals(lme2.kritisert)$fixed[2:8,3]
tabell.lme$upper.kritisert.uj <- c(intervals(lme1.kritisert)$fixed[2:6,3], NA, NA)
tabell.lme$p.flau.ju <- summary(lme2.flau)$tTable[2:8,5]
tabell.lme$p.flau.uj <- c(summary(lme1.flau)$tTable[2:6,5], NA, NA)
tabell.lme$p.ertet.ju <- summary(lme2.ertet)$tTable[2:8,5]
tabell.lme$p.ertet.uj <- c(summary(lme1.ertet)$tTable[2:6,5], NA, NA)
tabell.lme$p.kritisert.ju <- summary(lme2.kritisert)$tTable[2:8,5]
tabell.lme$p.kritisert.uj <- c(summary(lme1.kritisert)$tTable[2:6,5], NA, NA)
samlet.p.grade <- c(rep(NA,18), # samlet is overall
                    anova(lme2.flau, type= "marginal")[3,4],
                    anova(lme1.flau, type= "marginal")[3,4],
                    anova(lme2.ertet, type= "marginal")[3,4],
                    anova(lme1.ertet, type= "marginal")[3,4],
                    anova(lme2.kritisert, type= "marginal")[3,4],
                    anova(lme1.kritisert, type= "marginal")[3,4])
tabell.lme <- rbind(tabell.lme,samlet.p.grade)
row.names(tabell.lme)[8] <- "Grade" # flytter samlet p verdi til rett etter gender
tabell.lme <- tabell.lme[c(1,8,2:7),]

names(tabell.lme)
tabell.lme.ju <- tabell.lme[,c(1,7,13,19,3,9,15,21,5,11,17,23)]
tabell.lme.ju.round <- tabell.lme.ju
tabell.lme.ju.round[,c(1:3,5:7, 9:11)] <- 
  round(tabell.lme.ju[,c(1:3,5:7, 9:11)],2)
tabell.lme.ju.round[,c(4,8,12)] <- 
  round(tabell.lme.ju[,c(4,8,12)],3)

#take the table with adjustment for anxiety and depression bout of R
?write.table
write.table(x=tabell.lme.ju.round,file="lme adjusted.txt",
            quote = FALSE,sep = ";",na ="")
# output file in the same folder as the script, insert ; in the start of the 
# first row with variable names, copy all, paste into word, insert table, and 
# convert text to table

# correlations
cor.test(et$ace05n,et$ace08n) 
cor.test(et$ace05n,et$ace09n) 
# ace06 is ertet (teased) and coping
cor.test(et$ace06n,et$ace08n)
cor.test(et$ace06n,et$ace09n)
# next kritisert (criticised) and coping
cor.test(et$ace07n,et$ace08n)
cor.test(et$ace07n,et$ace09n)

# look at user satisfaction
cor.test(et$ace05n,et$brukforn.0.100.hr) # lav og negativt estimat, men sign (svakt)
cor.test(et$ace06n,et$brukforn.0.100.hr)
cor.test(et$ace07n,et$brukforn.0.100.hr)

# stigma and change (improvement) in outcome T2 - T1
# first change in anxiety
lm1.angstdiff <- lm(I(MASC.T1.0.100.hr-MASC.T2.0.100.hr)~
                      gender+trinn3+stor+ace05n+ace06n+ace07n, data = et)
summary(lm1.angstdiff)
drop1(lm1.angstdiff, test= "Chisq") # overall p-values for multi-category variables
confint(lm1.angstdiff)
# next change (improvement) in depression
lm1.depresjonsdiff <- lm(I(SMFQ.T1.0.100.hr-SMFQ.T2.0.100.hr)~
                      gender+trinn3+stor+ace05n+ace06n+ace07n, data = et)
summary(lm1.depresjonsdiff) 
drop1(lm1.depresjonsdiff, test= "Chisq")
confint(lm1.depresjonsdiff)

# finally descriptives
summary(et[,c("gender","trinn","stor")])
