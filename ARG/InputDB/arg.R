# need to install HMDLexis
library(devtools)
options(max.print = 6000)
setwd("C:/Users/User/Desktop/HMD_Arg")
load_all("HMDLexis")

# Use Official estimates instead of census (test)
C_or_O_or_E = "O"
obj <- read.table("ARG/InputDB/ARGpop.txt", 
                  sep = ",", 
                  na.strings = ".", 
                  header = TRUE, 
                  stringsAsFactors = FALSE,
                  strip.white = TRUE)
obj$LDB <- 0
obj$LDB[obj$Type == C_or_O_or_E] <- 1
obj[is.na(obj)] <- "."

write.table(obj, 
            file = "ARG/InputDB/ARGpop.txt", 
            sep = ",", 
            col.names = colnames(obj), 
            row.names = FALSE,
            quote = FALSE,
            eol = "\r\n")
rm(obj)

# get this to work
ARG     <- readInputDB(WORKING = "ARG", XXX="ARG")
Deaths  <- ARG$Deaths #xtabs(Deaths~Age+Year,Deaths,Age>79)
Pop     <- ARG$Population
Births  <- ARG$Births
Tadj    <- ARG$Tadj

# ----------------------------------------------------------------------
# process Deaths
# ----------------------------------------------------------------------

# sum-in late registered events (group by Year, forget YearReg). Can be considered a pre-processing step
Deaths <- d_agg(Deaths)
Deaths <- d_unk(Deaths) 
Deaths <- d_soainew(Deaths)
Deaths <- d_long(Deaths)
# the way we declare projection through year t (generating pop estimate for jan 1 in t+1)
# is by cutting down deaths
Deaths  <- subset(Deaths, Year <= 2013)

# ----------------------------------------------------------------------
# Steps to get Pop (recomendeds by Tim)
# ----------------------------------------------------------------------
# some first visualization of 2010
plot(Pop[Pop$Year==2010 & Pop$Sex=="m", c("Age", "Population")], t="l")

# 1) make sure p_soai is done on year 2010 census (if necessary)
# extend open ages prior to IC to be able to fill in up to ca age 80
# since p_soai() needs deaths 10 years to the left, we remove and then
# rbind back on the 1990 census
PopLate <- p_soai(Pop = subset(Pop, Year > 1995), Deaths = Deaths)
Pop     <- rbind(Pop[Pop$Year < 1995, ], PopLate)

# OK. omega seems to be 110 for HMP
plot(Pop[Pop$Year==2010 & Pop$Sex=="m", c("Age", "Population")], t="l", xlim=c(70,130), ylim=c(0,1000))

# 2) p_split() on 2000 official (make sure includes ages 79 properly)
Pop2001 <- p_split(Pop = subset(Pop, Year>1995), Deaths = Deaths, Births = Births) 

# mmm! Big problem in 79/80
plot(Pop2001[Pop2001$Year==2001 & Pop2001$Sex=="m", c("Age", "Population")], t="l")

# 3) p_soai() on 2000 official (make sure 79-80 join was good first)
# 4) p_split() to 1990
# 5) check that ages up to and including 79 are OK

# ----------------------------------------------------------------------
# final things
# ----------------------------------------------------------------------
Pop     <- p_ic(Pop, Deaths, Births)
Pop     <- p_postcensal(Pop = Pop, Deaths = Deaths, Births = Births)
# 1990 still has an open age group, which we remove for this
Pop     <- p_precensal(Pop = subset(Pop, !is.na(AgeIntervali)), Deaths = Deaths, Births = Births)
Pop     <- p_srecm(Pop, Deaths, Tadj)

# need to patch in age 79 in year 1990, hack:
# Pop79   <- p_ecm(Pop, Deaths, Tadj, a = 79)
# Age79in1990 <- Pop79[Pop79$Year == 1990 & Pop79$Agei == 79, ]
# Pop     <- resortPops(rbind(Pop, Age79in1990))
# finish off
Pop     <- p_long(Pop)

# build LDB file
ldb.by.sex <- computeLDB(Pop = Pop, Deaths = Deaths, Births = Births, Tadj=Tadj)

# save it:
writeLDB(ldb.by.sex, LDBpath = "ARG/LexisDB") # WORKING/XXX/LexisDB

# now process lifetables
load_all("C:/Users/User/Desktop/HMD_Arg/HMDLifeTables/HMDLifeTables/HMDLifeTables/R")
setwd("ARG")
getwd()
RunHMDCountry()
setwd("C:/Users/User/Desktop/HMD_Arg")
