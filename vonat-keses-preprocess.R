library(data.table)

##### ProcData #####

RawData <- rbindlist(lapply(
  list.files("./data/raw/", full.names = TRUE), function(f)
    rbindlist(readRDS(f), use.names = TRUE, fill = TRUE)),
  use.names = TRUE, fill = TRUE)

RawData[Menetrend.szerint == ""]$Menetrend.szerint <- NA
RawData[Menetrend.szerint.1 == ""]$Menetrend.szerint.1 <- NA
RawData[Tényleges == ""]$Tényleges <- NA
RawData[Tényleges.1 == ""]$Tényleges.1 <- NA
RawData[Várható == ""]$Várható <- NA
RawData[Várható.1 == ""]$Várható.1 <- NA
RawData[Km == ""]$Km <- NA

unique(RawData[is.na(Km)]$Állomás) # mind külföldi kell legyen
RawData <- RawData[!is.na(Km)]
length(which(is.na(as.numeric(RawData$Km)))) == 0
RawData$Km <- as.numeric(RawData$Km)

alkalmas <- function(x) {
  length(setdiff(unique(nchar(RawData$Menetrend.szerint)), c(NA, 5))) == 0 &&
    length(setdiff(unique(substring(RawData$Menetrend.szerint, 3, 3)), c(NA, ":"))) == 0 &&
    !any(is.na(as.numeric(substring(RawData$Menetrend.szerint, 1, 2))) & !is.na(RawData$Menetrend.szerint)) &&
    !any(is.na(as.numeric(substring(RawData$Menetrend.szerint, 4, 5))) & !is.na(RawData$Menetrend.szerint))
}

alkalmas(RawData$Menetrend.szerint)
alkalmas(RawData$Menetrend.szerint.1)
alkalmas(RawData$Tényleges)
alkalmas(RawData$Tényleges.1)

RawData[, Menetrend.szerint.num := as.numeric(substring(Menetrend.szerint, 1, 2)) * 60 + as.numeric(substring(Menetrend.szerint, 4, 5))]
RawData[, Menetrend.szerint.1.num := as.numeric(substring(Menetrend.szerint.1, 1, 2)) * 60 + as.numeric(substring(Menetrend.szerint.1, 4, 5))]
RawData[, Tényleges.num := as.numeric(substring(Tényleges, 1, 2)) * 60 + as.numeric(substring(Tényleges, 4, 5))]
RawData[, Tényleges.1.num := as.numeric(substring(Tényleges.1, 1, 2)) * 60 + as.numeric(substring(Tényleges.1, 4, 5))]

RawData[Tényleges.1.num - Tényleges.num < 0 & Tényleges.1.num - Tényleges.num >= -720,
        c("Tényleges", "Tényleges.1", "Tényleges.num", "Tényleges.1.num") := list(NA, NA, NA, NA)]

!any(RawData[, .(sum(is.na(Vonat)), sum(!is.na(Vonat))), .(Datum)][, !xor(V1, V2)]) # vagy minden Vonat ki van töltve vagy egy sem egy nap
# amelyik nap nincs, azt kitöltjük

RawData <- merge(RawData, unique(RawData[, .(Datum, VonatSzam)])[, .(VonatSzam, Vonat = 1:.N) , .(Datum)], by = c("Datum", "VonatSzam"))
RawData$Vonat <- ifelse(is.na(RawData$Vonat.x), RawData$Vonat.y, RawData$Vonat.x)
RawData$Vonat.x <- NULL
RawData$Vonat.y <- NULL

nrow(RawData[, .N, .(Datum, Vonat, Állomás)][N > 1]) == 0

RawData <- RawData[, if (.N > 1) .SD, .(Datum, Vonat)]

# ami nincs követve az nem is biztos, hogy aznapi vonat, kivesszük
# RawData[, .N, .(Datum)]

RawData[, Kovetett := sum(!is.na(Tényleges)) > 0 || sum(!is.na(Tényleges.1)) > 0, .(Datum, Vonat)]
RawData <- RawData[Kovetett == TRUE]
RawData$Kovetett <- NULL

# RawData[, .N, .(Datum)]
# plot(N ~ Datum, data = RawData[, .N, .(Datum)], type = "b")

ProcData <- rbind(
  RawData[
    , .(KmIndulo = Km[1], KmErkezo = Km[1], Indulo = Állomás[1],
        Erkezo = Állomás[1], Nominalis = 0, KumNominalis = 0,
        Tenyleges = Tényleges.1.num[1] - Menetrend.szerint.1.num[1],
        KumTenyleges = Tényleges.1.num[1] - Menetrend.szerint.1.num[1],
        Tipus = "InduloAllomas", ord = 1),
    .(Datum, Vonat, VonatSzam)][!is.na(ord)],
  RawData[
    , .(KmIndulo = Km[-length(Km)], KmErkezo = Km[-1],
        Indulo = Állomás[-length(Állomás)],
        Erkezo = Állomás[-1],
        Nominalis = Menetrend.szerint.num[-1] - Menetrend.szerint.1.num[-length(Menetrend.szerint.1.num)],
        KumNominalis = Menetrend.szerint.num[-1] - Menetrend.szerint.1.num[1],
        Tenyleges = Tényleges.num[-1] - Tényleges.1.num[-length(Tényleges.1.num)],
        KumTenyleges = Tényleges.num[-1] - Menetrend.szerint.1.num[1],
        Tipus = c(rep("Szakasz", .N - 2), "ZaroSzakasz"),
        ord = seq(2, by = 2, length.out = .N - 1)),
    .(Datum, Vonat, VonatSzam)][!is.na(ord)],
  RawData[
    , .(KmIndulo = Km[-c(1, .N)], KmErkezo = Km[-c(1, .N)],
        Indulo = Állomás[-c(1, .N)],
        Erkezo = Állomás[-c(1, .N)],
        Nominalis = Menetrend.szerint.1.num[-c(1, .N)] - Menetrend.szerint.num[-c(1, .N)],
        KumNominalis = Menetrend.szerint.1.num[-c(1, .N)] - Menetrend.szerint.1.num[1],
        Tenyleges = Tényleges.1.num[-c(1, .N)] - Tényleges.num[-c(1, .N)],
        KumTenyleges = Tényleges.1.num[-c(1, .N)] - Menetrend.szerint.1.num[1],
        Tipus = "KozbensoAllomas",
        ord = seq(3, by = 2, length.out = .N - 2)),
    .(Datum, Vonat, VonatSzam)][!is.na(ord)])[order(Datum, Vonat, VonatSzam, ord)]

ProcData[, ord := NULL]

ProcData[, Nominalis := fifelse(Nominalis < -720, Nominalis + 1440, Nominalis)]
ProcData[, KumNominalis := fifelse(KumNominalis < -720, KumNominalis + 1440, KumNominalis)]
ProcData[, Tenyleges := fifelse(Tenyleges < -720, Tenyleges + 1440, Tenyleges)]
ProcData[, KumTenyleges := fifelse(KumTenyleges < -720, KumTenyleges + 1440, KumTenyleges)]

ProcData$Keses <- ProcData$Tenyleges - ProcData$Nominalis
ProcData$KumKeses <- ProcData$KumTenyleges - ProcData$KumNominalis

ProcData[Indulo == "Bélapátfalvi Cementgyár"]$Indulo <- "Bélapátfalvai Cementgyár"
ProcData[Erkezo == "Bélapátfalvi Cementgyár"]$Erkezo <- "Bélapátfalvai Cementgyár"

names(ProcData)[names(ProcData) == "VonatSzam"] <- "VonatNev"

ProcData$VonatSzam <- as.numeric(sapply(strsplit(ProcData$VonatNev, " "), `[[`, 1))

ProcData$VonatNev <- stringr::str_squish(ProcData$VonatNev)

for(remstr in paste0(", 2025.06.", 11:20, "."))
  ProcData$VonatNev <- gsub(remstr, "", ProcData$VonatNev)

temp <- unlist(strsplit(unique(ProcData$VonatNev), " "))

for(remstr in c(unique(paste0(temp[grep("^S\\d+$", temp)], " ")),
                unique(paste0(temp[grep("^Z\\d+$", temp)], " ")),
                unique(paste0(temp[grep("^G\\d+$", temp)], " ")),
                unique(paste0(temp[grep("^IR\\d+$", temp)], " "))))
  ProcData[Datum >= "2025-06-11" & Datum <= "2025-06-20",
           VonatNev := gsub(remstr, "", VonatNev)]
ProcData[, VonatNev := gsub("TramTrain 1", "TramTrain", VonatNev)]

ProcData <- merge(ProcData, ProcData[, .(VonatNevLabel = names(sort(table(VonatNev), decreasing = TRUE))[1]) , .(VonatSzam)], by = "VonatSzam", sort = FALSE)

qgrepl <- function(x) grepl(x, ProcData$VonatNevLabel, ignore.case = TRUE)

ProcData$VonatNem <- dplyr::case_when(
  qgrepl("személyvonat") ~ "Személyvonat",
  qgrepl("InterCity") ~ "InterCity",
  qgrepl("InterRégió") ~ "InterRégió",
  qgrepl("railjet xpress") ~ "Railjet xpress",
  qgrepl("railjet") ~ "Railjet",
  qgrepl("gyorsvonat") ~ "Gyorsvonat",
  qgrepl("TramTrain") ~ "TramTrain",
  qgrepl("Expresszvonat") ~ "Expresszvonat",
  qgrepl("sebesvonat") ~ "Sebesvonat",
  qgrepl("EuroCity") ~ "EuroCity",
  qgrepl("EuRegio") ~ "EuRegio",
  qgrepl("EuroNight") ~ "EuroNight",
  qgrepl("Night Jet") ~ "Night Jet",
  qgrepl("Interregional") ~ "Interregional",
  qgrepl("International") ~ "International",
  qgrepl("vonatpótló autóbusz") ~ "Vonatpótló autóbusz",
  .default = "Egyéb"
)

# # unique(ProcData[VonatSzam < 100]$VonatNem)
# # unique(ProcData[VonatSzam < 100 & VonatNem == "Egyéb"]$VonatNev)
# ProcData[VonatSzam < 100][grepl("TRAIANUS", VonatNev)]$VonatNem <- "InterCity"
# 
# # unique(ProcData[VonatSzam >= 100 & VonatSzam < 500]$VonatNem)
# # unique(ProcData[VonatSzam >= 100 & VonatSzam < 500 & VonatNem == "Egyéb"]$VonatNev)
# # unique(ProcData[VonatSzam >= 100 & VonatSzam < 500 & VonatNem == "Személyvonat"]$VonatNev)
# ProcData[VonatSzam >= 100 & VonatSzam < 500][grepl("CORONA", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 100 & VonatSzam < 500][grepl("METROPOLITAN", VonatNev)]$VonatNem <- "EuroCity"
# ProcData[VonatSzam >= 100 & VonatSzam < 500][grepl("HERNÁD - ZEMPLÉN", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 100 & VonatSzam < 500][grepl("HERNÁD", VonatNev)]$VonatNem <- "InterCity"
# # 358 és társai: úgy tűnik ez tényleg személyvonat, a szám ellenére

# # unique(ProcData[VonatSzam >= 500 & VonatSzam < 1000]$VonatNem)
# # unique(ProcData[VonatSzam >= 500 & VonatSzam < 1000 & VonatNem == "Egyéb"]$VonatNev)
# # unique(ProcData[VonatSzam >= 500 & VonatSzam < 1000 & VonatNem == "Személyvonat"]$VonatNev)
ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("BAKONY", VonatNev)]$VonatNem <- "InterCity"
ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("SAVARIA", VonatNev)]$VonatNem <- "InterCity"
ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("GÖCSEJ", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("KRESZ GÉZA", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("SOMOGY", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("RIPPL-RÓNAI", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("MECSEK", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("NAPFÉNY", VonatNev)]$VonatNem <- "InterCity"
# ProcData[VonatSzam >= 500 & VonatSzam < 1000][grepl("ALFÖLD", VonatNev)]$VonatNem <- "Expresszvonat"
# 
# # 969, 968: ?
# # 642: úgy tűnik ez tényleg személyvonat, a szám ellenére

ProcData$VonatNev <- as.factor(ProcData$VonatNev)
ProcData$VonatNevLabel <- as.factor(ProcData$VonatNevLabel)
ProcData$Indulo <- as.factor(ProcData$Indulo)
ProcData$Erkezo <- as.factor(ProcData$Erkezo)
ProcData$Tipus <- as.factor(ProcData$Tipus)
ProcData$VonatNem <- as.factor(ProcData$VonatNem)

saveRDS(ProcData[, .(Datum, VonatSzam, VonatNev, VonatNevLabel, Indulo, Erkezo, Tipus, Keses, KumKeses, VonatNem)], "./data/ProcData.rds")

saveRDS(list(
  VonatNev = with(unique(ProcData[, .(VonatSzam, VonatNevLabel)])[order(VonatSzam)], setNames(VonatSzam, VonatNevLabel)),
  VonatNem = sort(unique(ProcData$VonatNem)),
  AllomasErkezo = sort(unique(ProcData$Erkezo)),
  AllomasErkezoIndulo =
    sort(unique(c(ProcData$Indulo, ProcData$Erkezo)))),
  "./data/choices.rds")

##### colstops #####

colstops <- highcharter::list_parse2(data.frame(
  q = seq(0, 1, length.out = 100),
  col = scales::pal_seq_gradient("blue", "red")(
    seq(0,1, length.out = 100))))

saveRDS(colstops, "./data/colstops.rds")

##### Állomás #####

allomaskoord <- overpass::overpass_query(paste0(
  '[out:csv(::id, ::type, "name", ::lat, ::lon)];',
  'area["ISO3166-1"="HU"][admin_level=2];',
  '(',
  '  node["railway"](area);',
  ');',
  'out center;'))
allomaskoord <- fread(allomaskoord)
allomaskoord <- allomaskoord[, .(Allomas = `name`, lat = `@lat`,
                                 lon = `@lon`)]
allomaskoord <- allomaskoord[Allomas != ""]
allomaskoord <- allomaskoord[!duplicated(Allomas)]
saveRDS(allomaskoord, "./data/allomaskoord.rds")