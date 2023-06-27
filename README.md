# H2_V2 x mlr3 na Supeku

  Ispod se nalaze upute za izvršavanje H2_V2.R skripte korištenjem mlr3 knjižnice na Supeku

## Koraci

  1. Korištenjem definicijske datoteke `image.def` i bash skripte`image.sh`, pripremiti kontejner `image.sif` sa svim ovisnostima potrebnim za izvršavanje
  1. **BITNO**: u istom direktoriju, napraviti datoteku `blob_key.txt` s vrijednosti Azure vjerodajnice
  1. Skinuti podatke na jednom od pristupnih poslužitelja korištenjem skripta `download.R` i `download.sh`
  1. Podnijeti posao `run.sh` koji će na Supeku izvršiti skriptu `run.R` (temeljenu na originalnoj `H2_V2.R`)

## Datoteke

  1. `image.def` - definicijska datoteka za izgradnju kontejnera
  1. `image.sh` - bash skripta koja će izgraditi kontejner
  1. `download.R` - R skripta koja dostavlja podatke s Azurea
  1. `download.sh` - bash skripta koja će skinuti podatke s Azurea
  1. `run.R` - ML skripta temeljena na `H2_V2.R`
  1. `run.sh` - definicijska datoteka za izgradnju kontejnera
