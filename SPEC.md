# O projektu

## Ciljevi

- Pristupačna vizuelizacija i analiza podataka o upisu u srednje škole od 2017. (primarno) / 2015. (sekundarno) godine do danas. [Napomena 1]
- Primena statističkih alata na relativno velikom skupu realnih podataka (~66k učenika, ~2300 smerova, ~1200 osnovnih škola, 500-600k želja po godini).
- Prikaz `shiny` frameworka i izrada kompletne veb aplikacije u R-u.
- Omogućiti krajnjem korisniku što više kontrole pri izboru parametera za vizualizacije i analize kroz veb interfejs.

## Podaci
Podaci su prikupljeni pomoću scraper-a koji je dostupan na [ovoj adresi](https://github.com/luka-j/UpisScraper). Između ostalog, sadrže:
- Za svakog učenika: ocene, uspeh na državnim takmičenjima, brojeve bodova na završnim ispitima, krug upisa, listu želja.
- Za svaki smer: naziv, mesto, okrug, kvotu, područje rada, srednju školu, proseke numeričkih podataka učenika (ocene, bodovi)
- Za svaku osnovnu školu: naziv, opštinu, broj učenika

## Statistički aspekti
- Računanje deskriptivnih statistika i crtanje tačkastih grafikona za proizvoljne numeričke parametre. Prikaz više od dve dimenzije (boje, faceting). Prilagođavanje za veliki broj tačaka: jitter, transparentnost.
- Ocena parametara: (?) izračunati ocene parametara i pokazati koliko su one "dobre". [Napomena 2]
- Testiranje hipoteza: neparametarski testovi, npr izdvojiti klase smerova (po mestu ili području rada) i videti da li ocene potiču iz iste raspodele; ili saglasnost sa raspodelom uz grafikon realnih podataka i raspodele s kojom se poredi.
- Regresioni modeli: linearna regresija, grafikon reziduala. [Napomena 3]

## Tehnički aspekti
- Projekat čini veb aplikacija koja će biti javno dostupna i mora da radi na bilo kom novijem pretraživaču.
- Aplikacija se sastoji od više tabova za različite oblasti i korisnik bira šta želi da mu se prikaže.
- Aplikacija mora da prikaže rezultate u razumnom roku i ne sme da "zabada" pri normalnoj upotrebi. [Napomena 4]
- Omogućeno je osnovno filtriranje podataka pre bilo koje akcije.
- Svi podaci se nalaze u relacionoj bazi podataka koju aplikacija koristi. Delovi se učitavaju u memoriju po potrebi.
- Sav kod je napisan u R-u, koristi `shiny` framework, biblioteku za pristup bazi podataka, ostale biblioteke po potrebi (`tidyverse` gde je moguće).

## Napomene
1. Podaci postoje od 2015. godine, ali su za 2015. i 2016. u blago drugačijem formatu (neki delovi fale, neki se drugačije zovu). Idealno, aplikacija bi trebalo nesmetano da radi sa svim podacima od 2015, ali će pri izradi fokus na novijim podacima.
2. Nisam siguran koliko ovo ima smisla, jer je dosta nezahvalno pretpostaviti raspodelu za bilo koji od ovih podataka.
3. Čini mi se da ovo može biti zanimljivo nezavisno od raspodele podataka.
4. Uz pretpostavku da je koristi jedan korisnik u datom trenutku. Nije cilj napraviti aplikaciju koja može da izdrži bilo kakvo netrivijalno opterećenje.