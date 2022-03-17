type 'a string_builder = 
| Vide
| Feuille of 'a * int
| Noeud of ('a string_builder) * int * ('a string_builder)

let word mot = string_builder(mot,String.length mot);;

let rec concat stb1 stb2 = match stb1 with
|Vide -> stb2
|Feuille(mot,longueur) -> Noeud(Feuille(mot,longueur),(* longueur + length stb2 *),stb2)
|Noeud(filsg,longueur,filsd) -> string_builder(filsg,(* longueur + length stb2 *),concat filsd stb2)
;;

(* Il faut que je regarde ce que j'ai fais sur mes anciens TP de Ocaml pour voir comment on gère le type *)