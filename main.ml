(* Question 1 *)

type 'a string_builder = 
| Feuille of 'a * int
| Noeud of ('a string_builder) * int * ('a string_builder)

(* Transorme un mot en une feuille *)
let word mot = Feuille(mot,String.length mot);;

(* Retourne la longueur de le chaine de caractère *)
let length_string_builder stb = match stb with
|Noeud(filg,longueur,fild) -> longueur
|Feuille(mot,longueur) -> longueur
;; 

(* concataine 2 string_builder, idée :  aller le plus en bas à droite *)
let rec concat stb1 stb2 =
  let longueur2 = length_string_builder stb2 in
  match stb1 with
  |Feuille(mot,longueur) -> Noeud(Feuille(mot,longueur), longueur + longueur2 , stb2)
  |Noeud(filsg,longueur,filsd) -> Noeud(filsg, longueur + longueur2 ,concat filsd stb2)
;;

(* Transforme un string_builder en un string inutile pour le moment *)
let rec conca_word stb = match stb with
|Feuille(mot,longueur) -> mot 
|Noeud(filsg,longueur,filsd) -> (conca_word filsd)^(conca_word filsd)
;;

(* Erreur ici *)
(* Question 2*)
let rec char_at stb i = match stb with
  |Noeud(Noeud(filsgg,longueurG,filsgd), longueurC , Noeud(filsdg,longueurD,filsdd)) ->
    if i <= longueurD then char_at Noeud(filsdg,longueurD,filsdd) i else char_at Noeud(filsgg,longueurG,filsgd) (i - longueurD)
  |Noeud(Feuille(mot,longueurM),longueurC,Noeud(filsdg,longueurD,filsdd)) ->
    if i <= longueurD then char_at Noeud(filsdg,longueurD,filsdd) i else String.get mot (i - longueurD)
  |Noeud(Noeud(filsgg,longueurG,filsgd),longueurC,Feuille(mot,longueurM)) ->
    if i <= longueurM then String.get mot i else char_at Noeud(filsgg,longueurG,filsgd) (i - longueurM)
  |Noeud(Feuille(motg,longueurMg),longueurC,Feuille(motd,longueurMd)) ->
    if i <= longueurMd then String.get motd i else String.get motg (i - longueurMd)
  ;;



  (* Question 3 *)
(*
let rec supp_debut stb i = 
  let ni = length_string_builder stb in
  match stb with
  |
*)