(* Question 1 *)

type 'a string_builder = 
| Feuille of 'a * int
| Noeud of ('a string_builder) * int * ('a string_builder)


let exemple1 = Noeud(Feuille("G",1),7,Noeud(Feuille("ATT",3),6,Noeud(Feuille("A",1),3,Feuille("CA",2))));;

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

let exemple2 = word "Oui";;
let resultat = concat exemple1 exemple2;;

(* Transforme un string_builder en un string inutile pour le moment *)
let rec conca_word stb = match stb with
|Feuille(mot,longueur) -> mot 
|Noeud(filsg,longueur,filsd) -> (conca_word filsg)^(conca_word filsd)
;;

let mot = conca_word resultat;;
print_string mot;;
print_string "\n";;

(* Question 2*)
let rec char_at stb i = match stb with
  |Noeud(g, l, d) -> let lg = length_string_builder g in
    if i < lg then char_at g i
    else char_at d (i - lg)
  |Feuille(s, l) -> String.get s i
;;

let char1 = char_at resultat 4;;
let char2 = char_at resultat 7;;

print_char char1;;
print_string "\n";;
print_char char2;;
print_string "\n";;

(* Question 3 *)

(* retire les caractère d'indices i à la fin du mot *)
let rec eraseEnd stb i = match stb with
  |Noeud(g,l,d) -> let ld = length_string_builder d in 
    if i > ld then Noeud(eraseEnd g (i - ld), l - ld, Feuille("",0))
    else Noeud(g,l-i,eraseEnd d i)
  |Feuille(mot,l) -> Feuille(String.sub mot 0 (l-i),(l-i))
;;

(* retire les caractères d'indices 0 à i*)
let rec eraseStart stb i = match stb with
|Noeud(g,l,d) -> let lg = length_string_builder g in
  if lg > i then Noeud(eraseStart g i,(l-i),d)
  else Noeud(Feuille("",0),l-lg-i,eraseStart d (i-lg))
|Feuille(mot,l) -> Feuille(String.sub mot i (l-i),(l-i))
;;

(* On retire les deux bouts, et on effaces les feuilles vides créent dans les fonction auxilière (a corriger) *)
(* Ca fonctionne mais c'est pas propre*)
let sub_string stb i m =
  let longueur = length_string_builder stb in 
  let debutMoins = eraseStart stb i in
  let finMoins = eraseEnd debutMoins (longueur - i - m) in
  let rec aux stb = match stb with
  |Noeud(Feuille("",0),0,Feuille("",0)) -> Feuille("",0)
  |Noeud(Feuille("",0),l,d) -> aux d
  |Noeud(g,l,Feuille("",0)) -> aux g
  |Noeud(g,l,d) -> Noeud(aux g,l,aux d)
  |Feuille(m,l) -> Feuille(m,l)
in aux finMoins
;;

let suppression = sub_string resultat 2 4;;
let string_supp = conca_word suppression;;
print_string string_supp;;
print_string "\n";;

(* Question 4 *)
let cost stb = 
  let rec aux stb cpt = match stb with
  |Feuille(mot,l) -> l*cpt
  |Noeud(g,l,d) -> (aux g (cpt+1)) + (aux d (cpt+1))
in aux stb 0
;;

(* Question 8 *)


let fonction nmax =
  let taille = (nmax*(nmax+1))/2 in
  let tableau = Array.make taille 0 in
  for i = 1 to nmax do 
    for j = 0 to (i-1) do 
      tableau.(((i*(i-1))/2 + j)) <- i;
      print_int i
    done;
  done;
Random.self_init();
tableau.(Random.int taille)
;;

let x = fonction 8;;
print_string "\n";;
print_int x;;
print_string "\n";;

(*
On va partir du noeud d'origine et créer un nombre aléatoire de fils
*)