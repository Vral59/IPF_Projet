Random.self_init();;

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

(* Question 5 *)

(* Fonction puissance en exponentiation rapide *)
let rec exponentiation x n =
  if n = 0 then 1
  else let y = exponentiation x (n/2) in
  if n mod 2 = 0 then y * y else x * y * y 
;;

(* Sert à compter le nombre de point marqué *)
let rec verif stb = match stb with
|Feuille(_,-1) -> 1
|Feuille(_,a) -> 0
|Noeud(fg,-1,fd) -> 1 + (verif fg) + (verif fd)
|Noeud(fg,a,fd) -> 0 + (verif fg) + (verif fd)
;;


(* Colore les points de l'arbre selon des règles précises : 
Si un enfant est rouge le père est rouge. 
Si le frère et le père sont rouge alors le point est rouge 

Note : A appliquer plusieurs fois jusqu'à que tout se stabilise 
*)
let rec colorisation stb = match stb with
|Noeud(Noeud(fgg,hg,fgd),-1,Noeud(fdg,hd,fdd)) -> if hg = -1 
  then Noeud(colorisation (Noeud(fgg,hg,fgd)),-1,colorisation (Noeud(fdg,-1,fdd)))
  else if hd = -1 
  then Noeud(colorisation (Noeud(fgg,-1,fgd)),-1,colorisation (Noeud(fdg,hd,fdd)))
  else Noeud(colorisation (Noeud(fgg,hg,fgd)),-1,colorisation (Noeud(fdg,hd,fdd)))
|Noeud(Feuille(m1,lg),-1,Feuille(m2,ld)) -> if lg = -1
  then Noeud(Feuille(m1,lg),-1,Feuille(m2,-1))
  else if ld = -1
  then Noeud(Feuille(m1,-1),-1,Feuille(m2,ld))
  else Noeud(Feuille(m1,lg),-1,Feuille(m2,ld))
|Noeud(fg,h,fd) -> let couleureG = length_string_builder fg in
  let couleureD = length_string_builder fd in
  if (couleureG = -1) || (couleureD = -1) then Noeud(colorisation fg,-1,colorisation fd)
  else Noeud(colorisation fg,h,colorisation fd)
|Feuille(mot,l) -> Feuille(mot,l)

(* On applique la colorisation jusqu'à que tout ce qui pouvait être coloré le soit *)
let colorisation_total stb = 
  let rec aux arbre ancien nouveau = match ancien,nouveau with
  |a,n when a = n -> arbre
  |a,n -> let nouveauStb = colorisation arbre in  aux nouveauStb n (verif nouveauStb)
  in aux stb 0 (verif stb)
;;


(* On supprime tous les points non colorié *)
let rec suppression_non_r stb = match stb with
|Feuille(mot,h) -> Feuille(mot,h)
|Noeud(Feuille("",hg),-1,Feuille("",hd)) -> if hg = -1 then Noeud(Feuille("",hg),-1,Feuille("",hd)) (* D'après la colorisation si hg = -1 alors ici hd = -1*)
  else Feuille("",-1)  
|Noeud(fg,-1,fd) -> Noeud(suppression_non_r fg, -1, suppression_non_r fd)
|Noeud(fg,h,fd) -> Feuille("a",-1)
;;


(* On créé l'arbre en placant les couleurs dès le départ *)
let create_complet2 hauteur =
  let rec aux stb hauteur acc  proba = match stb with
    |Feuille(mot,i) when acc = hauteur -> 
      let choix = Random.int proba in 
      if choix = 0 then Feuille("a",-1)
      else Feuille("a",0)
    |Feuille(mot,i) -> let choix = Random.int proba in
    if choix = 0 then Noeud(aux (Feuille("a",(i+1))) hauteur (acc+1) proba,-1,aux (Feuille("a",(i+1))) hauteur (acc+1) proba)
    else Noeud(aux (Feuille("a",(i+1))) hauteur (acc+1) proba,0,aux (Feuille("a",(i+1))) hauteur (acc+1) proba)
    |Noeud(fg,h,fd) -> let choix = Random.int proba in
    if choix = 0 then Noeud(aux fg hauteur (acc+1) proba,-1,aux fd hauteur (acc+1) proba)
    else Noeud(aux fg hauteur (acc+1) proba,0,aux fd hauteur (acc+1) proba)
in aux (Feuille("a",0)) hauteur 0 ((Random.int 10) + 2)
;;
(* Note : on va tirer a hasard la probabilité d'être coloré permet d'homogénéiser l'arbre*)

(* On s'assure d'avoir la base de l'arbre colorié *)
let rec colorier_base arbre  = match arbre with
  |Feuille(mot,h) -> Feuille(mot,-1)
  |Noeud(Feuille(mot1,h1),h,Feuille(mot2,h2)) -> Noeud(Feuille(mot1,-1),h,Feuille(mot2,-1))
  |Noeud(fg,h,fd) -> let choix = Random.int 2 in 
  if choix = 1 then Noeud(fg, h, colorier_base fd) 
  else Noeud(colorier_base fg, h, fd)
;;

let tree2 = create_complet2 5;;
let tmp = colorier_base tree2;;
print_string ("Autre méthode : \n");;
print_int (verif tmp);;
print_string "\n";;
let tmp = colorisation_total tmp;;
print_string ("Après colorisation total : \n");;
print_int (verif tmp);;
print_string "\n";;
let tmp = suppression_non_r tmp;;
