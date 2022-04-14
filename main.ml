Random.self_init();;
open Printf;;

(* ----- Question 1 ----- *)

type 'a string_builder = 
| Feuille of 'a * int
| Noeud of ('a string_builder) * int * ('a string_builder)


(* ----- Fonctions Bonus ----- *)

(* Transorme un mot en une feuille *)
let word mot = Feuille(mot,String.length mot);;

(* Retourne la longueur de le chaine de caractère *)
let length_string_builder stb = match stb with
|Noeud(filg,longueur,fild) -> longueur
|Feuille(mot,longueur) -> longueur
;; 

(* Retourne la hateur d'un stb *)
let rec hauteur_stb stb = match stb with
|Feuille(_,_) -> 1
|Noeud(g,i,d) -> 1 + (max (hauteur_stb g) (hauteur_stb d))
;;

(* ----- Fin Fonctions Bonus ----- *)


(* concataine 2 string_builder, idée :  aller le plus en bas à droite *)
let rec concat stb1 stb2 = Noeud(stb1,(length_string_builder stb1) + (length_string_builder stb2), stb2);;


(* Transforme un string_builder en un string inutile pour le moment *)
let rec conca_word stb = match stb with
|Feuille(mot,longueur) -> mot 
|Noeud(filsg,longueur,filsd) -> (conca_word filsg)^(conca_word filsd)
;;


(* ----- Question 2 ----- *)
let rec char_at stb i = match stb with
  |Noeud(g, l, d) -> let lg = length_string_builder g in
    if i < lg then char_at g i
    else char_at d (i - lg)
  |Feuille(s, l) -> String.get s i
;;


(* ----- Question 3 ----- *)

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


(* ----- Question 4 ----- *)

(* Renvoie un entier qui est le cout de l'arbre selon la formule donnée *)
let cost stb = 
  let rec aux stb cpt = match stb with
  |Feuille(mot,l) -> l*cpt
  |Noeud(g,l,d) -> (aux g (cpt+1)) + (aux d (cpt+1))
in aux stb 0
;;

(* ----- Question 5 ----- *)

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

(* Donne un string aléatoire A REFAIRE*)
let gen_passwd length =
  let gen() = match Random.int(26+26+10) with
      n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26 in
  let gen _ = String.make 1 (char_of_int(gen())) in
  String.concat "" (Array.to_list (Array.init length gen))
;;


(* On supprime tous les points non colorié et on met dans les feuilles de string aléatoire *)
let rec suppression_non_r stb = match stb with
|Noeud(Feuille(mot1,hg),-1,Feuille(mot2,hd)) -> if hg = -1 then let len1 = (1 + Random.int 4) in let len2 = (1 + Random.int 4) in
  Noeud(Feuille(gen_passwd len1,len1),-1,Feuille(gen_passwd len2,len2)) (* D'après la colorisation si hg = -1 alors ici hd = -1*)
  else let len = (1 + Random.int 4) in Feuille(gen_passwd len,len)  
|Noeud(fg,-1,fd) -> Noeud(suppression_non_r fg, -1, suppression_non_r fd)
|Noeud(fg,h,fd) -> let len = (1 + Random.int 4) in Feuille(gen_passwd len, len)
|Feuille(mot,h) -> let len = (1 + Random.int 4) in Feuille(gen_passwd len,len)
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
in aux (Feuille("a",0)) hauteur 0 ((Random.int 20) + 2)
;;
(* Note : on va tirer au hasard la probabilité d'être coloré permet d'homogénéiser l'arbre*)

(* On s'assure d'avoir la base de l'arbre colorié *)
let rec colorier_base arbre  = match arbre with
  |Feuille(mot,h) -> Feuille(mot,-1)
  |Noeud(Feuille(mot1,h1),h,Feuille(mot2,h2)) -> Noeud(Feuille(mot1,-1),h,Feuille(mot2,-1))
  |Noeud(fg,h,fd) -> let choix = Random.int 2 in 
  if choix = 1 then Noeud(fg, h, colorier_base fd) 
  else Noeud(colorier_base fg, h, fd)
;;


let change_c stb = match stb with
|Feuille(m,i) -> Feuille(m,-1)
|Noeud(g,i,d)-> Noeud(g,-1,d)
;;

let get_color stb = match stb with
|Feuille(m,i) -> i
|Noeud(g,i,d) -> i
;;

(* DFS  pour colirier tout l'arbre *)
let rec colorisation arbre = match arbre with
| Noeud(g,h,d) -> 
  let fg  = colorisation g in
  let fd = colorisation d in
  let ig = get_color fg in
  let id = get_color fd in
  if ig = -1 || id = -1 then Noeud(change_c fg,-1, change_c fd)
  else Noeud(fg,h,fd)
|Feuille(mot,h) -> Feuille(mot,h)
;;


let random_string i = 
  let arbre = create_complet2 i in
  let arbre = colorier_base arbre in
  let arbre = colorisation arbre in
  suppression_non_r arbre
;;
(* NOTE IL N'Y A PAS LES BONNES TAILLES SUR LES NOEUDS *)

(* ----- Question 6 ----- *)

(* Fait une liste des feuilles dans l'ordre du mot *)
let rec list_of_string stb = match stb with 
|Feuille(mot,_) -> [mot]
|Noeud(fg,_,fd) -> (list_of_string fg) @ (list_of_string fd)
;;


(* ----- Question 7 ----- *)

(* Retourne le minimum d'une liste *)
let min_list lst =
	List.fold_left (fun a b -> if a < b then a else b) (List.hd lst) lst
;;

(* Créer la liste des couts*)
let rec create_cost_list liste = match liste with
|t1::t2::[] -> [cost (Noeud(t1,(length_string_builder t1) + (length_string_builder t2), t2))]
|t1::t2::q -> (cost (Noeud(t1,(length_string_builder t1) + (length_string_builder t2), t2)))::(create_cost_list (t2::q))
|_ -> failwith "Situattion innatendu"
;;

(* renvoie l'indice dans la liste du premier éléments qui va être fusionner *)
let rec find_min listeCost minCost = match listeCost with
|t::q when t = minCost-> 0
|t::q -> 1 + (find_min q minCost)
|_ -> failwith "Min pas dans la liste"
;; 

(* Fusionne la paire d'indice i et i+i *)
let fusion liste indice = 
  let rec aux liste indice ite = match liste with
  |t1::t2::q when indice = ite -> (concat t1 t2)::q
  |t::q -> t::(aux q indice (ite+1))
  |_ -> failwith "Impossible on doit forcément arriver à l'indice"
in aux liste indice 0
;;

(* Prend un string_builder et renvoir la liste de ses feuilles *)
let rec list_of_Feuille stb = match stb with 
|Feuille(mot,x) -> [Feuille(mot,x)]
|Noeud(fg,_,fd) -> (list_of_Feuille fg) @ (list_of_Feuille fd)
;;

(* Equilibre un string_builder selon  l'algorithme proposé *)
let balance stb = 
  let liste = ref (list_of_Feuille stb) in
  let n = List.length !liste in
  for i = 0 to (n-2) do
    let listeC = create_cost_list !liste in
    let minCost = min_list listeC in
    let indice = find_min listeC minCost in
    liste := fusion !liste indice
  done;
  List.hd !liste
;;


(* ----- Question 8 ----- *)

(* Prend un nombre d'arbre à générer et la hateur de chaque arbre *)
let gain_balance n h = 
  let tableau = Array.make n h in
  let tableau_stb = Array.map random_string tableau in
  let tableauC = Array.map cost tableau_stb in
  let tableau_stb = Array.map balance tableau_stb in
  let tableauCBal = Array.map cost tableau_stb in 
  let minimum1 = ref max_int in
  let maximum1 = ref min_int in
  let minimum2 = ref max_int in
  let maximum2 = ref min_int in
  let somme1 = ref 0 in
  let somme2 = ref 0 in
  let res = Array.make 6 0 in
  for i = 0 to n-1 do
    somme1 := !somme1 + tableauC.(i);
    somme2 := !somme2 + tableauCBal.(i);
    if tableauC.(i) < !minimum1 then minimum1 := tableauC.(i);
    if tableauC.(i) > !maximum1 then maximum1 := tableauC.(i);
    if tableauCBal.(i) < !minimum2 then minimum2 := tableauCBal.(i);
    if tableauCBal.(i) > !maximum2 then maximum2 := tableauCBal.(i)
  done;
  res.(0) <- !somme1 / n;
  res.(1) <- !somme2 / n;
  res.(2) <- !maximum1;
  res.(3) <- !maximum2;
  res.(4) <- !minimum1;
  res.(5) <- !minimum2;
  res;;
(* Note : Faire la médiane *)


(* ------ TEST ----- *)

let exemple1 = Noeud(Feuille("G",1),7,Noeud(Feuille("ATT",3),6,Noeud(Feuille("A",1),3,Feuille("CA",2))));;

let exemple2 = word "Oui";;
let resultat = concat exemple1 exemple2;;

let mot = conca_word resultat;;
print_string mot;;
print_string "\n";;

let char1 = char_at resultat 4;;
let char2 = char_at resultat 7;;

print_char char1;;
print_string "\n";;
print_char char2;;
print_string "\n";;

let suppression = sub_string resultat 2 4;;
let string_supp = conca_word suppression;;
print_string string_supp;;
print_string "\n";;

let liste = list_of_string exemple1;;
let () = List.iter (printf "%s ") liste;;
print_string "\n";;


let balanceEx1 = balance exemple1;;
let liste = list_of_string balanceEx1;;
let () = List.iter (printf "%s ") liste;;
print_string "\n";;


print_int (hauteur_stb exemple1);;
print_string "\n";;
print_int (hauteur_stb balanceEx1);;
print_string "\n";;

let res = gain_balance 100 11;;

print_string "Moyenne sans balance :  ";;
print_int res.(0);;
print_string "\n";;
print_string "Moyenne avec balance :  ";;
print_int res.(1);;
print_string "\n";;
print_string "Maximum sans balance :  ";;
print_int res.(2);;
print_string "\n";;
print_string "Maximum avec balance :  ";;
print_int res.(3);;
print_string "\n";;
print_string "Minimum sans balance :  ";;
print_int res.(5);;
print_string "\n";;
print_string "Minimum avec balance :  ";;
print_int res.(5);;
print_string "\n";;


(* ------ FIN TEST ----- *)
