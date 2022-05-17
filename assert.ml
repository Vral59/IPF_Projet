open Main;;
open Printf;;
Random.self_init();;

(* Test Question 1 *)

let exemple1 = Noeud(Feuille("G",1),7,Noeud(Feuille("ATT",3),6,Noeud(Feuille("A",1),3,Feuille("CA",2))));;
let exemple2 = Noeud(Feuille("V",1),2,Feuille("B",1));;
let exemple3 = Feuille("petitmot",8);;

let () = assert (concat exemple1 exemple2 = Noeud(exemple1,9,exemple2));;
let () = assert (word "petitmot" = exemple3);;

(* Test Question 2 *)

let () = assert (char_at exemple1 0 = 'G');;
let () = assert (char_at exemple1 6 = 'A');;

(* Test Question 3 *)

let () = assert (sub_string exemple1 2 5 = Noeud(Feuille("TT",2),5,Noeud(Feuille("A",1),3,Feuille("CA",2))));;
let () = assert (sub_string exemple1 0 7 = exemple1);;
let () = assert (sub_string exemple3 2 3 = Feuille("tit",3));;

(* Test Question 4 *)

let () = assert (cost exemple3 = 8 );;
let () = assert (cost exemple1 = 23 );;


(* Test Question 5 *)

let stb = random_string 6;;
let () = assert (height_stb stb = 6);;

let stb = random_string 1;;
let () = assert (height_stb stb = 1);;

(* Test Question 6 *)

let () = assert (list_of_string exemple1 = ["G";"ATT";"A";"CA"]);;
let ()  = assert (list_of_string exemple3 = ["petitmot"]);;

(* Test Question 7 *)

let () = assert(balance exemple1 = Noeud( Noeud (Feuille("G",1),4,Feuille("ATT",3)),7,Noeud(Feuille("A",1),3,Feuille("CA",2))));;
let () = assert(balance exemple3 = exemple3)
(* Test Question 8 *)

(*
let res = gain_balance 100 10;;

let () = List.iter (printf "%d ") res
*)
