open Word
open Constants

(* gives the scrabble score of a letter *)
let letter_score l : int =
  match l.Letter.letter with
      'k' | 'w' | 'x' | 'y' | 'z' -> 10
    | 'j' | 'q' -> 8
    | 'f' | 'h' | 'v' -> 4
    | 'b' | 'c' | 'p' -> 3
    | 'd' | 'm' | 'g' -> 2
    | _ -> 1;;

(* gives the total score of a letter list *)
let rec word_score_aux llist score : int =
 match llist with
      | [] -> score
      | l::tail -> word_score_aux tail (score + (letter_score l));;

let word_score { word; _ } : int =
  word_score_aux word 0;;
  
  
(*
PoW : fitness
La règle de consensus consiste à choisir la chaîne de blocs
valides la plus longue.
Implicitement, il s’agit de celle qui a nécessité le plus de
travail.
PoS : "Fitness ou FCR (Fork Choice Rule)
Calcul un score pour un bloc. Permet de choisir le bloc
gagnant lors d’une divergence."
*)
let fitness st word =
  (* ignoring unused variables - to be removed *)
  ignore st ;
  ignore word ;
  ignore word_score ;
  (* end ignoring unused variables - to be removed *)
  (* TODO *)
  assert false

(*returns the highest scoring word from a word list *)
let rec get_best words best : word =
  match words with
  | [] -> best
  | x::xs -> if (word_score x) > (word_score best) then
             get_best xs x else
             get_best xs best

(* gives a list of words from the given period *)
let rec same_period_words wordList level res : word list =
  match wordList with
  | [] -> res
  | x::xs -> if x.level = level then
              same_period_words xs level (x::res) else
              same_period_words xs level res

(* gives the highest period word *)
let rec get_newest_word wordlist newest: word =
  match wordlist with
  | [] -> newest
  | w::ws -> if w.level > newest.level then get_newest_word ws w else get_newest_word ws newest


(* returns the word that should be choosen as head
la head est choisie en choisissant le mot de meilleur score
parmi les mots les plus récents et l'actuelle head
*)
let head ?level (st : Store.word_store) =
match level with
| None -> None
| Some l -> if (l = 0) then Some genesis_word else
   let wordlist = List.of_seq (Hashtbl.to_seq_values st.words_table) in    
    match wordlist with
    | [] -> Log.log_info "Aucun mot dans le store\n"; Some genesis_word (* s'il n'y a pas de mot depuis le début, le head est toujours genesisword *)
    | x::xs -> let newest = get_newest_word xs x in
               if newest.level = 0
                then Some genesis_word
                else  (* la  listes des mots de la periode la plus élevée concaténée au head référencé par ces mots *)
                  let possible_heads = (Hashtbl.find st.words_table newest.head)::(same_period_words wordlist newest.level []) in
                  let current_head = (get_best possible_heads newest) in
                  Some current_head