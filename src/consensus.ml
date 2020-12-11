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
  
  
(* ??????????????? sert probablement si on n'utilise plus le serveur
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
  | [] -> Log.log_info "WORD CHOSEN ---------------- %a blablablabla@." Word.pp best;best
  | x::xs -> if (word_score x) > (word_score best) then
             get_best xs x else
             get_best xs best

(* gives a list of words from the given period *)
let rec search_current_words wordList level acc : word list =
  match wordList with
  | [] -> acc
  | x::xs -> if x.level = level then
              search_current_words xs level (x::acc) else
              search_current_words xs level acc

(* returns the word (option) that should be chooser as head *)
let head ?level (st : Store.word_store) =
match level with
| None -> None
| Some l -> if (l = 0) then Some genesis_word else
   let wordSeq = Hashtbl.to_seq_values st.words_table in
    match search_current_words (List.of_seq wordSeq) l [] with
    | [] -> Some genesis_word
    | first::current_words -> Some (get_best current_words first)