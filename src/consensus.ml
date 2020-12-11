open Word
open Constants

(* ignoring unused variables - to be removed *)
let _ = ignore genesis

(* end ignoring unused variables - to be removed *)

let word_score { word; _ } : int =
  List.length word

let fitness st word =
  (* ignoring unused variables - to be removed *)
  ignore st ;
  ignore word ;
  ignore word_score ;
  (* end ignoring unused variables - to be removed *)
  (* TODO *)
  assert false

(* TODO *)
let rec get_best words best : word =
  match words with
  | [] -> best
  | x::xs -> if (word_score x) > (word_score best) then
             get_best xs x else
             get_best xs best

let rec search_current_words wordList level acc : word list =
  match wordList with
  | [] -> acc
  | x::xs -> if x.level = level then
              search_current_words xs level (x::acc) else
              search_current_words xs level acc

let head ?level (st : Store.word_store) =
match level with
| None -> None
| Some l -> if (l = 0) then Some genesis_word else
   let wordSeq = Hashtbl.to_seq_values st.words_table in
    match search_current_words (List.of_seq wordSeq) l [] with
    | [] -> None
    | first::current_words -> Some (get_best current_words first)

  (* TODO *)
