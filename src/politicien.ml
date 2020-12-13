(* open Messages *)
open Word
open Crypto

type politician = { sk : Crypto.sk; pk : Crypto.pk; score: int } [@@deriving yojson, show]

type state = {
  politician : politician;
  word_store : Store.word_store;
  letter_store : Store.letter_store;
  next_words : word list;
}

let bouchon () =
  true

let rec create_word paquet mot taille =
  
  if List.length mot >= taille then begin
   (Log.log_info "--------Mot créé de taille %d!\n" taille);
    mot;
    end
  else
    let element_tire = List.nth paquet (Random.int (List.length paquet)) in
    create_word (List.find_all (fun x -> x != element_tire) paquet) (element_tire :: mot) taille
;;

(*Retourne la liste des lettres ayant une periode supérieure à periode*)
let rec f_bonnes leetterpool headhash res = 
  match leetterpool with
  | [] -> res
  | x::xs -> if x.Letter.head = headhash 
    then f_bonnes xs headhash (x::res)
    else f_bonnes xs headhash res
;;

let rec only_one_per_auth leetterpool auteurs sortie =
  match leetterpool with
  | [] -> sortie
  | x::xs -> if List.mem x.Letter.author auteurs
  then only_one_per_auth xs auteurs sortie
  else only_one_per_auth xs ((x.Letter.author)::auteurs) (x::sortie)

(*Transforme une liste de letters en liste de chars*) 
let rec charlist_fro_letters pool acc = 
  match pool with  
  | x::xs -> charlist_fro_letters xs (x.Letter.letter::acc)
  | [] -> acc 
;;

let create_valid_word leetterpool periode = 
  let bonnes = only_one_per_auth (f_bonnes leetterpool periode []) [] [] in
  Log.log_info "%d lettres disponibles\n" (List.length bonnes);
  if List.length bonnes > 3 then
  let rec aux pool nb = 
    match nb with
    0 -> None
    | _ -> let wordlength = (Random.int ( (List.length pool) - 3)) + 3 in
            Log.log_info "Tentative de creation de mot avec %d lettres\n" wordlength;
          let tirage = (create_word pool [] wordlength) in
          let tirage_string = (String.of_seq (List.to_seq (charlist_fro_letters tirage []))) in 
      if List.mem tirage_string (Client_utils.list_of_dict ("./dict/dict_100000_1_10.txt")) (* Si le mot existe dans le dict*) (*Verfier tous les dictionnaires *)
      then Some tirage  (*  Alors on le rend*)
      else aux pool (nb-1) (*Sinon on itère avec nb -1 *)  
    in 
  aux bonnes 60
  else None



let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  Word.make ~word:letters ~level ~pk:politician.pk ~sk:politician.sk ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = hash head in
  make_word_on_hash level letters politician head_hash

let send_new_word level st =

  Option.iter
    (fun head ->
      (* Create new random word *)
      let newword = create_valid_word (List.of_seq (Hashtbl.to_seq_values st.letter_store.Store.letters_table)) (hash (Word.to_bigstring head)) (*ici fonction de creation*) in
      match newword with
      | None -> Log.log_info "Aucun mot n'a été trouvé avec les lettres actuelles.\n";()
      | Some w-> let word =
              make_word_on_blockletters
              level
              w
              st.politician
              (Word.to_bigstring head) (*Afficher head ici *)
              in
              Store.add_word st.word_store word;
              (* Send letter *)
              let message = Messages.Inject_word word in
              Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) st.word_store)
  (* generate a word above the blockchain head, with the adequate letters *)
  (* then send it to the server *)

let run ?(max_iter = 0) () =
  (* ignoring unused variables - to be removed *)
  (*ignore max_iter ;*)

  (* end ignoring unused variables - to be removed *)

  (* Generate public/secret keys *)
  Log.log_info "Generation de la clef prive/publique\n" ;
   let (pk, sk) = Crypto.genkeys () in
   let politicien_score = ref 0 in

  (* Get initial wordpool *)
  Log.log_info "Recuperation du wordpool\n" ;
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;

  (*
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in
*)
  let rec wait_for_wordpool (): Messages.wordpool =
    match Client_utils.receive () with
      | Messages.Full_wordpool wordpool -> wordpool
      | _ -> wait_for_wordpool ()
  in    
  let wordpool = wait_for_wordpool () in



  
(* 
let rec get_my_words wordpool res : word list =
  match wordpool with
  | [] -> res
  | x::xs -> if x.politician != pk
            then get_latest_word xs res
            else x::res

let rec get_latest_word wordlist lateset: word =
  match wordlist with
  | [] -> newest
  | w::ws -> if w.level > lateset.level
            then get_latest_word ws w
            else get_latest_word ws newest *)

  (* Generate initial blocktree *)
  Log.log_info "Generation du blockarbre\n" ;
  let storeword = Store.init_words () in
  Store.add_words storeword wordpool.words ;

  (* Get initial letterpool *)
  Log.log_info "Recupere le letter pool\n" ;
  let getpool = Messages.Get_full_letterpool in
  Client_utils.send_some getpool ;


  let rec wait_for_letterpool (): Messages.letterpool =
    match Client_utils.receive () with
      | Messages.Full_letterpool letterpool -> letterpool
      | Messages.Diff_letterpool sinceletterpool -> sinceletterpool.letterpool
      | _ -> wait_for_letterpool ()
  in    
  let letterpool = wait_for_letterpool () in

  (* Generate initial letterpool *)
  let storeletter = Store.init_letters () in
  Store.add_letters storeletter letterpool.letters;
  let poli : politician = { sk=sk ; pk=pk; score=0 } in
  let st : state = { politician = poli; word_store = storeword ; letter_store = storeletter; next_words = [] } in
  (* Create and send first word *)
  send_new_word wordpool.current_period st ;

  Client_utils.send_some Messages.Listen ;

  let get_latest_word wordpool : word = 
      let rec aux wpool best =
        match wpool with  
        |[] -> best  
        |x::xs -> if ((best.Word.politician <> pk) && (x.Word.politician = pk))
        then aux xs x 
        else aux xs best in 
      let rec aux2 wpool best = 
        match wpool with  
        | [] -> best  
        | x::xs -> if ((x.Word.politician = pk) && (x.Word.level > best.Word.level) )
          then  aux2 xs x 
          else aux2 xs best in      
      aux2 wordpool (aux wordpool (List.nth wordpool 0))   
    in
(*
  let liste2 liste = 
    let rec aux liste acc =
      match liste with  
      | [] -> acc 
      | ()

*)

  (*  main loop *)
  (*failwith ("à programmer" ^ __LOC__)*)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then Log.log_success "This is the end... du politicien, c'est important" 
    else (
      
      ( match Client_utils.receive () with
      (* le politicien doit savoir s'il y a une nouvelle head parce qu'il devra changer ses lettres// Pas sur *)
      (*| Messages.Inject_word w ->
          Store.add_word store w ;
          Option.iter
            (fun head ->
              if head = w then (
                Log.log_info "Head updated to incoming word %a@.\n" Word.pp w 
                
                (*send_new_letter sk pk !level store )*)
                (* Mon propre mot a peut-etre ete ajouté, je dois calculer mon score! *)
              else Log.log_info "incoming word %a not a new head@.\n" Word.pp w)
            (Consensus.head ~level:(!level - 1) store)*)
      | Messages.Next_turn _ -> (* Attention: Detail des lettres qui doublent *)
        Client_utils.send_some (Messages.Get_letterpool_since !level);
        let lpool = wait_for_letterpool () in
        Store.add_letters st.letter_store lpool.letters;
        level := lpool.Messages.current_period ;
        send_new_word !level st;
        (*Mettre a jour le word store -> maybe un get wordpool*)
        Client_utils.send_some (Messages.Get_full_wordpool);
        let wpool = wait_for_wordpool () in
        Store.add_words st.word_store wpool.words;
        if (List.length (List.map snd wpool.words)) <> 0 
        then
        let w = get_latest_word (List.map snd wpool.words) in
        Option.iter
            (fun head ->
              if head = w 
              then  (politicien_score := (!politicien_score + (Consensus.word_score head));
              Log.log_success "Score de du politicien : %d\n" !politicien_score ;))
            (Consensus.head ~level:(!level - 1) st.word_store)
      | Messages.Inject_letter _ | _ -> () );
      loop (max_iter - 1) )
  in
  loop max_iter;
  Log.log_success "Score final du politicien : %d\n" !politicien_score

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(20) ()
  in
  main

