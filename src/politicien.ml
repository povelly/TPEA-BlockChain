(* open Messages *)
open Word
open Crypto

type politician = { sk : Crypto.sk; pk : Crypto.pk } [@@deriving yojson, show]

type state = {
  politician : politician;
  word_store : Store.word_store;
  letter_store : Store.letter_store;
  next_words : word list;
}

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
      Log.log_info "----------Poli MADE CONSENSUS AND CHOSE HEAD : %a@." Word.pp head ;
      let word =
        make_word_on_blockletters
          level
          (List.of_seq (Hashtbl.to_seq_values st.letter_store.letters_table))
          st.politician
          (Word.to_bigstring head)
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
  Log.log_warn "Generation de la clef prive/publique" ;
   let (pk, sk) = Crypto.genkeys () in
  (* Get initial wordpool *)
  Log.log_warn "Recuperation du wordpool" ;
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in

  (* Generate initial blocktree *)
  Log.log_warn "Generation du blockarbre" ;
  let storeword = Store.init_words () in
  Store.add_words storeword wordpool.words ;

  (* Get initial letterpool *)
  Log.log_warn "Recupere le letter pool" ;
  let getpool = Messages.Get_full_letterpool in
  Client_utils.send_some getpool ;
  let letterpool =
    match Client_utils.receive () with
    | Messages.Full_letterpool letterpool -> letterpool
    | _ -> assert false
  in
  

  (* Generate initial letterpool *)
  let storeletter = Store.init_letters () in
  Store.add_letters storeletter letterpool.letters;
  let poli : politician = { sk ; pk } in
  let st : state = { politician = poli; word_store = storeword ; letter_store = storeletter; next_words = [] } in
  (* Create and send first word *)
  send_new_word wordpool.current_period st ;



  (* start listening to server messages *)
  Log.log_warn "j ecoute" ;
  Client_utils.send_some Messages.Listen ;

  (*  main loop *)
  (*failwith ("à programmer" ^ __LOC__)*)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      (*| Messages.Inject_word w ->
          Store.add_word store w ;
          Option.iter
            (fun head ->
              if head = w then (
                Log.log_info "Head updated to incoming word %a@." Word.pp w ;
                send_new_letter sk pk !level store )
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) store)*)
      | Messages.Next_turn p -> level := p ; send_new_word !level st
      | Messages.Inject_letter _ | _ -> () ) ;
      loop (max_iter - 1) )
  in
  loop max_iter

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main
