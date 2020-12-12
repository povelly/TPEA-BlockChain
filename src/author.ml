open Messages
open Letter
open Crypto

type author = {
  letter_bag : char list;
  score : int;
}

let make_letter_on_hash sk pk level head_hash letter : letter =
  Letter.make ~letter ~head:head_hash ~level ~pk ~sk

let make_letter_on_block sk pk level block letter : letter =
  let head_hash = hash block in
  make_letter_on_hash sk pk level head_hash letter

(* let random_char () = Random.int (122 - 65) + 65 |> Char.chr *)
let random_char () = Random.int 26 + 97 |> Char.chr

let get_random_char leetterbag =
  let n = Random.int (Array.length leetterbag) in
  Array.get leetterbag n;;

  (* 
  let get_random_char leetterbag =
  match (Array.length leetterbag) with 
  |  0 -> assert false
  | 1 -> Array.get leetterbag 0
  | _ -> let n = Random.int (Array.length leetterbag) in
      Array.get leetterbag n;;
  *)

let send_new_letter sk pk level store =
  (* Get blockchain head *)
  
  Option.iter
    (fun head ->
      (* Create new random letter *)
      let letter =
        make_letter_on_block
          sk
          pk
          level
          (Word.to_bigstring head) 
          (random_char ())  (*get_random_char !letter_bag  *) (* +suppression dans la letter bag *)
      in
      (* Send letter *)
      let message = Messages.Inject_letter letter in
      Client_utils.send_some message)
    (Consensus.head ~level:(level - 1) store)

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in

  (* Register to the server *)
  let reg_msg = Messages.Register pk in
  Client_utils.send_some reg_msg ;

  (* drop provided letter_bag *)
  (*
  ( match Client_utils.receive () with
  | Messages.Letters_bag _ -> () (*l -> letter_bag := l *)
  | _ -> assert false ) ; *)

  let rec wait_for_letterbag (): char list =
    match Client_utils.receive () with
      | Messages.Letters_bag letterbag -> letterbag
      | _ -> wait_for_letterbag ()
  in    
  let letterbag = wait_for_letterbag () in

  ignore letterbag;

  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;

  (*
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false (*  Ici gerer correctement la réponse*)
  in
  *)

  let rec wait_for_wordpool (): Messages.wordpool =
    match Client_utils.receive () with
      | Messages.Full_wordpool wordpool -> wordpool
      | _ -> wait_for_wordpool ()
  in    
  let wordpool = wait_for_wordpool () in

  let auth : author = { letter_bag = letterbag; score=0 } in

  ignore auth; 

  (* Generate initial blocktree *)
  let store = Store.init_words () in
  Store.add_words store wordpool.words ;
  (* Create and send first letter *)
  send_new_letter sk pk wordpool.current_period store ;

  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen ;

  (* start main loop *)
  let level = ref wordpool.current_period in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
          Store.add_word store w ;
          Option.iter
            (fun head ->
              if head = w then (
                Log.log_success "Head updated to incoming word %a@." Word.pp w ;
                (* Incrémenter le score ici *)
                (*send_new_letter sk pk !level store *))
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) store)
      | Messages.Next_turn p -> level := p;  send_new_letter sk pk !level store;  (* 2 étapes :*)
      | Messages.Inject_letter _ | _ -> () ) ;
      loop (max_iter - 1) )
  in
  loop max_iter

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) () (* Ici nombre de tours avant afficher les resultats *)
  in
  main
