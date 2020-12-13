val fitness : Store.word_store -> Word.t -> int

val head : ?level:int -> Store.word_store -> Word.t option

val letter_score : Letter.letter -> int

val word_score : Word.word -> int