Random.self_init ();;

let secret_list = ["aisle"; "pleat"; "early"; "hello"];;

let secret = List.nth secret_list (Random.int (List.length secret_list));;

let result = Array.make (String.length secret) '_';;

let listofstr s = List.map (String.make 1) (List.of_seq (String.to_seq s));; 

let strtochar s = List.init (String.length s) (String.get s)

let listofchar s = List.map (Char.lowercase_ascii) (List.of_seq (String.to_seq s));; 
    
let hangman c_str =
  let rec hangman_rec secret result c_list = 
    match c_list with
    |[]->result
    |c::cs->
        for i=0 to ((String.length secret)-1) do
          if c=secret.[i] then
            Array.set result i c 
        done;
        hangman_rec secret result cs
  in
  hangman_rec secret result (listofchar c_str);;
          
hangman "hello"
