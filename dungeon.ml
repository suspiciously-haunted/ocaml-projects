type direction = North | South | East | West
type item = Sword | Shield | Potion | Key
type enemy = { name: string; hp: int; attack: int }
type room = {
  description: string;
  exits: (direction * int) list;  (* direction -> room_id *)
  items: item list;
  enemy: enemy option;            (* None = safe room *)
}
let string_of_direction = function
  |North -> "North"
  |South -> "South"
  |East -> "East"
  |West -> "West";;
let string_of_item = function
  |Sword -> "Sword"
  |Shield -> "Shield"
  |Potion -> "Potion"
  |Key -> "Key";;
let string_of_enemy = function
  | None -> "Safe Room"
  | Some e -> e.name

let rec aux_exits exits = match exits with
  | [] -> ""
  | (d, i) :: e -> "  " ^ string_of_direction d ^ " -> " ^ string_of_int i ^ "\n" ^ aux_exits e

let rec aux_items items = match items with
  | [] -> "  None\n"
  | i :: is -> "  " ^ string_of_item i ^ "\n" ^ aux_items is
  
let describe_room room =
  "\n=== " ^ room.description ^ " ===\n" ^
  "Exits:\n" ^ aux_exits room.exits ^
  "Items:\n" ^ aux_items room.items ^
  "Enemy: " ^ string_of_enemy room.enemy ^ "\n" ^
  "--------------------\n";;
    
let find_exit direction room =
  let rec aux = function
    | [] -> None
    | (dir, id) :: rest -> if dir = direction then Some id else aux rest
  in aux room.exits
    
let move_player direction room = 
  let r = find_exit direction room in
  match r with
  |Some id -> Ok id
  |None -> Error "No exit that way";;

let find_item item room =
  let rec aux = function
    | [] -> None
    | i :: rest -> if i = item then Some i else aux rest
  in aux room.items;;

let remove item room =
  let rec remove_aux acc = function
    | [] -> List.rev acc
    | x :: xs -> if x = item then List.rev acc @ xs
        else remove_aux (x :: acc) xs
  in { room with items = remove_aux [] room.items };;

let pick_up item room =
  match find_item item room with
  | None -> None
  | Some i -> Some (i, remove item room);;

type 'a stack = Empty | Push of 'a * 'a stack ;;

let push y xs = Push(y, xs) ;;
let pop = function
  |Empty -> None
  |Push(x, xs) -> Some (x, xs);;
let peek = function
  |Empty -> None
  |Push(x, xs) -> Some x ;;
let to_list stack = 
  let rec aux acc = function
    |Empty -> List.rev acc
    |Push(x, xs) -> aux (x::acc) xs in
  aux [] stack;;

type game_state = {
  current_room : int;
  inventory    : item stack;
  history      : int stack;   (* stack of previous room ids *)
}

let undo_move state =
  match pop state.history with
  | None -> None
  | Some (prev_room, past) ->
      Some { state with current_room = prev_room; history = past };;

type 'a queue = 
  |Q of ('a list * 'a list);;
let empty = Q([], [])
let is_empty = function
  |Q([], []) -> true
  |_-> false;;
    
let enqueue x = function
  | Q (front, back) -> Q (front, x :: back)

let dequeue = function
  | Q ([], []) -> None
  | Q ([], back) -> 
      let front = List.rev back in
      Some (List.hd front, Q (List.tl front, []))
  | Q (x :: front, back) -> Some (x, Q (front, back));;
  
let run_combat enemy_q hp =
  let rec aux log q remaining_hp =
    if remaining_hp <= 0 then List.rev ("You died." :: log)
    else match dequeue q with
      | None -> List.rev ("You won!" :: log)
      | Some (e, rest_q) ->
          let new_hp = remaining_hp - e.attack in
          let msg = e.name ^ " hits you for " ^ string_of_int e.attack in
          let next_q = if e.hp > 0 then enqueue e rest_q else rest_q in
          aux (msg :: log) next_q new_hp
  in aux [] enemy_q hp;;
    
let rec take n lst = match lst with
  |[]->[]
  |x::xs-> if n>1 then x::(take (n-1) xs) else [x];;
let rec drop n lst = match lst with
  |[]->[]
  |x::xs -> if n>0 then (drop (n-1) xs) else (x::xs);;
  
let rec merge cmp xs ys = match xs, ys with
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs', y :: ys' ->
      if cmp x y <= 0 then x :: merge cmp xs' ys
      else y :: merge cmp xs ys'
             
let rec mergesort cmp = function
  | [] -> []
  | [x] -> [x]
  | lst ->
      let n = List.length lst / 2 in
      merge cmp (mergesort cmp (take n lst)) (mergesort cmp (drop n lst)) 
             
let item_priority = function
  |Potion -> 1
  |Key -> 2
  |Sword -> 3
  |Shield -> 4;;

let sort_inventory stack =
  let lst = to_list stack in
  let sorted = mergesort (fun a b -> compare (item_priority a) (item_priority b)) lst in
  sorted;;

let sort_enemies_by_hp enemies =
  mergesort (fun a b -> compare a.hp b.hp) enemies;;

let rec has_item item = function
    Empty->false
  |Push (x, xs) -> (x=item)||(has_item item xs);;
  
let rec has_exit dir room = 
  let id = find_exit dir room in
  match id with 
  |None -> false
  |Some _ -> true ;;

    
let enemy_alive threshold enemy = enemy.hp > threshold

let has_key    = has_item Key
let has_sword  = has_item Sword
let has_potion = has_item Potion

let can_go_north = has_exit North
let is_dead      = enemy_alive 0;;

let apply_if condition action x =
  if condition x then action x else x
    
type 'a seq = Nil | Cons of 'a * (unit -> 'a seq);;

let rec take k seq = match k, seq with
  |0, _ -> []
  |k, Nil -> []
  |k, Cons(a,af) -> a::(take (k-1) (af()));;

let rec map_lazy f seq = match seq with
  |Nil -> Nil
  |Cons(x, xf) -> Cons(f x, fun () -> map_lazy f (xf())) ;;

let rec filter_lazy p seq = match seq with
  | Nil -> Nil
  | Cons (x, xf) ->
      if p x then Cons (x, fun () -> filter_lazy p (xf ()))
      else filter_lazy p (xf ())

type command = 
  | Go of direction
  | PickUp of item
  | Undo
  | Inventory
  | Quit
  | Unknown

let parse_command = function
  | "go north" -> Go North
  | "go south" -> Go South
  | "go east"  -> Go East
  | "go west"  -> Go West
  | "pick up sword"  -> PickUp Sword
  | "pick up potion" -> PickUp Potion
  | "pick up key"    -> PickUp Key
  | "inventory" -> Inventory
  | "undo"      -> Undo
  | "quit"      -> Quit
  | _           -> Unknown;;
let find_room id dungeon =
  List.assoc_opt id dungeon;;

let rec room_stream id =
  let next_id = abs ((id * 1664525 + 1013904223) mod 1000000007) in
  let e = match next_id mod 4 with
    | 0 -> Some { name = "Goblin"; hp = 5; attack = 2 }
    | 1 -> Some { name = "Troll"; hp = 10; attack = 4 }
    | _ -> None in
  let exitlist = 
    [Some (North, next_id);
     (if id mod 2 = 0 then Some (East, next_id) else None);
     (if id mod 3 = 0 then Some (West, next_id) else None);
     (if id mod 5 = 0 then Some (South, next_id) else None)]
    |> List.filter_map Fun.id in
  let itemlist = match next_id mod 6 with
    | 0 -> [Potion]
    | 2 -> [Sword]
    | 4 -> [Key]
    | _ -> [] in
  let room = { description = "Room " ^ string_of_int id;
               exits = exitlist;
               items = itemlist;
               enemy = e } in 
  Cons ((id, room), fun () -> room_stream next_id)
    
let generate_dungeon seed size = take size (room_stream seed)

let dungeon = generate_dungeon 42 20

let init_state = {
  current_room = fst (List.hd dungeon);
  inventory = Empty;
  history = Empty;
}

let rec game_loop state dungeon =
  let room = find_room state.current_room dungeon in
  match room with
  | None -> print_endline "Error: room not found"
  | Some r ->
      print_endline (describe_room r);
      print_string "> ";
      let cmd = parse_command (input_line stdin) in
      match cmd with
      | Go dir ->
          (match move_player dir r with
           | Error msg -> print_endline msg; game_loop state dungeon
           | Ok new_id ->
               let new_history = push state.current_room state.history in
               game_loop { state with current_room = new_id; history = new_history } dungeon)
      | PickUp item ->
          (match pick_up item r with
           | None -> print_endline "Item not here."; game_loop state dungeon
           | Some (i, _) ->
               print_endline ("Picked up " ^ string_of_item i);
               game_loop { state with inventory = push i state.inventory } dungeon)
      | Undo ->
          (match undo_move state with
           | None -> print_endline "Nothing to undo."; game_loop state dungeon
           | Some s -> game_loop s dungeon)
      | Inventory ->
          List.iter (fun i -> print_endline (string_of_item i)) (sort_inventory state.inventory);
          game_loop state dungeon
      | Quit -> print_endline "Goodbye!"
      | Unknown -> print_endline "Unknown command."; game_loop state dungeon

let () = game_loop init_state dungeon;;


      
















