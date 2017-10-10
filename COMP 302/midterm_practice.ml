(* Part 1: define a function that computes the parity of a list of
   booleans.

   What we want is a function even_parity such as even_parity l
   returns a boolean that combined with the elements of l the
   number of 1’s (true) is even.
   This is a simple but important algorithm in data communication, its
   purpose is to detect errors (one bit flips). It is very easy to
   implement in circuit but it is not very reliable.
 *)

let rec even_parity = function
  | [] -> false
  | true::xs -> not (even_parity xs)
  | false::xs -> even_parity xs

(* This version is very natural but it is not tail recursive. Let's
   make the tail recursive version. *)

let even_parity_tr l =
  let rec parity p = function
    | [] -> p
    | p'::xs -> parity (p<>p') xs
  in
  parity false l

(* Part 2: Now prove that both functions are equivalent. You will need to use facts about the <> (XOR) operation *)

let ex_1 = [true]
let ex_2 = [true; true]
let ex_3 = [false; true; true; false; true]

let ex1 = even_parity ex_1
let ex2 = even_parity ex_2
let ex3 = even_parity ex_3

let t_1 = even_parity ex_1 = even_parity_tr ex_1
let t_2 = even_parity ex_2 = even_parity_tr ex_2
let t_3 = even_parity ex_3 = even_parity_tr ex_3

(* High order - croupier for simplified roulette *)

(* We have a simplified roulette, where we have only two colours that
   we can bet but if zero comes out, everyone loses *)

type colour = Red | Black        (* The two colours we can bet on *)

type result = colour option      (* The result of a run, it could be one of the colours or no colour if zero came up *)

type bet = int * colour          (* The bet amount and to what colour *)


(* It is simple to see who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(*
Solve all these questions without using recursion or pattern
matching on lists, but instead just use the HO functions we saw
in class.
 *)

 let all_bets = [(2,Black : bet); (3,Red : bet); (4,Black : bet)]

 let res1 = (Some Black : result)

(* Q1:  given a list of bets compute the results *)

  let results l res =
    List.map (fun x -> compute x res) l

  let ex1 = results all_bets res1



(* Q2: given a list of bets and a result compute a list of winning bets *)
  let winners l res=
    List.filter (fun x -> ((compute x res) > 0)) l

  let ex2 = winners all_bets res1

(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)
  let payback l res =
    List.fold_left (fun acc x -> acc + (compute x res)) 0 l

  let ex3 = payback all_bets res1

(* Q4: given a list of bets and a result compute if everyone won *)
  let everyone_wins l res =
    List.for_all (fun x -> (compute x res) > 0) l

  let ex4 = everyone_wins all_bets res1

(* Q5: given a list of bets and a result compute if someone won *)

  let someone_won l res =
    List.exists (fun x -> compute x res > 0) l

  let ex5 = someone_won all_bets res1


(* Q6: given a list of bets return the highest winning *)

  let highest_winner l res =
    List.fold_left (fun acc x -> max acc (compute x res)) 0 l

  let ex6 = highest_winner all_bets res1

(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)

  let balance l res =
    List.fold_left (fun acc x -> if (compute x res) > 0 then acc
     else
      match x with
      | y, _ -> acc + y) 0 l

  let ex7 = balance all_bets res1

(* Another implementation *)
  let balance2 l res =
    List.fold_left (fun acc x ->
      match x with (n1,_) -> acc + n1) 0 (List.filter (fun x -> compute x res = 0) l)

  let ex7_2 = balance2 all_bets res1

(* Ninja level  *)

(* Q8: Can you sort the results by the amount they made? *)

  let sortme l res =
    List.sort compare (results l res)

  let ex8 = sortme all_bets res1
