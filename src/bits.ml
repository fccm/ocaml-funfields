(* A functional bit field module *)
(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)

type t = int list

let zero = []

(* all bits set *)
let all = (lnot 0)

(* number of bits in ocaml ints *)
let nb = Sys.word_size - 1

let of_int d = d::[]
let to_int = function d::[] -> d | [] -> 0
  | _ -> failwith "Bits.to_int: too many bits to fit in an int"

let subset_bits bs1 bs2 =
  let rec aux = function
  | (b1::bs1), (b2::bs2) ->
      ((b1 land b2) = b1) && aux (bs1, bs2)
  | [], [] -> true
  | _ -> false
  in
  aux (bs1, bs2)

let rec set_bit b i =
  match b, i < nb with
  | (b0 :: bs), true -> (b0 lor (1 lsl i)) :: bs
  | (b0 :: bs), false -> b0 :: (set_bit bs (i-nb))
  | [], true -> (1 lsl i) :: []
  | [], false -> 0 :: (set_bit [] (i-nb))

let rec is_bit_set b i =
  match b with
  | [] -> false
  | b0 :: bs ->
      match i < nb with
      | true -> ((b0 lsr i) land 1) = 1
      | false -> is_bit_set bs (i-nb)

let rec unset_bit b i =
  match b, i < nb with
  | [], _ -> []
  | (b0 :: bs), false -> b0 :: (unset_bit bs (i-nb))
  | (b0 :: bs), true ->
      match (b0 land (lnot (1 lsl i))) with
      | 0 -> bs
      | b -> b :: bs

let rec rev_rem = function
  | 0 :: bs -> rev_rem bs
  | bs -> bs

(* remove higher words if null *)
let rem_null bs =
  List.rev (rev_rem (List.rev bs))

let unset_bit b i =
  let r = unset_bit b i in
  (rem_null r)

let set_bits b bits =
  List.fold_left set_bit b bits

let are_bits_set b bits =
  List.for_all (is_bit_set b) bits

let unset_bits b bits =
  List.fold_left unset_bit b bits

let rec eq b1 b2 =
  match (b1, b2) with
  | (b1::bs1), (b2::bs2) ->
      b1 = b2 && eq bs1 bs2
  | ([], []) -> true
  | ([], b::bs)
  | (b::bs, []) ->
      b = 0 && eq [] bs

let rec bits_or b1 b2 =
  match (b1, b2) with
  | (b1::bs1), (b2::bs2) ->
      (b1 lor b2) :: bits_or bs1 bs2
  | ([], []) -> []
  | ([], bs)
  | (bs, []) -> bs

let rec bits_xor b1 b2 =
  match (b1, b2) with
  | (b1::bs1), (b2::bs2) ->
      (b1 lxor b2) :: bits_xor bs1 bs2
  | ([], []) -> []
  | ([], bs)
  | (bs, []) -> bs

let bits_xor b1 b2 =
  let b = bits_xor b1 b2 in
  (rem_null b)

let rec bits_and b1 b2 =
  match (b1, b2) with
  | (b1::bs1), (b2::bs2) ->
      (b1 land b2) :: bits_and bs1 bs2
  | ([], _)
  | (_, []) -> []

let bits_and b1 b2 =
  let b = bits_and b1 b2 in
  (rem_null b)

let iteri f b =
  let n = List.length b in
  let n2 = n * nb in
  for i = 0 to pred n2 do
    f i (is_bit_set b i)
  done

let mapi f b =
  let n = List.length b in
  let n2 = n * nb in
  let rec aux i r =
    if i = n2 then r else
    let r =
      if f i (is_bit_set b i)
      then set_bit r i
      else r
    in
    aux (i+1) r
  in
  aux 0 zero

let invert b len =
  let rec set d n =
    if n <= 0 then d
    else set ((d lsl 1) lor 1) (n-1)
  in
  let rec aux n = function
  | [] -> []
  | b::bs ->
      if n > nb then (lnot b) :: aux (n - nb) bs
      else (b lxor (set 0 n)) :: bs
  in
  aux len b

let rec _bits acc d =
  if d = 0 then acc
  else _bits ((d land 1)::acc) (d lsr 1)

let bits d = _bits [] d

let string_of_int_bits d =
  let b = bits d in
  let n = List.length b in
  let pad = String.make (nb - n) '0' in
  let bs = List.map string_of_int b in
  pad ^ (String.concat "" bs)

let string_of_bits d =
  String.concat "_"
    (List.rev_map string_of_int_bits d)

let to_string = string_of_bits ;;

let int_bits_of_string s =
  let n = String.length s in
  let rec aux acc i j =
    if i < 0 then acc else
    match String.unsafe_get s i with
    | '1' -> aux (j::acc) (i-1) (j+1)
    | '0' -> aux acc (i-1) (j+1)
    | '_' -> aux acc (i-1) j
    | _ -> invalid_arg "bits_of_string"
  in
  aux [] (n-1) 0

let of_string s =
  let b = int_bits_of_string s in
  set_bits zero b

let bits_of_string = of_string ;;

