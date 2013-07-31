(** A functional bit field module *)
(* Copyright (C) 2013 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it freely.
*)

type t
(** There is no limit in the number of bits that this bit field can contain *)

val zero : t

val set_bit : t -> int -> t

val set_bits : t -> int list -> t

val unset_bit : t -> int -> t

val unset_bits : t -> int list -> t

val subset_bits : t -> t -> bool
(** [subset_bits d1 d2] returns true if d1 is a subset of d2 *)

val is_bit_set : t -> int -> bool

val are_bits_set : t -> int list -> bool

val eq : t -> t -> bool

val bits_and : t -> t -> t

val bits_or : t -> t -> t

val bits_xor : t -> t -> t

(** {4 Debug} *)

val string_of_bits : t -> string

val bits_of_string : string -> t

val to_string : t -> string
(** alias for [string_of_bits] *)

val of_string : string -> t
(** alias for [bits_of_string] *)

