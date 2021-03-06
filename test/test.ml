let exit_code = ref 0

let test lbl should bits1 bits2 =
  let d1 = List.fold_left Bits.set_bit Bits.zero bits1 in
  let d2 = List.fold_left Bits.set_bit Bits.zero bits2 in
  let res = Bits.subset_bits d1 d2 in
  if res = should then begin
    Printf.printf "Test %s: OK\n" lbl;
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test %s: ERROR\n" lbl;
    Printf.eprintf "d1 is subset of d2: (%B = %B)\n" res should;
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits d1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits d2);
  end

let test1 () =
  test "subset1" true
    [52;   36;   17;16;7;  3;0]
    [52;48;36;18;17;16;7;6;3;0]

let test2 () =
  test "subset2" false
    [   50;23;36;   17;16;7;  3;0]
    [52;      36;18;17;16;7;6;3;0]

let test3 () =
  let b0 = Bits.set_bits Bits.zero [40; 21; 20; 11; 6; 2] in
  let b1 = Bits.set_bits Bits.zero [    21;     11; 6; 2] in
  let b2 = Bits.unset_bits b0 [40; 20] in
  let res = Bits.eq b1 b2 in
  if res = true then begin
    Printf.printf "Test unset: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test unset: ERROR\n";
    Printf.eprintf "bits0: %s\n" (Bits.string_of_bits b0);
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
  end

let test4 () =
  let b1 = Bits.set_bits Bits.zero [2;3;30;44;50;51] in
  let b2 = Bits.set_bits Bits.zero [2;3;33;48;55] in
  let b3 = Bits.bits_or b1 b2 in
  if Bits.eq b3 (Bits.of_string
    "0000001000110100010000000000100_1000000000000000000000000001100")
  then Printf.printf "Test OR: OK\n"
  else begin
    exit_code := 1;
    Printf.eprintf "Test OR: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.to_string b1);
    Printf.eprintf "bits2: %s\n" (Bits.to_string b2);
    Printf.eprintf "bits3: %s\n" (Bits.to_string b3);
  end

let test5 () =
  let b1 = Bits.set_bits Bits.zero [2;3;30;44;50;51] in
  let b2 = Bits.set_bits Bits.zero [2;3;33;48;55] in
  let b3 = Bits.bits_and b1 b2 in
  if Bits.eq b3 (Bits.of_string "0000000000000000000000000001100")
  then Printf.printf "Test AND: OK\n"
  else begin
    exit_code := 1;
    Printf.eprintf "Test AND: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.to_string b1);
    Printf.eprintf "bits2: %s\n" (Bits.to_string b2);
    Printf.eprintf "bits3: %s\n" (Bits.to_string b3);
  end

let test6 () =
  let b1 = Bits.of_string "1001_0010_1110_0001" in
  let b2 = Bits.of_string "0100_0010_0111_0100" in
  let b3 = Bits.of_string "1101_0000_1001_0101" in
  let b4 = Bits.bits_xor b1 b2 in
  if (Bits.eq b3 b4)
  then Printf.printf "Test XOR: OK\n"
  else begin
    exit_code := 1;
    Printf.eprintf "Test XOR: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.to_string b1);
    Printf.eprintf "bits2: %s\n" (Bits.to_string b2);
    Printf.eprintf "bits3: %s\n" (Bits.to_string b3);
    Printf.eprintf "bits4: %s\n" (Bits.to_string b4);
  end

let test7 () =
  let b1 = Bits.of_string "1001_0000__0110_0001" in
  let b2 = Bits.set_bits Bits.zero [0;5;6;12;15] in
  if (Bits.eq b1 b2)
  then Printf.printf "Test of_string: OK\n"
  else begin
    exit_code := 1;
    Printf.eprintf "Test of_string: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.to_string b1);
    Printf.eprintf "bits2: %s\n" (Bits.to_string b2);
  end

let test8 () =
  let b1 = Bits.of_string "1001_0000__0110_0001" in
  let s2 = Bits.to_string b1 in
  let b2 = Bits.of_string s2 in
  if (Bits.eq b1 b2)
  then Printf.printf "Test to_string: OK\n"
  else begin
    exit_code := 1;
    Printf.eprintf "Test of_string: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.to_string b1);
    Printf.eprintf "bits2: %s\n" (Bits.to_string b2);
  end

let test9 () =
  let b = Bits.set_bits Bits.zero [54; 48; 21; 13; 7] in
  let res1 = Bits.is_bit_set b 7 in
  let res2 = Bits.is_bit_set b 21 in
  let res3 = Bits.is_bit_set b 48 in
  let res4 = Bits.is_bit_set b 54 in
  let res5 = Bits.is_bit_set b 6 in
  let res6 = Bits.is_bit_set b 52 in
  if res1 = true && res2 = true && res3 = true && res4 = true
  && res5 = false && res6 = false then begin
    Printf.printf "Test is_bit_set: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test is_bit_set: ERROR\n";
    Printf.eprintf "bits: %s\n" (Bits.string_of_bits b);
  end

let test10 () =
  let b = Bits.set_bits Bits.zero [54; 48; 21; 13; 7] in
  let res1 = Bits.are_bits_set b [7; 21; 48; 54] in
  let res2 = Bits.are_bits_set b [6; 52] in
  let res3 = Bits.are_bits_set b [6; 7; 13; 14] in
  if res1 = true && res2 = false && res3 = false then begin
    Printf.printf "Test are_bits_set: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test are_bits_set: ERROR\n";
    Printf.eprintf "bits: %s\n" (Bits.string_of_bits b);
  end

let test11 () =
  let bits = Bits.set_bits Bits.zero [54; 48; 21; 13; 7] in
  let res = ref [] in
  let f i b =
    if b then res := i :: !res
  in
  Bits.iteri f bits;
  if !res = [54; 48; 21; 13; 7] then begin
    Printf.printf "Test iteri: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test iteri: ERROR\n";
    Printf.eprintf "bits: %s\n" (Bits.string_of_bits bits);
  end

let test12 () =
  let b1 = Bits.set_bits Bits.zero [62; 54; 48; 32; 21; 15; 13; 7; 3] in
  let f i b = if i < 60 then not b else false in
  let b2 = Bits.mapi f b1 in
  if Bits.eq b2 (Bits.of_string
    "0011111011111011111111111111101_1111111110111110101111101110111")
  then begin
    Printf.printf "Test mapi: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test mapi: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
  end

let test13 () =
  let b1 = (Bits.of_string
    "0011111001111000011110011111101_1111111100111110101111101110111") in
  let b2 = Bits.invert b1 60 in
  if Bits.eq b2 (Bits.of_string
    "0000000110000111100001100000010_0000000011000001010000010001000")
  then begin
    Printf.printf "Test invert: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test invert: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
  end

let test14 () =
  let b1 = (Bits.of_int 0b0010000001100011100000111100011) in
  let b2 = (Bits.of_string "0010000001100011100000111100011") in
  if Bits.eq b1 b2
  then begin
    Printf.printf "Test of_int: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test of_int: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
  end

let test15 () =
  let d1 = Bits.to_int (Bits.of_string "0010000001100011100000111100011") in
  let d2 = 0b0010000001100011100000111100011 in
  if d1 = d2
  then begin
    Printf.printf "Test to_int: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test to_int: ERROR\n";
    Printf.eprintf "int1: %d\n" d1;
    Printf.eprintf "int2: %d\n" d2;
  end

let test16 () =
  let r =
    Bits.foldi
      (fun i b n -> if b then succ n else n)
      0 (Bits.of_int 0b0111010111010110001111100101110)
  in
  if r = 19
  then begin
    Printf.printf "Test foldi: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test foldi: ERROR\n";
    Printf.eprintf "result: %d\n" r;
    Printf.eprintf "should: 19\n";
  end

let test17 () =
  (* bit 30 is the tricky one! *)
  let b1 = Bits.set_bits Bits.zero [40; 21; 30; 11; 6; 3] in
  let b2 = Bits.set_bits Bits.zero [41; 22; 31; 12; 7; 4] in
  let res = Bits.shift_left b1 1 in
  if Bits.eq res b2 then begin
    Printf.printf "Test shilf_left 1: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test shilf_left 1: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
    Printf.eprintf "result %s\n" (Bits.string_of_bits res);
  end

let test18 () =
  (* bit 30 is the tricky one! *)
  let bits = [40; 39; 30; 19; 11; 6; 4; 3] in
  let n = 5 in
  let shifted = List.map ((+) n) bits in
  let b1 = Bits.set_bits Bits.zero bits in
  let b2 = Bits.set_bits Bits.zero shifted in
  let res = Bits.shift_left b1 n in
  if Bits.eq res b2 then begin
    Printf.printf "Test shilf_left 2: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test shilf_left 2: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
    Printf.eprintf "result %s\n" (Bits.string_of_bits res);
  end

let test19 () =
  let bits = [30; 19; 11; 6; 4; 3] in
  let n = 21 in
  let shifted = List.map ((+) n) bits in
  let b1 = Bits.set_bits Bits.zero bits in
  let b2 = Bits.set_bits Bits.zero shifted in
  let res = Bits.shift_left b1 n in
  if Bits.eq res b2 then begin
    Printf.printf "Test shilf_left 3: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test shilf_left 3: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
    Printf.eprintf "result %s\n" (Bits.string_of_bits res);
  end

let test20 () =
  let bits = [43; 30; 19; 11; 6; 4; 3] in
  let n = 41 in
  let shifted = List.map ((+) n) bits in
  let b1 = Bits.set_bits Bits.zero bits in
  let b2 = Bits.set_bits Bits.zero shifted in
  let res = Bits.shift_left b1 n in
  if Bits.eq res b2 then begin
    Printf.printf "Test shilf_left 4: OK\n";
  end
  else begin
    exit_code := 1;
    Printf.eprintf "Test shilf_left 4: ERROR\n";
    Printf.eprintf "bits1: %s\n" (Bits.string_of_bits b1);
    Printf.eprintf "bits2: %s\n" (Bits.string_of_bits b2);
    Printf.eprintf "result %s\n" (Bits.string_of_bits res);
  end

let () =
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  test7 ();
  test8 ();
  test9 ();
  test10 ();
  test11 ();
  test12 ();
  test13 ();
  test14 ();
  test15 ();
  test16 ();
  test17 ();
  test18 ();
  test19 ();
  test20 ();
  exit !exit_code;
;;
