let vrstice = String.split_on_char '\n'

let list_of_string seznam = seznam |> String.to_seq |> List.of_seq

(* od tu naprej je koda specifična za nalogo *)

let primerjaj vrednosti_seznama =
  let ints = vrstice vrednosti_seznama |> List.map int_of_string
  in
  let rec aux acc = function
  | [] -> acc 
  | x :: [] -> acc 
  | x :: y :: xys -> if x < y then aux (acc + 1) (y :: xys) else aux acc (y :: xys)
  in
  aux 0 ints

let primerjaj_vsote vrednosti_seznama = 
  let ints = vrstice vrednosti_seznama |> List.map int_of_string
  in
  let rec aux vrednost acc = function
    | [] | [_] | [_; _] -> acc - 1
    | x :: y :: z :: zs -> if (x + y + z) > vrednost then aux (x + y + z) (acc + 1) (y :: z :: zs) else aux (x + y + z) acc (y :: z :: zs)  
  in
  aux 0 0 ints

(* od tu naprej je koda zopet splošna *)

let naloga1 vsebina_datoteke =
  primerjaj vsebina_datoteke

let naloga2 vsebina_datoteke =
  primerjaj_vsote vsebina_datoteke

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "vhodi/day_1.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "izhodi/day_1_1.out" (string_of_int odgovor1);
  izpisi_datoteko "izhodi/day_1_2.out" (string_of_int odgovor2)