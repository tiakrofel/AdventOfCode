let vrstice = String.split_on_char '\n'

(* od tu naprej je koda specifična za nalogo *)

let prvi_element = function
    | [] -> failwith "Prekratek seznam"
    | x :: _ -> x

let drugi_element = function
    |[] | [_] -> failwith "Prekratek seznam"
    | x :: y :: ys -> y

let pozicije_podmornice vrednosti_seznama = 
  let ints = vrstice vrednosti_seznama 
  and locitev = String.split_on_char ' '
  in
  let uredi x = int_of_string (drugi_element (locitev x))
  in
  let rec aux horizontalno vertikalno = function
  | [] -> horizontalno * vertikalno 
  | x :: ys -> 
    if prvi_element (locitev x) = "forward" then aux (horizontalno + (uredi x)) vertikalno ys
    else if prvi_element (locitev x) = "down" then aux horizontalno (vertikalno + (uredi x)) ys
    else if prvi_element (locitev x) = "up" then aux horizontalno (vertikalno - (uredi x)) ys
    else failwith "Napačen seznam"
  in
  aux 0 0 ints

let primerjaj_podmornice_aim vrednosti_seznama = 
  let ints = vrstice vrednosti_seznama
  and locitev = String.split_on_char ' '
  in 
  let uredi x = int_of_string (drugi_element (locitev x))
  in
  let rec aux horizontalno vertikalno aim = function
  | [] -> horizontalno * vertikalno 
  | x :: ys -> 
    if prvi_element (locitev x) = "forward" then aux (horizontalno + (uredi x)) (vertikalno + aim * (uredi x)) aim ys
    else if prvi_element (locitev x) = "down" then aux horizontalno vertikalno (aim + (uredi x)) ys
    else if prvi_element (locitev x) = "up" then aux horizontalno vertikalno (aim - (uredi x)) ys
    else failwith "Napačen seznam"
  in
  aux 0 0 0 ints

(* od tu naprej je koda zopet splošna *)

let naloga1 vsebina_datoteke =
  pozicije_podmornice vsebina_datoteke

let naloga2 vsebina_datoteke =
  primerjaj_podmornice_aim vsebina_datoteke

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
  let vsebina_datoteke = preberi_datoteko "vhodi/day_2.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "izhodi/day_2_1.out" (string_of_int odgovor1);
  izpisi_datoteko "izhodi/day_2_2.out" (string_of_int odgovor2) 