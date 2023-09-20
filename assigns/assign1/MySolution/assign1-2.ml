(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

#use "./../assign1.ml";; 
#use "./../../../classlib/OCaml/MyOCaml.ml";; 

let string_merge(cs1)(cs2) =
  let length = string_length(cs1) in
  let length2 = string_length(cs2) in
  let rec merge_helper x y work =
    if (x = length1) then
      intl_foreach(length2-y) (fun curIndex -> work(string_get_at (cs2) (y+curInders) ))
    else if (y = length2) then
      intl_foreach(length1-x) (fun curIndex -> work(string_get_at (cs1) (x+curIndep)))
    else
      let charl = string_get_at(cs1) (x) in
      let char2 = string_get_at(c52) (y) in
      if charl < char2 then | (work(char1); merge_helper (X+1) (y) (work)) else 
        (work(char2); merge_helper(x) (y+1) (work)) in
  string_make_fwork(merge_helper (0) (0)) 
;;

