open FTools

let ngram_n = 3
(* Your code goes here: *)

(* Define the function that lower-cases and filters out non-alphabetic characters *)
(* Checks to each character in the string is a letter. String.lowercase_ascii gives the String.map makes the input string lowercase. *)
let cleanup_chars s = String.map(fun c -> if (c >= 'a' && c <= 'z') then c else ' ')(String.lowercase_ascii s)

(* extract a list of n-grams from a string, naively *)
(* given the number n, it splits the string into n size sub lists *)
let ngrams n s =
  let ngrams_helper = (String.length s - (n-1)) in List.init ngrams_helper (fun x -> String.sub s x n)

(* Define the function that converts a string into a list of "normalized" n-grams *)
(* using cleanup_chars, to clean it up and make it all lowercase, then the helper functions filters out any whitespace *)
let n_grams s =
  let _helper = ngrams ngram_n (cleanup_chars s) in
    List.filter(fun x -> not(String.contains x ' ')) _helper (* *)


(* Define a function to convert a list into a multiset *)
let multiset_of_list lst = let ms_helper = List.sort compare lst in (* Using List.sort and the given compare to sort the list *)
  List.fold_left (fun acc x -> match acc with
                    |[] -> [(x,1)]
                    |((y, num)::t) -> if x = y then ((y, num+1) ::t) else ((x, 1)::acc)) [] ms_helper (* Goes through the lists and finds if there are duplicates and adding a counter to the subset. *)

(* multiset utility functions *)

(* multiplicity of e in multiset b - 0 if not in the multiset *)
let multiplicity e b = List.fold_left(fun acc (k,v) -> if k = e then v else acc) 0 b

(* size of a multiset is the sum of the multiplicities of its elements *)
let size b = List.fold_left(fun acc (k,v) -> v + acc) 0 b

(* Define the similarity function between two multisets: size of intersection / size of union *)
(* checks to see if the k of s1 is in s2 and returns the counter acc *)
let intersection_size s1 s2 = List.fold_left (fun acc (k,v) -> let _helper = multiplicity k s2 in
                                                                  let _helper2 =  min v _helper in
                                                                    if _helper > 0 then acc + _helper2 else acc) 0 s1

let union_size s1 s2 = size s1 + size s2 - (intersection_size s1 s2) (* combines s1 and s2 minus the duplicates that would occur, then minus intersection for duplicates *)

let similarity s1 s2 = (float_of_int(intersection_size s1 s2) /. float_of_int(union_size s1 s2)) (* convert to float to be able to get the right ratio w/o rounding errors *)

(* Find the most similar representative file *)
(* Combines the two lists using List.combine and iterates through to see which element is the max. *)
let find_max repsims repnames = let max_helper = List.combine repsims repnames in
  match max_helper with
  |[] -> (0.,"")
  |(x,y)::t -> List.fold_left (fun acc (x',y') -> max acc (x',y'))(x,y) max_helper

let main all replist_name target_name =
  (* Read the list of representative text files *)
  let repfile_list = file_lines replist_name in
  (* Get the contents of the repfiles and the target file as strings *)
  let rep_contents = List.map file_as_string repfile_list in
  let target_contents = file_as_string target_name in
  (* Compute the list of normalized n-grams from each representative *)
  let rep_ngrams = List.map n_grams rep_contents in
  (* Convert the target text file into a list of normalized n-grams *)
  let target_ngrams = n_grams target_contents in
  (* Convert all of the stem lists into stem sets *)
  let rep_multisets = List.map multiset_of_list rep_ngrams in
  let target_multiset = multiset_of_list target_ngrams in
  (* Compute the similarities of each rep set with the target set *)
  let repsims = List.map (fun x -> similarity x target_multiset) rep_multisets in
  let (sim,best_rep) = find_max repsims repfile_list in
  let () = if all then
  (* print out similarities to all representative files *)
  let () = print_endline "File\tSimilarity"; List.iter2 (fun s f -> Printf.printf "%s\t%f\n" s f)repfile_list repsims in ()
   else
      begin
  (* Print out the winner and similarity *)
  let () = Printf.printf "The most similar file to %s was %s \n" target_name best_rep in
           Printf.printf "Similarity:%f \n" sim
     end in
  (* this last line just makes sure the output prints before the program exits *)

  flush stdout
