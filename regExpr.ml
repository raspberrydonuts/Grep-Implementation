(* Turning a string into a regular expression *)

(* Tokens (substrings that make up regular expressions) *)
type tok = LP | RP | U | DOT | Ch of char | ST | BR of string

(* Function to parse the value from an expression with type option.
Since String.index_from_opt returns an option, I need this to get the integer *)
let contents z = match z with Some c -> c

(* turn a string into a list of regex tokens *)
(* st is the string and i is the index to start tokenizing from  *)
let rec tokenize st i =
  if String.length st = i then []
  else if st.[i] = '\\' then read_escape st (i+1)
  else if st.[i] = '[' then read_bracket st (i+1) (* call read_bracket if encountering a '[' character *)
  else (match st.[i] with '.' -> DOT | '*' -> ST | c -> Ch c)::(tokenize st (i+1))
and read_escape st i =
  if String.length st = i then failwith "trailing backslash"
  else (match st.[i] with '(' -> [LP]
    | ')' -> [RP]
    | '|' -> [U]
    | '.' | '[' | ']' -> [Ch st.[i]]
    | c -> [Ch '\\'; Ch c]) @ (tokenize st (i+1))
and read_bracket st i =
  let end_bracket_index = if (i+1) < String.length st
    (* find the index where the end bracket occurs, if it does.
      We can fail if index parameter is bigger than length*)
    then String.index_from_opt st (i+1) ']' else failwith "opt index out of bounds" in
  if end_bracket_index = None then failwith "brackets unbalanced" (* if we can't find an end bracket, fail the procedure *)
  (* get the string until the closing bracket is found and return a
     BR value consed onto the result of tokenizing the rest of the string *)
  else (BR (String.sub st (i) ((contents end_bracket_index)-i)))::(tokenize st ((contents end_bracket_index) + 1))


type regexp = Union of regexp * regexp
| Concat of regexp * regexp
| Char of char
| Wild
| NullSet (* matches no strings at all *)
| Star of regexp
| Bracket of bool * char list (* Bracket contains a bool for
    if there's a caret in front of the char list, and the char list *)


(* parse a list of tokens into a regexp *)
(* each function takes a list of tokens, tries to parse a kind of regex from the list,
  and returns the list of unused tokens *)
let rec
  (* parse a "Base" Case regexp: Empty, NullSet, Bracket, Wild, or Char *)
  parse_base_case token_list = match token_list with
  | [] -> (NullSet, [])
  | DOT::tl -> (Wild,tl)
  | LP::tl -> let (re,tl') = parse_regex tl in
    (match tl' with RP::tl'' -> (re,tl'')
    | _ -> failwith "expected close paren")
  | (Ch c)::tl -> (Char c,tl)
  | (BR b)::tl -> (parse_bracket b,tl) (* if the first token in its argument is a BR, call parse_bracket*)
  | _ -> failwith "unexpected token"
(* parses expression containg a '*' *)
and parse_starred_base token_list = match parse_base_case token_list with
  | (expr, ST::tl) -> (Star expr, tl) (* if ST token found following base case, wrap regexp in a Star*)
  | (_, tl) -> parse_base_case token_list (* Otherwise just return results of parse_base_case*)
and
  term_helper tl re = match tl with (* parse a "factor" in a regex of the form r1 r2 (r3 ...) *)
  | RP::tl' -> (re,tl)
  | U::tl' -> (re,tl)
  | [] -> (re,[])
  | _ -> let (re_next,tl') = parse_starred_base tl in
    term_helper tl' (Concat (re,re_next))
and
  parse_terms tl = match parse_starred_base tl with (* parse a regex in the form r1 r2 (r3...) *)
  | (t,RP::tl') -> (t, RP::tl')
  | (t,U::tl') -> (t, U::tl')
  | (t,[]) -> (t, [])
  | (t,tl')-> term_helper tl' t
and
  regex_helper tl re = (* helper for parsing union of terms *)
    let (t_next,tl') = parse_terms tl in match tl' with
    | U::tl'' -> regex_helper tl'' (Union (re,t_next))
    | _ -> (Union (re, t_next), tl')
and parse_regex tl = match parse_terms tl with (* parse a regex *)
  | (t,U::tl') -> regex_helper tl' t
  | (t,tl') -> t,tl'
and char_range lchar rchar = (* given a range of chars, inclusively return all the characters between them *)
  let rec char_range_helper lc rc acc = (* Tail recursive function to accumulate the characters in range *)
    if (lchar > rchar) || (rc = '-') then failwith "invalid range"
    (* once the left character has been incremented to be higher than right char, return result*)
    else if (lc > rc) then acc
    (* recursive call to char_range_helper after appending the current char
       to acc and incrementing it for the recursive call*)
    else char_range_helper (char_of_int ((int_of_char lc)+1)) rc (lc::[] @ acc)
  in char_range_helper lchar rchar []
and parse_bracket tl = (* Reads string from a BR and produces a bracket value *)
  let rec parse_bracket_helper st i acc = (* Tail recursive function to accumulate all characters*)
    (* Iterates through all characters in the string and produces the appropriate bracket value*)
    if i >= (String.length st) then acc (* return accumulated list when at the end of the string*)
    (* if the last character is a '-', append it accordingly without thinking it's a range*)
    else if (st.[i] = '-') && (i = ((String.length st)-1)) then ('-'::[] @ acc)
    (* if the current character is the beginning of a range, call char_range and append the
       output to acc while also recursively calling with i+3 (since a range expression contains 3 chars)*)
    else if ((i+2) < String.length st && st.[i+1] = '-')
      then parse_bracket_helper st (i+3) ((char_range st.[i] st.[i+2]) @ acc)
    (* if it's any other character, parse it normally and recursively call for the next character in the string*)
    else parse_bracket_helper st (i+1) (st.[i]::[] @ acc)
  in
    (* output a Bracket value of (true, {string}) if first character of the string
       is a caret. (false, {string}) if otherwise*)
    if tl.[0] = '^' then Bracket (true, List.rev (parse_bracket_helper tl 1 []))
    else Bracket (false, List.rev (parse_bracket_helper tl 0 []))

(* searches a list to see if element is in the list. returns true if found, else false *)
let rec search lst c = match lst with
  | [] -> false
  | h::t -> if (h=c) then true else search t c

(* takes as input a Bracket value and a character c and checks whether
   c belongs to the range of characters the bracket expression represents *)
let bracket_match brack c = match brack with
  (* if there's a caret, any character that does not match the following list *)
  | Bracket (true, lst) -> not (search lst c)
  (* if no caret, search the char list for the char *)
  | Bracket (false, lst) -> search lst c
  (* if variant of regexp other than Bracket, output invalid_arg *)
  | _ -> invalid_arg "bracket_match"


let rex_parse s =
  let n = String.length s in
  let s' = if n > 1 && s.[0] = '^' then String.sub s 1 (n-1) else (".*"^s) in
  let n' = String.length s' in
  let s'' = if n' > 1 && s'.[n'-1] = '$' then String.sub s' 0 (n'-1) else (s'^".*") in
  let tok_list = tokenize s'' 0 in
  match parse_regex tok_list with
  | (re,[]) -> re
  | _ -> failwith ("regular expression string "^s^" is unbalanced")


(* Check if an exploded string matches a regex -  a helper function *)
(* uses "continuation"-style search: k is a "continuation function" that
   checks the rest of the string *)
let rec re_match re s k = match (re,s) with
  (* If the first character matches Char c, continue checking the rest of the string with k *)
  | (Char c, []) -> false
  | (Char c, c2::t) -> (c=c2) && (k t)
  (* If first character is bracket, do continuation search with the bracket elements*)
  | (Bracket (caret, lst), []) -> false
  | (Bracket (caret, lst), b2::t) -> (bracket_match re b2) && (k t)
  (* we can match Star r to s if we can continue matching s with k, or
     if we can match r to part of s and continue trying to match Star r to the remaining
     portion of s. *)
  | (Star r, []) -> true
  | (Star r, _) -> (k s) || re_match r s (fun s' -> re_match re s' k)
  (* Wild always eats a char *)
  | (Wild, []) -> false
  | (Wild, _::t) -> (k t)
  (* Check r1|r2 by first continuing with r1, and if that fails, continue with r2 *)
  | (Union (r1,r2), _) -> (re_match r1 s k) || (re_match r2 s k)
  (* Check r1 r2 by checking r1, and if that succeeds, "continue" with r2 *)
  | (Concat (r1,r2), _) -> (re_match r1 s (fun s' -> re_match r2 s' k))
  | NullSet, _ -> false


let explode s =
  let n = String.length s in
  let rec exphelp i acc =
    if i = n then acc else exphelp (i+1) (s.[i]::acc)
  in List.rev (exphelp 0 [])

(* given a regexp and string, decide if the string matches *)
let regex_match r s =
  re_match r (explode s) ((=) [])

(* Common ocaml idiom: other files will call the regexp type RegExpr.t *)
type t = regexp
