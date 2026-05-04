(** The [dbtype] type defines the various types that may appear
   in our database setting.

  We only need two primitive types here.

  - integers, represented by TInt
  - text values, represented by TText
 *)
type dbtype =
  | TInt  (* integer entries *)
  | TText (* text entries    *)
;;

(** The [coltype] type represents a column in a table of our system.

   It is a pair made of the type of the values stored in this column
   on one hand, and a boolean expressing whether this column may
   ([true]) or may not ([false]) hold the value [NULL] on the other hand. *)
type coltype = dbtype * bool ;;

(** The [dbvalue] type is the type of *VALUES* stored in the database.

  We need three kinds of values here.

  - integer values along with their value
  - text values along with their value
  - the null value, which can be considered indifferently of type
   [TInt] or [TText].  *)
type dbvalue =
  | VInt of int     (* integer values *)
  | VText of string (* text values    *)
  | VNull           (* the NULL value *)
;;

(** The schema of a table is a list of pairs whose first element is
   the column name and whose second element is the column type, of
   type [coltype]. *)
type schema = (string*coltype) list ;;


(** A row of a table is a list of values.
 *)
type row = dbvalue list ;;

(** A [table] is the data of a schema together with a list of rows. *)
type table = { cols : schema; rows : row list } ;;

(** The [fd] type represents the type of functional dependencies
   of a table.

   It is a pair (lhs,rhs) where each of the two members is a list of
   column names.

   The dependency (lhs,rhs) of course represents the dependency lhs -> rhs.
 *)
type fd = (string list) * (string list) ;;



(*--------Implementations of a few List functions------*)

(** [for_all p l] checks whether the predicate [p] is true for every element of [l]
type : ('a -> bool) -> 'a list -> bool
@requires p is a valid predicate compatible with the elements of l
@ensures returns true if p is true for every element, false otherwise
@raises none
*)
let for_all p l =
   List.fold_left (fun acc x -> acc && p x) true l;;


(** [rev l] reverses the order of the elements of the list [l].
This implementation uses an auxiliary function with an accumulator
to guarantee tail recursion, thus avoiding any risk of stack overflow
on very large lists.
type : 'a list -> 'a list
@requires no condition other than l being a list of elements.
@ensures returns a new list containing the same elements as l, but in reverse order.
@raises none
*)
let rev l =
  (* Tail-recursive auxiliary function *)
  let rec aux acc liste = match liste with
    | [] -> acc                  
    | x::xs ->           
        aux (x::acc) xs  
  in
  aux [] l;;
   

(** [filter p l] returns the list of elements of [l] satisfying [p]
type : ('a -> bool) -> 'a list -> 'a list
@requires p is a valid predicate applicable to the elements of the list.
 @ensures returns a list containing only the elements satisfying p, preserving their original order.
 @raises none
 *) 
let filter p l =
   let l_inverse = List.fold_left (fun acc x ->
      if p x then x::acc
      else acc
      ) [] l in
   rev l_inverse;;


(** [appartient a l] checks whether the element [a] belongs to the list [l]
type : 'a -> 'a list -> bool
 @requires none
 @ensures returns true if the element x is present in lst, false otherwise.
 @raises none
*)
let appartient a l = 
   List.fold_left (fun acc e -> (e = a) || acc ) false l;;


(*------------------------Part 1: Basic Functions------------*)

(** [check_table tbl] checks whether the table [tbl] is valid.
type : table -> bool = <fun>
@requires tbl is a value of type table
@ensures returns true if every row has the same length as the schema,
 if column names are unique and
 if the type and nullability constraints of every column are respected.
@raises none
 *)
let check_table tbl = 
   let n = List.length tbl.cols in
   
   let names = List.map (fun (n, _) -> n ) tbl.cols in
   let unique_names = List.fold_left (fun acc x ->
      if appartient x acc then acc else x::acc
   ) [] names in
   if List.length names <> List.length unique_names then false
   else
      for_all(fun row ->
          List.length row = n &&
          let (valid_constraints, _) = List.fold_left (fun (acc, remaining_cols) v ->
             match remaining_cols with
             | [] -> (acc, [])
             |(_, (type_col, nullable))::rest_cols ->
               let v_valid = 
                  match v, type_col, nullable with
                  | VNull, _, true -> true
                  |VNull, _, false -> false
                  | VInt _, TInt, _ -> true
                  | VText _, TText, _ -> true
                  | VInt _, TText, true | VInt _, TText, false
                     | VText _, TInt, true | VText _, TInt, false -> false in
               (acc && v_valid, rest_cols)
          ) (true, tbl.cols) row in 
          valid_constraints
      ) tbl.rows;;
             

(** [insert tbl r] inserts, if possible, the row [r] into the table [tbl].
type: table -> row -> table =<fun>
@requires tbl is a value of type table and row is also of type row
@ensures inserts row into the table tbl if tbl is a valid table and if row is a valid row
with respect to the table, that is to say, inserting row into a valid table yields a table
that is still valid
@raises Failure if the table is not valid or if the row we try to insert
is invalid with respect to the table
*)
let insert tbl row = 
   if not(check_table tbl) then failwith "table invalide"
   else
      let new_tbl = {cols = tbl.cols; rows = tbl.rows@[row]} in
      if not(check_table new_tbl) then failwith "la ligne qu'on veut inserer est invalide"
      else new_tbl ;;


(** [prod tbl1 tbl2] computes the cartesian product of the tables [tbl1] and [tbl2]
type : table -> table -> table = <fun>
@requires tbl1 and tbl2 are of type table
@ensures computes the cartesian product of tbl1 and tbl2 and returns the resulting table
@raises Failure if one of the tables is not valid
 *)                   
let prod tbl1 tbl2 = 
   if not (check_table tbl1) || not (check_table tbl2) then failwith "produit cartesien de tables invalides"
   else
      (* The schema of the product is just the concatenation of the two schemas *)
      let new_cols = (tbl1.cols)@(tbl2.cols) in
      let new_rows = List.fold_left (fun acc r1 -> 
                        acc@(List.map (fun r2 -> r1@r2) tbl2.rows)) [] tbl1.rows in
      {cols = new_cols; rows = new_rows};;



(*-----------Helper functions for projection--------*)

(** [check_field_exists f sch] checks whether the field [f] exists in the schema [sch]
type : string -> schema -> bool
@requires f is of type string and sch is of type schema
@ensures returns true if the field exists in the schema sch, false otherwise
@raises none
*)
let check_field_exists f sch =
   List.fold_left(fun acc (n,_) -> acc || (n = f)) false sch;;


(** [get_col_info f sch] returns the coltype information associated with the field [f]
type : string -> schema -> coltype
@requires f is a field name and sch is of type schema.
@ensures returns the type information (coltype) associated with the field f.
@raises Failure if the field f does not exist in sch.
*)  
let get_col_info f sch =
   if not (check_field_exists f sch) then failwith "Champ absent"
   else
      List.fold_left (fun acc (n, i) ->
         (* So as soon as we find an element whose name matches f the accumulator
         will not change anymore *)
         if n = f then i else acc
      ) (TInt, false) sch;;


(** type : string -> schema -> row -> dbvalue
 @requires name is present in the schema sch, and r is of type row and has the same length as sch.
 @ensures extracts the value from the row r corresponding to the column name.
 @raises Failure if the column is not found while walking the row.
 *)
let get_val_from_row name sch r =
   (* We walk the schema and the row in parallel using a triple accumulator:
   - acc_val   : the value found (initialized to VNull)
   - current_row : the remaining part of the row to walk
   - acc_found : a boolean indicating whether the column has already been found *)
   let (res, _, found) = List.fold_left (fun (acc_val, current_row, acc_found) (n, _) ->
      match current_row with
        | [] -> (acc_val, [], acc_found) 
        | v::rest ->
            if n = name then (v, rest, true)
            else (acc_val, rest, acc_found)
   ) (VNull, r, false) sch in
   if not found then failwith "Nom de champs introuvable"
   else res;;


(** 
type : string list -> schema -> row -> row
@requires fields is a subset of the columns of sch, r has the same length as sch.
@ensures builds a new row containing only the values of the fields requested in fields.
@raises Failure if a column in fields was not found in the schema of the table
*)  
let extract_row fields sch r =
   List.map (fun f -> get_val_from_row f sch r) fields;;


(** [projection tbl fields] performs the projection of the table [tbl]
   on the field list [fields] 
type : table -> string list -> table
@requires tbl is of type table and fields is a subset of columns existing in tbl
@ensures returns a table projected on the specified fields, in the order of fields
@raises Failure if one of the fields does not exist (the Failures are raised in get_col_info
and get_val_from_row)
*)
let projection tbl fields =
   let new_cols = List.map (fun f -> (f, get_col_info f tbl.cols)) fields in
   let new_rows = List.map (fun r -> extract_row fields tbl.cols r ) tbl.rows in
   {cols = new_cols; rows = new_rows};;


(** [restrict tbl test] performs the restriction of the data present
   in the table [tbl] according to the function [test]. The result
   only keeps the rows for which [test] returns [true]. 
   type : table -> (row -> bool) -> table = <fun> 
   @requires test is a predicate applicable to a row.
   @ensures returns a table containing only the rows satisfying the test.
   @raises none 
*)
let restrict tbl test = 
   {cols = tbl.cols; rows = filter test tbl.rows} ;;


(*--------------------Part 2: Normalization----------------------*)       

(** [sous_listes l] returns the list of all possible sub-lists of [l] without taking the order of elements into account
type : 'a list -> 'a list list = <fun>
@requires none.
@ensures returns the list of all possible sub-lists generated from l without taking the order of elements into account.
@raises none
*)
(* For instance, here [1;2;3] and [2;1;3] are considered to be the same list *)
let sous_listes l =
   List.fold_left (fun acc x ->
        (* sub_x: every sub-list of size (size of accumulator + 1) containing x as its first element *)
        let sub_x = List.map (fun s -> x::s) acc in
          acc@sub_x
   ) [[]] l;;


(** [verify_df tbl (lhs, rhs)] checks whether the functional dependency [(lhs, rhs)] holds
type : table -> fd -> bool
@requires tbl is a valid table, lhs and rhs are subsets of the schema of tbl.
@ensures returns true if the functional dependency lhs -> rhs is satisfied for every pair of rows of tbl.
@raises Failure if a field of lhs or rhs does not exist in the table (handled in extract_row).
*) 
let verify_df tbl (lhs, rhs) =
   for_all (fun r1 -> 
      for_all ( fun r2 ->
          (* Extract the sub-list of r1 over the columns of lhs *)
          let lhs_r1 = extract_row lhs tbl.cols r1 in
          (* Extract the sub-list of r2 over the columns of lhs *)
          let lhs_r2 = extract_row lhs tbl.cols r2 in
          (* Extract the sub-list of r1 over the columns of rhs *)
          let rhs_r1 = extract_row rhs tbl.cols r1 in
          (* Extract the sub-list of r2 over the columns of rhs *)
          let rhs_r2 = extract_row rhs tbl.cols r2 in

          if lhs_r1 = lhs_r2 then rhs_r1 = rhs_r2
          else true
      ) tbl.rows
   ) tbl.rows;;


(** [compute_deps tbl] returns ALL the functional dependencies
   found by studying the data present in [tbl] 
type : table -> fd list
@requires tbl is a valid table.
@ensures returns all the functional dependencies.
@raises Failure if the table tbl is not valid
*)
let compute_deps tbl = 
   if not(check_table tbl) then failwith "table invalide"
   else
   let info_cols = List.fold_left (fun acc (n, _)  -> n::acc) [] tbl.cols in
   let sub_lists = sous_listes info_cols in 
   (* Build the cartesian product sub_lists x sub_lists to get every
   possible pair (X, Y) *)
   let deps = List.fold_left (fun acc x ->
      let couples = List.map (fun y -> (x, y)) sub_lists in
      acc@couples
   ) [] sub_lists in
   filter (fun (lhs, rhs) -> verify_df tbl (lhs, rhs)) deps;;


(*------A few internal functions defined to determine the elementary dependencies*)   

(** [est_inclus sub l] tests the inclusion of the list sub in l
type : 'a list -> 'a list -> bool
 @requires none
 @ensures returns true if every element of sub is present in l.
 @raises none
 *)
let est_inclus sub l =
  for_all (fun x -> appartient x l) sub ;;   


(** [est_inclus_strict sub l] tests the strict inclusion of the list sub in l
type : 'a list -> 'a list -> bool
 @requires none
 @ensures returns true if sub is included in l and its size is strictly smaller.
 @raises none
 *)
let est_inclus_strict sub l =
   est_inclus sub l && (List.length sub < List.length l);;


(** [compute_elementary_deps tbl] returns ALL the elementary functional
   dependencies found by studying the data present in [tbl] 
type : table -> fd list
@requires tbl is a valid table.
@ensures returns ALL the elementary functional dependencies.
@raises Failure if the table tbl is not valid
*)
let compute_elementary_deps tbl = 
   if not(check_table tbl) then failwith "table invalide"
   else
   let deps = compute_deps tbl in
   (* Only consider dependencies of the form X -> a_i with
   a_i (a singleton attribute) not belonging to X *)
   let first_condition_filter = List.fold_left ( fun acc (lhs, rhs) ->
      match rhs with
        | [a] -> if not (appartient a lhs) then (lhs, [a])::acc 
                 else acc
        | _ -> acc
   ) [] deps in

   (* After this first filter, perform the second one, which consists in eliminating
   dependencies of the form Y -> a_i knowing that we have X -> a_i and Y is strictly included in X *)
   filter ( fun (lhs, rhs) ->
      let sub_lists_lhs = sous_listes lhs in
      let est_minimal = List.fold_left (fun acc_min sub ->
          if (est_inclus_strict sub lhs) && (appartient (sub, rhs) first_condition_filter)
             then false
          else acc_min
      ) true sub_lists_lhs in 
      est_minimal
   ) first_condition_filter ;; 


(** [est_cle_candidate x tbl] determines whether the column subset [x] is a candidate key,
that is, whether it is the smallest subset of columns of [tbl] that determines all
the attributes of [tbl].
type : string list -> table -> bool
 @requires tbl is a valid table, x is a subset of the columns of tbl.
 @ensures returns true if x is a candidate key.
 @raises none
 *)
let est_cle_candidate x tbl = 
   let cols = List.map (fun (n, _) -> n) tbl.cols in
   let est_supercle = verify_df tbl (x, cols) in 
   if not est_supercle then false
   else
      let sous_x = sous_listes x in
      let strict_sous_x = filter (fun e -> est_inclus_strict e x) sous_x in 
      (* Check whether one of the strict subsets of x is potentially a candidate key
      and return the negation of this boolean *)
      let existe_strict_sous_x_supercle = List.fold_left ( fun acc sub ->
          acc || (verify_df tbl (sub, cols))
      ) false strict_sous_x in 
      not existe_strict_sous_x_supercle;;

      
(** [respecte_2NF (lhs, rhs) candidate_keys tbl] checks whether the functional dependency (lhs, rhs)
satisfies the 2NF rule
type : fd -> string list list -> table -> bool
 @requires the functional dependency (lhs, rhs) is elementary, 
 candidate_keys is a list of candidate keys and tbl is of type table
@ensures returns true if the functional dependency (lhs, rhs) satisfies the 2NF rule
 (that is, either lhs is a candidate key, or rhs belongs to a 
 candidate key in the list of candidate keys [candidate_keys], or lhs
 contains an element that does not belong to any key)
 @raises none 
 *)
let respecte_2NF (lhs, rhs) candidate_keys tbl =
   let first_verify = est_cle_candidate lhs tbl in
   let second_verify = 
      match rhs with
      | [a] -> List.fold_left (fun acc cle -> 
           acc || appartient a cle) false candidate_keys
      | _ -> false
      in
   let third_verify = List.fold_left ( fun acc x ->
       let not_appartient_aucune_cle = for_all (fun key -> not(appartient x key)) candidate_keys in
       acc || not_appartient_aucune_cle
   ) false lhs in 
   first_verify || second_verify || third_verify;;


(** [respecte_3NF (lhs, rhs) candidate_keys tbl] checks whether the functional dependency (lhs, rhs)
satisfies the 3NF rule
type : fd -> string list list -> table -> bool
 @requires the functional dependency (lhs, rhs) is elementary,
 candidate_keys is a list of candidate keys and tbl is of type table.
@ensures returns true if the functional dependency (lhs, rhs) satisfies the 3NF rule
 (that is, either lhs is a candidate key, or rhs belongs to a 
 candidate key in the list of candidate keys [candidate_keys])
 @raises none 
 *)
let respecte_3NF (lhs, rhs) candidate_keys tbl = 
   let first_verify = est_cle_candidate lhs tbl in
   let second_verify = 
      match rhs with
      | [a] -> List.fold_left (fun acc cle -> 
           acc || appartient a cle) false candidate_keys
      | _ -> false
      in
   first_verify || second_verify;;
     

(** [normalization_level tbl] returns the normalization level of
   [tbl] as an integer. 
type : table -> int
@requires tbl is a valid table.
@ensures returns an integer between 1 and 3.
@raises Failure if the table tbl is not valid
*)
let normalization_level tbl =
   if not(check_table tbl) then failwith "table invalide"
   else
   let elementary_deps = compute_elementary_deps tbl in 
   (* Build the list of all candidate keys by walking every subset of columns *)
   let col_names = List.map (fun (n, _) -> n) tbl.cols in
   let sous_ensembles_cols = sous_listes col_names in
   let cle_candidates = filter (fun x -> est_cle_candidate x tbl) sous_ensembles_cols in

   let est_2NF = for_all (fun (x, a) -> respecte_2NF (x, a) cle_candidates tbl ) elementary_deps in
   let est_3NF = for_all (fun (x, a) -> respecte_3NF (x, a) cle_candidates tbl ) elementary_deps in

   if est_3NF then 3
   else if est_2NF then 2
   (* Otherwise, the table is in 1NF *)
   else 1;;


