#use "projet.ml"

(* RDBMS VALIDATION TESTS *)


(* --- TABLES USED FOR THE VARIOUS TESTS --- *)
(**
The expected normalization levels of the 3 valid tables are demonstrated in the report.
*)
(* Table 'produit' (expected in 3NF) *)
let produit = {
  cols = [
    ("n_produit", (TText, false)); 
    ("libelle", (TText, false)); 
    ("prix", (TInt, false))
  ];
  rows = [
    [VText "P1"; VText "stylo"; VInt 2];
    [VText "P2"; VText "cahier"; VInt 3];
    [VText "P3"; VText "regle"; VInt 1]
  ]
};;

(* Table 'commande' (expected in 2NF) *)
let commande = {
  cols = [
    ("n_comm", (TText, false)); 
    ("n_produit", (TText, false)); 
    ("qtte", (TInt, false)); 
    ("type", (TText, false))
  ];
  rows = [
    [VText "C01"; VText "P1"; VInt 3; VText "A"];
    [VText "C02"; VText "P1"; VInt 5; VText "A"];
    [VText "C03"; VText "P2"; VInt 1; VText "B"];
    [VText "C04"; VText "P2"; VInt 1; VText "B"]; 
    [VText "C05"; VText "P3"; VInt 3; VText "A"]
  ]
}
;;

(* Table 'inscription' (expected in 1NF) *)
let inscription = {
  cols = [
    ("n_etudiant", (TText, false));
    ("ue", (TText, false));
    ("nom_etudiant", (TText, false))
  ];
  rows = [
    [VText "E1"; VText "INPF12"; VText "Alice"];
    [VText "E1"; VText "LAOB12"; VText "Alice"];
    [VText "E2"; VText "INPF12"; VText "Alice"];
    [VText "E2"; VText "STAT12"; VText "Alice"];
    [VText "E3"; VText "LAOB12"; VText "Toto"]
  ]
};;


(* Invalid table: VNull in a non-nullable column (nullable = false).
   The "id" column does not allow NULL values, yet the second
   row contains VNull at that position. *)
let invalide_null = {
  cols = [
    ("id", (TInt, false));
    ("nom", (TText, false))
  ];
  rows = [
    [VInt 1; VText "Alice"];
    [VNull; VText "Bob"]
  ]
};;

(* Invalid table: VText in a column of type TInt.
   The "id" column expects integers (TInt), yet the second row
   contains a VText at that position. *)
let invalide_type = {
  cols = [
    ("id", (TInt, false));
    ("nom", (TText, false))
  ];
  rows = [
    [VInt 1; VText "Alice"];
    [VText "pas_un_entier"; VText "Bob"]
  ]
};;


(**---------------------HELPER FUNCTIONS---------------------*)

(** Recursive function printing the elements of a list, separated by the
separator sep given as a parameter
type : string -> 'a string -> string = <fun>
@requires sep is of type string and l is a list of strings
@ensures prints the elements of the list l on a single line, separated by sep 
@raises none
*)
let rec print_string_list_sep sep l = match l with
  | [] -> Printf.printf "" 
  | [x] -> Printf.printf "%s" x
  | x :: xs -> 
      let _ = Printf.printf "%s%s" x sep in
      print_string_list_sep sep xs;;


(**
 Convert a database value into a string for display.
 type : dbvalue -> string = <fun>
 @requires value is of type dbvalue
 @ensures returns the value (int, string, or NULL) held by value as a string
 @raises none  
*)
let string_of_dbvalue value = match value with
  | VInt i -> string_of_int i
  | VText s -> s
  | VNull -> "NULL";;


(** Print a table as a matrix.
type : string -> table -> string = <fun>
@requires nom_table and tbl have the right types
@ensures prints the table with the schema on one row,
and each data row on a single line whose elements are
separated by the separator sep.
@raises none
*)
let print_table nom_table tbl =
  let _ = Printf.printf "\n-- Table : %s --\n" nom_table in
  
  let header_cols = List.map (fun (n, _) -> n) tbl.cols in
  let _ = print_string_list_sep " | " header_cols in
  let _ = Printf.printf "\n" in
  
  (* Separator line between the schema and the rows *)
  let _ = Printf.printf "--------------------------------------------------------\n" in
  
  (* Print the rows using List.fold_left and a constant accumulator of value 0 *)
  let _ = List.fold_left (fun acc row ->
    let row_strs = List.map string_of_dbvalue row in
    let _ = print_string_list_sep " | " row_strs in
    let _ = Printf.printf "\n" in
    acc (* Return acc to keep the loop going *)
  ) 0 tbl.rows in
  Printf.printf "\n";;


(** Print a functional dependency.
type : fd -> string = <fun>
@requires (lhs, rhs) is of type fd
@ensures prints on a single line the dependency in the form {lhs} -> {rhs}
@raises none
*)
let print_fd (lhs, rhs) =
  let _ = Printf.printf " {" in
  let _ = print_string_list_sep ", " lhs in
  let _ = Printf.printf "} -> {" in
    let _ = print_string_list_sep ", " rhs in
    Printf.printf "}\n";;


(* Display the initial state *)
let _ = print_table "produit" produit in
  let _ = print_table "commande" commande in
  let _ = print_table "inscription" inscription in 
  Printf.printf "\n" ;;


(* Tests of the basic functions *)
let _ =
  let _ = Printf.printf "---------------------------------------------------------\n" in
  let _ = Printf.printf "-------DISPLAY OF THE RELATIONAL OPERATIONS------------\n" in
  let _ = Printf.printf "-----------------------------------------------------------\n" in


  (* Validity tests *)
  let _ = Printf.printf "[TEST] check_table on table with forbidden VNull\n" in
  let _ =
    if not (check_table invalide_null) then
       Printf.printf "[OK] Table correctly invalid.\n\n"
    else
       Printf.printf "[FAIL] The table should have been invalid.\n\n"
  in
  let _ = Printf.printf "[TEST] check_table on table with incompatible type\n" in
  if not (check_table invalide_type) then
      Printf.printf "[OK] Table correctly invalid.\n\n"
  else
      Printf.printf "[FAIL] The table should have been invalid.\n\n" in


  (* Insertion tests *) 
  (* Insertion of a valid row in the 'produit' table.
   The table must go from 3 to 4 rows without raising an exception *)
  let _ = Printf.printf "[TEST] Insertion of a valid row\n" in
  let produit_apres_insert = insert produit [VText "P4"; VText "gomme"; VInt 1] in
  let _ = 
      if List.length produit_apres_insert.rows = 4 then
           Printf.printf "[0K] The table contains 4 rows after insertion.\n\n"
      else
          Printf.printf "[FAIL] Incorrect number of rows.\n\n"
  in

  (* Insertion of a row with an incompatible type (VText into a TInt column).
  The "prix" column expects a VInt, but we give it a VText.
  insert must raise a Failure exception. *)
  let _ = Printf.printf "[TEST] Insertion with incompatible type (VText into a TInt)\n" in
  let _ = 
     try
        let _ = insert produit [VText "P5"; VText "stylo"; VText "pas_un_prix"] in
        Printf.printf "[FAIL] The insertion should have failed.\n\n"
      with Failure _ -> Printf.printf "[OK] Failure exception raised as expected.\n\n" 
  in
  
  (* Insertion of a row with VNull in a non-nullable column.
   The "n_produit" column has nullable = false, and we put VNull there.
   insert must raise a Failure exception. *)
  let _ = Printf.printf "[TEST] Insertion with VNull in a non-nullable column\n" in
  let _ =
  try
    let _ = insert produit [VNull; VText "crayon"; VInt 1] in
    Printf.printf "[FAIL] The insertion should have failed.\n\n"
  with Failure _ ->
    Printf.printf "[OK] Failure exception raised as expected.\n\n"
  in

  (* Insertion of a row that is too short (2 values instead of 3).
   The row does not respect the dimensional consistency.
   insert must raise a Failure exception. *)
  let _ = Printf.printf "[TEST] Insertion of a row that is too short\n" in
  let _ =
  try
    let _ = insert produit [VText "P6"; VText "stylo"] in
    Printf.printf "[FAIL] The insertion should have failed.\n\n"
  with Failure _ ->
    Printf.printf "[OK] Failure exception raised as expected.\n\n"
  in


  (* Restriction tests *)
  let _ = Printf.printf "[TEST] Restrict the 'commande' table to n_comm = 'C01'\n" in
  let predicat_c01 row = 
    (get_val_from_row "n_comm" commande.cols row) = VText "C01" 
  in
  let res_restrict = restrict commande predicat_c01 in
  let _ = print_table "Restrict result" res_restrict in


  (* Projection tests *)
  let _ = Printf.printf "[TEST] Projection of 'commande' on the columns n_produit and type\n" in
  let res_proj = projection commande ["n_produit"; "type"] in
  let _ = print_table "Projection result" res_proj in


  (* Cartesian product tests *)
  let _ = Printf.printf "[TEST] Cartesian product of 'commande' X 'produit'\n" in
  let res_prod = prod commande produit in
  print_table "Cartesian product result" res_prod;;



(* Normalization tests on the valid tables *)
let _ =
  let _ = Printf.printf "----------------------------------------------------------\n" in
  let _ = Printf.printf "----------NORMALIZATION LEVEL TESTS-----------------------\n" in
  let _ = Printf.printf "----------------------------------------------------------\n" in

  (* Elementary dependency tests on the 'produit' table *)
  let elem_produit = compute_elementary_deps produit in
  let _ = Printf.printf "\n--- Table 'produit' ---\n" in
  let _ = Printf.printf "Elementary FDs:\n" in
  let _ = List.fold_left (fun acc fd -> 
    let _ = print_fd fd in
    acc ) 0 elem_produit in

  let niv_produit = normalization_level produit in
  let _ = Printf.printf "-> Level of the 'produit_simple' table: %d \n" niv_produit in
  let _ = 
    if niv_produit = 3 then 
      Printf.printf "[OK] The table is correctly evaluated as 3NF.\n"
    else 
      Printf.printf "[FAIL] The table should be in 3NF.\n"
  in
  let _ = Printf.printf "\n" in

  (* compute_deps and compute_elementary_deps on the 'commande' table *)
  let elem_commande = compute_elementary_deps commande in
  let _ = Printf.printf "\n--- Table 'commande' ---\n" in
  let _ = Printf.printf "Elementary FDs:\n" in
  let _ = List.fold_left (fun acc fd -> 
    let _ = print_fd fd in acc ) 0 elem_commande in

  let niv_commande = normalization_level commande in
  let _ = Printf.printf "-> Level of the 'commande' table: %d \n" niv_commande in
  let _ = 
    if niv_commande = 2 then 
      Printf.printf "[OK] The table is correctly evaluated as 2NF.\n"
    else 
      Printf.printf "[FAIL] The table should be in 2NF.\n"
  in
  let _ = Printf.printf "\n" in

 (* compute_deps and compute_elementary_deps on the 'inscription' table *)
  let elem_inscription = compute_elementary_deps inscription in
  let _ = Printf.printf "\n--- Table 'inscription' ---\n" in
  let _ = Printf.printf "Elementary FDs:\n" in
  let _ = List.fold_left (fun acc fd -> 
    let _ = print_fd fd in acc) 0 elem_inscription in

  let niv_inscription = normalization_level inscription in
  let _ = Printf.printf "-> Level of the 'commande_produit' table: %d \n" niv_inscription in
  
  let _ = 
    if niv_inscription = 1 then 
      Printf.printf "[OK] The table is correctly identified as 1NF.\n"
    else 
      Printf.printf "[FAIL] Expected: 1NF, got: %dNF.\n" niv_inscription
  in
  Printf.printf "\n" 
  ;;


