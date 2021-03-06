(* Java generator for Marmalade *)


open Ast
open Sast

(* rewrite AST types as the actual java types in the file. *)

let write_type = function 
    | Int -> "j_int"
    | String -> "j_string"
    | Note -> "j_note"
    | Measurepoo -> "j_measure"
    | Phrase -> "j_phrase"
    | Song -> "j_song"
    | TimeSig -> "TimeSig"
    | Instr -> "int"
    | Tempo -> "int"
    | Intlist -> "j_intlist"
    | Stringlist -> "j_stringlist"
    | _ -> raise(Failure "Error: Type string of PD_Tuple or Null_Type being generated")

(* rewrite operations to their actual expressions in java. *)

let write_op_primitive op e1 e2 = 
    match op with
    Plus -> "new j_int(j_int.add(" ^ e1 ^ ", " ^ e2 ^ "))"
    | Minus -> "new j_int(j_int.sub(" ^ e1 ^ ", " ^ e2 ^ "))"
    | Times -> "new j_int(j_int.mult(" ^ e1 ^ ", " ^ e2 ^ "))"
    | Divide -> "new j_int(j_int.divide(" ^ e1 ^ ", " ^ e2 ^ "))"
    | Equal -> "j_int.eq(" ^ e1 ^ ", " ^ e2 ^ ")"
    | Neq -> "j_int.neq(" ^ e1 ^ ", " ^ e2 ^ ")"
    | Less -> "j_int.lt(" ^ e1 ^ ", " ^ e2 ^ ")"    
    | Leq -> "j_int.leq(" ^ e1 ^ ", " ^ e2 ^ ")"
    | Greater -> "j_int.gt(" ^ e1 ^ ", " ^ e2 ^ ")"
    | Geq -> "j_int.geq(" ^ e1 ^ ", " ^ e2 ^ ")"
    | And -> "(" ^ e1 ^ ") && (" ^ e2 ^ ")"  
    | Or -> "(" ^ e1 ^ ") || (" ^ e2 ^ ")"  
    | _ -> raise (Failure "Error: and/or begin applied to a java primitive")

(* notes map to values in jmusic *)

let write_rhythm dr =
    match dr with 
    's' -> "0.125"  (* sixteenth note maps to 0.125 *)
    | 'e' -> "0.25" (* eigth note maps to 0.25 *)
    | 'q' -> "0.5"
    | 'h' -> "1.0"
    | 'w' -> "2.0"

(* get type of expression *)

let rec get_typeof_dexpr = function
     S_Int_Lit(intLit, t) -> t
    | S_String_Lit(strLit, t) -> t
    | S_Id (str, t) -> t
    | S_Arr(dexpr_list, t) -> t
    | S_Binop (dexpr1, op, dexpr2, t) -> t
    | S_Noexpr -> Null_Type
    | S_Call(str, _, dexpr_list, _, t) -> t

(* write actual java compare expression *)

let write_op_compares e1 op e2 =
    match op with 
    Equal -> "(" ^ e1 ^ ").equals(" ^ e2 ^ ")"
    | Less ->  "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " < 0"
    | Leq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " <= 0"
    | Greater -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " > 0"
    | Geq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " >= 0"
    | Neq -> "(" ^ e1 ^ ").compareTo(" ^ e2 ^ ")" ^ " != 0"
    | _ -> raise (Failure("Error: Not a comparator operation."))

(* convert marmalade's sast expressions into java expressions *)

let rec write_expr = function
    S_Int_Lit(intLit, t) -> "(new j_int(" ^ string_of_int intLit ^ "))"
    | S_String_Lit(strLit, t) -> "(new j_string(\"" ^ strLit ^ "\"))"
    | S_Id (str, yt) -> str
    | S_Arr(dexpr_list, t) -> write_array_expr dexpr_list t
    | S_Binop (dexpr1, op, dexpr2, t) -> write_binop_expr dexpr1 op dexpr2 t
    | S_Db_Arr(call, mark) -> (
            match mark with
            S_Arr(l_one, l_two) ->   write_expr call 
            | S_Noexpr -> write_expr call)
    | S_Measure(s_note_list, s_time, typ) -> "new j_measure(new j_note[] {" ^ (String.concat ", " (List.map write_expr s_note_list)) ^ "}, new TimeSig (" ^ write_expr s_time ^ "))"
    | S_Phrase(s_measure_list, s_instr, typ) -> "new j_phrase(new j_measure[] {" ^ (String.concat ", " (List.map write_expr s_measure_list)) ^ "}, " ^ write_expr s_instr ^ ")"
    | S_Song(s_phrase_list, s_tempo, typ) -> "new j_song(new j_phrase[] {" ^ (String.concat ", " (List.map write_expr s_phrase_list)) ^ "}, " ^ write_expr s_tempo ^ ")"
    | S_Noexpr -> ""
    | S_Note(i, ch, tp) -> "new j_note(" ^ string_of_int i ^ ", " ^ write_rhythm ch ^ ")"
    | S_TimeSig(i, i_2, tp) -> string_of_int i ^ ", " ^ string_of_int i_2 
    | S_Instr(str, tp) -> str
    | S_Tempo(i, tp) -> string_of_int i
    | S_Index(str, i, tp) -> str ^ ".get(" ^ write_expr i ^ ")"
	| S_Call(str, exp, dexpr_list,t_ret, t_send) -> (match str with 
       					 "print" -> "System.out.println("  ^ write_expr exp ^ ");\n"							  
    					| "play" -> write_expr exp ^ ".play();\n"
						| "write" -> "Write.midi(" ^ write_expr exp ^ ".getObj(), \"out.mid\");\n"
						| "evaluate_measure" -> "new j_measure(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ") "
                        | "evaluate_phrase" -> "new j_phrase(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ") "  
                        | "evaluate_song" -> "new j_song(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ") " 
                        | "evaluate_note" -> "new j_note(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ") "
                        | "length_measure" -> "new j_int(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ".length())"		
                        | "length_phrase" -> "new j_int(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ".length())"             
                        | "length_song" -> "new j_int(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ".length())"            
                        | "length_int_list" -> "new j_int(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ".length())"            
                        | "length_string_list" -> "new j_int(" ^ String.concat "" (List.map write_expr dexpr_list) ^ ".length())"                                 	 
						| _ -> ( match exp with 
								  	S_Noexpr -> str ^ "(" ^ String.concat "," (List.map write_expr dexpr_list) ^ ")"
								  | _ -> write_expr exp ^ "." ^ str ^ "(" ^ String.concat "," (List.map write_expr dexpr_list) ^ ");/n")
								)
	| S_Call_lst(s) -> String.concat "" (List.map write_expr s)
	| _ -> raise(Failure("Error: Not a valid expression."))


(* this function matches to each kind of s_stmt, calling the function write_expr to write each of them in Java. *)

and write_stmt d vg = (match d with
      S_CodeBlock(dblock) -> write_block dblock vg
    | S_expr(dexpr) -> write_expr dexpr ^ ";"
    | S_Assign (name, dexpr, t) -> (match vg with
        true -> (
        match dexpr with
        S_Db_Arr(a1, a2) -> write_expr (S_Db_Arr(a1, a2)) ^ write_assign name a2 t true ^ ";\n"
        | _ -> write_assign name dexpr t true ^ ";\n" )
        | false -> (match dexpr with
        S_Db_Arr(a1, a2) -> write_expr (S_Db_Arr(a1, a2)) ^ write_assign name a2 t false ^ ";\n"
        | _ -> write_assign name dexpr t false ^ ";\n" ) )
    | S_Return(dexpr) -> "return " ^ write_expr dexpr ^ ";\n"
    | S_If(dexpr, dstmt1, dstmt2) -> "if(" ^ write_expr dexpr ^  ")" ^  write_stmt dstmt1 vg ^ "else"  ^ write_stmt dstmt2 vg
    | S_While(dexpr, dblock) -> "while(" ^ write_expr dexpr ^ ")"  ^ write_block dblock vg (* check true *)
    | S_Index_Update(nme, expr_1, expr_2, typ) -> 
    (match typ with 
        
        (* jMusic syntax for setting a note, measure, and part (which is the same as a phrase in marmalade) *)
        Measurepoo -> nme ^ ".set_Note(" ^ write_expr expr_2 ^ "," ^ write_expr expr_1 ^ ");\n" 
        | Phrase -> nme ^ ".set_Measure( " ^ write_expr expr_1 ^ ", " ^ write_expr expr_2 ^ ");\n"
        | Song -> nme ^ ".set_Part( " ^ write_expr expr_1 ^ ", " ^ write_expr expr_2 ^ ");\n" )
    | _ -> raise(Failure(" is not a valid statement")))

and write_stmt_true d = write_stmt d true 

and write_stmt_false d = write_stmt d false

(* function that matches the expression on each side of the binop, then writes it *)

and write_binop_expr expr1 op expr2 t =
    let e1 = write_expr expr1 and e2 = write_expr expr2 in 
        let write_binop_expr_help e1 op e2 = 
            match t with
                Int -> (match op with 
                    (Plus | Minus | Times | Divide | Equal | Neq | Less | Leq | Greater | Geq | And | Or) ->  
                    write_op_primitive op e1 e2)
              | String -> (match op with 
                     Plus -> "new j_string(j_string.add(" ^ e1 ^ ", " ^ e2 ^
                     "))"
                    | (Equal | Less | Leq | Greater | Geq) -> write_op_compares e1 op e2
                    | _ -> raise(Failure(write_op_primitive op e1 e2 ^ " is not a supported operation for String_Type")))
              | Note -> (match op with (Plus | Minus | Divide | Times) -> "new j_note( " ^ write_op_primitive op e1 e2 ^ ", " ^ e1 ^ ".getLength() )"
                        | _ -> raise(Failure("Error: Cannot add to note.")) ) 
              | _ -> raise(Failure("Error: " ^ write_op_primitive op e1 e2 ^ " is not a supported operation for " ^ write_type t ^ "."))
        in write_binop_expr_help e1 op e2 

(* writes an array expression *)

and write_array_expr dexpr_list t =
      match t with
        Int -> "new j_intlist (new j_int[] {" ^ String.concat "," (* if Int, then write an int list *)
              (List.map write_expr dexpr_list) ^ "})" 
        | String -> "new j_stringlist (new j_string[] {" ^ String.concat "," (* if String, then write a string list *)
        (List.map write_expr dexpr_list) ^ "})" 
        | _ -> "new " ^ write_type t ^ " []"  ^ " {" ^ String.concat "," (List.map write_expr dexpr_list) ^ "}"

(* helper function to apply java toString function *)

and tostring_str dexpr =
    let t = get_typeof_dexpr dexpr in
    match t with  
         Int -> write_expr dexpr
        | String -> write_expr dexpr
        | _ -> "(" ^ write_expr dexpr ^ ").toString()"

and write_scope_var_decl_func svd =
    let (n, b, t, _) = svd in 
        write_type t ^ " " ^ n

and write_scope_var_decl svd =
    write_scope_var_decl_func svd ^ ";\n"

and write_global_scope_var_decl gsvd = 
    "static " ^ write_scope_var_decl_func gsvd ^ ";\n"

(* write assign expression in java *)

and write_assign name dexpr t vg =
    match vg with 

    true -> (match t with
      String | Instr | Tempo | Intlist | Stringlist -> name ^ " = " ^ write_expr dexpr
    | Int | Note | TimeSig | Measurepoo | Phrase | Song  -> name ^ " = " ^ "(" ^ write_expr dexpr ^ ")"
    | _ -> raise(Failure("Error: " ^ write_type t ^ " is not a valid assign_type.")))
    | false -> (match t with
      String | Instr | Tempo | Intlist | Stringlist -> write_type t ^ " " ^ name ^ " = " ^ write_expr dexpr
    | _ -> raise(Failure("Error: " ^ write_type t ^ " is not a valid assign_type."))) 

and write_block dblock vg =
    match vg with
    true -> "{\n" ^ String.concat "\n" (List.map write_stmt_true dblock.s_statements ) ^ "\n}"
    | false -> "{\n" ^ String.concat "\n" (List.map write_scope_var_decl dblock.s_locals) ^ String.concat "\n" (List.map write_stmt_false dblock.s_statements ) ^ "\n}"

               
(* include necessary java lines -> main *)

let write_func_wrapper x str =
    String.concat "\n"
    (let write_func dfunc = 
        match (dfunc.s_fname, str) with
        ("main", String) -> "public static void main(String[] args)" ^ write_block dfunc.s_fblock true
    | (_, _) -> (String.concat "\n" (let match_type ftype = 
        match ftype with
        str -> "static " ^ write_type dfunc.s_ret_type ^ " " ^
        dfunc.s_fname ^ "("  ^ String.concat "," (List.map write_scope_var_decl_func
        dfunc.s_formals) ^ ")" ^ write_block dfunc.s_fblock true
        | _ -> "" in
        List.map match_type dfunc.s_f_type)) in
    List.map write_func x)

(* Below is necessary java placed into the file *)

let gen_pgm pgm name = 
    "import java.util.Arrays;\n" ^
    "import java.util.ArrayList;\n" ^ 
    "import jm.JMC;\n" ^
    "import jm.music.data.*;\n" ^
    "import jm.util.*;\n" ^
    "import marmalade.*;\n" ^
    "import jm.midi.event.TimeSig;\n" ^


    "public class " ^ name ^ " implements JMC{\n" ^ String.concat "\n" (List.map write_global_scope_var_decl pgm.s_gvars) ^ 
     (write_func_wrapper pgm.s_pfuncs String) ^ 
    "\n\n" ^

    "public static class j_int extends m_Int {\n" ^
    "public j_int(int n) {\n" ^
    "super(n);\n}\n" ^
     "public j_int(j_int n) {\n" ^
     "super(n);\n}" ^
     (write_func_wrapper pgm.s_pfuncs Int) ^ 
     "\n}\n\n" ^

     "public static class j_intlist extends m_Int_List {\n" ^
     "public j_intlist(j_int[] j) {\n" ^
     "super(j);\n}" ^
     "public j_int get(int i) {\n" ^
     "return new j_int(getList()[i]);\n}" ^
     (write_func_wrapper pgm.s_pfuncs Intlist) ^
     "\n}\n\n" ^


     "public static class j_string extends m_String {\n" ^
     "public j_string(j_string x) {\n" ^
     "super(x);\n}" ^
     "public j_string(String x) {\n" ^
     "super(x);\n}" ^
     (write_func_wrapper pgm.s_pfuncs String) ^
     "\n}\n\n" ^


     "public static class j_stringlist extends m_String_List {\n" ^
     "public j_stringlist(j_string[] j) {\n" ^
     "super(j);\n}" ^
     "public j_string get(int i) {\n" ^
     "return new j_string(getList()[i]);\n}" ^
     (write_func_wrapper pgm.s_pfuncs Stringlist) ^
     "\n}\n\n" ^


     "public static class j_note extends m_Note {\n" ^
     "public j_note(Note n) {\n" ^
     "super(n);\n}" ^
     "public j_note(int pitch, double length) {\n" ^
     "super(pitch, length);\n}" ^
     "public j_note(j_int pitch, double length) {\n" ^
     "super(pitch, length);\n}" ^ 
     (write_func_wrapper pgm.s_pfuncs Note) ^
     "\n}\n\n" ^ 


     "public static class j_measure extends Measure {\n\n" ^ 
     "public j_measure(j_note[] m, TimeSig n) {\n" ^
     "  super(m, n);\n}" ^
     "public j_measure(Phrase p) {\n" ^
     "  super(p);\n}" ^
     "public j_measure(j_measure l) \n
     {\n    super(l.getObj()); \n}\n"^
     "public j_note get(int i) {\n" ^
     "  Note n = getObj().getNote(i);\n     j_note m = new j_note(n);\n     return m;\n}" ^
     "public j_note get(j_int i) {\n" ^
     "  Note n = getObj().getNote(i.get());\n   j_note m = new j_note(n);\n     return m;\n}" ^
     "public void set_Note(j_note i, j_int k){
        this.p.setNote(i.getObj(), k.get());\n
     }\n" ^
     "public void set_Note(j_note i, int k){
        this.p.setNote(i.getObj(), k);
     }\n" ^     
     (write_func_wrapper pgm.s_pfuncs Measurepoo) ^ 
     "\n}\n" ^ 


     "public static class j_phrase extends
     m_Phrase {\n" ^
     "  public j_phrase(Part p) {\n" ^
     "super(p);\n}" ^
     "  public j_phrase(j_measure[] m, int n) {\n" ^
     "super(m, n);\n}" ^
     "  public j_phrase(j_measure[] m, j_int n) {\n" ^
     "super(m, n);\n}\n" ^
    "public j_phrase(j_phrase l) \n 
     {\n    super(l.getObj()); \n}\n" ^ 
     "  public j_measure get(int i) {\n" ^
     "Phrase p = getObj().getPhrase(i);\n" ^
     "  return (new j_measure(p));\n}" ^
     "public j_measure get(j_int i) {\n" ^
     "  Phrase p = getObj().getPhrase(i.get());\n" ^
     "return (new j_measure(p));\n}" ^  

     "public void set_Measure(j_int idx, j_measure n_measure)\n
     {\n
        this.set_Measure(idx.get(), (Measure) n_measure); \n
    }\n" ^

    "public void set_Measure(int idx, j_measure n_measure)\n
     {\n
        this.set_Measure(idx, (Measure) n_measure); \n
    }\n" ^

     (write_func_wrapper pgm.s_pfuncs Phrase) ^
     "\n}\n" ^ 

     "public static class j_song
     extends Song {\n" ^
     "public j_song(j_phrase[] m, int n) {\n" ^
     "  super(m, n);\n}" ^
     "public j_song(j_phrase[] m, j_int n) {\n" ^
     "  super(m, n);\n}\n" ^
     "public j_song(j_song l) \n
     {\n   super(l); \n}\n" ^
     "public j_phrase get(int i) {\n" ^
     "  Part s = getObj().getPart(i);\n" ^
     "return (new j_phrase(s));\n}" ^

     "public j_phrase get(j_int i) {\n" ^
     "Part s = getObj().getPart(i.get());\n
     return (new j_phrase(s));\n}" ^

    "public void set_Part(j_int idx, j_phrase n_phrase)\n
     {\n
        this.set_Part(idx.get(), (m_Phrase) n_phrase); \n
    }\n" ^

    "public void set_Part(int idx, j_phrase n_phrase)\n
     {\n
        this.set_Part(idx, (m_Phrase) n_phrase); \n
    }\n" ^

     (write_func_wrapper pgm.s_pfuncs Song) ^
     "\n}\n}\n" 