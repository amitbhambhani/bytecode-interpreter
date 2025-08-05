(* initialize stack values *)
type stackValue = BOOL of string | INT of int | ERROR of string | UNIT of string 
| STRING of string | NAME of string | CLOSURE of (stackValue * stackValue * (command list) * (stackValue * stackValue) list list)

(* initialize commands *)
and command = ADD | SUB | MUL | DIV | REM | NEG | SWAP | POP
| TOSTRING | PRINTLN | CAT | AND | OR | NOT | EQUAL | LESSTHAN 
| BIND | IF | LET | END | FUN of stackValue * stackValue 
| INOUTFUN of stackValue * stackValue | FUNEND | RETURN | CALL
| QUIT | PUSH of stackValue;;

let interpreter ((input : string), (output : string)) : unit = 
    let ic = open_in input in
    
    let oc = open_out output in

    let rec loop_read acc =
        try 
            let l = String.trim(input_line ic) in loop_read (l::acc)
        with
            | End_of_file -> List.rev acc
    in

    let file_write string_val = Printf.fprintf oc "%s\n" string_val in

    (* strList represents every line of the file in list form *)
    let strList = loop_read [] in
    
    let str2sv s = 
        if String.contains s '.' == true then
            ERROR(":error:")
        else
            match s.[0] with
            (* if the first char is a number or a negative, the rest must be a number *)
            | '0' -> INT(0)
            | '1' -> INT(int_of_string s)
            | '2' -> INT(int_of_string s)
            | '3' -> INT(int_of_string s)
            | '4' -> INT(int_of_string s)
            | '5' -> INT(int_of_string s)
            | '6' -> INT(int_of_string s)
            | '7' -> INT(int_of_string s)
            | '8' -> INT(int_of_string s)
            | '9' -> INT(int_of_string s)
            | '-' -> INT(int_of_string s)
            (* there are 4 types that start with : *)
            | ':' -> 
                if s.[1] == 't' then
                    BOOL(":true:")
                else if s.[1] == 'f' then
                    BOOL(":false:")
                else if s.[1] == 'u' then
                    UNIT(":unit:")
                else    
                    (* catch-all case where :_ will just be error type *)
                    ERROR(":error:")
            (* strings will only start with double quotes *)
            | '"' -> STRING(s)
            (* name can start with _ *)
            | '_' -> NAME(s)
            (* if it's not a string, then anything that has letters must be a name *)
            | 'a'..'z' -> NAME(s)
            | 'A'..'Z' -> NAME(s)
            | _ -> ERROR(":error:")
    in 

    let str2cmd s = 
        match s with    
        (* basic conversion of string to its stackValue type *)
        | "add" -> ADD
        | "sub" -> SUB
        | "mul" -> MUL
        | "div" -> DIV
        | "rem" -> REM
        | "neg" -> NEG
        | "swap" -> SWAP
        | "pop" -> POP
        | "toString" -> TOSTRING
        | "println" -> PRINTLN
        | "cat" -> CAT
        | "and" -> AND
        | "or" -> OR
        | "not" -> NOT
        | "equal" -> EQUAL
        | "lessThan" -> LESSTHAN
        | "bind" -> BIND
        | "if" -> IF
        | "let" -> LET
        | "end" -> END
        | "funEnd" -> FUNEND
        | "return" -> RETURN
        | "call" -> CALL
        | "quit" -> QUIT
        | _ ->
            (* check for push and the value to push *)
            if String.length s >= 5 then
                https://ocaml.org/manual/5.3/api/String.html *)
                match (s.[0], s.[1], s.[2], s.[3], s.[4]) with 
                | ('p', 'u', 's', 'h', ' ') -> 
                    let value = String.sub s 5 ((String.length s) - 5) in 
                    (* use str2sv to turn this value (a string) into its actual type *)
                    PUSH (str2sv value)
                | ('f', 'u', 'n', ' ', _) ->
                    let vals = String.split_on_char ' ' s in
                    (match vals with
                    | fun1::name1::name2::[] -> FUN (NAME(name1), NAME(name2))
                    | _ -> QUIT)
                | ('i', 'n', 'O', 'u', 't') ->
                    let vals = String.split_on_char ' ' s in
                    (match vals with
                    | fun1::name1::name2::[] -> INOUTFUN (NAME(name1), NAME(name2))
                    | _ -> QUIT)
                | _ -> QUIT
            else 
                QUIT   
    in

    (* generate command list by mapping all lines in the file to its command equivalent *)
    let comList = List.map str2cmd strList in
    
    let pushCheck pv s =
        (* this returns any of the 6 stackValues prepended to the stack *)
        match pv with
            | BOOL(b) -> (BOOL(b)::s)
            | INT(i) ->  (INT(i)::s)
            | STRING(str) ->  (STRING(str)::s)
            | NAME(n) ->  (NAME(n)::s)
            | ERROR(e) ->  (ERROR(e)::s)
            | UNIT(u) ->  (UNIT(u)::s)
            | CLOSURE(isIO, arg, ccl, cenv) -> (CLOSURE(isIO, arg, ccl, cenv)::s)
    in

    let rec envLookup var env = 
        match env with
            | (NAME(k), INT(v))::kvs -> 
                if String.equal var k then
                    Some(INT(v))
                else 
                    envLookup var kvs
            | (NAME(k), BOOL(":true:"))::kvs -> 
                if String.equal var k then
                    Some(BOOL(":true:"))
                else 
                    envLookup var kvs
            | (NAME(k), BOOL(":false:"))::kvs -> 
                if String.equal var k then
                    Some(BOOL(":false:"))
                else 
                    envLookup var kvs
            | (NAME(k), STRING(v))::kvs -> 
                if String.equal var k then
                    Some(STRING(v))
                else 
                    envLookup var kvs
            | (NAME(k), UNIT(":unit:"))::kvs -> 
                if String.equal var k then
                    Some(UNIT(":unit:"))
                else 
                    envLookup var kvs
            | (NAME(k), CLOSURE(isIO, NAME(arg), ccl, cenv))::kvs -> 
                if String.equal var k then
                    Some(CLOSURE(isIO, NAME(arg), ccl, cenv))
                else 
                    envLookup var kvs
            | [] -> None
            | _ -> None
    in

    let rec envSwitcher var envList = 
        match envList with
        | hd::tl -> 
            (* go through the current env *)
            (match envLookup var hd with
            | Some(v) -> Some(v)
            (* if the value isn't in the current env, go to the next one *)
            | None -> envSwitcher var tl)
        | [] -> None
    in

    (* used to check for isIO in function calls *)
    let rec funCheck var envList = 
        match envList with
        | hd::tl -> 
            (match envLookup var envList with
            | Some(v) -> Some(v)
            | None -> None)
        | [] -> None
    in

    (* generates the list of commands within a function *)
    let rec funCommands cl fcl scope currScope =
        match cl with
        (* function within a function, increase scope level *)
        | FUN(n, a)::cs -> funCommands cs (FUN(n, a)::fcl) (scope + 1) currScope
        (* end the function only if scope levels are the same *)
        | FUNEND::cs -> 
            if scope = currScope then
                https://ocaml.org/manual/5.3/api/List.html *)
                ((List.rev (FUNEND::fcl)), cs)
            else 
                (* keep recording commands if we haven't met the right scope level *)
                funCommands cs (FUNEND::fcl) (scope - 1) currScope
        (* record commands that aren't related to the function bounds *)
        | cmd::cmds -> funCommands cmds (cmd::fcl) scope currScope
        | [] -> ([],[])
    in

    let rec getVar envList =
        match envList with
        | (NAME(param), w)::(NAME(arg), v)::(NAME(b), BOOL(c))::tl -> (arg,param) 
        | hd::tl -> getVar tl
        | [] -> (":error:", ":error:")
    in

    (* now go through the command list and execute *)
    let rec processor cl stack env (scope:int) = 
        match (cl, stack) with
        | (clh::clt, hd::tl) ->
            (match (clh, hd) with
            (* add will check if there's exactly 2 ints at the head of the stack and replace them with their sum *)
            | (ADD::cs, INT(a)::INT(b)::svs) -> processor (cs::clt) ((INT(a+b)::svs)::tl) env scope
            (* VARIABLES *)
            | (ADD::cs, NAME(a)::INT(b)::svs) -> 
                (* check if the variable is in any env *)
                (match envSwitcher a env with
                (* if it is, extract it and do the calculation *)
                | Some(INT(a)) -> processor (cs::clt) ((INT(b + a)::svs)::tl) env scope
                (* if not, push everything back on the stack along with :error: *)
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope)
            | (ADD::cs, INT(a)::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(INT(b)) -> processor (cs::clt) ((INT(b + a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope)
            | (ADD::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(a)), Some(INT(b))) -> processor (cs::clt) ((INT(b + a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            (* return :error: in any other stack arrangement *)
            | (ADD::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* sub is the same as add, except it subtracts the top of the stack from the int before it *)
            | (SUB::cs, INT(a)::INT(b)::svs) -> processor (cs::clt) ((INT(b-a)::svs)::tl) env scope
            (* VARIABLES *)
            | (SUB::cs, NAME(a)::INT(b)::svs) -> 
                (match envSwitcher a env with
                | Some(INT(a)) -> processor (cs::clt) ((INT(b - a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope)
            | (SUB::cs, INT(a)::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(INT(b)) -> processor (cs::clt) ((INT(b - a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope)
            | (SUB::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(a)), Some(INT(b))) -> processor (cs::clt) ((INT(b - a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (SUB::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* mul has same logic as add, except it multiplies *)
            | (MUL::cs, INT(a)::INT(b)::svs) -> processor (cs::clt) ((INT(a*b)::svs)::tl) env scope
            (* VARIABLES *)
            | (MUL::cs, NAME(a)::INT(b)::svs) -> 
                (match envSwitcher a env with
                | Some(INT(a)) -> processor (cs::clt) ((INT(b * a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope)
            | (MUL::cs, INT(a)::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(INT(b)) -> processor (cs::clt) ((INT(b * a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope)
            | (MUL::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(a)), Some(INT(b))) -> processor (cs::clt) ((INT(b * a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (MUL::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* div first checks if the top of the stack is 0, preventing divide by 0 *)
            | (DIV::cs, INT(0)::INT(b)::svs) -> processor (cs::clt) ((ERROR(":error:")::INT(0)::INT(b)::svs)::tl) env scope
            (* div then divides the top of the stack into the int before it *)
            | (DIV::cs, INT(a)::INT(b)::svs) -> processor (cs::clt) ((INT(b/a)::svs)::tl) env scope
            (* VARIABLES *)
            | (DIV::cs, NAME(a)::INT(b)::svs) -> 
                (match envSwitcher a env with
                | Some(INT(0)) -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope
                | Some(INT(a)) -> processor (cs::clt) ((INT(b / a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope)
            | (DIV::cs, INT(a)::NAME(b)::svs) -> 
                if a == 0 then 
                    processor (cs::clt) ((ERROR(":error:")::INT(0)::NAME(b)::svs)::tl) env scope
                else
                    (match envSwitcher b env with
                    | Some(INT(b)) -> processor (cs::clt) ((INT(b / a)::svs)::tl) env scope
                    | _ -> processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope)
            | (DIV::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(0)), _) -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope
                | (Some(INT(a)), Some(INT(b))) -> processor (cs::clt) ((INT(b / a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (DIV::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* rem works similar to div *)
            | (REM::cs, INT(0)::INT(b)::svs) -> processor (cs::clt) ((ERROR(":error:")::INT(0)::INT(b)::svs)::tl) env scope
            | (REM::cs, INT(a)::INT(b)::svs) -> processor (cs::clt) ((INT(b mod a)::svs)::tl) env scope
            (* VARIABLES *)
            | (REM::cs, NAME(a)::INT(b)::svs) -> 
                (match envSwitcher a env with
                | Some(INT(0)) -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope
                | Some(INT(a)) -> processor (cs::clt) ((INT(b mod a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope)
            | (REM::cs, INT(a)::NAME(b)::svs) -> 
                if a == 0 then 
                    processor (cs::clt) ((ERROR(":error:")::INT(0)::NAME(b)::svs)::tl) env scope
                else
                    (match envSwitcher b env with
                    | Some(INT(b)) -> processor (cs::clt) ((INT(b mod a)::svs)::tl) env scope
                    | _ -> processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope)
            | (REM::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(0)), _) -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope
                | (Some(INT(a)), Some(INT(b))) -> processor (cs::clt) ((INT(b mod a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (REM::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* neg first looks for 0, keeping it the same *)
            | (NEG::cs, INT(0)::svs) -> processor (cs::clt) ((INT(0)::svs)::tl) env scope
            (* a is a guaranteed int, so -a gives its negation *)
            | (NEG::cs, INT(a)::svs) -> processor (cs::clt) ((INT(-a)::svs)::tl) env scope
            (* VARIABLE *)
            | (NEG::cs, NAME(a)::svs) -> 
                (match envSwitcher a env with
                | Some(INT(a)) -> processor (cs::clt) ((INT(-a)::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::svs)::tl) env scope)
            | (NEG::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* push just keeps the processor going, pushCheck does all the work *)
            | (PUSH(sv)::cs, s) -> processor (cs::clt) ((pushCheck sv s)::tl) env scope
                
                
    
            (* pop continues processor with whatever's after the top of the stack *)
            | (POP::cs, a::svs) -> processor (cs::clt) (svs::tl) env scope
            (* if the stack is empty, push :error: *)
            | (POP::cs, []) -> processor (cs::clt) ((ERROR(":error:")::[])::tl) env scope
    
            (* swap switches the top of the stack with whatever value comes before it *)
            | (SWAP::cs, a::b::svs) -> processor (cs::clt) ((b::a::svs)::tl) env scope
            | (SWAP::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* println works in tandem with tostring, and only strings can be written *)
            | (PRINTLN::cs, STRING(a)::svs) -> file_write a; processor (cs::clt) (svs::tl) env scope
            | (PRINTLN::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* tostring has to stringify every stackValue type *)
            | (TOSTRING::cs, INT(a)::svs) -> processor (cs::clt) ((STRING(string_of_int a)::svs)::tl) env scope
            | (TOSTRING::cs, BOOL(":true:")::svs) -> processor (cs::clt) ((STRING(":true:")::svs)::tl) env scope
            | (TOSTRING::cs, BOOL(":false:")::svs) -> processor (cs::clt) ((STRING(":false:")::svs)::tl) env scope
            | (TOSTRING::cs, ERROR(":error:")::svs) -> processor (cs::clt) ((STRING(":error:")::svs)::tl) env scope
            | (TOSTRING::cs, UNIT(":unit:")::svs) -> processor (cs::clt) ((STRING(":unit:")::svs)::tl) env scope
            (* used sub again to remove the double quotes at the start and end of the string *)
            | (TOSTRING::cs, STRING(a)::svs) -> processor (cs::clt) ((STRING(String.sub a 1 ((String.length a) - 2))::svs)::tl) env scope
            | (TOSTRING::cs, NAME(a)::svs) -> processor (cs::clt) ((STRING(a)::svs)::tl) env scope
            | (TOSTRING::cs, CLOSURE(isIO, arg, cmds, envList)::svs) -> processor (cs::clt) ((STRING(":fun:")::svs)::tl) env scope
            | (TOSTRING::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* cat strips the quotes of the string using sub function in order for printing to work properly *)
            | (CAT::cs, STRING(a)::STRING(b)::svs) -> processor (cs::clt) ((STRING((String.sub b 0 ((String.length b) - 1)) ^ (String.sub a 1 ((String.length a) - 1)))::svs)::tl) env scope
            (* VARIABLES *)
            | (CAT::cs, NAME(a)::STRING(b)::svs) -> 
                (match envSwitcher a env with
                | Some(STRING(a)) -> processor (cs::clt) ((STRING((String.sub b 0 ((String.length b) - 1)) ^ (String.sub a 1 ((String.length a) - 1)))::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::STRING(b)::svs)::tl) env scope)
            | (CAT::cs, STRING(a)::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(STRING(b)) -> processor (cs::clt) ((STRING((String.sub b 0 ((String.length b) - 1)) ^ (String.sub a 1 ((String.length a) - 1)))::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::STRING(a)::NAME(b)::svs)::tl) env scope)
            | (CAT::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(STRING(a)), Some(STRING(b))) -> processor (cs::clt) ((STRING((String.sub b 0 ((String.length b) - 1)) ^ (String.sub a 1 ((String.length a) - 1)))::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (CAT::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* and computes logical and only with bool values *)
            | (AND::cs, BOOL(":true:")::BOOL(":true:")::svs) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
            | (AND::cs, BOOL(":false:")::BOOL(":true:")::svs) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
            | (AND::cs, BOOL(":true:")::BOOL(":false:")::svs) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
            | (AND::cs, BOOL(":false:")::BOOL(":false:")::svs) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
            (* VARIABLES *)
            | (AND::cs, NAME(a)::BOOL(":true:")::svs) -> 
                (match envSwitcher a env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::BOOL(":true:")::svs)::tl) env scope)
            | (AND::cs, NAME(a)::BOOL(":false:")::svs) -> 
                (match envSwitcher a env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::BOOL(":false:")::svs)::tl) env scope)
            | (AND::cs, BOOL(":true:")::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::BOOL(":true:")::NAME(b)::svs)::tl) env scope)
            | (AND::cs, BOOL(":false:")::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::BOOL(":false:")::NAME(b)::svs)::tl) env scope)
            | (AND::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(BOOL(":true:")), Some(BOOL(":true:"))) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | (Some(BOOL(":true:")), Some(BOOL(":false:"))) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | (Some(BOOL(":false:")), Some(BOOL(":true:"))) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | (Some(BOOL(":false:")), Some(BOOL(":false:"))) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (AND::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* or computes logical or only with bool values *)
            | (OR::cs, BOOL(":true:")::BOOL(":true:")::svs) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
            | (OR::cs, BOOL(":false:")::BOOL(":true:")::svs) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
            | (OR::cs, BOOL(":true:")::BOOL(":false:")::svs) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
            | (OR::cs, BOOL(":false:")::BOOL(":false:")::svs) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
            (* VARIABLES *)
            | (OR::cs, NAME(a)::BOOL(":true:")::svs) -> 
                (match envSwitcher a env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::BOOL(":true:")::svs)::tl) env scope)
            | (OR::cs, NAME(a)::BOOL(":false:")::svs) -> 
                (match envSwitcher a env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::BOOL(":false:")::svs)::tl) env scope)
            | (OR::cs, BOOL(":true:")::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::BOOL(":true:")::NAME(b)::svs)::tl) env scope)
            | (OR::cs, BOOL(":false:")::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::BOOL(":false:")::NAME(b)::svs)::tl) env scope)
            | (OR::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(BOOL(":true:")), Some(BOOL(":true:"))) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | (Some(BOOL(":true:")), Some(BOOL(":false:"))) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | (Some(BOOL(":false:")), Some(BOOL(":true:"))) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | (Some(BOOL(":false:")), Some(BOOL(":false:"))) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope)
            | (OR::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* not computes logical not only with bool values *)
            | (NOT::cs, BOOL(":true:")::svs) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
            | (NOT::cs, BOOL(":false:")::svs) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
            (* VARIABLE *)
            | (NOT::cs, NAME(a)::svs) ->
                (match envSwitcher a env with
                | Some(BOOL(":true:")) -> processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | Some(BOOL(":false:")) -> processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::svs)::tl) env scope)
            | (NOT::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* equal assesses integer equality *)
            | (EQUAL::cs, INT(a)::INT(b)::svs) -> 
                if b = a then
                    processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                else if b <> a then
                    processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                else
                    processor (cs::clt) ((ERROR(":error:")::svs)::tl) env scope
            (* VARIABLES *)
            | (EQUAL::cs, NAME(a)::INT(b)::svs) ->
                (match envSwitcher a env with
                | Some(INT(a)) -> 
                    if b = a then
                        processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                    else 
                        processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ ->
                        processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope) 
            | (EQUAL::cs, INT(a)::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(INT(b)) -> 
                    if b = a then
                        processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                    else 
                        processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ ->
                        processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope) 
            | (EQUAL::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(a)), Some(INT(b))) -> 
                    if b = a then
                        processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                    else 
                        processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ ->
                        processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope) 
            | (EQUAL::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* less than assesses if the top of the stack is greater than the int before it *)
            | (LESSTHAN::cs, INT(a)::INT(b)::svs) -> 
                if b < a then
                    processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                else if b >= a then
                    processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                else
                    processor (cs::clt) ((ERROR(":error:")::svs)::tl) env scope
            (* VARIABLES *)
            | (LESSTHAN::cs, NAME(a)::INT(b)::svs) -> 
                (match envSwitcher a env with
                | Some(INT(a)) -> 
                    if b < a then
                        processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                    else 
                        processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ ->
                        processor (cs::clt) ((ERROR(":error:")::NAME(a)::INT(b)::svs)::tl) env scope) 
            | (LESSTHAN::cs, INT(a)::NAME(b)::svs) -> 
                (match envSwitcher b env with
                | Some(INT(b)) -> 
                    if b < a then
                        processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                    else 
                        processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ ->
                        processor (cs::clt) ((ERROR(":error:")::INT(a)::NAME(b)::svs)::tl) env scope) 
            | (LESSTHAN::cs, NAME(a)::NAME(b)::svs) -> 
                (match (envSwitcher a env, envSwitcher b env) with
                | (Some(INT(a)), Some(INT(b))) -> 
                    if b < a then
                        processor (cs::clt) ((BOOL(":true:")::svs)::tl) env scope
                    else 
                        processor (cs::clt) ((BOOL(":false:")::svs)::tl) env scope
                | _ ->
                        processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope) 
            | (LESSTHAN::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* bind adds NAME-stackValue pairs in tuples to the current env *)
            | (BIND::cs, INT(a)::NAME(b)::svs) -> 
                (match env with
                | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), INT(a))::ehd)::etl) scope
                | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), INT(a))::[])::[]) scope)
            | (BIND::cs, BOOL(":true:")::NAME(b)::svs) -> 
                (match env with
                | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), BOOL(":true:"))::ehd)::etl) scope
                | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), BOOL(":true:"))::[])::[]) scope)
            | (BIND::cs, BOOL(":false:")::NAME(b)::svs) -> 
                (match env with
                | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), BOOL(":false:"))::ehd)::etl) scope
                | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), BOOL(":false:"))::[])::[]) scope)
            | (BIND::cs, UNIT(":unit:")::NAME(b)::svs) -> 
                (match env with
                | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), UNIT(":unit:"))::ehd)::etl) scope
                | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), UNIT(":unit:"))::[])::[]) scope)
            | (BIND::cs, STRING(a)::NAME(b)::svs) -> 
                (match env with
                | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), STRING(a))::ehd)::etl) scope
                | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), STRING(a))::[])::[]) scope)
            | (BIND::cs, CLOSURE(isIO, param, ccl, cenv)::NAME(b)::svs) -> 
                (match env with
                | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), CLOSURE(isIO, param, ccl, cenv))::ehd)::etl) scope
                | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), CLOSURE(isIO, param, ccl, cenv))::[])::[]) scope)
            (* VARIABLE *)
            | (BIND::cs, NAME(a)::NAME(b)::svs) ->
                (match envSwitcher a env with
                | Some(v) -> 
                    (match env with
                    | ehd::etl -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), v)::ehd)::etl) scope
                    | [] -> processor (cs::clt) ((UNIT(":unit:")::svs)::tl) (((NAME(b), v)::[])::[]) scope)
                | None -> processor (cs::clt) ((ERROR(":error:")::NAME(a)::NAME(b)::svs)::tl) env scope) 
            | (BIND::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
    
            (* if returns the first of 3 elements if the 3rd is true, and the second element if the 3rd is false *)
            | (IF::cs, a::b::BOOL(c)::svs) ->
                (* variables are not sought out in the env list for if *)
                (match c with
                | ":true:" -> processor (cs::clt) ((a::svs)::tl) env scope
                | ":false:" -> processor (cs::clt) ((b::svs)::tl) env scope
                | _ -> processor (cs::clt) ((ERROR(":error:")::a::b::BOOL(c)::svs)::tl) env scope)
            | (IF::cs, a::b::NAME(c)::svs) ->
                (match envSwitcher c env with
                | Some(BOOL(c)) -> 
                    (match c with
                    | ":true:" -> processor (cs::clt) ((a::svs)::tl) env scope
                    | ":false:" -> processor (cs::clt) ((b::svs)::tl) env scope
                    | _ -> processor (cs::clt) ((ERROR(":error:")::a::b::NAME(c)::svs)::tl) env scope)
                | _ -> processor (cs::clt) ((ERROR(":error:")::a::b::NAME(c)::svs)::tl) env scope)
            | (IF::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope
            
            (* let creates a new stack and a new env *)
            | (LET::cs, s) -> processor (cs::clt) ([]::stack) ([]::env) scope
            (* env breaks down the stack and env lists *)
            | (END::cs, s) ->
                (match (stack, env) with
                | (shd::stl, ehd::etl) -> 
                    (* the current env list gets thrown out *)
                    (match (shd, stl) with
                    (* add the head of the current stack to the stack of the outer scope *)
                    | (h::_, prev::stlt) -> processor (cs::clt) ((h::prev)::stlt) etl scope
                    (* if the outer stack has nothing in it, add the head to an empty list list *)
                    | (h::_, []) -> processor (cs::clt) [[h]] etl scope
                    (* if there's nothing to add, throw out the current stack *)
                    | ([], _) -> processor (cs::clt) stl etl scope)
                | _ -> processor (cs::clt) ((ERROR(":error:")::hd)::tl) env scope)

            | (FUN(name, arg)::cs, s) -> 
                (* adds a scope and generates commands within the function *)
                let stacks = funCommands cs [] (scope+1) (scope+1) in
                (* creates closure *)
                let closure =
                    (match (stacks, arg) with
                    | ((fcl,rcl), NAME(arg)) -> CLOSURE(BOOL(":false:"), NAME(arg), fcl, env)
                    | _ -> ERROR(":error:")) 
                in
                (match stacks with
                | (fcl, rcl) -> 
                    (* adds binding to env and increments scope*)
                    (match name, env with
                    | NAME(name), [] -> processor (rcl::clt) ((UNIT(":unit:")::hd)::tl) (((NAME(name), closure)::[])::[]) (scope+1)
                    | NAME(name), ehd::etl -> processor (rcl::clt) ((UNIT(":unit:")::hd)::tl) (((NAME(name), closure)::ehd)::etl) (scope+1)
                    | _ -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope))
            
            | (FUNEND::cs, shd::stl) -> 
                (match stack, env with
                | shdh::stlt, envh::envt ->
                    (* check if function is normal or in/out *)
                    (match funCheck "isIO" envh with 
                    | Some(BOOL(":true:")) -> 
                        (* grab the arg and parameter from the environment *)
                        (match getVar envh with
                        | (arg, param) -> 
                            (* get the most recent value of the parameter *)
                            let updated = envLookup param envh in 
                            (match updated with
                            | Some(v) -> 
                                (* assign the argument passed into the function to the new value *)
                                let newBind = (NAME(arg), v) in
                                (match envt with
                                (* drop the scope after ending the function, and put the new binding into the previous env *)
                                | envth::envtt -> processor clt stlt ((newBind::envth)::envtt) (scope-1)
                                | [] -> processor clt stlt ((newBind::[])::[]) (scope-1))
                            | None -> ()))
                    | Some(BOOL(":false:")) -> 
                        (* if just ending the regular function, drop scope *)
                        (match stlt with
                        | tlh::tlt -> processor clt tlt envt (scope-1)  
                        | [] -> processor clt [] envt (scope-1))
                    | _ -> ()) 
                | _ -> ())
            | (FUNEND::cs, []) -> 
                (match env with
                | envh::envt ->
                    (* check if function is normal or in/out *)
                    (match funCheck "isIO" envh with 
                    | Some(BOOL(":true:")) -> 
                        (* grab the arg and parameter from the environment *)
                        (match getVar envh with
                        | (arg, param) -> 
                            (* get the most recent value of the parameter *)
                            let updated = envLookup param envh in 
                            (match updated with
                            | Some(v) -> 
                                (* assign the argument passed into the function to the new value *)
                                let newBind = (NAME(arg), v) in
                                (match envt with
                                (* drop the scope after ending the function, and put the new binding into the previous env *)
                                | envth::envtt -> processor clt tl ((newBind::envth)::envtt) (scope-1)
                                | [] -> processor clt tl ((newBind::[])::[]) (scope-1))
                            | None -> ()))
                    | Some(BOOL(":false:")) -> 
                        (* if just ending the regular function, drop scope *)
                        (match tl with
                        | tlh::tlt -> processor clt tlt envt (scope-1)  
                        | [] -> processor clt [] envt (scope-1))
                    | _ -> ()) 
                | _ -> ())
                (* (match env with
                | envh::envt -> processor clt tl envt scope  
                | _ -> ()) *)
            
            | (CALL::cs, NAME(arg)::NAME(func)::svs) -> 
                (match (envSwitcher func env, envSwitcher arg env) with
                    | (Some(CLOSURE(isIO1, param1, ccl1, cenv1)), Some(CLOSURE(isIO2, param2, ccl2, cenv2))) -> 
                        (* bind the parameter to the argument passed in *)
                        let argBind = (param1, CLOSURE(isIO2, param2, ccl2, cenv2)) in
                        (* bind the argument to its value *)
                        let paramBind = (NAME(arg), CLOSURE(isIO2, param2, ccl2, cenv2)) in
                        (* load the type of function *)
                        let ioCheck = (NAME("isIO"), isIO1) in
                        (match cenv1 with
                        | cenvh::cenvt -> processor (ccl1::cs::clt) ([]::svs::tl) ((argBind::paramBind::ioCheck::cenvh)::env) scope
                        | [] -> processor (ccl1::cs::clt) ([]::svs::tl) ([argBind;paramBind;ioCheck]::env) scope)
                    | (Some(CLOSURE(isIO, param, ccl, cenv)), Some(INT(a))) -> 
                        let argBind = (param, INT(a)) in
                        let paramBind = (NAME(arg), INT(a)) in
                        let ioCheck = (NAME("isIO"), isIO) in
                        (match cenv with
                        | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::paramBind::ioCheck::cenvh)::env) scope
                        | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;paramBind;ioCheck]::env) scope)
                    | (Some(CLOSURE(isIO, param, ccl, cenv)), Some(STRING(a))) -> 
                        let argBind = (param, STRING(a)) in
                        let paramBind = (NAME(arg), STRING(a)) in
                        let ioCheck = (NAME("isIO"), isIO) in
                        (match cenv with
                        | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::paramBind::ioCheck::cenvh)::env) scope
                        | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;paramBind;ioCheck]::env) scope)
                    | (Some(CLOSURE(isIO, param, ccl, cenv)), Some(BOOL(a))) -> 
                        let argBind = (param, BOOL(a)) in
                        let paramBind = (NAME(arg), BOOL(a)) in
                        let ioCheck = (NAME("isIO"), isIO) in
                        (match cenv with
                        | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::paramBind::ioCheck::cenvh)::env) scope
                        | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;paramBind;ioCheck]::env) scope)
                    | (Some(CLOSURE(isIO, param, ccl, cenv)), Some(UNIT(a))) -> 
                        let argBind = (param, UNIT(a)) in
                        let paramBind = (NAME(arg), UNIT(a)) in
                        let ioCheck = (NAME("isIO"), isIO) in
                        (match cenv with
                        | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::paramBind::ioCheck::cenvh)::env) scope
                        | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;paramBind;ioCheck]::env) scope)
                    | _ ->
                            processor (cs::clt) ((ERROR(":error:")::NAME(arg)::NAME(func)::svs)::tl) env scope) 
            | (CALL::cs, INT(arg)::NAME(func)::svs) -> 
                (match envSwitcher func env with
                | Some(CLOSURE(isIO, param, ccl, cenv)) -> 
                    let argBind = (param, INT(arg)) in
                    let ioCheck = (NAME("isIO"), isIO) in
                    (match cenv with
                    | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                    | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)
                | _ -> processor (cs::clt) ((ERROR(":error:")::INT(arg)::NAME(func)::svs)::tl) env scope)
                    
            | (CALL::cs, STRING(arg)::NAME(func)::svs) -> 
                (match envSwitcher func env with
                | Some(CLOSURE(isIO, param, ccl, cenv)) -> 
                    let argBind = (param, STRING(arg)) in
                    let ioCheck = (NAME("isIO"), isIO) in
                    (match cenv with
                    | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                    | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)
                | _ -> processor (cs::clt) ((ERROR(":error:")::STRING(arg)::NAME(func)::svs)::tl) env scope)

            | (CALL::cs, BOOL(arg)::NAME(func)::svs) -> 
                (match envSwitcher func env with
                | Some(CLOSURE(isIO, param, ccl, cenv)) -> 
                    let argBind = (param, BOOL(arg)) in
                    let ioCheck = (NAME("isIO"), isIO) in
                    (match cenv with
                    | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                    | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)
                | _ -> processor (cs::clt) ((ERROR(":error:")::BOOL(arg)::NAME(func)::svs)::tl) env scope)
            
            | (CALL::cs, UNIT(arg)::NAME(func)::svs) -> 
                (match envSwitcher func env with
                | Some(CLOSURE(isIO, param, ccl, cenv)) -> 
                    let argBind = (param, UNIT(arg)) in
                    let ioCheck = (NAME("isIO"), isIO) in
                    (match cenv with
                    | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                    | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)
                | _ -> processor (cs::clt) ((ERROR(":error:")::UNIT(arg)::NAME(func)::svs)::tl) env scope)

            | (CALL::cs, INT(arg)::CLOSURE(isIO, param, ccl, cenv)::svs) -> 
                let argBind = (param, INT(arg)) in
                let ioCheck = (NAME("isIO"), isIO) in
                (match cenv with
                | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)

            | (CALL::cs, STRING(arg)::CLOSURE(isIO, param, ccl, cenv)::svs) -> 
                let argBind = (param, STRING(arg)) in
                let ioCheck = (NAME("isIO"), isIO) in
                (match cenv with
                | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)

            | (CALL::cs, BOOL(arg)::CLOSURE(isIO, param, ccl, cenv)::svs) -> 
                let argBind = (param, BOOL(arg)) in
                let ioCheck = (NAME("isIO"), isIO) in
                (match cenv with
                | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)


            | (CALL::cs, UNIT(arg)::CLOSURE(isIO, param, ccl, cenv)::svs) -> 
                let argBind = (param, UNIT(arg)) in
                let ioCheck = (NAME("isIO"), isIO) in
                (match cenv with
                | cenvh::cenvt -> processor (ccl::cs::clt) ([]::svs::tl) ((argBind::ioCheck::cenvh)::env) scope
                | [] -> processor (ccl::cs::clt) ([]::svs::tl) ([argBind;ioCheck]::env) scope)

            | (CALL::cs, s) -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope

            | (RETURN::cs, shd::stl) -> 
                (match stack, env with
                | shdh::stlt, envh::envt ->
                    (* check the type of the function *)
                    (match funCheck "isIO" envh with 
                    | Some(BOOL(":true:")) -> 
                        (match stlt with
                        | tlh::tlt -> 
                            (match getVar envh with
                            | (arg, param) -> 
                                let updated = envLookup param envh in 
                                (match updated with
                                | Some(v) -> 
                                    let newBind = (NAME(arg), v) in
                                    (match envt with
                                    | envth::envtt -> 
                                        (* same logic as FUNEND, except now add the head of the stack to the previous stack *)
                                        (match shd with
                                        | NAME(n) -> 
                                            (match envSwitcher n env with
                                            | Some(v) -> processor clt ((v::tlh)::tlt) ((newBind::envth)::envtt) (scope-1)
                                            | _ -> ())
                                        | _ -> processor clt ((shd::tlh)::tlt) ((newBind::envth)::envtt) (scope-1))
                                    | [] -> processor clt ((shd::tlh)::tlt) ((newBind::[])::[]) (scope-1))
                                (* updated should always be something, worst case it'll be the original param binding *)
                                | None -> ()))
                        | [] -> processor clt ((shd::[])::[]) envt scope) 
                    | Some(BOOL(":false:")) -> 
                        (match shd with
                        | NAME(v) -> 
                            (match envSwitcher v env with 
                            | Some(sv) -> 
                                (* just add head of stack to previous with regular functions *)
                                (match stlt with
                                | tlh::tlt -> processor clt ((sv::tlh)::tlt) envt (scope-1)  
                                | [] -> processor clt ((shd::[])::[]) envt (scope-1))
                            | _ -> processor (cs::clt) ((ERROR(":error:")::stl)::[]) envt scope)
                        | sv -> 
                            (match stlt with
                            | tlh::tlt -> processor clt ((sv::tlh)::tlt) envt (scope-1)
                            | [] -> processor clt ((shd::[])::[]) envt (scope-1)))
                    | _ -> processor (cs::clt) ((ERROR(":error:")::stl)::[]) envt scope)
                | _ ->  processor (cs::clt) (stl::tl) env scope)

            | (RETURN::cs, []) -> 
                (match env with
                | envh::envt ->
                    (* check if function is normal or in/out *)
                    (match funCheck "isIO" envh with 
                    | Some(BOOL(":true:")) -> 
                        (* grab the arg and parameter from the environment *)
                        (match getVar envh with
                        | (arg, param) -> 
                            (* get the most recent value of the parameter *)
                            let updated = envLookup param envh in 
                            (match updated with
                            | Some(v) -> 
                                (* assign the argument passed into the function to the new value *)
                                let newBind = (NAME(arg), v) in
                                (match envt with
                                (* drop the scope after ending the function, and put the new binding into the previous env *)
                                | envth::envtt -> processor clt tl ((newBind::envth)::envtt) (scope-1)
                                | [] -> processor clt tl ((newBind::[])::[]) (scope-1))
                            | None -> ()))
                    | Some(BOOL(":false:")) -> 
                        (* if just ending the regular function, drop scope *)
                        (match tl with
                        | tlh::tlt -> processor clt tlt envt (scope-1)  
                        | [] -> processor clt [] envt (scope-1))
                    | _ -> ()) 
                | _ -> ())
                (* (match env with
                | envh::envt -> processor clt tl envt (scope-1)  
                | _ -> ()) *)

            | (INOUTFUN(name, arg)::cs, s) -> 
                (* same as regular fun, return is where they differ *)
                let stacks = funCommands cs [] (scope+1) (scope+1) in
                let closure =
                    (match (stacks, arg) with
                    | ((fcl,rcl), NAME(arg)) -> CLOSURE(BOOL(":true:"), NAME(arg), fcl, env)
                    | _ -> ERROR(":error:")) 
                in
                (match stacks with
                | (fcl, rcl) -> 
                    (match name, env with
                    | NAME(name), [] -> processor (rcl::clt) ((UNIT(":unit:")::hd)::tl) (((NAME(name), closure)::[])::[]) (scope+1)
                    | NAME(name), ehd::etl -> processor (rcl::clt) ((UNIT(":unit:")::hd)::tl) (((NAME(name), closure)::ehd)::etl) (scope+1)
                    | _ -> processor (cs::clt) ((ERROR(":error:")::s)::tl) env scope))

            (* quit just ends everything, and processor has a type of unit *)
            | (QUIT::cs, _) -> ()
    
            (* empty command list *)
            | ([], _) -> (
                (match env with
                | envh::envt -> processor clt tl envt scope
                | [] -> processor clt tl [] scope)))
        | _ -> ()
        
    in

    (* initialize an empty stack and empty env in list lists *)
    processor [comList] [[]] [[]] 0
;;
    
(* interpreter ("input1.txt", "output0.txt");; *)
