val maxMemSize = 64

exception nonInt
exception nonBooleanException
exception incorrectOpcode
exception codeSubscript
exception illformatted
exception nonbracket

fun toInt s =
  (*
   * converts the string to an integer,
   * no non-numeric character other than negative sign
   * allowed in the string,
   *)
  let fun recur [] = 0
    | recur (x::t) =
      let val res = ord(x) - ord(#"0")
      in
        if res < 0 orelse res > 9 then
          raise nonInt
        else res + 10 * (recur t)
      end
  in
    let val l = explode s in
      if (hd l) = #"-" orelse (hd l) = #"~"
      then ~(recur(rev (tl l)))
      else recur(rev l)
    end
  end

fun get_input() = (
  TextIO.output(TextIO.stdOut, "input: ");
  TextIO.flushOut(TextIO.stdOut);
  valOf(Int.fromString (valOf(TextIO.inputLine TextIO.stdIn)))
  )
  (* get a single integer as input from user *)
  (* 
   * can extend this similar to toInt above, for ex. only allow whitespace or
   * integer character but don't think that's needed
   *)

fun output(x:int) = print(Int.toString x ^ "\n")
  (* print a single integer to the console *)

fun printError c s = print("Line " ^ (Int.toString c) ^ ": " ^ s)
  (* prints the error message along with line number *)

fun notBool(x:int) = ((x <> 0) andalso (x <> 1))

fun step (code, c, mem) =
  (*
   * actual engine for running the code,
   * c represents the current line
   *)
  if (c < 0) orelse (c >= Vector.length(code)) then
    raise codeSubscript
    (* raise an error if line doesn't exist *)
  else let val (opc, opd1, opd2, tgt) = Vector.sub(code, c)
  in
    if opc = 0 then ()
    (* halt *)
    else if opc = 14 then step(code, tgt, mem)
    (* goto *)
    else if opc = 13 then (
         (* if cond goto *)
      case Array.sub(mem, opd1) of
           0 => step(code, c + 1, mem)
         | 1 => step(code, tgt, mem)
         | _ => raise nonBooleanException
         (* throw exception for non-boolean values *)
    ) else (
      let fun mov x = Array.update(mem, tgt, x) (* function for mem[k] := *)
      in
        if opc = 1 then mov(get_input()) (* mem[k] := user input *)
        else if opc = 16 then mov(opd1)
        else let val left = Array.sub(mem, opd1)
          (*
           * opcode 2, 3, and 15 access only mem[i]
           * so mem[j] may refer to an invalid location in this case
           *)
        in
          if opc = 15 then output(left)
          else
            mov(
              case opc of
                   2 => left
                        (* mem[k] := mem[i] *)
                 | 3 => if (left = 0) then 1
                        else if (left = 1) then 0
                        else raise nonBooleanException
                        (* mem[k] := not mem[i] *)
                 | _ => (
                   let val right = Array.sub(mem, opd2) in
                     case opc of 
                          4 => if (notBool(left) orelse notBool(right)) then raise nonBooleanException
                               else if ((left = 1) orelse (right = 1)) then 1 else 0
                               (* mem[k] := mem[i] or mem[j] *)
                        | 5 => if (notBool(left) orelse notBool(right)) then raise nonBooleanException
                               else if ((left = 1) andalso (right = 1)) then 1 else 0
                               (* mem[k] := mem[i] and mem[j] *)
                        | 6 => left + right
                        | 7 => left - right
                        | 8 => left * right
                        | 9 => left div right
                        | 10 => left mod right
                        | 11 => if (left = right) then 1 else 0
                        | 12 => if (left > right) then 1 else 0
                        | _ => raise incorrectOpcode
                        (* happens if opcode doesn't lie in [0,16] *)
                   end
                 )
        )
      end
    end
    ; step(code, c + 1, mem)
    )
  end
  handle nonBooleanException => printError c "boolean value expected\n"
    | incorrectOpcode => printError c "illegal opcode encountered\n"
    | codeSubscript => printError c "non-existent line referenced\n"
    | Subscript => printError c "out-of-bounds memory accessed\n"

fun get_quadruple s =
    if (String.sub(s, 0) <> #"(" orelse String.sub(s, (size s) - 1) <> #")")
      then raise nonbracket
    else let
      val l = (String.fields (fn c => c = #",") (substring (s, 1, (size s) - 2)))
      (*
       * substring removes the first and last character (which are brackets)
       * fields splits the string into list of strings wrt , delimiter
       *)
    in
      let val a::b::c::d::[] = map toInt l
      in (a, b, c, d) end
    end
    (*
     * converts a string "(a,b,c,d)" to tuple (a,b,c,d)
     * raising the appropriate exceptions when needed
     *)

fun lines_to_list [""] _ = []
  | lines_to_list (""::t) d = (
        printError d "empty line\n";
        raise illformatted
      )
  | lines_to_list (a::t) d = get_quadruple(a)::(lines_to_list t (d + 1))
  handle nonbracket => (printError d "improper bracketing\n"; raise illformatted)
    | Bind => (printError d "illegal number of operands\n"; raise illformatted)
    | nonInt => (printError d "non-numeric input\n"; raise illformatted)
  (*
   * converts the list of lines in iput text into the code list
   * handling and raising the appropriate exceptions
   *)

fun interpret file =
  let
    val lines = String.fields (fn c => c = #"\n") (TextIO.inputAll(TextIO.openIn file))
    (* fields splits the string wrt the newline character *)
    in
      step(
        Vector.fromList(lines_to_list lines 0),
        0,
        Array.array(maxMemSize, ~1)
      )
  end
  handle illformatted => print("Ill-formatted input\n")
