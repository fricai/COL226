val maxMemSize = 64;

fun get_input() = (
  TextIO.output(TextIO.stdOut, "input: ");
  TextIO.flushOut(TextIO.stdOut);
  valOf(Int.fromString (valOf(TextIO.inputLine TextIO.stdIn)))
  (* take care of multiple values in line*)
  );

fun output(x:int) = print ("output: " ^ Int.toString x ^ "\n");

fun step (code, c, mem) =
  let val (opc, opd1, opd2, tgt) = Vector.sub(code, c)
  in
    if opc = 0 then ()
    else if opc = 14 then step(code, tgt, mem)
    else if opc = 13 then (
      case Array.sub(mem, opd1) of
           0 => step(code, c + 1, mem)
         | 1 => step(code, tgt, mem)
         | _ => print "non-boolean value"
         (* throw exception for non-boolean values *)
    ) else (
    let
      val left = Array.sub(mem, opd1)
      val right = Array.sub(mem, opd2)
      (* try to access left and right only if needed by operation *)
    in
      if opc = 15 then output(left)
      else
        Array.update(mem, tgt,
          case opc of
               1 => get_input()
             | 2 => left
             | 3 => if (left = 0) then 1 else 0
             | 4 => if ((left = 1) orelse (right = 1)) then 1 else 0
             | 5 => if ((left = 1) andalso (right = 1)) then 1 else 0
             | 6 => left + right
             | 7 => left - right
             | 8 => left * right
             | 9 => left div right
             | 10 => left mod right
             | 11 => if (left = right) then 1 else 0
             | 12 => if (left > right) then 1 else 0
             | 16 => opd1
             | _ => (print "wtf no\n"; 0)
             (* this should be an impossible case, throw a fatal error ig?*)
      )
    end
    ; step(code, c + 1, mem)
    )
  end

fun get_quadruple s =
  let val l = (String.fields (fn c => c = #",") (substring (s, 1, (size s) - 2)))
  in
    let val a::b::c::d::[] = map (fn t => valOf(Int.fromString t)) l
    in (a, b, c, d) end
  end;

fun interpret file =
  let
    val inStream = TextIO.openIn file
    in 
      step(
        Vector.fromList(map get_quadruple (rev (tl (rev (String.fields (fn c:char => c = #"\n") (TextIO.inputAll inStream)))))),
        0,
        Array.array(maxMemSize, 784)
      )
  end;
