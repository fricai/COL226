signature STACK = 
sig
  type 'a Stack;
  exception EmptyStack
  exception Error of string
  val create     : unit -> 'a Stack
  val push       : 'a * 'a Stack -> 'a Stack
  val pop        : 'a Stack -> 'a Stack
  val top        : 'a Stack -> 'a
  val empty      : 'a Stack -> bool
  val poptop     : 'a Stack -> ('a * 'a Stack) option
  val nth        : 'a Stack * int -> 'a
  val drop       : 'a Stack * int -> 'a Stack
  val depth      : 'a Stack -> int
  val app        : ('a -> unit) -> 'a Stack -> unit
  val map        : ('a -> 'b) -> 'a Stack -> 'b Stack
  val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
  val find       : ('a -> bool) -> 'a Stack -> 'a option
  val filter     : ('a -> bool) -> 'a Stack -> 'a Stack
  val foldr      : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
  val foldl      : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
  val exists     : ('a -> bool) -> 'a Stack -> bool
  val all        : ('a -> bool) -> 'a Stack -> bool
  val list2stack : 'a list -> 'a Stack
  val stack2list : 'a Stack -> 'a list
  val toString   : ('a -> string) -> 'a Stack -> string
end

structure FunStack :> STACK = 
struct
  type 'a Stack = 'a list
  exception EmptyStack
  exception Error of string
  fun create ()       = []
  fun push (x, lst)   = x::lst
  fun pop lst         = tl lst
    handle Empty => raise EmptyStack
  fun top lst         = hd lst
    handle Empty => raise EmptyStack
  fun empty lst       = null lst
  fun poptop lst      = List.getItem lst

  fun nth (lst, idx)  = List.nth (lst, idx)
    handle Subscript => raise Error "Index out-of-bounds"

  fun drop (lst, idx) = List.drop (lst, idx)
    handle Subscript => raise Error "Index out-of-bounds"

  fun depth lst       = length lst
  fun app f l         = List.app f l
  fun map f l         = List.map f l
  fun mapPartial f l  = List.mapPartial f l
  fun find f l        = List.find f l
  fun filter f l      = List.filter f l
  fun foldr f init l  = List.foldr f init l
  fun foldl f init l  = List.foldl f init l
  fun exists f l      = List.exists f l
  fun all f l         = List.all f l
  fun list2stack lst  = lst
  fun stack2list lst  = lst
  fun toString f lst  = 
    let fun recur l =  case l of
                            [] => "]"
                          | [x] => (f x) ^ "]"
                          | x::xs => (f x) ^ ", " ^ (recur xs)
    in
      "[" ^ (recur lst)
    end
end
