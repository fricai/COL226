structure DATATYPES =
struct
  datatype MemoryStackType  = INT of int | BOOL of bool
  datatype ControlStackType = VAR of string
                            | BOOL of bool
                            | INT of int
                            | SET  | SEQ   | ITE   | WH
                            | AND  | OR    | NOT
                            | LT   | LEQ   | EQ    | GT  | GEQ | NEQ
                            | PLUS | MINUS | TIMES | DIV | MOD
                            | NEGATIVE
                            | READ | WRITE
end
