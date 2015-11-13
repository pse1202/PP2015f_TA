exception TODO

type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec calc e =
  match e with
  | NUM i -> i
  | PLUS(i,j) -> calc i + calc j
  | MINUS(i,j) -> calc i - calc j
let rec eval (f: formula): bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT x -> not (eval x)
  | ANDALSO(x,y) -> eval x && eval y
  | ORELSE(x,y) -> eval x || eval y
  | IMPLY(x,y) -> not (eval x && not (eval y))
  | LESS (x,y) -> calc x < calc y
