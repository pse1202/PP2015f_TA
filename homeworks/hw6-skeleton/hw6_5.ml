exception TODO

 type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (f: ae) (x: string): ae =
	match f with
	| VAR y -> if x<>y then CONST 0 else  CONST 1
	| POWER (y,i) -> if x<>y then CONST 0 else TIMES([CONST i;POWER(y,i-1)])
	| TIMES (h::t) -> SUM ([TIMES([diff h x;TIMES t]);TIMES([h;diff (TIMES t) x])])
	| SUM (h::t) -> SUM([diff h x;diff (SUM t) x])
	| _ -> CONST 0
