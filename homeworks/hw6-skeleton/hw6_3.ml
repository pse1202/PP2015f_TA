exception TODO

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let rec parenize n =
	match n with
	| LEAF Korea -> "Korea"
	| LEAF France -> "France"
	| LEAF Usa -> "Usa"
	| LEAF Brazil -> "Brazil"
	| LEAF Japan -> "Japan"
	| LEAF Nigeria -> "Nigeria"
	| LEAF Cameroon -> "Cameroon"
	| LEAF Poland -> "Poland"
	| LEAF Portugal -> "Portugal"
	| LEAF Italy -> "Italy"
	| LEAF Germany -> "Germany"
	| LEAF Norway -> "Norway"
	| LEAF Sweden -> "Sweden"
	| LEAF England -> "England"
	| LEAF Argentina -> "Argentina"
	| NODE (x,y) -> "("^(parenize x)^" "^(parenize y)^")"

let rec drop (t: tourna) (d: team): string =
	match t with
	| LEAF d -> ""
	| NODE(LEAF team,rest)|NODE(rest,LEAF team) -> parenize rest
	| NODE(x,y) -> "("^(drop x d)^" "^(drop y d)^")"
	| _ -> parenize t
