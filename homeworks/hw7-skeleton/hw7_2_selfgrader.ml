open CommonGrade
open Hw7_2

let test1 = SkiLiquid.M ((SkiLiquid.M ((SkiLiquid.M (SkiLiquid.I, SkiLiquid.S)), SkiLiquid.K)), SkiLiquid.S)
let test2_1 = SkiLiquid.M (SkiLiquid.I, (SkiLiquid.M (SkiLiquid.M (SkiLiquid.V "CSEseminar", SkiLiquid.K), SkiLiquid.I)))
let test2_2 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.V "eec", SkiLiquid.I), SkiLiquid.M (SkiLiquid.K, (SkiLiquid.V "pl")))
let test2_3 = SkiLiquid.M (SkiLiquid.V "physlab", SkiLiquid.K)
let test2 = SkiLiquid.M (test2_1, SkiLiquid.M (test2_2, test2_3))

let _ = output (fun () -> 
"x" = 
(SkiLiquid.pprint 
(SkiLiquid.react 
(SkiLiquid.M 
(SkiLiquid.M (SkiLiquid.M (SkiLiquid.S,SkiLiquid.K),
SkiLiquid.I),
SkiLiquid.V "x"))))
)

let _ = output (fun () -> 
"x" = 
(SkiLiquid.pprint
(SkiLiquid.react 
(SkiLiquid.M 
(SkiLiquid.M (SkiLiquid.K,(SkiLiquid.V "x")),
SkiLiquid.M (SkiLiquid.I,(SkiLiquid.V "x"))))))
)

let _ = output (fun () ->
"(((x y) z) w)" =
(SkiLiquid.pprint
(SkiLiquid.M 
(SkiLiquid.M 
(SkiLiquid.M (SkiLiquid.V "x",SkiLiquid.V "y"),
SkiLiquid.V "z"),
SkiLiquid.V "w")))
)

let _ = output (fun () ->
"(((CSEseminar K) I) (((eec I) (K pl)) (physlab K)))" =
(SkiLiquid.pprint (SkiLiquid.react test2))
)

let test3 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.I, SkiLiquid.M (SkiLiquid.K, SkiLiquid.S)),
SkiLiquid.M (SkiLiquid.M (SkiLiquid.S, SkiLiquid.I), SkiLiquid.I)), SkiLiquid.S), SkiLiquid.S)

let _ = output (fun () ->
"((S S) S)" =
(SkiLiquid.pprint (SkiLiquid.react test3))
)

let test4 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.K), SkiLiquid.M (SkiLiquid.V "swpp", SkiLiquid.M (SkiLiquid.S, SkiLiquid.M (SkiLiquid.K, SkiLiquid.S)))), SkiLiquid.K)

let _ = output (fun () ->
"(K K)" =
(SkiLiquid.pprint (SkiLiquid.react test4))
)

let test5 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.I, SkiLiquid.K), SkiLiquid.S), SkiLiquid.I)

let _ = output (fun () ->
"S" =
(SkiLiquid.pprint (SkiLiquid.react test5))
)

module Ski = SkiLiquid

let _ = output (fun () ->
  "S" =
    (Ski.pprint 
    (Ski.S))
)
let _ = output (fun () ->
  "S" =
    (Ski.pprint 
    (Ski.react
    (Ski.S)))
)

let _ = output (fun () ->
  "(Hey Jude)" =
    (Ski.pprint 
    (Ski.react
      (Ski.M 
        ((Ski.M 
          ((Ski.M 
            ((Ski.M (Ski.S ,(Ski.M (Ski.K, (Ski.M (Ski.S,Ski.I)))))), 
            Ski.K)),
          (Ski.V "Jude"))), 
        (Ski.V "Hey"))))
))

let _ = output (fun () ->
  "((S K) S)" =
  (Ski.pprint 
    (Ski.react
      (Ski.M 
        ((Ski.I ,
          (Ski.M 
            ((Ski.M 
              ((Ski.M (Ski.I, Ski.S)) ,
            Ski.K), 
        Ski.S )) 
)))))))

let _ = output (fun () ->
  "y" =
  (Ski.pprint 
    (Ski.react
  (Ski.M (Ski.M ((Ski.M (Ski.S,Ski.K)), Ski.I), Ski.V "y"))
)))

let _ = output (fun () ->
  "K" =
  (Ski.pprint 
    (Ski.react
      (Ski.M (Ski.M ((Ski.M (Ski.S,Ski.K)), Ski.I), Ski.K))
)))

let _ = output (fun () ->
  "((K ILL) (B ILL))" =
  (Ski.pprint 
    (Ski.react
      (Ski.M (Ski.M ((Ski.M (Ski.S,Ski.V "K")), Ski.V "B"), Ski.V "ILL"))
)))

let _ = output (fun () ->
  "(K (I (S S)))" =
  (Ski.pprint 
    (Ski.react
      (Ski.M (Ski.M (Ski.M (Ski.M (Ski.I,Ski.K), Ski.K), (Ski.M (Ski.S,Ski.S))), (Ski.M (Ski.V "I", (Ski.M (Ski.S, Ski.S))))))
)))

let _ = output (fun () ->
  "((up down) (up down))" =
  (Ski.pprint 
    (Ski.react
     (Ski.M ((Ski.M ((Ski.M (Ski.S, (Ski.M (Ski.I,Ski.I)))), Ski.I)), (Ski.M (Ski.M (Ski.I,Ski.I), (Ski.M (Ski.V "up",Ski.V "down"))))))
)))

let _ = output (fun () ->
  "((I SEOUL) U)" =
  (Ski.pprint 
    (Ski.react
   (Ski.M ((Ski.M ((Ski.M ( (Ski.M ((Ski.I, (Ski.M (Ski.K, Ski.V "I"))))), (Ski.M ((Ski.M (Ski.S, Ski.I)), Ski.I) ))), Ski.V "SEOUL")), Ski.V "U"))
)))