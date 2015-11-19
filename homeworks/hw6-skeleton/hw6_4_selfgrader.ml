open CommonGrade
open Hw6_4

let _ = output (fun () ->
  (eval (IMPLY (ORELSE (FALSE, LESS (NUM 0, (PLUS (NUM (-1), NUM 0)))), FALSE))))

let _ = output (fun () -> eval (IMPLY(FALSE, FALSE)))
let _ = output (fun () -> eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE)))))
let _ = output (fun () -> (eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE)))))
let _ = output (fun () -> (eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE)))))
let _ = output (fun () -> (eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE))))
let _ = output (fun () -> eval (LESS (NUM 3, NUM 5)))
let _ = output (fun () -> eval (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2))))
let _ = output (fun () -> eval (LESS (MINUS (NUM 3, NUM 5), MINUS (NUM 1, NUM 2))))
let _ = output (fun () -> eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE)))
let _ = output (fun () -> eval (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1))))))