type design = TURTLE | WAVE | DRAGON    (* three design patterns *)
type orientation = NW | NE | SE | SW
type box = BOX of orientation * design | GLUED of box * box * box * box

module type FRAME =
sig
  val box: box
  val rotate: box -> box                (* roatate box clockwise *)
  val pp: box -> int * int -> unit      (* pretty printer *)
  val size: int
end

module BasicFrame (Design: sig val design: design end): FRAME = 
struct 
  exception NON_BASIC_BOX
  let box = BOX (NW, Design.design)     (* a box is defined *)
  let rec rotate (b:box) : box =
    match b with
    | BOX (NW,x) -> BOX (NE,x)
    | BOX (NE,x) -> BOX (SE,x)
    | BOX (SE,x) -> BOX (SW,x)
    | BOX (SW,x) -> BOX (NW,x)
    | _ -> raise NON_BASIC_BOX

  let pp b center = 
    match b with
    | BOX (NW,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (NE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SE,x) -> ()                  (* dummy, fill it if you want *)
    | BOX (SW,x) -> ()                  (* dummy, fill it if you want *)
    | _ -> raise NON_BASIC_BOX
  let size = 1
end

module Rotate (Box:FRAME) : FRAME =
struct 
  let box = Box.rotate Box.box
  let rec rotate (b:box) : box = 
    match b with
    | BOX (NW,x) -> BOX (NE,x)
    | BOX (NE,x) -> BOX (SE,x)
    | BOX (SE,x) -> BOX (SW,x)
    | BOX (SW,x) -> BOX (NW,x)
    | GLUED (nw,ne,se,sw) -> GLUED (rotate sw,rotate nw,rotate ne,rotate se)
  let pp b center = ()                  (* dummy, fill it if you want *)
  let size = Box.size
end

module Glue (Nw:FRAME) (Ne:FRAME) (Se:FRAME) (Sw:FRAME) : FRAME =
struct
  exception DIFFERENT_SIZED_BOXES
  let _ = if not ((Nw.size = Ne.size) && (Ne.size = Se.size) && (Se.size = Sw.size)) then raise DIFFERENT_SIZED_BOXES
  let box = GLUED (Nw.box, Ne.box, Se.box, Sw.box)
  let rotate b = GLUED (Sw.rotate Sw.box,Nw.rotate Nw.box,Ne.rotate Ne.box,Se.rotate Se.box)
  let pp b center = ()
  let size = Nw.size * 2
end
