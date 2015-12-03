module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module type ArgTy = 
sig
  type t
  val is_eq : t -> t -> bool
end

module QueueMake (Arg: ArgTy) 
  : Queue with type element = Arg.t =
struct
    type element = Arg.t
    type queue = element list * element list
    exception EMPTY_Q
    let emptyq = ([],[])
    let enq = fun (q,el) -> (el::(fst q) , (snd q))
    let deq = fun q -> 
      match q with
      | ([],[]) -> raise EMPTY_Q
      | (l,[]) -> (List.hd(List.rev(l)),([],List.tl(List.rev(l))))
      | (_,_) -> (List.hd(snd q),((fst q),List.tl(snd q)))
end
