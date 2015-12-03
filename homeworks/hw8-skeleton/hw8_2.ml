module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringSetQ : Queue with type element = string = 
	struct 
		type element = string
		type queue = element list * element list
		exception EMPTY_Q
		let emptyq = ([],[])
		let enq = fun (q,el) -> let x = (if (List.exists (fun k -> (k = el)) (fst q @ snd q)) then (fst q) else (el::(fst q))) in
		(x , (snd q))
		let deq = fun q -> 
			match q with
			| ([],[]) -> raise EMPTY_Q
			| (l,[]) -> (List.hd(List.rev(l)),([],List.tl(List.rev(l))))
			| (_,_) -> (List.hd(snd q),((fst q),List.tl(snd q)))
	end

let rec queue2list (q:StringSetQ.queue) : string list =
  try let (e,r) = StringSetQ.deq q in
      e::(queue2list r)
  with StringSetQ.EMPTY_Q -> []

module StringSetQQ : Queue with type element = StringSetQ.queue = 
	struct 
		type element = StringSetQ.queue
  		type queue = element list * element list
		exception EMPTY_Q
		let emptyq = ([],[])
		let enq = fun (q,el) -> let x = (if (List.exists (fun k -> (queue2list k = queue2list el)) (fst q @ snd q)) then (fst q) else (el::(fst q))) in
		(x , (snd q))
		let deq = fun q -> 
			match q with
			| ([],[]) -> raise EMPTY_Q
			| (l,[]) -> (List.hd(List.rev(l)),([],List.tl(List.rev(l))))
			| (_,_) -> (List.hd(snd q),((fst q),List.tl(snd q)))
	end