include PrioQueue_intf

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  type t =
    { mutable size : int
    ; mutable data : elt array
    }

  let size pq = if Array.length pq.data = 0 then 0 else pq.size
  let is_empty pq = if size pq = 0 then true else false

  let init ?(n = 16) () =
    match n with
    | i when i <= 0 -> raise (Invalid_argument "invalid queue size")
    | i when i >= 16 -> { size = i; data = [||] }
    | _ -> { size = 16; data = [||] }
  ;;

  let expand pq =
    let realloc q size =
      let new_data = Array.make size q.data.(0) in
      Array.blit q.data 0 new_data 0 (Array.length q.data);
      q.data <- new_data
    in
    match pq.size with
    | n when n = Sys.max_array_length -> raise (Failure "Priority Queue is overflow")
    | n when n > Sys.max_array_length / 5 * 4 -> realloc pq Sys.max_array_length
    | n -> realloc pq (n / 4 * 5)
  ;;

  let swap pq i j =
    let tmp = pq.data.(i) in
    pq.data.(i) <- pq.data.(j);
    pq.data.(j) <- tmp
  ;;

  let rec up_heap pq idx =
    let parent i = (i - 1) / 2 in
    if idx > 0
    then (
      let p_idx = parent idx in
      if Ord.compare pq.data.(idx) pq.data.(p_idx) > 0
      then (
        swap pq idx p_idx;
        up_heap pq p_idx))
  ;;

  let rec down_heap pq idx =
    let lchild i = (2 * i) + 1 in
    let rchild i = lchild i + 1 in
    let lc_idx = lchild idx
    and rc_idx = rchild idx
    and tgt_idx = ref idx in
    if lc_idx < pq.size && Ord.compare pq.data.(lc_idx) pq.data.(!tgt_idx) > 0
    then tgt_idx := lc_idx;
    if rc_idx < pq.size && Ord.compare pq.data.(rc_idx) pq.data.(!tgt_idx) > 0
    then tgt_idx := rc_idx;
    if idx <> !tgt_idx
    then (
      swap pq idx !tgt_idx;
      down_heap pq !tgt_idx)
  ;;

  let insert pq x =
    if Array.length pq.data = 0
    then (
      pq.data <- Array.make pq.size x;
      pq.size <- 1)
    else (
      if Array.length pq.data = pq.size then expand pq;
      pq.data.(pq.size) <- x;
      pq.size <- pq.size + 1;
      up_heap pq (pq.size - 1))
  ;;

  let peek pq =
    if Array.length pq.data = 0 || pq.size <= 0 then raise Not_found;
    pq.data.(0)
  ;;

  let extract pq =
    if Array.length pq.data = 0 then raise Not_found;
    match pq.size with
    | n when n <= 0 -> raise Not_found
    | n when n = 1 ->
      pq.size <- 0;
      pq.data.(0)
    | n ->
      let result = pq.data.(0) in
      pq.data.(0) <- pq.data.(n - 1);
      pq.size <- pq.size - 1;
      down_heap pq 0;
      result
  ;;
end
