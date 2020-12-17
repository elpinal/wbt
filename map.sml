signature MAP = sig
  type 'a t
  type key

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val from_list : (key * 'a) list -> 'a t
  val insert : key -> 'a -> 'a t -> 'a t
  val delete : key -> 'a t -> 'a t

  val size : 'a t -> int
  val member : key -> 'a t -> bool
  val lookup : key -> 'a t -> 'a option

  val fold : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val to_asc_list : 'a t -> (key * 'a) list
  val to_desc_list : 'a t -> (key * 'a) list

  exception Empty
  val min : 'a t -> key * 'a (* Empty *)
  val delete_min : 'a t -> 'a t (* Empty *)


  (* wanted *) (*
  val union : 'a t -> 'a t -> 'a t
  val disjoint_union : 'a t -> 'a t -> 'a t
  val fold_eq : ('a * 'b * 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c
  val intersection : ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
  *)
end

functor Map (X : sig
  type t
  val compare : t * t -> order
end) :> MAP where type key = X.t = struct
  type size = int
  type key = X.t

  datatype 'a t
    = Tip
    | Bin of size * key * 'a t * 'a t * 'a

  val empty = Tip

  fun member kx =
    fn Tip => false
     | Bin(_, ky, l, r, _) =>
         case X.compare (kx, ky) of
              LESS    => member kx l
            | GREATER => member kx r
            | EQUAL   => true

  fun lookup kx =
    fn Tip => NONE
     | Bin(_, ky, l, r, v) =>
         case X.compare (kx, ky) of
              LESS    => lookup kx l
            | GREATER => lookup kx r
            | EQUAL   => SOME v

  exception Empty

  fun min Tip = raise Empty
    | min (Bin node) = min (#3 node) handle Empty => (#2 node, #5 node)

  val size =
    fn Tip      => 0
     | Bin node => #1 node

  fun singleton k v = Bin(1, k, Tip, Tip, v)

  fun bin (k, v) l r = Bin(size l + size r + 1, k, l, r, v)

  val delta = 3
  val gamma = 2

  fun isBalanced a b = delta * (size a + 1) >= (size b + 1)

  fun isSingle a b = (size a + 1) < gamma * (size b + 1)

  val rec balanced =
    fn Tip => true
     | Bin(_, _, l, r, _) =>
         isBalanced l r andalso isBalanced r l
         andalso balanced l andalso balanced r

  structure L = struct
    fun singleL kv1 t1 =
      fn Tip => raise Fail "singleL"
       | Bin node => bin (#2 node, #5 node) (bin kv1 t1 (#3 node)) (#4 node)

    fun doubleL kv1 t1 =
      fn Bin(_, k2, (Bin node), t4, v2) => bin (#2 node, #5 node) (bin kv1 t1 (#3 node)) (bin (k2, v2) (#4 node) t4)
       | _ => raise Fail "doubleL"

    fun rotateL kv l =
      fn Tip => raise Fail "rotateL"
       | Bin node =>
           if isSingle (#3 node) (#4 node)
           then singleL kv l (Bin node)
           else doubleL kv l (Bin node)

    fun balanceL kv l r =
      if isBalanced l r
      then bin kv l r
      else rotateL kv l r
  end

  structure R = struct
    fun singleR _ Tip _ = raise Fail "singleR"
      | singleR kv1 (Bin node) t1 = bin (#2 node, #5 node) (#3 node) (bin kv1 (#4 node) t1)

    fun doubleR kv1 (Bin(_, k2, t4, Bin node, v2)) t1 = bin (#2 node, #5 node) (bin (k2, v2) t4 (#3 node)) (bin kv1 (#4 node) t1)
      | doubleR _ _ _ = raise Fail "doubleR"

    fun rotateR _ Tip _ = raise Fail "rotateR"
      | rotateR kv (Bin node) r =
           if isSingle (#4 node) (#3 node)
           then singleR kv (Bin node) r
           else doubleR kv (Bin node) r

    fun balanceR kv l r =
      if isBalanced r l
      then bin kv l r
      else rotateR kv l r
  end

  open L
  open R

  fun insert kx vx =
    fn Tip => singleton kx vx
     | Bin(s, ky, l, r, vy) =>
         case X.compare (kx, ky) of
              LESS    => balanceR (ky, vy) (insert kx vx l) r
            | GREATER => balanceL (ky, vy) l (insert kx vx r)
            | EQUAL   => Bin(s, kx, l, r, vx)

  val rec delete_min =
    fn Tip                => raise Empty
     | Bin(_, k, l, r, v) => balanceL (k, v) (delete_min l) r handle Empty => r

  fun delete kx =
    fn Tip => Tip
     | Bin(_, ky, l, r, vy) =>
         case X.compare (kx, ky) of
              LESS    => balanceL (ky, vy) (delete kx l) r
            | GREATER => balanceR (ky, vy) l (delete kx r)
            | EQUAL   => delete' l r

  and delete' Tip r = r
    | delete' l Tip = l
    | delete' l r = balanceR (min r) l (delete_min r)

  fun fold f acc =
    fn Tip                => acc
     | Bin(_, k, l, r, v) => fold f (f (k, v, fold f acc l)) r

  fun to_asc_list t = fold (fn (k, v, acc) => fn xs => acc ((k, v) :: xs)) (fn xs => xs) t []

  fun to_desc_list t = fold (fn (k, v, acc) => (k, v) :: acc) [] t

  fun from_list xs = foldl (fn ((k, v), acc) => insert k v acc) empty xs
end
