functor Wbt (X : sig
  type t
  val compare : t * t -> order
end) :> sig
  type set

  val empty : set
  val singleton : X.t -> set
  val from_list : X.t list -> set
  val insert : X.t -> set -> set
  val delete : X.t -> set -> set

  val size : set -> int
  val member : X.t -> set -> bool

  val fold : (X.t * 'a -> 'a) -> 'a -> set -> 'a
  val to_asc_list : set -> X.t list
  val to_desc_list : set -> X.t list

  exception Empty
  val min : set -> X.t (* Empty *)
  val delete_min : set -> set (* Empty *)


  (* wanted for maps *) (*
  val union : 'a t -> 'a t -> 'a t
  val disjoint_union : 'a t -> 'a t -> 'a t
  val fold_eq : ('a * 'b * 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c
  val intersection : ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
  *)
end = struct
  type size = int

  datatype set
    = Tip
    | Bin of size * X.t * set * set

  val empty = Tip

  fun member kx =
    fn Tip => false
     | Bin(_, ky, l, r) =>
         case X.compare (kx, ky) of
              LESS    => member kx l
            | GREATER => member kx r
            | EQUAL   => true

  exception Empty

  fun min Tip = raise Empty
    | min (Bin node) = min (#3 node) handle Empty => #2 node

  val size =
    fn Tip      => 0
     | Bin node => #1 node

  fun singleton k = Bin(1, k, Tip, Tip)

  fun bin k l r = Bin(size l + size r + 1, k, l, r)

  val delta = 3
  val gamma = 2

  fun isBalanced a b = delta * (size a + 1) >= (size b + 1)

  fun isSingle a b = (size a + 1) < gamma * (size b + 1)

  val rec balanced =
    fn Tip => true
     | Bin(_, _, l, r) =>
         isBalanced l r andalso isBalanced r l
         andalso balanced l andalso balanced r

  structure L = struct
    fun singleL k1 t1 =
      fn Tip => raise Fail "singleL"
       | Bin node => bin (#2 node) (bin k1 t1 (#3 node)) (#4 node)

    fun doubleL k1 t1 =
      fn Bin(_, k2, (Bin node), t4) => bin (#2 node) (bin k1 t1 (#3 node)) (bin k2 (#4 node) t4)
       | _ => raise Fail "doubleL"

    fun rotateL k l =
      fn Tip => raise Fail "rotateL"
       | Bin node =>
           if isSingle (#3 node) (#4 node)
           then singleL k l (Bin node)
           else doubleL k l (Bin node)

    fun balanceL k l r =
      if isBalanced l r
      then bin k l r
      else rotateL k l r
  end

  structure R = struct
    fun singleR k1 Tip _ = raise Fail "singleR"
      | singleR k1 (Bin node) t1 = bin (#2 node) (#3 node) (bin k1 (#4 node) t1)

    fun doubleR k1 (Bin(_, k2, t4, Bin node)) t1 = bin (#2 node) (bin k2 t4 (#3 node)) (bin k1 (#4 node) t1)
      | doubleR _ _ _ = raise Fail "doubleR"

    fun rotateR k Tip _ = raise Fail "rotateR"
      | rotateR k (Bin node) r =
           if isSingle (#4 node) (#3 node)
           then singleR k (Bin node) r
           else doubleR k (Bin node) r

    fun balanceR k l r =
      if isBalanced r l
      then bin k l r
      else rotateR k l r
  end

  open L
  open R

  fun insert kx =
    fn Tip => singleton kx
     | Bin(s, ky, l, r) =>
         case X.compare (kx, ky) of
              LESS    => balanceR ky (insert kx l) r
            | GREATER => balanceL ky l (insert kx r)
            | EQUAL   => Bin(s, kx, l, r)

  val rec delete_min =
    fn Tip             => raise Empty
     | Bin(_, k, l, r) => balanceL k (delete_min l) r handle Empty => r

  fun delete kx =
    fn Tip => Tip
     | Bin(s, ky, l, r) =>
         case X.compare (kx, ky) of
              LESS    => balanceL ky (delete kx l) r
            | GREATER => balanceR ky l (delete kx r)
            | EQUAL   => delete' l r

  and delete' Tip r = r
    | delete' l Tip = l
    | delete' l r = balanceR (min r) l (delete_min r)

  fun fold f acc =
    fn Tip             => acc
     | Bin(_, k, l, r) => fold f (f (k, fold f acc l)) r

  fun to_asc_list t = fold (fn (k, acc) => fn xs => acc (k :: xs)) (fn xs => xs) t []

  fun to_desc_list t = fold (fn (k, acc) => k :: acc) [] t

  fun from_list xs = foldl (fn (x, acc) => insert x acc) empty xs
end
