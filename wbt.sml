functor Wbt (X : sig
  type t
  val compare : t * t -> order
end) : sig
  type set

  val empty : set
  val singleton : X.t -> set
  val insert : X.t -> set -> set

  val size : set -> int
end = struct
  type size = int

  datatype set
    = Tip
    | Bin of size * X.t * set * set

  val empty = Tip

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
    fun balanceR k l r = raise Fail "TODO"
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
end
