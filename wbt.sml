functor Wbt (X : sig type t end) = struct
  type size = int

  datatype set
    = Tip
    | Bin of size * X.t * set * set

  val size =
    fn Tip      => 0
     | Bin node => #1 node

  fun singleton k = Bin(1, k, Tip, Tip)

  fun bin k l r = Bin(size l + size r + 1, k, l, r)
end
