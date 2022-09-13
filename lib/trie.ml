exception NotFound

module type FINITE_MAP = sig
  type key
  type 'a map

  val empty : 'a map
  val bind : key * 'a * 'a map -> 'a map
  val lookup : key * 'a map -> 'a
end

module Trie (M : FINITE_MAP) : FINITE_MAP = struct
  type key = M.key list
  type 'a map = Trie of 'a option * 'a map M.map

  let empty = Trie (None, M.empty)

  let rec lookup = function
    | [], Trie (None, _) -> raise NotFound
    | [], Trie (Some x, _) -> x
    | k :: ks, Trie (_, m) -> lookup (ks, M.lookup (k, m))

  let rec bind = function
    | [], x, Trie (_, m) -> Trie (Some x, m)
    | k :: ks, x, Trie (v, m) ->
        let t =
          try M.lookup (k, m) with
          | NotFound -> empty
        in
        let t' = bind (ks, x, t) in
        Trie (v, M.bind (k, t', m))
end

type 'a tree =
  | E
  | T of 'a * 'a tree * 'a tree

module TrieOfTrees (M : FINITE_MAP) : FINITE_MAP = struct
  type key = M.key tree
  type 'a map = Trie of 'a option * 'a map map M.map

  let empty = Trie (None, M.empty)

  let rec lookup : 'a. key * 'a map -> 'a = function
    | E, Trie (None, _) -> raise NotFound
    | E, Trie (Some x, _) -> x
    | T (k, a, b), Trie (v, m) -> lookup (b, lookup (a, M.lookup (k, m)))

  let rec bind : 'a. key * 'a * 'a map -> 'a map = function
    | E, x, Trie (_, m) -> Trie (Some x, m)
    | T (k, a, b), x, Trie (v, m) ->
        let tt =
          try M.lookup (k, m) with
          | NotFound -> empty
        in
        let t =
          try lookup (a, tt) with
          | NotFound -> empty
        in
        let t' = bind (b, x, t) in
        let tt' = bind (a, t', tt) in
        Trie (v, M.bind (k, tt', m))
end

type exp =
  | Var of string
  | Lam of string * exp
  | App of exp * exp

module ExpTrie (M : FINITE_MAP with type key = string) :
  FINITE_MAP with type key = exp = struct
  type key = exp

  type 'a map =
    | Empty
    | Nonempty of 'a M.map * 'a map M.map * 'a map map

  let empty = Empty

  let rec lookup : 'a. key * 'a map -> 'a = function
    | _, Empty -> raise NotFound
    | Var s, Nonempty (vmap, lmap, amap) -> M.lookup (s, vmap)
    | Lam (s, e), Nonempty (vmap, lmap, amap) -> lookup (e, M.lookup (s, lmap))
    | App (e1, e2), Nonempty (vmap, lmap, amap) -> lookup (e2, lookup (e1, amap))

  let rec bind : 'a. key * 'a * 'a map -> 'a map = function
    | Var s, x, Empty -> Nonempty (M.bind (s, x, M.empty), M.empty, empty)
    | Lam (s, e), x, Empty ->
        Nonempty (M.empty, M.bind (s, bind (e, x, empty), M.empty), empty)
    | _, _, _ -> failwith "TODO"
end
