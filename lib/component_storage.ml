module type COMPONENT_SIG = sig
  type t
end

module type COMPONENT_STORAGE = sig
  type component

  val insert : int -> component -> unit
  val get : int -> component
  val remove : int -> unit
end

module ComponentStorage (C : COMPONENT_SIG) = struct
  module Make () : COMPONENT_STORAGE = struct
    open Containers

    type component = C.t

    let storage = ref (Vector.create ())
    let insert entity component = Vector.set !storage entity component
    let get entity = Vector.get !storage entity

    let remove entity =
      match Vector.length !storage with
      | 0 -> ()
      | n ->
          let last = Vector.pop_exn !storage in
          if n > 1 then Vector.set !storage entity last
  end
end
