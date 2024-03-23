module type COMP_DEFS = sig
  open Eq

  type 'a component
  type 'a component_type

  val get_comp_type : 'a component -> 'a component_type
  val get_comp_type_id : 'a component_type -> int
  val eq_comp_type : 'a component_type * 'b component_type -> ('a, 'b) eq option
  val component_map : f:('a -> 'b) -> 'a component -> 'b
end

module World (C : COMP_DEFS) : sig
  type t
  type entity
  type get_error = EntityNotFound | ComponentNotFound

  val spawn : t -> 'a C.component list -> entity

  val get : t -> entity -> 'a C.component_type -> ('a, get_error) result
  (** [get world entity comp_type] retrieves the component of type [a] associated with an entity from the world.

    @param world The world from which to retrieve the component.
    @param entity The entity ID for which to retrieve the component.
    @param comp_type The type of component to retrieve, specified as a value of type [a C.component_type].
    @return A result containing either:
    - [Ok component] containing the requested component if the entity has the component of type [a] and the entity is valid.
    - [Error InvalidEntity] if the entity is not found in the world's entity allocator.
    - [Error ComponentNotFound] if the entity does not have a component of the specified type.
  *)

  val insert : t -> entity -> 'a C.component -> (unit, string) result
  val remove : t -> entity -> 'a C.component_type -> (unit, string) result
end = struct
  open Containers
  open Generational_allocator
  open Eq
  module List = ListLabels
  module IntSet = Set.Make (Int)

  type get_error = EntityNotFound | ComponentNotFound
  type any_component = AnyComponent : 'a C.component -> any_component
  type 'a component_store = 'a Vector.vector

  type any_store =
    | AnyStore : 'a C.component_type * 'a component_store -> any_store

  type entity_location = { archetype_id : int; index_in_archetype : int }

  module EntityAllocator = GenerationalAllocator (struct
    type metadata = entity_location
  end)

  type entity = EntityAllocator.entity

  type archetype = {
    mask : IntSet.t;
    entities : int Vector.vector;
    component_stores : (int, any_store) Hashtbl.t;
  }

  type t = {
    entity_allocator : EntityAllocator.t;
    archetypes : (int, archetype) Hashtbl.t;
  }

  let swap_remove vector idx =
    match Vector.length vector with
    | 0 -> None
    | n ->
        let last = Vector.pop_exn vector in
        if n = 1 then Some last
        else begin
          let removed = Vector.get vector idx in
          Vector.set vector idx last;
          Some removed
        end
  [@@inline]

  let extract_store :
      type a. a C.component_type -> any_store -> a component_store =
   fun a (AnyStore (b, x)) ->
    match C.eq_comp_type (a, b) with
    | Some Eq -> x
    | _ -> failwith "ExtractStoreFailed"
  [@@inline]

  let extract_component :
      type a. a C.component_type -> any_component -> a C.component =
   fun a (AnyComponent b) ->
    match C.eq_comp_type (a, C.get_comp_type b) with
    | Some Eq -> b
    | _ -> failwith "ExtractComponentFailed"
  [@@inline]

  let migrate_component idx from_store to_store =
    match swap_remove from_store idx with
    | None -> None
    | Some removed -> begin
        let new_idx = Vector.length to_store in
        Vector.push to_store removed;
        Some new_idx
      end
  [@@inline]

  (*
  Go through old arch's comp stores, migrate stuff to new store 
  Add new comp to new arch's comp store 
  Return new location
*)
  let migrate_entity world from_location to_arch_idx new_comp =
    let to_arch = Hashtbl.find world.archetypes to_arch_idx in
    let from_arch = Hashtbl.find world.archetypes from_location.archetype_id in
    let new_idx = Vector.length to_arch.entities in
    let new_comp_type = C.get_comp_type new_comp in
    let new_comp_type_id = C.get_comp_type_id new_comp_type in
    let () =
      let comp_store =
        extract_store new_comp_type
          (Hashtbl.find to_arch.component_stores new_comp_type_id)
      in
      C.component_map new_comp ~f:(fun data -> Vector.push comp_store data)
    in
    let f (comp_id : int) (AnyStore (comp_type, from_comp_store)) =
      (* Internal, shouldn't fail *)
      let to_comp_store =
        extract_store comp_type (Hashtbl.find to_arch.component_stores comp_id)
      in
      match
        migrate_component from_location.index_in_archetype from_comp_store
          to_comp_store
      with
      | None -> failwith "weird"
      | Some _i -> ignore ()
    in
    Hashtbl.iter f from_arch.component_stores;
    { index_in_archetype = new_idx; archetype_id = to_arch_idx }
  [@@inline]

  let generate_mask comps =
    comps
    |> Seq.of_list
    |> Seq.map C.get_comp_type
    |> Seq.map C.get_comp_type_id
    |> Seq.sort_uniq ~cmp:Int.compare
    |> Seq.fold (fun set cur -> IntSet.add cur set) IntSet.empty
  [@@inline]

  let get : type a. t -> entity -> a C.component_type -> (a, get_error) result =
   fun world entity comp_type ->
    (* User input, they might make this fail so we should raise a nice error *)
    let open Result in
    let* entity_location =
      EntityAllocator.get_metadata world.entity_allocator entity
      |> Option.to_result EntityNotFound
    in
    (* Internal, should never fail *)
    let archetype =
      Hashtbl.find world.archetypes entity_location.archetype_id
    in
    let comp_type_id = C.get_comp_type_id comp_type in
    if not (IntSet.mem comp_type_id archetype.mask) then Error ComponentNotFound
    else
      (* Internal, should never fail *)
      let any_store = Hashtbl.find archetype.component_stores comp_type_id in
      let store = extract_store comp_type any_store in
      Ok (Vector.get store entity_location.index_in_archetype)
  [@@inline]

  let insert : type a. t -> entity -> a C.component -> (unit, string) result =
   fun world entity comp ->
    (* User input, they might make this fail so we should raise a nice error *)
    let open Result in
    let* entity_location =
      EntityAllocator.get_metadata world.entity_allocator entity
      |> Option.to_result "InvalidEntity"
    in
    (* Shouldn't fail if the archetype was added correctly *)
    let arch = Hashtbl.find world.archetypes entity_location.archetype_id in
    let comp_type = C.get_comp_type comp in
    let comp_type_id = C.get_comp_type_id comp_type in
    if not (IntSet.mem comp_type_id arch.mask) then
      (*
        If component doesn't exist in archetype,
        first check if there's another archetype with the right mask.
        If it does exist, update entity's location to the new archetype and
        remove from the old one. Then copy entity's components to the new one
        If it doesn't, create a new archetype with the right mask and
        copy entity's component to it.
    *)
      (* Try to find an existing archetype with a matching mask *)
      let new_mask = IntSet.add comp_type_id arch.mask in
      let new_archetype =
        Hashtbl.to_seq world.archetypes
        |> Seq.find (fun (_, a) -> IntSet.equal a.mask new_mask)
      in
      match new_archetype with
      | Some (to_arch_id, _a) ->
          let new_location =
            migrate_entity world entity_location to_arch_id comp
          in
          EntityAllocator.update_metadata world.entity_allocator entity
            new_location;
          Ok ()
      | None -> begin (* Create new archetype *)
                      Ok () end
    else
      (* If component exists in archetype, just overwrite *)
      let any_store = Hashtbl.find arch.component_stores comp_type_id in
      (* Shouldn't fail if the mask matched *)
      let store = extract_store comp_type any_store in
      (* Can't fail if we've generated code correctly *)
      let component = extract_component comp_type (AnyComponent comp) in
      Ok
        (C.component_map component ~f:(fun data ->
             Vector.set store entity_location.index_in_archetype data))
  [@@inline]

  let remove :
      type a. t -> entity -> a C.component_type -> (unit, string) result =
   fun world entity comp_type ->
    (* User input, they might make this fail so we should raise a nice error *)
    let open Result in
    let* entity_location =
      EntityAllocator.get_metadata world.entity_allocator entity
      |> Option.to_result "InvalidEntity"
    in
    (* Internal, should never fail *)
    let arch = Hashtbl.find world.archetypes entity_location.archetype_id in
    let comp_type_id = C.get_comp_type_id comp_type in
    if not (IntSet.mem comp_type_id arch.mask) then
      (* Archetype doesn't contain that component type in their mask *)
      Error "ComponentNotFound"
    else
      let any_store = Hashtbl.find arch.component_stores comp_type_id in
      (* Shouldn't fail if the mask matched *)
      let store = extract_store comp_type any_store in
      Ok (swap_remove store entity_location.index_in_archetype |> ignore)
  [@@inline]

  (*

  Here is the plan:
   
   We spawn things with `spawn` and component lists 
   We compute a bundle set for the component list 
   With that bundle set, we try to find an exact match archetype 
   If we find it, that's our archetype
   If we don't, we create an archetype with that bundle set and that's our archetype
   We then stick our entity in the archetype (creating an entry in the world)

   Archetypes, when created, should be able to create component stores for the 
   components they contain.

   So, given a component_type_id, create a storage for that component
   In `spawn`, the flow of types is: 
   `Position data => `Position => int (position_id) => position Vector.vector
  *)
  let spawn (type a) (world : t) (comps : a C.component list) =
    let _entity_id = 0 in
    let mask = generate_mask comps in
    EntityAllocator.allocate world.entity_allocator
      { archetype_id = 0; index_in_archetype = 0 }
end
