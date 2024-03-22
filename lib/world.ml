(* Userland code *)
type position = { x : float; y : float }
type velocity = { vx : float; vy : float }
type health = private int

type _ component =
  | Position : position -> position component
  | Velocity : velocity -> velocity component
  | Health : health -> health component

(* Library code *)
open Containers
open Generational_allocator
module List = ListLabels
module IntSet = Set.Make (Int)

(* PPX-generated *)
type _ component_type =
  | PositionComponent : position component_type
  | VelocityComponent : velocity component_type
  | HealthComponent : health component_type

type 'a component_store = 'a Vector.vector

(* PPX-generated *)
let get_comp_type : type a. a component -> a component_type = function
  | Position _ -> PositionComponent
  | Velocity _ -> VelocityComponent
  | Health _ -> HealthComponent
[@@inline]

(* PPX-generated *)
let get_comp_type_id : type a. a component_type -> int = function
  | PositionComponent -> 0
  | VelocityComponent -> 1
  | HealthComponent -> 2
[@@inline]

type (_, _) eq = Eq : ('a, 'a) eq

(* PPX generated *)
let eq_comp_type :
    type a b. a component_type * b component_type -> (a, b) eq option = function
  | PositionComponent, PositionComponent -> Some Eq
  | VelocityComponent, VelocityComponent -> Some Eq
  | HealthComponent, HealthComponent -> Some Eq
  | _ -> None
[@@inline]

(* PPX generated *)
let component_map : type a b. f:(a -> b) -> a component -> b =
 fun ~f comp ->
  match comp with
  | Position data -> f data
  | Velocity data -> f data
  | Health data -> f data
[@@inline]

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

type any_component = AnyComponent : 'a component -> any_component

type any_store =
  | AnyStore : 'a component_type * 'a component_store -> any_store

let extract_store : type a. a component_type -> any_store -> a component_store =
 fun a (AnyStore (b, x)) ->
  match eq_comp_type (a, b) with
  | Some Eq -> x
  | _ -> failwith "ExtractStoreFailed"
[@@inline]

let extract_component : type a. a component_type -> any_component -> a component
    =
 fun a (AnyComponent b) ->
  match eq_comp_type (a, get_comp_type b) with
  | Some Eq -> b
  | _ -> failwith "ExtractComponentFailed"
[@@inline]

type entity_location = { archetype_id : int; index_in_archetype : int }

module EntityAllocator = GenerationalAllocator (struct
  type location = entity_location
end)

type entity = EntityAllocator.address

type archetype = {
  mask : IntSet.t;
  entities : int Vector.vector;
  component_stores : (int, any_store) Hashtbl.t;
}

type world = {
  entity_allocator : EntityAllocator.t;
  archetypes : (int, archetype) Hashtbl.t;
}

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
let migrate_entity entity_location new_comp from_arch to_arch to_arch_idx =
  let new_idx = Vector.length to_arch.entities in
  let new_comp_type = get_comp_type new_comp in
  let new_comp_type_id = get_comp_type_id new_comp_type in
  let () =
    let comp_store =
      extract_store new_comp_type
        (Hashtbl.find to_arch.component_stores new_comp_type_id)
    in
    component_map new_comp ~f:(fun data -> Vector.push comp_store data)
  in
  let f (comp_id : int) (AnyStore (comp_type, from_comp_store)) =
    (* Internal, shouldn't fail *)
    let to_comp_store =
      extract_store comp_type (Hashtbl.find to_arch.component_stores comp_id)
    in
    match
      migrate_component entity_location.index_in_archetype from_comp_store
        to_comp_store
    with
    | None -> failwith "weird"
    | Some _i -> ignore ()
  in
  Hashtbl.iter f from_arch.component_stores;
  { index_in_archetype = new_idx; archetype_id = to_arch_idx }

let get : type a. world -> entity -> a component_type -> (a, string) result =
 fun world entity (comp_type : a component_type) ->
  (* User input, they might make this fail so we should raise a nice error *)
  let open Result in
  let* entity_location =
    EntityAllocator.get world.entity_allocator entity
    |> Option.to_result "InvalidEntity"
  in
  (* Internal, should never fail *)
  let archetype = Hashtbl.find world.archetypes entity_location.archetype_id in
  let comp_type_id = get_comp_type_id comp_type in
  if not (IntSet.mem comp_type_id archetype.mask) then Error "ComponentNotFound"
  else
    (* Internal, should never fail *)
    let any_store = Hashtbl.find archetype.component_stores comp_type_id in
    let store = extract_store comp_type any_store in
    Ok (Vector.get store entity_location.index_in_archetype)
[@@inline]

let insert : type a. world -> entity -> a component -> (unit, string) result =
 fun world entity comp ->
  (* User input, they might make this fail so we should raise a nice error *)
  let open Result in
  let* entity_location =
    EntityAllocator.get world.entity_allocator entity
    |> Option.to_result "InvalidEntity"
  in
  (* Shouldn't fail if the archetype was added correctly *)
  let arch = Hashtbl.find world.archetypes entity_location.archetype_id in
  let comp_type = get_comp_type comp in
  let comp_type_id = get_comp_type_id comp_type in
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
    | Some (a_id, a) ->
        let new_location = migrate_entity entity_location comp arch a a_id in
        EntityAllocator.update_location world.entity_allocator entity
          new_location
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
      (component_map component ~f:(fun data ->
           Vector.set store entity_location.index_in_archetype data))
[@@inline]

let remove :
    type a. world -> entity -> a component_type -> (unit, string) result =
 fun world entity comp_type ->
  (* User input, they might make this fail so we should raise a nice error *)
  let open Result in
  let* entity_location =
    EntityAllocator.get world.entity_allocator entity
    |> Option.to_result "InvalidEntity"
  in
  (* Internal, should never fail *)
  let arch = Hashtbl.find world.archetypes entity_location.archetype_id in
  let comp_type_id = get_comp_type_id comp_type in
  if not (IntSet.mem comp_type_id arch.mask) then
    (* Archetype doesn't contain that component type in their mask *)
    Error "ComponentNotFound"
  else
    let any_store = Hashtbl.find arch.component_stores comp_type_id in
    (* Shouldn't fail if the mask matched *)
    let store = extract_store comp_type any_store in
    Ok (swap_remove store entity_location.index_in_archetype |> ignore)
[@@inline]

let get_bundle_set comps =
  comps
  |> Seq.of_list
  |> Seq.map get_comp_type
  |> Seq.map get_comp_type_id
  |> Seq.sort_uniq ~cmp:Int.compare
  |> Seq.fold (fun set cur -> IntSet.add cur set) IntSet.empty

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

let spawn (type a) _world (_comps : a component list) =
  let _entity_id = 0 in
  0
