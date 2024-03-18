type position = { x : float; y : float }
type velocity = { vx : float; vy : float }
type health = private int

type component =
  [ `Position of position | `Velocity of velocity | `Health of health ]

open Containers
open Entity_allocator
module List = ListLabels
module IntSet = Set.Make (Int)

(* PPX-generated *)
type component_type = [ `Position | `Velocity | `Health ]

(* PPX-generated *)
type component_storage =
  | PositionStorage : position Vector.vector -> component_storage
  | VelocityStorage : velocity Vector.vector -> component_storage
  | HealthStorage : health Vector.vector -> component_storage

type archetype = {
  mask : IntSet.t;
  entities : int Vector.vector;
  component_stores : (int, component_storage) Hashtbl.t;
}

type world = {
  entity_allocator : EntityAllocator.t;
  archetypes : (int, archetype) Hashtbl.t;
}

(* PPX-generated *)
let get_comp_type (comp : component) : component_type =
  match comp with
  | `Position _ -> `Position
  | `Velocity _ -> `Velocity
  | `Health _ -> `Health
[@@inline]

(* PPX-generated *)
let get_comp_type_id (comp_type : component_type) : int =
  match comp_type with `Position -> 0 | `Velocity -> 1 | `Health -> 2
[@@inline]

(* PPX-generated *)
let get_comp_id (comp : component) : int =
  match comp with `Position _ -> 0 | `Velocity _ -> 1 | `Health _ -> 2
[@@inline]

let get_comp_store archetype comp_type_id =
  Hashtbl.find archetype.component_stores comp_type_id
[@@inline]

let get world entity (comp_type : component_type) =
  (* User input, they might make this fail so we should raise a nice error *)
  let entity_location = EntityAllocator.get world.entity_allocator entity in
  (* Internal, should never fail *)
  let archetype = Hashtbl.find world.archetypes entity_location.archetype_id in
  let comp_type_id = get_comp_type_id comp_type in
  if IntSet.mem comp_type_id archetype.mask then
    (* Internal, should never fail *)
    let f store = Vector.get store entity_location.index_in_archetype in
    let comp =
      (* PPX-generated *)
      match get_comp_store archetype comp_type_id with
      | PositionStorage store -> `Position (f store)
      | VelocityStorage store -> `Velocity (f store)
      | HealthStorage store -> `Health (f store)
    in
    Some comp
  else None
[@@inline]

let insert world entity (comp : component) =
  (* User input, they might make this fail so we should raise a nice error *)
  let entity_location = EntityAllocator.get world.entity_allocator entity in
  (* Internal, should never fail *)
  let archetype = Hashtbl.find world.archetypes entity_location.archetype_id in
  let comp_type_id = get_comp_id comp in
  if IntSet.mem comp_type_id archetype.mask then
    (* If component exists in archetype, just overwrite *)
    let f store data =
      (* Internal, should never fail *)
      Vector.set store entity_location.index_in_archetype data
    in
    match get_comp_store archetype comp_type_id with
    (* PPX-generated *)
    | PositionStorage store -> begin
        match comp with
        | `Position data -> f store data
        | _ -> raise (Invalid_argument "error setting component")
      end
    | VelocityStorage store -> begin
        match comp with
        | `Velocity data -> f store data
        | _ -> raise (Invalid_argument "error setting component")
      end
    | HealthStorage store -> begin
        match comp with
        | `Health data -> f store data
        | _ -> raise (Invalid_argument "error setting component")
      end
  else
    (*
        If component doesn't exist in archetype,
        first check if there's another archetype with the right mask.
        If it does exist, update entity's location to the new archetype and
        remove from the old one. Then copy entity's components to the new one
        If it doesn't, create a new archetype with the right mask and
        copy entity's component to it.
    *)
    ()
[@@inline]

(*
let remove world (entity : int) (comp_type : component_type) : unit =
  (* swap_remove *)
  let f store =
    match Vector.length store with
    | 0 -> ()
    | n ->
        let last = Vector.pop_exn store in
        if n > 1 then Vector.set store entity last
  in
  match comp_type with
  | `Position -> f world.component_stores.position
  | `Velocity -> f world.component_stores.velocity
  | `Health -> f world.component_stores.health
[@@inline]
  *)

(* This wouldn't need to be written through PPX *)

let get_bundle_set comps =
  comps
  |> Seq.of_list
  |> Seq.map get_comp_id
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

let spawn _world (_comps : component list) =
  let _entity_id = 0 in
  0
