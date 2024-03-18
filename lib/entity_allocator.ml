(** Entity allocator for managing entities in an Entity Component System. 
      It tracks entity IDs, generations, locations within archetypes, and handles allocation and deallocation of entities. *)
module EntityAllocator : sig
  type t
  (** The type of the entity allocator. *)

  type entity = { id : int; generation : int }
  (** The type representing an entity, containing its ID and generation. *)

  type entity_location = { archetype_id : int; index_in_archetype : int }
  (** The type representing the location of an entity in an archetype. *)

  val create : unit -> t
  (** [create ()] creates a new entity allocator.
      @returns A new entity allocator.
      {[
        let allocator = EntityAllocator.create ()
      ]} *)

  val allocate : t -> int -> (entity, string) result
  (** [allocate allocator archetype_id] allocates a new entity in the allocator within the specified archetype.
      @param allocator The entity allocator.
      @param archetype_id The ID of the archetype to allocate the entity in.
      @returns [Ok entity] with the new entity if successful, or [Error msg] on failure.
      {[
        let entity = EntityAllocator.allocate allocator 1
      ]} *)

  val deallocate : t -> entity -> (unit, string) result
  (** [deallocate allocator entity] deallocates the given entity from the allocator.
      @param allocator The entity allocator.
      @param entity The entity to deallocate.
      @returns [Ok ()] if successful, or [Error msg] on failure.
      {[
        let result = EntityAllocator.deallocate allocator entity
      ]} *)

  val update_location : t -> entity -> entity_location -> (unit, string) result
  (** [update_location allocator entity location] updates the location of the given entity in the allocator.
      @param allocator The entity allocator.
      @param entity The entity whose location is to be updated.
      @param location The new location of the entity.
      @returns [Ok ()] if successful, or [Error msg] on failure.
      {[
        let result = EntityAllocator.update_location allocator entity new_location
      ]} *)

  val get : t -> entity -> entity_location
  (** [get allocator entity] Gets the current location of an entity.
      @param allocator The entity allocator.
      @param entity The entity to get the location for.
      @returns [entity_location] if the entity is live.
      @raises Invalid_argument if the entity is not live.
      {[
        let get = EntityAllocator.get allocator entity
      ]} *)

  val is_live : t -> entity -> bool
  (** [is_live allocator entity] checks if the given entity is currently live (allocated) in the allocator.
      @param allocator The entity allocator.
      @param entity The entity to check.
      @returns [true] if the entity is live, [false] otherwise.
      {[
        let live = EntityAllocator.is_live allocator entity
      ]} *)
end = struct
  open Containers

  type entity = { id : int; generation : int }
  type entity_location = { archetype_id : int; index_in_archetype : int }

  type entity_entry = {
    mutable is_live : bool;
    mutable generation : int;
    mutable location : entity_location;
  }

  type t = { entries : entity_entry Vector.vector; mutable free : int list }

  let create () = { entries = Vector.create (); free = [] }

  let get allocator entity =
    let entry = Vector.get allocator.entries entity.id in
    if entry.is_live && entity.generation = entry.generation then entry.location
    else raise (Invalid_argument "entity is not live anymore")

  let allocate allocator archetype_id =
    match allocator.free with
    | index :: rest ->
        allocator.free <- rest;
        let entry = Vector.get allocator.entries index in
        if entry.is_live then Error "EntityAlreadyAllocated"
        else begin
          entry.is_live <- true;
          Ok { id = index; generation = entry.generation }
        end
    | [] ->
        let generation = 0 in
        let new_entry =
          {
            is_live = true;
            generation;
            location = { archetype_id; index_in_archetype = 0 };
          }
        in
        Vector.push allocator.entries new_entry;
        Ok { id = Vector.length allocator.entries - 1; generation }

  let deallocate allocator entity =
    if entity.id >= Vector.length allocator.entries then Error "EntityNotFound"
    else
      let entry = Vector.get allocator.entries entity.id in
      if not entry.is_live then Error "EntityAlreadyDeallocated"
      else begin
        entry.is_live <- false;
        entry.generation <- entry.generation + 1;
        allocator.free <- entity.id :: allocator.free;
        Ok ()
      end

  let update_location allocator entity location =
    if entity.id >= Vector.length allocator.entries then Error "EntityNotFound"
    else
      let entry = Vector.get allocator.entries entity.id in
      if not entry.is_live then Error "EntityAlreadyDeallocated"
      else begin
        entry.location <- location;
        Ok ()
      end

  let is_live allocator entity =
    try
      get allocator entity |> ignore;
      true
    with _ -> false
end
