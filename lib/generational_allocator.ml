module type ALLOCATOR_DEF = sig
  type metadata
end

(** 
    Generational allocator to manage entitys through entries. 
    It tracks entitys and handles their almetadata, dealmetadata and liveness. *)
module GenerationalAllocator (Def : ALLOCATOR_DEF) : sig
  type t
  (** The type of the generational allocator. *)

  type entity
  (** The type representing an item. *)

  type metadata = Def.metadata
  (** The type representing the metadata of an item. *)

  val create_allocator : unit -> t
  (** [create ()] creates a new entity allocator.
      @returns A new entity allocator.
      {[
        let allocator = GenerationalAllocator.create ()
      ]} *)

  val allocate : t -> metadata -> entity
  (** [allocate allocator metadata_entity] allocates a new item in 
      the allocator within the specified metadata_entity.
      @param allocator The generational allocator.
      @param metadata_entity The entity of the metadata to allocate the entity in.
      @returns entity with the new entity.
      {[
        let entity = GenerationalAllocator.allocate allocator 1
      ]} *)

  val deallocate : t -> entity -> unit
  (** [deallocate allocator entity] deallocates the given entity from the allocator.
      Does nothing if the entity has already been deallocated..

      @param allocator The generational allocator.
      @param entity The entity to deallocate.
      {[
        let () = GenerationalAllocator.deallocate allocator entity
      ]} *)

  val update_metadata : t -> entity -> metadata -> unit
  (** [update_metadata allocator entity metadata] updates the metadata of the 
      given entity in the allocator.

      @param allocator The generational allocator.
      @param entity The entity whose metadata is to be updated.
      @param metadata The new metadata of the entity.
      @raises Invalid_argument if the entity has been deallocated
      {[
        GenerationalAllocator.update_metadata allocator entity new_metadata
      ]} *)

  val get_metadata : t -> entity -> metadata option
  (** [get allocator entity] Gets the current metadata of an entity.
      @param allocator The entity allocator.
      @param entity The entity to get the metadata for.
      @returns [Some metadata] if the entity is live, [None] if it is not.
      {[
        match GenerationalAllocator.get allocator entity with 
        | Some metadata -> ()
        | None -> ()
      ]} *)

  val is_live : t -> entity -> bool
  (** [is_live allocator entity] checks if the given entity is currently live 
      (allocated) in the allocator.
      @param allocator The entity allocator.
      @param entity The entity to check.
      @returns [true] if the entity is live, [false] otherwise.
      {[
        let live = GenerationalAllocator.is_live allocator entity
      ]} *)
end = struct
  open Containers

  type entity = { id : int; generation : int }
  type metadata = Def.metadata

  type entry_metadata = {
    mutable is_live : bool;
    mutable generation : int;
    mutable metadata : metadata;
  }

  type t = { entries : entry_metadata Vector.vector; mutable free : int list }

  let create_allocator () = { entries = Vector.create (); free = [] }

  let allocate allocator metadata =
    match allocator.free with
    | index :: rest ->
        allocator.free <- rest;
        let entry = Vector.get allocator.entries index in
        if entry.is_live then failwith "Freed live entity"
        else begin
          entry.is_live <- true;
          { id = index; generation = entry.generation }
        end
    | [] ->
        let generation = 0 in
        let id = Vector.length allocator.entries in
        Vector.push allocator.entries { is_live = true; generation; metadata };
        { id; generation }

  let deallocate allocator entity =
    (* Can't ever fail if we're allocating correctly *)
    let entry = Vector.get allocator.entries entity.id in
    (* Do nothing if entry is not live *)
    if entry.is_live then begin
      entry.is_live <- false;
      entry.generation <- entry.generation + 1;
      allocator.free <- entity.id :: allocator.free
    end

  let get_metadata allocator entity =
    (* Can't ever fail if we're allocating correctly *)
    let entry = Vector.get allocator.entries entity.id in
    if entry.is_live && entity.generation = entry.generation then
      Some entry.metadata
    else None

  let update_metadata allocator entity metadata =
    (* Can't ever fail if we're allocating correctly *)
    let entry = Vector.get allocator.entries entity.id in
    if not entry.is_live then failwith "Entity is not live"
    else entry.metadata <- metadata

  let is_live allocator entity = Option.is_some (get_metadata allocator entity)
end
