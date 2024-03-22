module type ALLOCATOR_DEF = sig
  type location
end

(** 
    Generational allocator to manage addresss through entries. 
    It tracks addresss and handles their allocation, deallocation and liveness. *)
module GenerationalAllocator (Def : ALLOCATOR_DEF) : sig
  type t
  (** The type of the generational allocator. *)

  type address = { address : int; generation : int }
  (** The type representing an item. *)

  type location = Def.location
  (** The type representing the location of an item. *)

  val create : unit -> t
  (** [create ()] creates a new address allocator.
      @returns A new address allocator.
      {[
        let allocator = GenerationalAllocator.create ()
      ]} *)

  val allocate : t -> location -> (address, string) result
  (** [allocate allocator location_address] allocates a new item in 
      the allocator within the specified location_address.
      @param allocator The generational allocator.
      @param location_address The address of the location to allocate the address in.
      @returns [Ok address] with the new address if successful, or [Error msg] on failure.
      {[
        let address = GenerationalAllocator.allocate allocator 1
      ]} *)

  val deallocate : t -> address -> (unit, string) result
  (** [deallocate allocator address] deallocates the given address from the allocator.
      @param allocator The generational allocator.
      @param address The address to deallocate.
      @returns [Ok ()] if successful, or [Error msg] on failure.
      {[
        let result = GenerationalAllocator.deallocate allocator address
      ]} *)

  val update_location : t -> address -> location -> (unit, string) result
  (** [update_location allocator address location] updates the location of the 
      given address in the allocator.
      @param allocator The generational allocator.
      @param address The address whose location is to be updated.
      @param location The new location of the address.
      @returns [Ok ()] if successful, or [Error msg] on failure.
      {[
        let result = GenerationalAllocator.update_location allocator address new_location
      ]} *)

  val get : t -> address -> location option
  (** [get allocator address] Gets the current location of an address.
      @param allocator The address allocator.
      @param address The address to get the location for.
      @returns [address_location] if the address is live.
      @raises Invalid_argument if the address is not live.
      {[
        let get = GenerationalAllocator.get allocator address
      ]} *)

  val is_live : t -> address -> bool
  (** [is_live allocator address] checks if the given address is currently live 
      (allocated) in the allocator.
      @param allocator The address allocator.
      @param address The address to check.
      @returns [true] if the address is live, [false] otherwise.
      {[
        let live = GenerationalAllocator.is_live allocator address
      ]} *)
end = struct
  open Containers

  type address = { address : int; generation : int }
  type location = Def.location

  type address_entry = {
    mutable is_live : bool;
    mutable generation : int;
    mutable location : location;
  }

  type t = { entries : address_entry Vector.vector; mutable free : int list }

  let create () = { entries = Vector.create (); free = [] }

  let get allocator address =
    let entry = Vector.get allocator.entries address.address in
    if entry.is_live && address.generation = entry.generation then
      Some entry.location
    else None

  let allocate allocator location =
    match allocator.free with
    | index :: rest ->
        allocator.free <- rest;
        let entry = Vector.get allocator.entries index in
        if entry.is_live then Error "AddressAlreadyAllocated"
        else begin
          entry.is_live <- true;
          Ok { address = index; generation = entry.generation }
        end
    | [] ->
        let generation = 0 in
        let new_entry = { is_live = true; generation; location } in
        Vector.push allocator.entries new_entry;
        Ok { address = Vector.length allocator.entries - 1; generation }

  let deallocate allocator address =
    if address.address >= Vector.length allocator.entries then
      Error "AddressNotFound"
    else
      let entry = Vector.get allocator.entries address.address in
      if not entry.is_live then Error "AddressAlreadyDeallocated"
      else begin
        entry.is_live <- false;
        entry.generation <- entry.generation + 1;
        allocator.free <- address.address :: allocator.free;
        Ok ()
      end

  let update_location allocator address location =
    if address.address >= Vector.length allocator.entries then
      Error "AddressNotFound"
    else
      let entry = Vector.get allocator.entries address.address in
      if not entry.is_live then Error "AddressAlreadyDeallocated"
      else begin
        entry.location <- location;
        Ok ()
      end

  let is_live allocator address =
    try
      get allocator address |> ignore;
      true
    with _ -> false
end
