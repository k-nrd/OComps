open Eq

(* Userland code *)
type position = { x : float; y : float }
type velocity = { vx : float; vy : float }
type health = private int

type _ component =
  | Position : position -> position component
  | Velocity : velocity -> velocity component
  | Health : health -> health component

(* PPX-generated *)
type _ component_type =
  | PositionComponent : position component_type
  | VelocityComponent : velocity component_type
  | HealthComponent : health component_type

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
