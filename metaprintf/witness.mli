(** Type-level witness *)

type (_,_) eq = Eq: ('x,'x) eq
(** Equality type *)


type ('driver,'c) show = Show: ('driver -> 'a -> 'c) * 'a -> ('driver,'c) show
(** [Show(f,x)] groups together the two part of a %a specifier *)


type ('x,'m) exs = <x:'x; l:'l; fl:'x -> 'l;
                    driver:'driver; mid:'c
                   >
  constraint 'm = 'l * 'driver * 'c
(** Simple specifier *)


type 'm alpha =
  <x:('driver,'c) show; l:'l; fl:('driver -> 'y -> 'c) -> 'y -> 'l;
   driver: 'driver;
   mid:'c
  >
  constraint 'm = 'l * 'driver * 'y * 'c
(** [%a] specifier *)


type 'm theta = <x:('driver -> 'c as 't); l:'l; fl: 't -> 'l;driver: 'driver;
                 mid:'c
                >
  constraint 'm = 'l * 'driver * 'c
(** [%t] specifier *)

type _ s =
  | Char : char s
  | Int: int s
  | Int32: int32 s
  | Int64: int64 s
  | Nativeint: nativeint s
  | Bool: bool s
  | Float: float s
  | String: string s (**)
(** Simple specifier witness *)

val (===): 'x s -> 'y s -> ('x,'y) eq option
(** Witness the equality between two simple specifier *)

type _ arg =
  | S:  'x s -> ('x, _ ) exs arg
  | Int_param : 'y s ->
    <x:int * 'y ; l:'l; fl:int -> 'y -> 'l;
     driver:'driver; mid:'c
    > arg
  | Int2_param : 'y s ->
    <x: int * int * 'y; l:'l; fl:int -> int -> 'y -> 'l;
     driver:'driver; mid:'c
    > arg
  | A:  _ alpha arg
  | T: _ theta arg (**)
(** Generic argument type for witness list *)

and (_,_,_,_) l =
  | []: ('tail * 'moretail ,'tail *'moretail,'driver,'mid) l
  | (::): <x:'x ; l:'m; fl:'fm; driver:'driver; mid:'mid > arg *
          ('l * 'm, 'tail,'driver,'mid) l
    -> (('x -> 'l) *  'fm,'tail,'driver,'mid) l (**)
(** Witness for the type of a list of specifier *)

val leq:  ('a * _, 'r * _, 'd, 'm) l
  -> ('b * _, 'r * _ , 'd, 'm) l -> ('a,'b) eq option
(** Witness equality for the metafmt argument convention
    (aka one argument by specifier)
*)

val (@):
  ('lists,'middle_lists,'d,'m) l
  -> ('middle_lists,'results,'d,'m) l
  -> ('lists,'results,'d,'m) l
(** Append two specifier list witnesses *)



type ('b,'d,'mid,'driver) dyn = Dyn: ('a * 'b,'c * 'd,'mid,'driver) l ->
  ('b,'d,'mid,'driver) dyn
(** Forget the types of the arguments in both convention *)


type (_,_,_,_,_) h = H: ('a * 'b,'c * 'd,'dr,'m) l -> ('b,'d,'c,'dr,'m) h
(** Only remember the argument types in the format convention *)

val (@/):
 ('list, 'mid_list,'y,'d,'m) h -> ('mid_list,'result,'y,'d,'m) h
 -> ('list,'result,'y,'d,'m) h
(** Append two format-side specifier lists *)
