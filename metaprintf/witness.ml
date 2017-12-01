type fmt = Format.formatter

type 'a cp = (fmt -> 'a -> unit)
type show = Show: (fmt -> 'a -> unit) * 'a -> show

type (_,_) arg =
  | Int: ('l * 'm, (int->'l) * (int -> 'm) ) arg
  | Float: ('l * 'm , (float->'l) * (float -> 'm)) arg
  | A: ('l * 'm , (show -> 'l) * ('a cp -> 'a -> 'm) ) arg
  | String: ('l * 'm, (string -> 'l) * (string -> 'm) ) arg

type (_,_) l =
  | []: ('tail * 'moretail ,'tail *'moretail) l
  | (::): ('l , 'res) arg
          * ('l,'tail) l -> ('res,'tail) l


type (_,_) eq = Eq: ('x,'x) eq
type dyn = Dyn: ('a * 'b,'c * 'd) l -> dyn



let eq_arg: type x left left_more right right_more.
  (x,left * left_more) arg -> (x,right * right_more) arg  -> (left,right) eq option =
  fun x y ->
  match x, y with
  | Int, Int -> Some Eq
  | Float, Float -> Some Eq
  | A, A -> Some Eq
  | String, String -> Some Eq
  | _ -> None

let conv_arg: type x left right left_more right_more.
  (x,left * left_more) arg -> (x,right * right_more) arg  ->
  (x,left * left_more) arg  option =
  fun x y ->
  match x, y with
  | Int, Int -> Some Int
  | Float, Float -> Some Float
  | A, A -> Some A
  | String, String -> Some String
  | _ -> None


let rec (===): type left left_more right right_more t.
  (left * left_more, t) l -> (right * right_more,t) l -> (left,right) eq option =
  fun x y ->
  match x, y with
  | [], [] -> Some Eq
  | [], _ :: _ -> None
  | _ :: _, [] -> None
  | Int :: l , Int :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end
  | A :: l , A :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end
  | Float :: l , Float :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end

  | String :: l , String :: r ->
    begin match l === r with
      | None -> None
      | Some Eq -> Some Eq
    end
  | _ -> None


let rec (<:): type left left_more right right_more t t2.
  (left * left_more,t) l -> (right * right_more,t2) l ->
  (right * right_more, t2) l option = fun x y ->
  match x, y with
  | [], [] -> Some []
  | [], _ :: _ -> None
  | _ :: _, [] -> None
  | Int :: l , Int :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (Int :: r)
    end
  | A :: l , A :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (A :: r)
    end
  | Float :: l , Float :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (Float :: r)
    end

  | String :: l , String :: r ->
    begin match l <: r with
      | None -> None
      | Some r -> Some (String :: r )
    end
  | _ -> None

           (*match eq_arg a b with
      | None -> None
      | Some Eq -> Some Eq

         *)

let rec (@): type ls l2 t. (ls, l2) l -> (l2,t) l -> (ls,t) l =
  fun l r ->
    match l with
    | [] -> r
    | a :: q -> a :: ( q @ r )

let remember (type a b c d) (x:(a*b,c*d) l ) (Dyn y) =
  y <: x

let (<::)(Dyn l) x = l <: x

let l = [A;A;Int;Float]
let test = [A;A;Int;Float] === [A;A;Int;Float]
let retype = (Dyn l) <:: l

type box = Box: ('core * _, unit * _) l * 'core -> box


let rec do_nothing: type a b c. (a * b, unit * unit ) l -> b = fun spec ->
  match spec with
  | [] -> ()
  | Int :: q -> fun _ -> do_nothing q
  | Float :: q -> fun _ -> do_nothing q
  | String :: q -> fun _ -> do_nothing q
  | A :: q -> fun _ _ -> do_nothing q


let rec unbox: type a b c. (a * b, unit * unit) l -> box -> b =
  fun spec (Box(spec',f)) ->
  match spec, spec' with
  | [], [] -> ()
  | Int :: l, Int :: r ->
    fun (n:int) -> unbox l (Box(r, f n))
  | Float :: l, Float :: r ->
    fun (x:float) -> unbox l (Box(r, f x))
  | A :: l, A :: r ->
    fun show x -> unbox l (Box(r, f (Show(show,x))))
  | _ -> do_nothing spec

let box =
  let f i (Show(f,x)) =
    Format.printf "i:%d" i;
    Format.printf "show:%a" f x in
  Box([Int;A],f)

;; unbox [Int;A] box 6 Format.pp_print_string "Hi"
