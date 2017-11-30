
let maybe f x = match x with
  | Some x -> f x
  | None -> ()

let bind f x =  match x with
  | Some x -> f x
  | None -> None


let (>>) x f = maybe f x
let (>>=) x f = bind f x
let (>>|) x f = x >>= (fun x -> Some(f x))
