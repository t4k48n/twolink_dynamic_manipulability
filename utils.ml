let print_bool_list lst =
  let sep = ref "" in
  List.iter (fun b -> print_string (!sep ^ if b then "T" else "F"); sep := " ") lst

let print_lf () = print_char '\n'

let println_bool_list lst = print_bool_list lst |> print_lf

(* テスト名とテスト結果を表示する。いずれ変数名も自動で表示したい。
 * マクロを使わなければならないのか？ *)
(* println_tests : string -> bool list -> () *)
let println_tests name bs =
  print_string (name ^ ": ");
  println_bool_list bs

(* a in A, b in Bのリストから(a, b) in CとなるリストCを返す *)
(* list_zip : 'a list -> 'b list -> ('a * 'b) list *)
let list_zip la lb = List.map2 (fun x y -> (x, y)) la lb

(* リスト全てに判定fを適用し、全て真ならば真を返す *)
(* list_all : ('a -> bool) -> 'a list -> bool *)
let list_all f lst = List.fold_left ( && ) true (List.map f lst)

(* リストの中身を足す *)
let float_list_sum = List.fold_left ( +. ) 0.0

let list_mean data =
  assert (data <> []);
  let len = List.length data in
  float_list_sum data /. float len

let list_variance data =
  let m = list_mean data in
  let len = List.length data in
  float_list_sum (List.map (fun d -> (m -. d) **2.) data) /. float len

let list_stddev data =
  sqrt (list_variance data)

let list_range a b =
  let rec loop acc b = if b < a then acc else loop (b :: acc) (b - 1) in
  loop [] (b - 1)

let list_init lst =
  let rec loop acc lst = match lst with
    | [] -> failwith "list_init : empty list"
    | [x] -> acc
    | x :: xs -> loop (x :: acc) xs in
  List.rev (loop [] lst)

let list_linspace ?(endpoint=true) a b n =
  let d = (b -. a) /. float (if endpoint then n - 1 else n) in
  let rec loop acc n =
    if n < 0 then acc else loop (a +. d *. float n :: acc) (n - 1) in
  loop [] (n - 1)

(* 値がほぼ等しい（差がしきい値未満）ことを確認する *)
(* nearly_equal : ?threshold:float -> float -> float -> bool *)
let nearly_equal ?(threshold=1E-6) x y = abs_float (x -. y) < threshold
