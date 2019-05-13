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
