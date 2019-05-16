open Utils

open Model

let postures =
  let ticks = list_linspace 0.0 (2.0 *. pi) 21 in
  List.concat @@ List.map (fun q1 -> List.map (fun q2 -> (q1, q2)) ticks) ticks

let uncertainties =
  let ticks = list_linspace (-1.0) 1.0 21 in
  List.concat @@ List.concat @@ List.map (fun q1 -> List.map (fun q2 -> List.map (fun q3 -> (q1, q2, q3)) ticks) ticks) ticks

(* MC法で求めた各姿勢における動的可操作性 *)
let dynamic_manipulabilities_with_monte_carlo =
  List.map (fun p -> List.map (fun z -> dynamic_manipulability_with_grasp p z 0.5) uncertainties) postures

let () =
  let dynman_means = List.map list_mean dynamic_manipulabilities_with_monte_carlo in
  let dynman_variances = List.map list_variance dynamic_manipulabilities_with_monte_carlo in
  List.iter2 (fun m v -> Printf.printf "%f,%f\n" m v) dynman_means dynman_variances
