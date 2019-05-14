(* #use "utils.ml" *)
open Utils

(* 値がほぼ等しい（差がしきい値未満）ことを確認する *)
(* nearly_equal : ?threshold:float -> float -> float -> bool *)
let nearly_equal ?(threshold=1E-6) x y = abs_float (x -. y) < threshold

(* a in A, b in Bのリストから(a, b) in CとなるリストCを返す *)
(* list_zip : 'a list -> 'b list -> ('a * 'b) list *)
let list_zip la lb = List.map2 (fun x y -> (x, y)) la lb

(* リスト全てに判定fを適用し、全て真ならば真を返す *)
(* list_all : ('a -> bool) -> 'a list -> bool *)
let list_all f lst = List.fold_left ( && ) true (List.map f lst)

let nearly_equal_tests =
  let t1 = nearly_equal 1.0 1.1 = false in
  let t2 = nearly_equal 1.0 1.0001 = false in
  let t3 = nearly_equal 1.0 1.00000001 = true in
  let t4 = nearly_equal ~threshold:1.0 1.0 1.9 = true in
  [t1; t2; t3; t4]

(* 円周率 *)
let pi = 4.0 *. atan 1.0

(* リンク長さ *)
let l1 = 1.0
let l2 = 1.0
let dl1 = 0.02
let dl2 = 0.02

(* 線密度 *)
let rho1 = 1.0
let rho2 = 1.0

(* 把持質点の質量の不確かさの比率。dm = 0.1で質点が1 kgならば、質点の実際の質量
 * は[0.9, 1.1]に分布する *)
let dm = 0.1

(* 2次元ベクトル *)
type v2_t = float * float

(* 2次元零ベクトル *)
let v2_zero = (0., 0.)

let random_uniform a b =
  assert (b >= a);
  Random.float (b -. a) +. a

(* 手先位置 *)
let endpoint (q1, q2) (z1, z2) =
  let x = (l1 +. dl1 *. z1) *. cos (q1) +. (l2 +. dl2 *. z2) *. cos (q1 +. q2) in
  let y = (l1 +. dl1 *. z1) *. sin (q1) +. (l2 +. dl2 *. z2) *. sin (q1 +. q2) in
  (x, y)

(* ヤコビアン *)
let jacobian (q1, q2) (z1, z2) =
  let dxdq1 = -. (l1 +. dl1 *. z1) *. sin (q1) -. (l2 +. dl2 *. z2) *. sin (q1 +. q2) in
  let dxdq2 = -. (l2 +. dl2 *. z2) *. sin (q1 +. q2) in
  let dydq1 = (l1 +. dl1 *. z1) *. cos (q1) +. (l2 +. dl2 *. z2) *. cos (q1 +. q2) in
  let dydq2 = (l2 +. dl2 *. z2) *. cos (q1 +. q2) in
  (dxdq1, dxdq2, dydq1, dydq2)

(* 2x2行列 *)
type m22_t = float * float * float * float

(* 2x2零行列 *)
let m22_zero = (0., 0., 0., 0.)

let m22_transpose (a11, a12, a21, a22) =
  (a11, a21, a12, a22)

let m22_multiply (a11, a12, a21, a22) (b11, b12, b21, b22) =
  let c11 = a11 *. b11 +. a12 *. b21 in
  let c12 = a11 *. b12 +. a12 *. b22 in
  let c21 = a21 *. b11 +. a22 *. b21 in
  let c22 = a21 *. b12 +. a22 *. b22 in
  (c11, c12, c21, c22)

let m22_determinant (a, b, c, d) =
  a *. d -. b *. c

let m22_inverse ((a, b, c, d) as m) =
  let det = m22_determinant m in
  let idet = det ** (-1.0) in
  (idet *. d, -. idet *. b, -. idet *. c, idet *. a)

let string_of_m22 (a11, a12, a21, a22) =
  Printf.sprintf "%f,%f\n%f,%f" a11 a12 a21 a22

let manipulability q z =
  let j = jacobian q z in
  let jt = m22_transpose j in
  let jjt = m22_multiply j jt in
  let m = sqrt @@ m22_determinant jjt in
  match compare m nan with
    | 0 -> 0.0
    | _ -> m

let manipulability_tests =
  let t1 = nearly_equal (manipulability (0.0, 0.0) v2_zero) 0.0 in
  let t2 = nearly_equal (manipulability (pi, pi) v2_zero) 0.0 in
  let t3 = nearly_equal (manipulability (pi, 0.0) v2_zero) 0.0 in
  let t4 = nearly_equal (manipulability (0.0, pi) v2_zero) 0.0 in
  [t1; t2; t3; t4]

(* 質量mの質点を把持したときの慣性行列 *)
let inertia_matrix_with_grasp (q1, q2) (z1, z2, z3) m =
  let l1' = l1 +. dl1 *. z1 in
  let l2' = l2 +. dl2 *. z2 in
  let m1' = l1' *. rho1 in
  let m2' = l2' *. rho2 in
  let m3' = m +. m *. dm *. z3 in
  let inert1 = 1.0 /. 12.0 *. m1' *. l1' ** 2.0 in
  let inert2 = 1.0 /. 12.0 *. m2' *. l2' ** 2.0 in
  let m11 = inert1 +. inert2 +. l1' ** 2.0 *. m1' /. 4.0 +. l1' ** 2.0 *. m2' +. l1' ** 2.0 *. m3' +. l1' *. l2' *. m2' *. cos(q2) +. 2.0 *. l1' *. l2' *. m3' *. cos(q2) +. l2' ** 2.0 *. m2' /. 4.0 +. l2' ** 2.0 *. m3' in
  let m12 = inert2 +. l1' *. l2' *. m2' *. cos(q2) /. 2.0 +. l1' *. l2' *. m3' *. cos(q2) +. l2' ** 2.0 *. m2' /. 4.0 +. l2' ** 2.0 *. m3' in
  let m21 = inert2 +. l1' *. l2' *. m2' *. cos(q2) /. 2.0 +. l1' *. l2' *. m3' *. cos(q2) +. l2' ** 2.0 *. m2' /. 4.0 +. l2' ** 2.0 *. m3' in
  let m22 = inert2 +. l2' ** 2.0 *. m2' /. 4.0 +. l2' ** 2.0 *. m3' in
  (m11, m12, m21, m22)

(* 慣性行列 *)
let inertia_matrix q (z1, z2) = inertia_matrix_with_grasp q (z1, z2, 0.0) 0.0

let dynamic_manipulability q z =
  let j = jacobian q z in
  let jt = m22_transpose j in
  let m = inertia_matrix q z in
  let mt = m22_transpose m in
  let result = sqrt @@ m22_determinant @@ m22_multiply j @@ m22_multiply (m22_inverse @@ m22_multiply mt m) jt in
  match compare result nan with
    | 0 -> 0.0
    | _ -> result

let dynamic_manipulability_tests =
  let qs = List.init 10 (fun _ -> (random_uniform 0.0 (2. *. pi), random_uniform 0.0 (2. *. pi))) in
  let zs = List.init 10 (fun _ -> (random_uniform (-1.0) 1.0, random_uniform (-1.0) 1.0)) in
  (* 動的可操作性w_dと可操作性wのあいだには次の関係が成り立つことを確認する。
   *    w_d = w / det(im)
   * ここでimは慣性行列である *)
  let t1 =
    let f q z =
      let wd = dynamic_manipulability q z in
      let w = manipulability q z in
      let im = inertia_matrix q z in
      nearly_equal wd (w /. m22_determinant im)
      in
    List.fold_left ( && ) true (List.map2 f qs zs)
    in
  [t1]

let sum = List.fold_left ( +. ) 0.0

let list_mean data =
  assert (data <> []);
  let len = List.length data in
  sum data /. float len

let list_mean_tests =
  let t1 = list_mean [1.; 2.; 3.] = 2. in
  let t2 = list_mean [0.] = 0. in
  [t1; t2]

let list_variance data =
  let m = list_mean data in
  let len = List.length data in
  sum (List.map (fun d -> (m -. d) **2.) data) /. float len

let list_variance_tests =
  let t1 = list_variance [1.; 2.; 3.] = 2. /. 3. in
  let t2 = list_variance [0.] = 0. in
  let t3 = list_variance [1.; 1.; 1.] = 0. in
  [t1; t2; t3]

let list_stddev data =
  sqrt (list_variance data)

let list_stddev_tests =
  let t1 = list_stddev [1.; 2.; 3.] = sqrt (2. /. 3.) in
  let t2 = list_stddev [0.] = 0. in
  let t3 = list_stddev [1.; 1.; 1.] = 0. in
  [t1; t2; t3]

let list_range a b =
  let rec loop acc b = if b < a then acc else loop (b :: acc) (b - 1) in
  loop [] (b - 1)

let list_range_tests =
  let t1 = list_range 0 5 = [0; 1; 2; 3; 4] in
  let t2 = list_range 3 5 = [3; 4] in
  let t3 = list_range 3 0 = [] in
  [t1; t2; t3]

(* 計算結果がnanになるのを避けるための値 *)
let epsilon = 1E-10

let q_ticks = List.map (fun r -> float r *. 2. *. pi /. 32. +. epsilon) (list_range 0 32)

let z_ticks = List.map (fun r -> float r *. 2. /. 32. -. 1. +. epsilon) (list_range 0 32)

let q_list =
  List.map (fun q1 -> List.map (fun q2 -> (q1, q2)) q_ticks) q_ticks |> List.concat

let z_list =
  List.map (fun z1 -> List.map (fun z2 -> (z1, z2)) z_ticks) z_ticks |> List.concat

let dynm_mean_stddev_list =
  let f q =
    let dynm_list = List.map (fun z -> dynamic_manipulability q z) z_list in
    (list_mean dynm_list, list_stddev dynm_list) in
  List.map (fun q -> f q) q_list

let () =
  Printf.printf "q1,q2,mean,stddev,stddev/mean\n";
  let f (q1, q2) (mean, stddev) =
    Printf.printf "%17.15f,%17.15f,%17.15f,%17.15f,%17.15f\n" q1 q2 mean stddev (stddev /. mean) in
  List.iter2 f q_list dynm_mean_stddev_list

let run_tests () =
  println_tests "dynamic_manipulability_tests" dynamic_manipulability_tests;
  println_tests "nearly_equal_tests" nearly_equal_tests;
  println_tests "manipulability_tests" manipulability_tests;
  println_tests "list_mean_tests" list_mean_tests;
  println_tests "list_variance_tests" list_variance_tests;
  println_tests "list_stddev_tests" list_stddev_tests;
  println_tests "list_range_tests" list_range_tests
