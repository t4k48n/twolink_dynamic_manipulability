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
