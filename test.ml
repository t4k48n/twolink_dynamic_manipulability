open Utils

open Model

let list_mean_tests =
  let t1 = list_mean [1.; 2.; 3.] = 2. in
  let t2 = list_mean [0.] = 0. in
  [t1; t2]

let list_variance_tests =
  let t1 = list_variance [1.; 2.; 3.] = 2. /. 3. in
  let t2 = list_variance [0.] = 0. in
  let t3 = list_variance [1.; 1.; 1.] = 0. in
  [t1; t2; t3]

let list_stddev_tests =
  let t1 = list_stddev [1.; 2.; 3.] = sqrt (2. /. 3.) in
  let t2 = list_stddev [0.] = 0. in
  let t3 = list_stddev [1.; 1.; 1.] = 0. in
  [t1; t2; t3]

let list_range_tests =
  let t1 = list_range 0 5 = [0; 1; 2; 3; 4] in
  let t2 = list_range 3 5 = [3; 4] in
  let t3 = list_range 3 0 = [] in
  [t1; t2; t3]

let list_linspace_tests =
  let t1 = list_linspace 0.0 1.0 2 = [0.0; 1.0] in
  let t2 = list_linspace 1.0 2.0 3 = [1.0; 1.5; 2.0] in
  let t3 = list_linspace ~endpoint:false 1.0 2.0 2 = [1.0; 1.5] in
  let t4 = list_linspace ~endpoint:false 0.0 5.0 5 = [0.0; 1.0; 2.0; 3.0; 4.0] in
  [t1; t2; t3; t4]

let list_init_tests =
  let t1 = list_init [1] = [] in
  let t2 = list_init [1; 2] = [1] in
  let t3 = list_init [true; true; true; false; false] = [true; true; true; false] in
  [t1; t2; t3]

let nearly_equal_tests =
  let t1 = nearly_equal 1.0 1.1 = false in
  let t2 = nearly_equal 1.0 1.0001 = false in
  let t3 = nearly_equal 1.0 1.00000001 = true in
  let t4 = nearly_equal ~threshold:1.0 1.0 1.9 = true in
  [t1; t2; t3; t4]

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

let manipulability_tests =
  let t1 = nearly_equal (manipulability (0.0, 0.0) v2_zero) 0.0 in
  let t2 = nearly_equal (manipulability (pi, pi) v2_zero) 0.0 in
  let t3 = nearly_equal (manipulability (pi, 0.0) v2_zero) 0.0 in
  let t4 = nearly_equal (manipulability (0.0, pi) v2_zero) 0.0 in
  [t1; t2; t3; t4]

let () =
  println_tests "dynamic_manipulability_tests" dynamic_manipulability_tests;
  println_tests "nearly_equal_tests" nearly_equal_tests;
  println_tests "manipulability_tests" manipulability_tests;
  println_tests "list_mean_tests" list_mean_tests;
  println_tests "list_variance_tests" list_variance_tests;
  println_tests "list_stddev_tests" list_stddev_tests;
  println_tests "list_range_tests" list_range_tests;
  println_tests "list_linspace_tests" list_linspace_tests;
  println_tests "list_init_tests" list_init_tests
