open Utils

open Model

let postures =
  let ticks = list_linspace 0.0 (2.0 *. pi) 361 in
  (* List.concat @@ List.map (fun q1 -> List.map (fun q2 -> (q1, q2)) ticks) ticks *)
  let q1 = pi /. 4.0 in
  List.map (fun q2 -> (q1, q2)) ticks

let uncertainties =
  let ticks = list_linspace (-1.0) 1.0 21 in
  List.concat @@ List.concat @@ List.map (fun q1 -> List.map (fun q2 -> List.map (fun q3 -> (q1, q2, q3)) ticks) ticks) ticks

(* MC法で求めた各姿勢における動的可操作性 *)
let dynamic_manipulabilities_with_monte_carlo =
  List.map (fun p -> List.map (fun z -> dynamic_manipulability_with_grasp p z 1.0) uncertainties) postures

let () =
  let dynman_means = List.map list_mean dynamic_manipulabilities_with_monte_carlo in
  let dynman_variances = List.map list_variance dynamic_manipulabilities_with_monte_carlo in
  let lines_to_be_printed = List.combine postures @@ List.combine dynman_means dynman_variances in
  let b = Buffer.create 0 in
  Buffer.add_string b
    "import numpy\n\
     import matplotlib.pyplot as pyplot\n\
     data = numpy.matrix('";
  List.iter (fun ((q1, q2), (m, v)) -> Buffer.add_string b (Printf.sprintf "%f %f %f %f;" q1 q2 m v)) lines_to_be_printed;
  Buffer.add_string b
    "'[:-1])\n\
     pyplot.subplot(311)\n\
     pyplot.plot(data[:,1], data[:,2])\n\
     pyplot.subplot(312)\n\
     pyplot.plot(data[:,1], data[:,3])\n\
     pyplot.subplot(313)\n\
     pyplot.plot(data[:,1], data[:,2]/data[:,3])\n\
     pyplot.show()\n\
     ";
  let pyf = open_out "main.ml.py" in
  Printf.fprintf pyf "%s" (Buffer.contents b);
  close_out pyf;
  let _ = Unix.system "pipenv run python3 main.ml.py" in ()
