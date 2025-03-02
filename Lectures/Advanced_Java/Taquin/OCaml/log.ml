open Printf

let log0 format =
  Printf.fprintf stderr format;
  flush stderr

let log1 format x1 =
  Printf.fprintf stderr format x1;
  flush stderr

let log2 format x1 x2 =
  Printf.fprintf stderr format x1 x2;
  flush stderr

let log3 format x1 x2 x3 =
  Printf.fprintf stderr format x1 x2 x3;
  flush stderr

let log4 format x1 x2 x3 x4 =
  Printf.fprintf stderr format x1 x2 x3 x4;
  flush stderr

let die0 format =
  log0 format;
  exit 1

let die1 format x1 =
  log1 format x1;
  exit 1

let die2 format x1 x2 =
  log2 format x1 x2;
  exit 1

let die3 format x1 x2 x3 =
  log3 format x1 x2 x3;
  exit 1

let die4 format x1 x2 x3 x4 =
  log4 format x1 x2 x3 x4;
  exit 1

