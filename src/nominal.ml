let uid_counter = ref 0

type t = {
  uid : int;
  initial_name : string
}

let uid_of_t t =
  t.uid

let string_of_t t =
  t.initial_name

let t_of_string initial_name =
  let uid = !uid_counter in
  incr uid_counter;
  { uid ; initial_name }
