type t = int

let size = 4
let max = 0xFFFF_FFFF
let first = 1

let of_int version =
  if version >= 0 && version <= max then Ok version
  else
    Error
      (Kms_error.Malformed
         (Printf.sprintf "%d is not a key version: a version is from 0 to %d" version max))

let of_int_exn version =
  match of_int version with
  | Ok version -> version
  | Error error -> invalid_arg (Kms_error.to_string error)

let to_int t = t

let next t =
  if t < max then Ok (t + 1)
  else Error (Kms_error.Malformed (Printf.sprintf "no key version after %d" t))

let stamp t bytes =
  let out = Bytes.create (size + String.length bytes) in
  (* [Int32.of_int] keeps the low thirty-two bits, which is the version. *)
  Bytes.set_int32_be out 0 (Int32.of_int t);
  Bytes.blit_string bytes 0 out size (String.length bytes);
  Bytes.unsafe_to_string out

let read ~what bytes =
  if String.length bytes < size then
    Error
      (Kms_error.Malformed
         (Printf.sprintf "%d bytes cannot be %s: the version alone is %d"
            (String.length bytes) what size))
  else
    Ok
      ( Int32.to_int (String.get_int32_be bytes 0) land max,
        String.sub bytes size (String.length bytes - size) )

let equal = Int.equal
let compare = Int.compare
let pp = Format.pp_print_int
