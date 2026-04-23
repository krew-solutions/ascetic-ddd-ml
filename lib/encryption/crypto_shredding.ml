(** Crypto-Shredding — right to erasure via key destruction.

    Instead of deleting individual records (which is complex with
    event sourcing), destroy the encryption key. All data encrypted
    with that key becomes permanently unreadable.

    - Community-level shredding: destroy the community KEK
    - Member-level shredding: destroy the member's DEK *)

module type KEY_DESTROYER = sig
  val destroy_dek : key_id:string -> (unit, string) result
  (** Destroy a single DEK — makes one aggregate's data unreadable. *)

  val destroy_community_kek : community_id:string -> (unit, string) result
  (** Destroy a community KEK — makes ALL data in the community unreadable.
      This also invalidates all DEKs encrypted with this KEK. *)
end

type shred_result = {
  keys_destroyed : int;
  aggregates_affected : int;
}
[@@deriving show]

let shred_member (module D : KEY_DESTROYER) ~key_id =
  match D.destroy_dek ~key_id with
  | Ok () -> Ok { keys_destroyed = 1; aggregates_affected = 1 }
  | Error e -> Error e

let shred_community (module D : KEY_DESTROYER) ~community_id =
  match D.destroy_community_kek ~community_id with
  | Ok () -> Ok { keys_destroyed = 1; aggregates_affected = 0 }
  | Error e -> Error e
