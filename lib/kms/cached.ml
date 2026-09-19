module Make (K : Kms_port.S) = struct
  type session = K.session

  (* [order] breaks the tie between keys unwrapped at the same reading of the
     clock: the oldest is the one that came first. *)
  type entry = { key : Key.t; since : Mtime.t; order : int }

  type t = {
    kms : K.t;
    capacity : int;
    ttl : float;
    clock : Eio.Time.Mono.ty Eio.Resource.t;
    entries : (string * string, entry) Hashtbl.t;
    mutable arrivals : int;
    mutex : Mutex.t;
        (** The sections under it touch memory and nothing else, so no fiber waits inside
            one; the lock is for sessions of several domains. *)
  }

  let default_capacity = 1_000
  let default_ttl = 300.0

  let create ?(capacity = default_capacity) ?(ttl = default_ttl) ~clock kms =
    {
      kms;
      capacity;
      ttl;
      clock :> Eio.Time.Mono.ty Eio.Resource.t;
      entries = Hashtbl.create 64;
      arrivals = 0;
      mutex = Mutex.create ();
    }

  let inner t = t.kms
  let length t = Mutex.protect t.mutex (fun () -> Hashtbl.length t.entries)
  let is_empty t = length t = 0

  let fresh t ~now entry =
    Mtime.Span.to_float_ns (Mtime.span entry.since now) /. 1e9 < t.ttl

  (* The key unwrapped for the tenant from the wrapped bytes, if still kept. *)
  let get t ~tenant_id wrapped =
    let now = Eio.Time.Mono.now t.clock in
    Mutex.protect t.mutex (fun () ->
        match Hashtbl.find_opt t.entries (tenant_id, wrapped) with
        | Some entry when fresh t ~now entry -> Some entry.key
        | Some _ | None -> None)

  let oldest entries =
    Hashtbl.fold
      (fun id entry oldest ->
        match oldest with
        | Some (_, other) when other.order <= entry.order -> oldest
        | Some _ | None -> Some (id, entry))
      entries None

  (* Keeps the key, making room first: expired entries go, then the oldest. *)
  let put t ~tenant_id wrapped key =
    if t.capacity > 0 && t.ttl > 0.0 then begin
      let now = Eio.Time.Mono.now t.clock in
      Mutex.protect t.mutex (fun () ->
          Hashtbl.filter_map_inplace
            (fun _ entry -> if fresh t ~now entry then Some entry else None)
            t.entries;
          let rec make_room () =
            if Hashtbl.length t.entries >= t.capacity then
              match oldest t.entries with
              | Some (id, _) ->
                  Hashtbl.remove t.entries id;
                  make_room ()
              | None -> ()
          in
          make_room ();
          t.arrivals <- t.arrivals + 1;
          Hashtbl.replace t.entries (tenant_id, wrapped)
            { key; since = now; order = t.arrivals })
    end

  let forget t ~tenant_id =
    Mutex.protect t.mutex (fun () ->
        Hashtbl.filter_map_inplace
          (fun (tenant, _) entry ->
            if String.equal tenant tenant_id then None else Some entry)
          t.entries)

  let encrypt_dek t session ~tenant_id dek = K.encrypt_dek t.kms session ~tenant_id dek

  (* The cached key, or the service's, kept. *)
  let decrypt_dek t session ~tenant_id encrypted_dek =
    match get t ~tenant_id encrypted_dek with
    | Some key -> Ok key
    | None ->
        Result.map
          (fun key ->
            put t ~tenant_id encrypted_dek key;
            key)
          (K.decrypt_dek t.kms session ~tenant_id encrypted_dek)

  let generate_dek t session ~tenant_id = K.generate_dek t.kms session ~tenant_id
  let rotate_kek t session ~tenant_id = K.rotate_kek t.kms session ~tenant_id

  let rewrap_dek t session ~tenant_id encrypted_dek =
    K.rewrap_dek t.kms session ~tenant_id encrypted_dek

  (* Deletes through the service and forgets the tenant's keys held here. *)
  let delete_kek t session ~tenant_id =
    Result.map (fun () -> forget t ~tenant_id) (K.delete_kek t.kms session ~tenant_id)
end
