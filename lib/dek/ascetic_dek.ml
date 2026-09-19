(** Data-encryption keys, one per resource and versioned, wrapped by the tenant's
    key-encryption key through [ascetic_ddd.kms].

    The lower half of envelope encryption, the KMS being the upper, for whatever a
    repository keeps under a key of its own: an aggregate's stream in an event store, a
    document. This library is the model, {!Resource}, {!Versioned_cipher}, {!Keyring}, and
    the port, {!Dek_store.S}. [ascetic_ddd.dek.pg] keeps the keys in a PostgreSQL table;
    [ascetic_ddd.dek.envelope] is the sealing stage of the bus. See [README.md]. *)

module Resource = Resource
module Canonical = Canonical
module Versioned_cipher = Versioned_cipher
module Keyring = Keyring
module Dek_error = Dek_error
module Dek_store = Dek_store
