(** Key management for envelope encryption.

    A tenant has a key-encryption key, versioned and rotated, that wraps the
    data-encryption keys its data is sealed with; deleting the tenant's key-encryption
    keys is crypto-shredding. This library is the model, {!Master_key}, {!Kek},
    {!Wrapped_key}, over the primitive, {!Cipher}, {!Algorithm}, {!Key}; the port,
    {!Kms_port.S}; and {!Cached}, a cache in front of any service. The adapters are
    sub-libraries: [ascetic_ddd.kms.pg] keeps keys in a PostgreSQL table under a master
    key, [ascetic_ddd.kms.vault] leaves them to HashiCorp Vault Transit. See [README.md].
*)

module Key = Key
module Key_version = Key_version
module Wrapped_key = Wrapped_key
module Cipher = Cipher
module Algorithm = Algorithm
module Aes256gcm = Aes256gcm
module Kek = Kek
module Master_key = Master_key
module Kms_error = Kms_error
module Kms_port = Kms_port
module Cached = Cached
