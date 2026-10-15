(** Key management for a tenant's keys.

    For envelope encryption: a tenant has a key-encryption key, versioned and rotated, that
    wraps the data-encryption keys its data is sealed with; deleting the tenant's
    key-encryption keys is crypto-shredding. For what a tenant signs and authenticates:
    digital signatures under the tenant's key of an algorithm, MACs under the tenant's key
    for a purpose. And custody of the KEK, apart from wrapping with it. This library is
    the model, {!Master_key}, {!Kek}, {!Wrapped_key}, {!Signing_key}, {!Mac_key}, over the
    primitives, {!Cipher}, {!Algorithm}, {!Key}; the ports, {!Kms_port.S},
    {!Key_custody.S}, {!Signing_port.S}, {!Mac_port.S}; {!Cached}, a cache in front of any
    service; and {!In_memory_keys}, for the tests of what consumes the signing and MAC
    ports. The adapters are sub-libraries: [ascetic_ddd.kms.pg] keeps keys in PostgreSQL
    tables under a master key, [ascetic_ddd.kms.vault] leaves them to HashiCorp Vault
    Transit. See [README.md]. *)

module Key = Key
module Key_version = Key_version
module Wrapped_key = Wrapped_key
module Cipher = Cipher
module Algorithm = Algorithm
module Aes256gcm = Aes256gcm
module Kek = Kek
module Kek_options = Kek_options
module Master_key = Master_key
module Signature_algorithm = Signature_algorithm
module Signature = Signature
module Public_key = Public_key
module Signing_key = Signing_key
module Key_label = Key_label
module Mac = Mac
module Mac_key = Mac_key
module Kms_error = Kms_error
module Kms_port = Kms_port
module Key_custody = Key_custody
module Signing_port = Signing_port
module Mac_port = Mac_port
module Cached = Cached
module In_memory_keys = In_memory_keys
