open Das_helpers

let do_hash = Crypto.blake2b
let hash_empty = Bytes.empty
let do_hash_pair l r = do_hash @@ Bytes.cat l r
