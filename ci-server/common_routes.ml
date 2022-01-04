type t = {
  server_name: string;
  key_name: string;
  priv_key: Mirage_crypto_ec.Ed25519.priv;
  pub_key: Mirage_crypto_ec.Ed25519.pub;
  ctx: Mimic.ctx;
  store: Store.Store.t
}
