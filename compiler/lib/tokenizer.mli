open Jacklang

type t

val create : In_channel.t -> t
val next_token : t -> Token.t option
