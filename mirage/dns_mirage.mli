(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

module Make
    (T : Mirage_protocols_lwt.TCP with type ipaddr = Ipaddr.V4.t)
    (U: Mirage_protocols_lwt.UDP with type ipaddr = Ipaddr.V4.t) : sig

  module IS : Set.S with type elt = Ipaddr.V4.t
  (** [IS] is a set of [ipaddr]. *)

  module IM : sig
    include Map.S with type key = Ipaddr.V4.t
    val find : Ipaddr.V4.t -> 'a t -> 'a option
  end
  (** [IM] is a map using [ipaddr] as key. *)

  module IPM : sig
    include Map.S with type key = Ipaddr.V4.t * int
    val find : Ipaddr.V4.t * int -> 'a t -> 'a option
  end
  (** [IPM] is a map using [ip * port] as key. *)

  type f
  (** A 2byte-length per message flow abstraction, the embedding of DNS frames
     via TCP. *)

  val of_flow : T.flow -> f
  (** [of_flow flow] is [f]. *)

  val flow : f -> T.flow
  (** [flow f] is the underlying flow. *)

  val read_tcp : f -> (Cstruct.t, unit) result Lwt.t
  (** [read_tcp f] returns either a buffer or an error (logs actual error). *)

  val send_tcp : T.flow -> Cstruct.t -> (unit, unit) result Lwt.t
  (** [send_tcp flow buf] sends the buffer, either succeeds or fails (logs
     actual error). *)

  val send_udp : U.t -> int -> Ipaddr.V4.t -> int -> Cstruct.t -> unit Lwt.t
  (** [send_udp stack source_port dst dst_port buf] sends the [buf] as UDP
     packet to [dst] on [dst_port]. *)
end
