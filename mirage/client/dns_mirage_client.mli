
module Make (TCP: Mirage_protocols_lwt.TCP with type ipaddr = Ipaddr.V4.t)(UDP: Mirage_protocols_lwt.UDP) : sig
  module Uflow : Dns_client_flow.S
    with type flow = TCP.flow
     and type io_addr = Ipaddr.V4.t * int
     and type (+'a, +'b) io = ('a, 'b) Lwt_result.t

  include module type of Dns_client_flow.Make(Uflow)
end

(*
type dns_ty

val config : dns_ty Mirage.impl
(** [config] is the *)

module Make :
  functor (Time:Mirage_time_lwt.S) ->
  functor (IPv4:Mirage_stack_lwt.V4) ->
    S

*)
