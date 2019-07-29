(** {!Lwt_unix} helper module for {!Dns_client}.
    For more information see the {!Dns_client_flow.Make} functor.
*)


(** A flow module based on non-blocking I/O on top of the
    Lwt_unix socket API. *)
module Uflow : Dns_client_flow.S
  with type flow = Lwt_unix.file_descr
   and type io_addr = Lwt_unix.inet_addr * int
   and type (+'a,+'b) io = ('a,'b) Lwt_result.t
   and type tcp = unit
   and type udp = unit

include module type of Dns_client_flow.Make(Uflow)
