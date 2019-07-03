open Lwt.Infix

let src = Logs.Src.create "dns_mirage_client" ~doc:"effectful DNS client layer"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (TCP: Mirage_protocols_lwt.TCP with type ipaddr = Ipaddr.V4.t)(UDP: Mirage_protocols_lwt.UDP) = struct

  module Uflow : Dns_client_flow.S
    with type tcp = TCP.t
     and type udp = UDP.t
     and type flow = TCP.flow
     and type (+'a,+'b) io = ('a, 'b) Lwt_result.t
           constraint 'b = [> `Msg of string]
     and type io_addr = Ipaddr.V4.t * int = struct

    type tcp = TCP.t
    type udp = UDP.t
    type flow = TCP.flow
    type io_addr = Ipaddr.V4.t * int
    type ns_addr = [`TCP | `UDP] * io_addr
    type (+'a,+'b) io = ('a, 'b) Lwt_result.t
      constraint 'b = [> `Msg of string]
    type t = {
      nameserver : ns_addr ;
      tcp : TCP.t ;
      udp : UDP.t ;
    }

    let create ?(nameserver = `TCP, (Ipaddr.V4.of_string_exn "91.239.100.100", 53)) tcp udp =
      { nameserver ; tcp ; udp }

    let nameserver { nameserver ; _ } = nameserver

    let map = Lwt_result.bind
    let resolve = Lwt_result.bind_result
    let lift = Lwt_result.lift

    let connect ?nameserver:ns t =
      let _proto, addr = match ns with None -> nameserver t | Some x -> x in
      TCP.create_connection t.tcp addr >|= function
      | Error e ->
        Log.err (fun m -> m "error connecting to nameserver %a"
                    TCP.pp_error e) ;
        Error (`Msg "connect failure")
      | Ok flow -> Ok flow

    let recv flow =
      TCP.read flow >|= function
      | Error e -> Error (`Msg (Fmt.to_to_string TCP.pp_error e))
      | Ok (`Data cs) -> Ok cs
      | Ok `Eof -> Ok Cstruct.empty

    let send flow s =
      TCP.write flow s >|= function
      | Error e -> Error (`Msg (Fmt.to_to_string TCP.pp_write_error e))
      | Ok () -> Ok ()
  end

  include Dns_client_flow.Make(Uflow)

end

(*
type dns_ty = Dns_client

let config : 'a Mirage.impl =
  let open Mirage in
  impl @@ object inherit Mirage.base_configurable
    method module_name = "Dns_client"
    method name = "Dns_client"
    method ty : 'a typ = Type Dns_client
    method! packages : package list value =
      (Key.match_ Key.(value target) @@ begin function
          | `Unix -> [package "dns-client-unix"]
          | _ -> []
        end
      )
    method! deps = []
  end
*)
