module type S = sig
  type tcp
  type udp
  type flow
  type (+'a,+'b) io constraint 'b = [> `Msg of string]
  type io_addr
  type ns_addr = ([`TCP | `UDP]) * io_addr
  type t

  val create : ?nameserver:ns_addr -> tcp -> udp -> t

  val nameserver : t -> ns_addr

  val connect : ?nameserver:ns_addr -> t -> (flow,'err) io
  val send : flow -> Cstruct.t -> (unit,'b) io
  val recv : flow -> (Cstruct.t, 'b) io

  val resolve : ('a,'b) io -> ('a -> ('c,'b) result) -> ('c,'b) io
  val map : ('a,'b) io -> ('a -> ('c,'b) io) -> ('c,'b) io
  val lift : ('a,'b) result -> ('a,'b) io
end

module Make = functor (Uflow:S) ->
struct

  let create = Uflow.create

  let nameserver = Uflow.nameserver

  let getaddrinfo (type requested) t ?nameserver (query_type:requested Dns.Rr_map.key) name
    : (requested, [> `Msg of string]) Uflow.io =
    let proto, _ = match nameserver with None -> Uflow.nameserver t | Some x -> x in
    let tx, state =
      Dns_client.make_query
        (match proto with `UDP -> `Udp | `TCP -> `Tcp) name query_type
    in
    let (>>|) = Uflow.map in
    Uflow.connect ?nameserver t >>| fun socket ->
    Logs.debug (fun m -> m "Connected to NS.");
    Uflow.send socket tx >>| fun () ->
    let () = Logs.debug (fun m -> m "Receiving from NS") in
    let rec recv_loop acc =
      Uflow.recv socket >>| fun recv_buffer ->
      Logs.debug (fun m -> m "Read @[<v>%d bytes@]"
                     (Cstruct.len recv_buffer)) ;
      let buf = Cstruct.append acc recv_buffer in
      match Dns_client.parse_response state buf with
      | Ok x -> Uflow.lift (Ok x)
      | Error (`Msg xxx) ->
        Uflow.lift (Error (`Msg( "err: " ^ xxx)))
      | Error `Partial -> begin match proto with
          | `TCP -> recv_loop buf
          | `UDP -> Uflow.lift (Error (`Msg "Truncated UDP response")) end
    in recv_loop Cstruct.empty

  let gethostbyname t ?nameserver domain =
    let (>>=) = Uflow.resolve in
    getaddrinfo t ?nameserver Dns.Rr_map.A domain >>= fun (_ttl, resp) ->
    match Dns.Rr_map.Ipv4_set.choose_opt resp with
    | None -> Error (`Msg "No A record found")
    | Some ip -> Ok ip

  let gethostbyname6 t ?nameserver domain =
    let (>>=) = Uflow.resolve in
    getaddrinfo t ?nameserver Dns.Rr_map.Aaaa domain >>= fun (_ttl, res) ->
    match Dns.Rr_map.Ipv6_set.choose_opt res with
    | None -> Error (`Msg "No AAAA record found")
    | Some ip -> Ok ip
end
