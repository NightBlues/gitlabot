build:
	ocamlbuild src/fetcher.native

clean:
	ocamlbuild -clean

env:
	opam depext ssl conf-libev
	opam install conf-libev lwt lwt_ssl cohttp ppx_deriving_yojson ssl

