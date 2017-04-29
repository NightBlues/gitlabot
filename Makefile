build:
	ocamlbuild src/fetcher.native

clean:
	ocamlbuild -clean

env:
	opam depext ssl
	opam install lwt cohttp ppx_deriving_yojson ssl

