# Vdoml = virtual DOM + OCaml

### _Exceedingly_ work-in-progress.

Vdoml provides an elm-like (also react+flux-like) UI library for OCaml.

It's already possible to build reactive HTML using [dbuenzli/react](http://erratique.ch/software/react) (note: _nothing like_ Facwbook's react) with Tyxml_js. But that can get awkward with interconnected networks of signals, which are hard to debug and force you to architect your code in particular ways to avoid lots of unnecessary rendering work.

By including a VDOM algorithm, the rendering and update code can be made (more or less) pure and stateless, which should be easier to reason about and test, while taking advantage of immutability to cut out unnecessary re-rendering.

It's still an experiment - I could be wrong :)

See examples/ for a TodoMVC example.

I haven't actually _used_ elm, react or flux. And I haven't thought too hard about how it will work more broadly with asynchronous (lwt) behaviour. So it's all very subject to change.

Thanks a lot to the [Ocsigen](http://ocsigen.org/) folks for Lwt, Js_of_ocaml and Tyxml - without any of these this project would be dramatically harder. Also some of the Html module is adopted from [Jane Street's virtual_dom](https://github.com/janestreet/virtual_dom)
