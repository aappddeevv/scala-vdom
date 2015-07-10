# scala-vdom

scala-vdom is a simple virtual DOM written entirely in scala. It is based heavily on virtual-dom
 and other virtual DOM implementations.

This library is currently a proof of concept. See [Issues]

The library is initially targeted for the client using scala.js, it will also target the server side
perhaps with phantomjs or some otherclever rendering approach.

This virtual DOM implementation supports:

* HTML5

The virtual DOM may work across browser vendors although quirks in those browse may cause problems.

Note that the implementation approach allows you to use scala-vdom in multiple ways. For example,
you can use it as a traditional virtual dom with diffing the full tree or you can use it to 
stream a set of patches to a node for application (using patches and IOActions) and hence skip the entire
vdom diffing process altogether. This last approach is much like the [github.com/incremental-dom](incremental-dom) 
approach. In the "incremental" model, patches hold content information
that is applied incrementally to the node. Note that the incremental-dom model is essentially
like a XAML processor from the MS-world. XAML processors take XML and send a stream
of "element" building instructions to an engine. This last approach does require significant
amounts of state in the instruction processing engine but does remove the need for diffing
DOM trees. Notice that we get this for free by explicitly modeling patches as a programmer-level
class versus making it an opaque object. incremental-dom actually does store the virtual tree,
it just stores it directly in the DOM elements themselves (:-)).

It's weird how all of the implementations that deal with trees and UIs are similar but slightly
 different across the virtual dom world and many approaches used in the past and current.


## Issues
There are many issues that currently make this library unusable for production use. These 
issues will be resolved in future releases.

* There are no optimizations in the diff'ing algorithm. It's a straight diff of the entire tree.
* Adjacent VText nodes have bad behavior when rendered in the DOM. Browsers merge text nodes
together in some cases. The general rule is to avoid adjacent text nodes in your virtual dom.
* No support is provided for events.
* There is no convenient virtual script-like interface. In the future, scalatag's will be adapted
to produce virtual nodes.
* While all backend specific code is isolated into a "Backend" object, the use of ElementAction
seems duplicative. These classes need to be more cleverly refactored.
* The presence of the correct ExceutionContext still needs to be traced and worked out so that
it can always be specified by the programmer.
* Should diff'ing and rendering have future return values to allow them to be async by default?
Not sure this makes sense in every backend environment. Can't the programmer just wrap it into
a future themselves if they want it async? There *are* side effects for rendering, potentially,
but probably not diff'ing.
* Attributes/properties are not back-end independent. I need to merge the ElementAction model
into the core IOAction model while retaining the ability to link get/set information to the
attribute/property specification.


## Setting Attributes & Properties

Most vdom libraries allow you to set attributes and properties on a javascript object. Technically
all values that can be specified on an element are either approved attributes (like `id`) or 
customer attributes specified by `aria-` or `data-`. Other key-value pairs are not technically
allowed as  attributes in HTML, specifically the markup. However, when programmin the DOM using
javascript, some attribuse are set when their
object properties are set e.g. `myHTMLElement[id] = 'foo'` actually sets the `id` attribute
as if `myHTMLElement.setAttribute('id', 'foo')` had been called.

In the object representation of the DOM used when programming with javascript, many
libraries specify properties on the objects. These properties either assist the UI
programming design of a specific library or are set using index notation as a short-hand instead
of using the `setAttribute` methods. The HTML specs actually suggest using the index notation
instead of `setAttribute` for some attributes to improve cross-browser compatibility. 

## Toy Example

Assume that `test7` is an id in your DOM where you want the toy example to render into:

```scala
    //
    // Expanding box
    //
     val target7 = document.getElementById("test7")

    def box(count: Int) = VNode("div", Some("box"),
      Seq(textAlign := "center", lineHeight := s"${100 + count}px",
        border := "1px solid red", width := s"${100 + count}px", height := s"${100 + count}px"),
      VNode(count.toString))

    var count = 0
    var tree = box(count)
    var rootNode = DOMBackend.render(tree)
    rootNode.foreach(target7.appendChild(_)) // manual append

    val cancel = setInterval(1000) {
      count += 1
      val newTree = box(count)
      val patch = diff(tree, newTree)
      rootNode.flatMap { n => DOMBackend.run(patch(n)) }
      tree = newTree
    }
    setTimeout(10 seconds)(clearInterval(cancel))
```
