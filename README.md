# scala-vdom

scala-vdom is a simple virtual DOM written entirely in scala. It is based heavily on virtual-dom
 and other virtual DOM implementations.

This library is currently a proof of concept. See [[Issues]]

The library is initially targeted for the client using scala.js, it will also target the server side
perhaps with phantomjs or some otherclever rendering approach.

This virtual DOM implementation supports:

* HTML5

The virtual DOM may work across browser vendors although quirks in those browse may cause problems.

## Issues
There are many issues that currently make this library unusable for production use. These 
issues will be resolved in future releases.

* There are no optimizations in the diff'ing algorithm. It's a straight diff of the entire tree.
* Adjacent VText nodes have bad behavior when rendered in the DOM. Browsers merge text nodes
together in some cases. The general rule is to avoid adjacent text nodes in your virtual dom.
* No support is provided for callbacks.
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
