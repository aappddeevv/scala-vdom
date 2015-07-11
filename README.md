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
stream a set of patches to a node for application (using patches and IOActions) and hence skip the entire vdom diffing process altogether.

scala-vdom is designed to have multiple, replacable layers:

* VDom: The virtual DOM layer that creates "nodes" that are eventually rendered into a
Backend specific UI widget set, such as the DOM.
* Patch: A recipe that describes how to change one VDom into another VDom. The recipe
sometimes uses a VDom instance to conveniently collect together the arguments for
describing that change, but it is not conceptually dependent on the VDom layer.
* IOAction: A monad that exuecutes side effects. Apply a Patch to a Backend specific
object is considered a side-effect. However, this layer is completely generally
and can be used  for a variety of compositional design patterns.
a Backend specific environment.
* Backend: A UI environment specific object that knows how to render a VNode, take
a patch and create a side-effecting action specific to a UI environment and how to
run an IOAction.

The layers have been created to allow scala-vdom to execute efficienly in multiple 
environments. scala-vdom was designed to run on clients and servers in both scalajs
and non-scalajs environments. Most of the core layers create immutable objects that
are Backend independent or at least highly decoupled from a Backend.

## VDom
Virtual DOM trees are built using the VDom classes. VDom objects are immutable. Use of 
VDom is entirely option as you could generate the patches directly yourself.

VDom trees can be created in multiple ways:

* Creating the VDom tree using a function you define. The function may use
application specific logic to customize the VDom tree to reflect changes in application state.
* Creating a singe VDom tree then using a lens (shapeless, scalaz, monacle) to mutate
the tree.
* Using a more friendly API such as scalatags. Scalatags needs to configured to
generate VDom objects instead of text or DOM objects.

The VDom class hierarchy is sealed. If you wish to write your own VDom type
so that you can add it to a tree, use `ThunkNode` and provide a function that
produces a Patch when diff is called. If you just want to produce a general Patch,
write a scala function to create a Patch. ThunkNode allows you to add that 
function as a child into a VitualElementNode.

## Patching
Patching is the process of updating a DOM node. The updates can be calculated a number
of different ways depending on your application.

* Through the `diff(original, target)` method. The diff method applies an 
external algorithm to create a Patch. The external algorithm also narrows down
the patch creation process so that a VNode's `diff` method is called on objects
that are of the same type. Each VNode knows how to `diff` with an object like itself
much in the same way that it can determine if it is equal to another object of the same
type.
* By creating the patches directly yourself based on your knowledge of which updates are
needed.
* By creating the patches directly yourself and sending them from the server to the client
where they are interpreted and applied to the DOM.
* Skip the VNode interface completely, and generate your own patches based on your own
virtual node concept.
* Using a scala lens (such as those in shapeless, scalaz or monacle) to change a single
element in a VDom tree and then running caling `diff`.
* Receiving a patch then using the IOAction combinators to continue to build your
own DOM mutations directly on the returned DOM elements. This approach is not recommended
of course. 

## Diffing
Diffing occurs at two levels.

The first level is to compare VNodes and find out if tree-level changes are needed. For
example, if there is no node but a node is being asked to be created, then the first level
of diff computation issues a patch to create a node.

The second level is at the node level. The diff algorithm will run a node level diff
if the VNodes are the same type. Each VNode knows how to diff with an object of the
same class but not with objets of another VNode class. This allows the diff logic to
be concentrated in the VNode class.

You can control the 

The diff algorithm tries to find the smallest and/or most efficient set of Patches
to mutate the original VDom into the target VDome.

Currently, there are no optimizations an the entire tree is re-created :-) Once teh basic
design of the library is fleshed out, I'll optimize the diff process.

## Issues
There are many issues that currently make this library unusable for production use. These 
issues will be resolved in future releases.

* There are no optimizations in the diff'ing algorithm. It's a straight diff of the entire tree.
* Adjacent VText nodes have bad behavior when rendered in the DOM. Browsers merge text nodes
together in some cases. The general rule is to avoid adjacent text nodes in your virtual dom.
* No support is provided for events.
* The presence of the correct ExceutionContext still needs to be traced and worked out so that
it can always be specified by the programmer.
* Should diff'ing and rendering have future return values to allow them to be async by default?
Not sure this makes sense in every backend environment. Can't the programmer just wrap it into
a future themselves if they want it async? There *are* side effects for rendering, potentially,
but probably not diff'ing.
* I've not take the time to add more attributes to the set and adding your own attributes
with custom hints is not easy at the moment, you have to create your own Backend object.

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

## Handling Events

This is currently a gap in the library as I am still researching what to do for events. It 
seems that attaching directly to a DOm object may be Ok but some virtual dom libraries
attach to the top of the tree.

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

## Virtual DOM implementations
Here's a list of virtual dom implementations that I looked at:

* react: Facebook's well known version
* diffDom: js
* virtual-dom: js
* incremental-dom: js, essentially reproduces a XAML-ish type environment. From google.
* dom-layer: js
* ember / glimmer: js, have not looked at this quite yet

There are a few more of course and I'll add the links when I update the documentation next.
