/**
 * Copyright 2015 Devon Miller
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.im
package events

import scalajs.js
import org.scalajs.dom
import scala.language.implicitConversions

/**
 * When processing events using a Delegate, Matcher is used to determine if the
 * delegate function should be called on the node in the traversal
 * from the node where the event occurred to the Delegate's root.
 * Return true to allow a Delegate to process the current node in the
 * traversal.
 */
trait Matcher extends Function2[dom.EventTarget, dom.EventTarget, Boolean] {
  def and(rhs: Matcher) = AndMatch(this, rhs)
  def &&(rhs: Matcher) = and(rhs)
  def or(rhs: Matcher) = OrMatch(this, rhs)
  def ||(rhs: Matcher) = or(rhs)
  def not = NotMatch(this)
  def unary_! = not
}

private[events] case class NotMatch(matcher: Matcher) extends Matcher {
  def apply(root: dom.EventTarget, current: dom.EventTarget) = !matcher(root, current)
}

private[events] case class AndMatch(lhs: Matcher, rhs: Matcher) extends Matcher {
  def apply(root: dom.EventTarget, current: dom.EventTarget) = lhs(root, current) && rhs(root, current)
}

private[events] case class OrMatch(lhs: Matcher, rhs: Matcher) extends Matcher {
  def apply(root: dom.EventTarget, current: dom.EventTarget) = lhs(root, current) || rhs(root, current)
}

/**
 * Convenience constructors.
 */
object Matcher {

  def apply(f: Function2[dom.EventTarget, dom.EventTarget, Boolean]) =
    new Matcher { def apply(root: dom.EventTarget, current: dom.EventTarget) = f(root, current) }

  /**
   * Match nothing.
   */
  val NoMatch = Matcher { (_, _) => false }

  /**
   * Match everything.
   */
  val MatchAll = Matcher { (_, _) => true }

  /**
   * Match on the tag. Current node must be an Element.
   */
  def MatchTag(tagName: String) = Matcher {
    (_, _) match {
      case (_, el: dom.Element) if (el.tagName.toLowerCase == tagName.toLowerCase) => true
      case _ => false
    }
  }

  /**
   * True if the current node is the root.
   */
  def MatchRoot = Matcher { (root, target) => root == target }

  /**
   * Polyfill but not quite as robust as the reference below.
   *
   * @see [matches](https://developer.mozilla.org/en-US/docs/Web/API/Element/matches)
   */
  private[this] def matches(el: dom.EventTarget, selector: String): Boolean = {
    val ns = dom.document
    val qmatches = ns.querySelectorAll(selector)
    for (i <- 0 until qmatches.length)
      if (qmatches(i) == el) return true
    false
  }

  /**
   * True if the potential target matches a selector.
   *
   * @see [HTML selectors](http://www.w3.org/TR/CSS21/selector.html%23id-selectors)
   */
  def MatchSelector(selector: String) = Matcher { (_, el: dom.EventTarget) => matches(el, selector) }

  /**
   * True if the target node is an Element and has a matching id.
   */
  def MatchId(id: String) = Matcher {
    (_, _) match {
      case (_, el: dom.Element) => el.id == id
      case _ => false
    }
  }
}

/**
 * A listener receives a dom.Event and the current Node that
 * is allowed to process the event. Return true if the propagation up
 * the tree should continue and false if it should stop. The handler
 * is a scala function because the scala machinery eventually
 * retrieves the handler and executes it.
 *
 */
trait Handler extends scala.Function2[dom.Event, dom.Node, Boolean]

/**
 * Importing Handler.Implicits into scope brings in some implicits for automatic
 * conversion of scala functions to Handler objects.
 *
 */
object Handler {
  def apply(f: scala.Function2[dom.Event, dom.Node, Boolean]) = new Handler {
    def apply(event: dom.Event, node: dom.Node) = f(event, node)
  }

  def apply(f: scala.Function1[dom.Event, Boolean]) = new Handler {
    def apply(event: dom.Event, node: dom.Node) = f(event)
  }

  object Implicits {
    implicit def toHandler(f: (dom.Event, dom.Node) => Boolean) = Handler(f)
    implicit def toHandlerUnit(f: (dom.Event, dom.Node) => Unit) = new Handler {
      def apply(event: dom.Event, node: dom.Node) = {
        f(event, node)
        true
      }
    }
    implicit def toHandler1(f: dom.Event => Boolean) = Handler(f)
  }
}

/**
 * A handler, a matcher and a capture flag. The matcher and capture flag
 * are used to qualify whether a handler is used in processing an event.
 * Capture refers to the capturing or bubbling phases of event processing.
 */
private[events] case class QualifiedHandler(handler: Handler, matcher: Matcher = Matcher.MatchRoot, capture: Boolean = false)

/**
 * An object that allows a side-effecting call to `cancel`.
 * `delegate` is stuck in there for convienence.
 */
trait Cancelable {
  def cancel(): Unit
  def delegate(): Delegate
}

/**
 * Delegate all event calls on the root to registered handlers.
 * You can change the root object at any time and the handlers
 * are properly deregistered/registered. The delegate is *not*
 * attached to the DOM element. The calling program should do that
 * if desired.
 *
 * The approach used is standard logic in java swing programs
 * with jgoodies.
 *
 * @see [UI EVents](http://www.w3.org/TR/DOM-Level-3-Events/#interface-EventListener)
 */
case class Delegate(private[events] var root: Option[dom.EventTarget] = None,
    private[events] val handlers: collection.mutable.Map[String, collection.mutable.Set[QualifiedHandler]] = collection.mutable.Map.empty) {
  self =>

  /**
   * Construct with a specific root.
   */
  def this(root: dom.EventTarget) = this(Some(root))

  /**
   * Handle an event. This is the universal listener attached
   * to the root. A mechanism is in place to not process an event
   * when it crosses to a different Delegate instances that may have
   * attached handlers further up the tree and the event has been
   * marked to be ignored by delegate processing.
   * 
   * Handlers can return a false value to indicate that delegates
   * should ignore the event.
   *
   * @see [eventPhase](https://developer.mozilla.org/en-US/docs/Web/API/Event/eventPhase)
   */
  @js.annotation.JSExport
  protected def handler(event: dom.Event): Unit = {

    import js.DynamicImplicits.truthValue

    // If a special marker is found, other instances of Delegate 
    // found up the chain should ignore this event as well.
    if (truthValue(event.asInstanceOf[js.Dynamic].__DELEGATEIGNORE))
      return

    var target =
      event.target match {
        case d: dom.Node if (d.nodeType == 3) => d.parentNode
        case n@_ => n.asInstanceOf[dom.Node]
      }

    // Build a listener list to process based on the event type and phase...
    // If eventPhase is defined, use it, otherwise, determine it from the targets...
    val phase =
      if (truthValue(event.eventPhase.asInstanceOf[js.Dynamic])) event.eventPhase
      else if (event.target != event.currentTarget) 3 // bubbling
      else 2 // at target

    // filter registered handlers based on whether they are for capture and the processing phase.
    val registeredHandlers = handlers.getOrElse(event.`type`, Set.empty).filter { qhandler =>
      if (qhandler.capture && (phase == 1 || phase == 2)) true
      else if (!qhandler.capture && (phase == 2 || phase == 3)) true
      else false
    }

    var cont = true
    root.foreach { rt =>
      while (target != null) {
        registeredHandlers.foreach { qhandler =>
          if (qhandler.matcher(rt, target)) {
            cont = qhandler.handler(event, target.asInstanceOf[dom.Node])
          }
          if (!cont) {
            event.asInstanceOf[js.Dynamic].__DELEGATEIGNORE = true
            event.preventDefault
            return // ouch! rework logic so this is not needed in the middle
          }
        }
        if (target == rt)
          return
        target = target.parentNode
      }
    }
  }

  /**
   * List of events that by default should be captured versus bubbled.
   */
  private[events] val captureList = Seq("abort", "blur",
    "error", "focus", "load",
    "mouseenter", "mouseleave",
    "resize", "scroll", "unload")

  /**
   * Whether the event should by default, be processed in the capture phase or not.
   */
  protected def isDefaultCapture(eventName: String) = captureList.contains(eventName)

  /**
   * Apply this configured Delegate to the root and return a new Delegate.
   * You can attach to the root `Document.documentElement` to listen to
   * events globally.
   */
  def root(el: Option[dom.EventTarget]): Delegate = {
    root.foreach(stopListeningTo(_))
    root = el
    el.foreach(startListeningTo(_))
    this
  }

  protected def stopListeningTo(el: dom.EventTarget): Unit = {
    require(el != null)
    val stops = collection.mutable.ListBuffer[(String, QualifiedHandler)]()
    for {
      et <- handlers.keys
      setOfQL <- handlers.get(et)
      ql <- setOfQL
    } { stops += ((et, ql)) }
    stops.foreach { p =>
      el.removeEventListener(p._1, handler _, p._2.capture)
    }
  }

  protected def startListeningTo(el: dom.EventTarget): Unit = {
    require(el != null)
    for {
      et <- handlers.keys
      setOfQL <- handlers.get(et)
      ql <- setOfQL
    } {
      el.addEventListener(et, handler _, ql.capture)
    }
  }

  /**
   * Turn off listening for events for a specific eventType. Individual
   * handlers should be cancelled using the Cancelable returned from `on`.
   *
   * @param eventType The event type or None indicating all handlers for all event types.
   */
  def off(eventType: Option[String] = None): Delegate = {
    import vdom.OptionOps
    var removals: collection.mutable.Set[(String, QualifiedHandler)] = collection.mutable.Set.empty

    // Find removals.
    for {
      et <- handlers.keys
      setOfQL <- handlers.get(et)
      ql <- setOfQL
    } {
      if (Some(et) wildcardEq eventType) {
        removals += ((et, ql))
        setOfQL -= ql
      }
    }

    // Remove event listeners based on removals.
    for { r <- removals } root.foreach(_.removeEventListener(r._1, this.handler _, r._2.capture))

    this
  }

  /**
   * Add a handler for a specific event. The same handler can be added multiple times.
   *
   * @return A Cancelable used to cancel the listening of the handler.
   */
  def on(eventType: String,
    handler: Handler,
    matcher: Matcher = Matcher.MatchRoot,
    useCapture: Option[Boolean] = None): Cancelable = {

    val capture = useCapture.getOrElse(isDefaultCapture(eventType))

    // If root defined, add universal handler to root if this is the
    // first time that eventType has been found.
    if (!handlers.contains(eventType))
      root.foreach(_.addEventListener(eventType, this.handler _, capture))

    // Create new listeners set for a specific event by adding the new delegate.
    val qhandler = QualifiedHandler(handler, matcher, capture)
    val newHandlers = handlers.getOrElse(eventType, collection.mutable.Set.empty) + qhandler

    // Update the handler set.
    handlers += (eventType -> newHandlers)

    new Cancelable {
      private val _eventType = eventType
      private val _qhandler = qhandler
      def cancel(): Unit = {
        handlers.get(_eventType).foreach(_.remove(_qhandler))
        if (handlers.get(_eventType).map(_.size).getOrElse(0) > 0) {
          // no need to listen to this event type, no handlers
          root.foreach(_.removeEventListener(_eventType, self.handler _, capture))
        }
      }
      def delegate() = self
    }
  }
}
