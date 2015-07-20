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

/**
 * When processing events using a Delegate, Matcher is used to determine if the
 * delegate function should be called on the node in the traversal
 * from the node where the event occured to the Delegate's root.
 * Return true to allow a Delegate to process the current node in the
 * traversal.
 */
trait Matcher extends Function2[dom.EventTarget, dom.EventTarget, Boolean]

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
   * Match on the tage. Current node must be an Element.
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
 * is allowed to process the event. Return true if the propogation up
 * the tree should continue and false if it should stop.
 */
trait Handler extends Function2[dom.Event, dom.Node, Boolean]

object Handler {
  def apply(f: Function2[dom.Event, dom.Node, Boolean]) = new Handler {
    def apply(event: dom.Event, node: dom.Node) = f(event, node)
  }
  
  def apply(f: Function1[dom.Event, Boolean]) = new Handler { 
    def apply(event: dom.Event, node: dom.Node) = f(event)
  }
  
}

/**
 * A handler, a matcher and a capture flag. The matcher and capture flag
 * are used to qualify whether a handler is used in processing an event.
 * Capture refers to the capturing or bubbling phases of event processing.
 */
private[events] case class QualifiedHandler(handler: Handler, matcher: Matcher = Matcher.MatchRoot, capture: Boolean = false)

/**
 * Delegate all event calls on the root to registered handlers.
 * You can change the root object at any time and the handlers
 * are properly deregistered. A Delegate is immutable so adding
 * handlers or changing the root creates a copy.
 *
 * This is actually quite standad logic in java swing programs
 * using jgoodies.
 *
 * @see [UI EVents](http://www.w3.org/TR/DOM-Level-3-Events/#interface-EventListener)
 */
case class Delegate(private[events] var root: Option[dom.EventTarget] = None,
    private[events] val handlers: collection.mutable.Map[String, collection.mutable.Set[QualifiedHandler]] = collection.mutable.Map.empty) {

  /**
   * Construct with a specific root.
   */
  def this(root: dom.EventTarget) = this(Some(root))

  /**
   * Handle an event. This is the universal listener attached
   * to the root. A mechanism is in place to not process an event
   * when it crosses to different Delegate instances that may have
   * attached handlers up the tree.
   *
   * @see [eventPhase](https://developer.mozilla.org/en-US/docs/Web/API/Event/eventPhase)
   */
  @js.annotation.JSExport
  protected def handler(event: dom.Event): Unit = {

    // If a special marker is found, other instances of Delegate 
    // found up the chain should ignore this event as well.
    if (js.DynamicImplicits.truthValue(event.asInstanceOf[js.Dynamic].__DELEGATEIGNORE))
      return

    var target =
      event.target match {
        case d: dom.Node if (d.nodeType == 3) => d.parentNode
        case n@_ => n.asInstanceOf[dom.Node]
      }

    // Build a listener list to process based on the event type and phase...
    val phase = event.eventPhase
    val registeredHandlers = handlers.getOrElse(event.`type`, Set.empty).filter { qhandler =>
      if (!qhandler.capture && (phase == 2 || phase == 3)) true
      else if (qhandler.capture && (phase == 1 || phase == 2)) true
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
  private[events] val captureList = Seq("blur", "error", "focus", "load", "resize", "scroll")

  /**
   * Whether the event should by default, be processed in the capture phase or not.
   */
  protected def isDefaultCapture(eventName: String) = captureList.contains(eventName)

  /**
   * Apply this configured Delegate to the root and return a new Delegate.
   */
  def root(el: Option[dom.EventTarget]): Delegate = {
    root.foreach(stopListeningTo(_))
    root = el
    el.foreach(startListeningTo(_))
    this
  }

  protected def stopListeningTo(el: dom.EventTarget): Unit = {
    val stops = collection.mutable.ListBuffer[(String, QualifiedHandler)]()
    for {
      et <- handlers.keys
      setOfQL <- handlers.get(et)
      ql <- setOfQL
    } { stops += ((et, ql)) }
    stops.foreach { p => el.removeEventListener(p._1, handler _, p._2.capture) }
  }

  protected def startListeningTo(el: dom.EventTarget): Unit = {
    for {
      et <- handlers.keys
      setOfQL <- handlers.get(et)
      ql <- setOfQL
    } {
      el.addEventListener(et, handler _, ql.capture)
    }
  }

  /**
   * Turn off listening for events for a specific eventType, handler, matcher,
   * capture or some combination thereof. `None` implies wildcard for that
   * component and will match all values for that parameter.
   * Think of the parameters as a query by example model.
   *
   * @param eventType
   * @param handler
   * @param matcher
   * @param useCapture
   *
   */
  def off(eventType: Option[String] = None, handler: Option[Handler] = None,
    matcher: Option[Matcher] = Some(Matcher.MatchAll), useCapture: Option[Boolean] = None): Delegate = {
    import vdom.OptionOps
    var removals: collection.mutable.Set[(String, QualifiedHandler)] = collection.mutable.Set.empty

    // Find removals.
    for {
      et <- handlers.keys
      setOfQL <- handlers.get(et)
      ql <- setOfQL
    } {
      if ((Some(et) wildcardEq eventType) && (Some(ql.handler) wildcardEq handler) &&
        (Some(ql.matcher) wildcardEq matcher) && (Some(ql.capture) wildcardEq useCapture)) {
        removals += ((et, ql))
        setOfQL -= ql
      }
    }

    // Remove event listeners based on removals.
    for { r <- removals } root.foreach(_.removeEventListener(r._1, this.handler _, r._2.capture))

    this
  }

  /**
   * Add a handler for a specific event.
   */
  def on(eventType: String,
    handler: Handler,
    matcher: Matcher = Matcher.MatchRoot,
    useCapture: Option[Boolean] = None): Delegate = {

    val capture = useCapture.getOrElse(isDefaultCapture(eventType))

    // If root defined, add universal handler to root if this is the
    // first time that eventType has been found.
    if (!handlers.contains(eventType))
      root.foreach(_.addEventListener(eventType, this.handler _, capture))

    // Create new listeners set for a specific event by adding the new delegate.
    val newSet = handlers.getOrElse(eventType, collection.mutable.Set.empty) + QualifiedHandler(handler, matcher, capture)

    // Create the copy updating handlers.
    handlers += (eventType -> newSet)

    this
  }
}
